module Playwright.Connection.Handlers
  ( handlerLoop
  , registerDispatchHandler
  , registerEventHandler
  , registerScopedEventHandler
  , removeScopedEventHandlers
  , runEventHandlers
  , waitForDispatchedHandlers
  ) where

import Control.Concurrent
  ( myThreadId
  )
import Control.Concurrent.STM
  ( atomically
  , retry
  , throwSTM
  )
import Control.Concurrent.STM.TQueue
  ( readTQueue
  , writeTQueue
  )
import Control.Exception
  ( SomeException
  , catch
  , throwIO
  , try
  )
import Control.Monad
  ( forever
  , unless
  , when
  )
import Data.Aeson
  ( Value
  )
import Data.Foldable
  ( traverse_
  )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Playwright.Connection.Types
  ( Connection (connectionHandlerQueue)
  , modifySession
  , readSession
  )
import Playwright.EventHandler
  ( EventHandler (EventHandler, eventHandlerGuid, eventHandlerMethod, eventHandlerOnDispatch, eventHandlerRun, eventHandlerScope)
  , ListenerRemoval (RemoveWaiting)
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Session
  ( ScopedRuns (ScopedRuns, scopedRunsErrors, scopedRunsPending)
  , Session (sessionClosed, sessionEventHandlers, sessionHandlerThreads, sessionPendingHandlers, sessionScopedRuns)
  )

registerEventHandler :: Connection -> Guid -> MethodName -> (Value -> IO ()) -> IO ()
registerEventHandler conn guid method run =
  registerDispatchHandler conn guid method (\_ -> pure ()) run

-- | Registers a handler whose 'onDispatch' runs on the reader thread as the
-- event is read, so a handler it registers sees the events that follow.
registerDispatchHandler :: Connection -> Guid -> MethodName -> (Value -> IO ()) -> (Value -> IO ()) -> IO ()
registerDispatchHandler conn guid method onDispatch run =
  atomically $
    modifySession conn $ \session ->
      session {sessionEventHandlers = EventHandler guid method Nothing onDispatch run : sessionEventHandlers session}

-- | Registers a handler under a scope, so 'removeScopedEventHandlers' can
-- remove it later. The scope is the owner the listener belongs to, such as a
-- page.
registerScopedEventHandler :: Connection -> Guid -> Guid -> MethodName -> (Value -> IO ()) -> IO ()
registerScopedEventHandler conn scope guid method run =
  atomically $
    modifySession conn $ \session ->
      session
        { sessionEventHandlers =
            EventHandler guid method (Just scope) (\_ -> pure ()) run : sessionEventHandlers session
        }

-- | Removes every handler registered under the scope for the method. Runs
-- already queued keep going, but with 'RemoveWaiting' the call returns only
-- after they finish, and rethrows the first error one of them raised.
removeScopedEventHandlers :: Connection -> Guid -> MethodName -> ListenerRemoval -> IO ()
removeScopedEventHandlers conn scope method removal = do
  tid <- myThreadId
  let
    key = (scope, method)
    scopedHere handler = eventHandlerScope handler == Just scope && eventHandlerMethod handler == method
    -- Errors are collected from the removal onwards only, as upstream does.
    -- A waiting removal keeps its entry so the runs in flight can report to it.
    keepEntry :: ScopedRuns -> Maybe ScopedRuns
    keepEntry runs = Just runs {scopedRunsErrors = []}
    idleOrKeep :: ScopedRuns -> Maybe ScopedRuns
    idleOrKeep runs =
      if scopedRunsPending runs == 0
        then Nothing
        else keepEntry runs
  atomically $
    modifySession conn $ \session ->
      session
        { sessionEventHandlers = filter (not . scopedHere) (sessionEventHandlers session)
        , sessionScopedRuns =
            Map.update (if removal == RemoveWaiting then keepEntry else idleOrKeep) key (sessionScopedRuns session)
        }
  case removal of
    RemoveWaiting -> do
      errors <- atomically $ do
        session <- readSession conn
        case sessionClosed session of
          Just err ->
            throwSTM err
          Nothing ->
            pure ()
        let
          runs = Map.lookup key (sessionScopedRuns session)
          pending = maybe 0 scopedRunsPending runs
        when (pending > 0 && Set.notMember tid (sessionHandlerThreads session)) retry
        modifySession conn $ \s -> s {sessionScopedRuns = Map.delete key (sessionScopedRuns s)}
        pure (maybe [] scopedRunsErrors runs)
      case reverse errors of
        err : _ ->
          throwIO err
        [] ->
          pure ()
    _ ->
      pure ()

runEventHandlers :: Connection -> Guid -> MethodName -> Value -> IO ()
runEventHandlers conn guid method params = do
  session <- atomically (readSession conn)
  let
    matching =
      filter
        ( \handler ->
            eventHandlerGuid handler == guid && eventHandlerMethod handler == method
        )
        (sessionEventHandlers session)
  traverse_ (dispatchHandler conn params) matching

dispatchHandler :: Connection -> Value -> EventHandler -> IO ()
dispatchHandler conn params handler = do
  eventHandlerOnDispatch handler params
    `catch` (\(_ :: SomeException) -> pure ())
  enqueueHandler conn params handler

enqueueHandler :: Connection -> Value -> EventHandler -> IO ()
enqueueHandler conn params handler =
  atomically $ do
    modifySession conn $ \session ->
      session
        { sessionPendingHandlers =
            if countsForSend handler
              then sessionPendingHandlers session + 1
              else sessionPendingHandlers session
        , sessionScopedRuns =
            maybe
              (sessionScopedRuns session)
              (\key -> Map.alter (Just . addPending . maybe (ScopedRuns 0 []) id) key (sessionScopedRuns session))
              (scopedKey handler)
        }
    writeTQueue (connectionHandlerQueue conn) (runQueuedHandler conn params handler)
  where
    addPending runs = runs {scopedRunsPending = scopedRunsPending runs + 1}

scopedKey :: EventHandler -> Maybe (Guid, MethodName)
scopedKey handler =
  fmap (\scope -> (scope, eventHandlerMethod handler)) (eventHandlerScope handler)

countsForSend :: EventHandler -> Bool
countsForSend handler =
  eventHandlerMethod handler /= MethodName "route"

handlerLoop :: Connection -> IO ()
handlerLoop conn =
  forever $ do
    job <- atomically (readTQueue (connectionHandlerQueue conn))
    job

runQueuedHandler :: Connection -> Value -> EventHandler -> IO ()
runQueuedHandler conn params handler = do
  tid <- myThreadId
  atomically $
    modifySession conn $ \session ->
      session {sessionHandlerThreads = Set.insert tid (sessionHandlerThreads session)}
  result <- try (eventHandlerRun handler params)
  atomically $
    modifySession conn $ \session ->
      session
        { sessionHandlerThreads = Set.delete tid (sessionHandlerThreads session)
        , sessionPendingHandlers =
            if countsForSend handler
              then sessionPendingHandlers session - 1
              else sessionPendingHandlers session
        , sessionScopedRuns = finishScoped result (scopedKey handler) (sessionScopedRuns session)
        }

-- | Records a finished run against its scope. An error is kept for a waiting
-- removal, and a run whose scope has already been removed is no longer
-- counted.
finishScoped ::
  Either SomeException () ->
  Maybe (Guid, MethodName) ->
  Map.Map (Guid, MethodName) ScopedRuns ->
  Map.Map (Guid, MethodName) ScopedRuns
finishScoped result mkey runs =
  case mkey of
    Nothing ->
      runs
    Just key ->
      Map.adjust finish key runs
  where
    finish current =
      current
        { scopedRunsPending = scopedRunsPending current - 1
        , scopedRunsErrors =
            either (: scopedRunsErrors current) (const (scopedRunsErrors current)) result
        }

waitForDispatchedHandlers :: Connection -> IO ()
waitForDispatchedHandlers conn = do
  tid <- myThreadId
  atomically $ do
    session <- readSession conn
    case sessionClosed session of
      Just err ->
        throwSTM err
      Nothing ->
        unless (Set.member tid (sessionHandlerThreads session)) $ do
          when (sessionPendingHandlers session > 0) retry

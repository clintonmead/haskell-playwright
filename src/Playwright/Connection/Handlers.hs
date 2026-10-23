module Playwright.Connection.Handlers
  ( handlerLoop
  , registerDispatchHandler
  , registerEventHandler
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
  , finally
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
import Data.Set qualified as Set
import Playwright.Connection.Types
  ( Connection (connectionHandlerQueue)
  , modifySession
  , readSession
  )
import Playwright.EventHandler
  ( EventHandler (EventHandler, eventHandlerGuid, eventHandlerMethod, eventHandlerOnDispatch, eventHandlerRun)
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Session
  ( Session (sessionClosed, sessionEventHandlers, sessionHandlerThreads, sessionPendingHandlers)
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
      session {sessionEventHandlers = EventHandler guid method onDispatch run : sessionEventHandlers session}

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
    when (countsForSend handler) $
      modifySession conn $ \session ->
        session {sessionPendingHandlers = sessionPendingHandlers session + 1}
    writeTQueue (connectionHandlerQueue conn) (runQueuedHandler conn params handler)

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
  let
    cleanup :: IO ()
    cleanup =
      atomically $
        modifySession conn $ \session ->
          session
            { sessionHandlerThreads = Set.delete tid (sessionHandlerThreads session)
            , sessionPendingHandlers =
                if countsForSend handler
                  then sessionPendingHandlers session - 1
                  else sessionPendingHandlers session
            }
  eventHandlerRun handler params
    `catch` (\(_ :: SomeException) -> pure ())
    `finally` cleanup

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

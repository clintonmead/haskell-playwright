module Playwright.Connection.Handlers
  ( handlerLoop
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
  ( EventHandler (EventHandler, eventHandlerGuid, eventHandlerMethod, eventHandlerRun)
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.MethodName
  ( MethodName
  )
import Playwright.Session
  ( Session (sessionClosed, sessionEventHandlers, sessionHandlerThreads, sessionPendingHandlers)
  )

registerEventHandler :: Connection -> Guid -> MethodName -> (Value -> IO ()) -> IO ()
registerEventHandler conn guid method run =
  atomically $
    modifySession conn $ \session ->
      session {sessionEventHandlers = EventHandler guid method run : sessionEventHandlers session}

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
  traverse_ (enqueueHandler conn params) matching

enqueueHandler :: Connection -> Value -> EventHandler -> IO ()
enqueueHandler conn params handler =
  atomically $ do
    modifySession conn $ \session ->
      session {sessionPendingHandlers = sessionPendingHandlers session + 1}
    writeTQueue (connectionHandlerQueue conn) (runQueuedHandler conn params handler)

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
            , sessionPendingHandlers = sessionPendingHandlers session - 1
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

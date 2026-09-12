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
  , modifyTVar'
  , readTVar
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
  ( Connection (connectionClosed, connectionEventHandlers, connectionHandlerQueue, connectionHandlerThreads, connectionPendingHandlers)
  , EventHandler (EventHandler, eventHandlerGuid, eventHandlerMethod, eventHandlerRun)
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.MethodName
  ( MethodName
  )

registerEventHandler :: Connection -> Guid -> MethodName -> (Value -> IO ()) -> IO ()
registerEventHandler conn guid method run =
  atomically $
    modifyTVar' (connectionEventHandlers conn) (EventHandler guid method run :)

runEventHandlers :: Connection -> Guid -> MethodName -> Value -> IO ()
runEventHandlers conn guid method params = do
  handlers <- atomically (readTVar (connectionEventHandlers conn))
  let
    matching =
      filter
        ( \handler ->
            eventHandlerGuid handler == guid && eventHandlerMethod handler == method
        )
        handlers
  traverse_ (enqueueHandler conn params) matching

enqueueHandler :: Connection -> Value -> EventHandler -> IO ()
enqueueHandler conn params handler =
  atomically $ do
    modifyTVar' (connectionPendingHandlers conn) (+ 1)
    writeTQueue (connectionHandlerQueue conn) (runQueuedHandler conn params handler)

handlerLoop :: Connection -> IO ()
handlerLoop conn =
  forever $ do
    job <- atomically (readTQueue (connectionHandlerQueue conn))
    job

runQueuedHandler :: Connection -> Value -> EventHandler -> IO ()
runQueuedHandler conn params handler = do
  tid <- myThreadId
  atomically (modifyTVar' (connectionHandlerThreads conn) (Set.insert tid))
  let
    cleanup :: IO ()
    cleanup =
      atomically $ do
        modifyTVar' (connectionHandlerThreads conn) (Set.delete tid)
        modifyTVar' (connectionPendingHandlers conn) (subtract 1)
  eventHandlerRun handler params
    `catch` (\(_ :: SomeException) -> pure ())
    `finally` cleanup

waitForDispatchedHandlers :: Connection -> IO ()
waitForDispatchedHandlers conn = do
  tid <- myThreadId
  atomically $ do
    closed <- readTVar (connectionClosed conn)
    case closed of
      Just err ->
        throwSTM err
      Nothing -> do
        handlerThreads <- readTVar (connectionHandlerThreads conn)
        unless (Set.member tid handlerThreads) $ do
          pending <- readTVar (connectionPendingHandlers conn)
          when (pending > 0) retry

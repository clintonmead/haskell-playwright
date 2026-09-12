module Playwright.Connection.Wait
  ( deliverEvent
  , registerEventWaiter
  , registerFutureEventWaiter
  , waitForChannelEvent
  , waitForFutureChannelEvent
  , waitForFutureChannelEventUnlessClosed
  , waitForLifecycle
  , waitUntilRemote
  , waitUntilRequestSettled
  , waitUntilUrl
  ) where

import Control.Concurrent.STM
  ( STM
  , TMVar
  , atomically
  , modifyTVar'
  , newEmptyTMVarIO
  , putTMVar
  , readTVar
  , retry
  , takeTMVar
  , throwSTM
  , tryReadTMVar
  , writeTVar
  )
import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( unless
  , when
  )
import Data.Aeson
  ( Value
  )
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( isJust
  )
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Playwright.Connection.Types
  ( Connection (connectionClosed, connectionClosedGuids, connectionEventBuffer, connectionEventWaiters, connectionFinishedRequests, connectionObjects)
  , EventWaiter (EventWaiter, eventWaiterBox, eventWaiterGuid, eventWaiterMethod)
  )
import Playwright.Error
  ( targetClosedError
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.Json.Decode
  ( fromJsonValue
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( FrameInitializer (frameInitializerLoadStates, frameInitializerUrl)
  , LifecycleEvent
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
  )
import Playwright.Url
  ( Url
  )

deliverEvent :: Connection -> Guid -> MethodName -> Value -> IO ()
deliverEvent conn guid method params =
  atomically $ do
    waiters <- readTVar (connectionEventWaiters conn)
    case break matches waiters of
      (before, waiter : after) -> do
        writeTVar (connectionEventWaiters conn) (before <> after)
        putTMVar (eventWaiterBox waiter) params
      (_, []) ->
        modifyTVar' (connectionEventBuffer conn) $
          Map.insertWith (flip (<>)) (guid, method) (Seq.singleton params)
  where
    matches waiter =
      eventWaiterGuid waiter == guid && eventWaiterMethod waiter == method

registerEventWaiter :: Connection -> Guid -> MethodName -> IO (TMVar Value)
registerEventWaiter conn guid method = do
  box <- newEmptyTMVarIO
  atomically $ do
    buffer <- readTVar (connectionEventBuffer conn)
    case Seq.viewl (Map.findWithDefault Seq.empty (guid, method) buffer) of
      params Seq.:< rest -> do
        writeTVar (connectionEventBuffer conn) (Map.insert (guid, method) rest buffer)
        putTMVar box params
      Seq.EmptyL ->
        modifyTVar' (connectionEventWaiters conn) (EventWaiter guid method box :)
  pure box

waitForChannelEvent :: Connection -> Guid -> MethodName -> IO Value
waitForChannelEvent conn guid method = do
  box <- registerEventWaiter conn guid method
  atomically (takeTMVar box)

registerFutureEventWaiter :: Connection -> Guid -> MethodName -> IO (TMVar Value)
registerFutureEventWaiter conn guid method = do
  box <- newEmptyTMVarIO
  atomically $
    modifyTVar' (connectionEventWaiters conn) (EventWaiter guid method box :)
  pure box

waitForFutureChannelEvent :: Connection -> Guid -> MethodName -> IO Value
waitForFutureChannelEvent conn guid method = do
  box <- registerFutureEventWaiter conn guid method
  atomically (takeTMVar box)

unregisterWaiterSTM :: Connection -> TMVar Value -> STM ()
unregisterWaiterSTM conn box =
  modifyTVar' (connectionEventWaiters conn) (filter (\waiter -> eventWaiterBox waiter /= box))

waitForFutureChannelEventUnlessClosed :: Connection -> Guid -> MethodName -> IO Value
waitForFutureChannelEventUnlessClosed conn guid method = do
  already <- atomically (Set.member guid <$> readTVar (connectionClosedGuids conn))
  when already (throwIO targetClosedError)
  box <- registerFutureEventWaiter conn guid method
  closeBox <- registerEventWaiter conn guid (MethodName "close")
  atomically $ do
    closedConn <- readTVar (connectionClosed conn)
    case closedConn of
      Just err -> do
        unregisterWaiterSTM conn box
        unregisterWaiterSTM conn closeBox
        throwSTM err
      Nothing -> do
        closedSet <- readTVar (connectionClosedGuids conn)
        eventParams <- tryReadTMVar box
        closeFired <- tryReadTMVar closeBox
        case eventParams of
          Just params -> do
            unregisterWaiterSTM conn box
            unregisterWaiterSTM conn closeBox
            pure params
          Nothing
            | Set.member guid closedSet || isJust closeFired -> do
                unregisterWaiterSTM conn box
                unregisterWaiterSTM conn closeBox
                throwSTM targetClosedError
            | otherwise ->
                retry

waitForLifecycle :: Connection -> Guid -> LifecycleEvent -> IO ()
waitForLifecycle conn guid wanted =
  atomically (waitForLifecycleSTM conn guid wanted)

waitForLifecycleSTM :: Connection -> Guid -> LifecycleEvent -> STM ()
waitForLifecycleSTM conn guid wanted = do
  closed <- readTVar (connectionClosed conn)
  case closed of
    Just err ->
      throwSTM err
    Nothing -> do
      objects <- readTVar (connectionObjects conn)
      case Map.lookup guid objects of
        Nothing ->
          retry
        Just obj ->
          case fromJsonValue (remoteInitializer obj) of
            Left err ->
              throwSTM err
            Right initializer ->
              unless (wanted `elem` frameInitializerLoadStates initializer) retry

waitUntilUrl :: Connection -> Guid -> (Url -> Bool) -> IO ()
waitUntilUrl conn guid predicate =
  atomically (waitUntilUrlSTM conn guid predicate)

waitUntilRemote :: Connection -> Guid -> (RemoteObject -> STM a) -> IO a
waitUntilRemote conn guid useObject =
  atomically $ do
    closed <- readTVar (connectionClosed conn)
    case closed of
      Just err ->
        throwSTM err
      Nothing -> do
        objects <- readTVar (connectionObjects conn)
        case Map.lookup guid objects of
          Nothing ->
            retry
          Just obj ->
            useObject obj

waitUntilUrlSTM :: Connection -> Guid -> (Url -> Bool) -> STM ()
waitUntilUrlSTM conn guid predicate = do
  closed <- readTVar (connectionClosed conn)
  case closed of
    Just err ->
      throwSTM err
    Nothing -> do
      objects <- readTVar (connectionObjects conn)
      case Map.lookup guid objects of
        Nothing ->
          retry
        Just obj ->
          case fromJsonValue (remoteInitializer obj) of
            Left err ->
              throwSTM err
            Right initializer ->
              unless (predicate (frameInitializerUrl initializer)) retry

waitUntilRequestSettled :: Connection -> Guid -> IO ()
waitUntilRequestSettled conn guid =
  atomically $ do
    closed <- readTVar (connectionClosed conn)
    case closed of
      Just err ->
        throwSTM err
      Nothing -> do
        finished <- readTVar (connectionFinishedRequests conn)
        unless (Set.member guid finished) retry

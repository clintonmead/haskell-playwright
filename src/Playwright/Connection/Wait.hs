module Playwright.Connection.Wait
  ( deliverEvent
  , registerEventWaiter
  , registerFutureEventWaiter
  , waitForChannelEvent
  , waitForChannelEventUnlessClosed
  , waitForFutureChannelEvent
  , waitForFutureChannelEventUnlessClosed
  , waitForLifecycle
  , waitForWaiter
  , waitForWaiterUnlessClosed
  , waitUntilRemote
  , waitUntilRequestSettled
  , waitUntilUrl
  ) where

import Control.Concurrent.STM
  ( STM
  , atomically
  , retry
  , throwSTM
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
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Playwright.Connection.Types
  ( Connection
  , readSession
  , stateSession
  , withSession
  )
import Playwright.Error
  ( PlaywrightError
  , targetClosedError
  )
import Playwright.EventWaiter
  ( EventWaiter (EventWaiter, eventWaiterValue)
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.Json.Decode
  ( throwDecode
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
import Playwright.Session
  ( Session (sessionClosed, sessionClosedGuids, sessionEventBuffer, sessionEventWaiters, sessionFinishedRequests, sessionNextWaitId, sessionObjects, sessionWaiters)
  , removeWaiter
  )
import Playwright.Url
  ( Url
  )
import Playwright.WaitId
  ( WaitId
  )

deliverEvent :: Connection -> Guid -> MethodName -> Value -> IO ()
deliverEvent conn guid method params =
  atomically $
    stateSession conn $ \session ->
      case filter (waitingFor session) (sessionEventWaiters session) of
        [] ->
          bufferEvent session
        waitIds ->
          ( ()
          , session
              { sessionWaiters = foldl' (fillWaiter params) (sessionWaiters session) waitIds
              , sessionEventWaiters = filter (`notElem` waitIds) (sessionEventWaiters session)
              }
          )
  where
    waitingFor session waitId =
      case Map.lookup waitId (sessionWaiters session) of
        Just (EventWaiter waiterGuid waiterMethod Nothing) ->
          waiterGuid == guid && waiterMethod == method
        _ ->
          False
    bufferEvent session =
      ( ()
      , session
          { sessionEventBuffer =
              Map.insertWith
                (flip (<>))
                (guid, method)
                (Seq.singleton params)
                (sessionEventBuffer session)
          }
      )
    fillWaiter :: Value -> Map.Map WaitId EventWaiter -> WaitId -> Map.Map WaitId EventWaiter
    fillWaiter value waiters waitId =
      case Map.lookup waitId waiters of
        Nothing ->
          waiters
        Just waiter ->
          Map.insert waitId waiter {eventWaiterValue = Just value} waiters

registerEventWaiter :: Connection -> Guid -> MethodName -> IO WaitId
registerEventWaiter conn guid method =
  atomically $
    stateSession conn $ \session ->
      let
        waitId = sessionNextWaitId session
        buffer = Map.findWithDefault Seq.empty (guid, method) (sessionEventBuffer session)
      in
        case Seq.viewl buffer of
          params Seq.:< rest ->
            ( waitId
            , session
                { sessionNextWaitId = succ waitId
                , sessionWaiters =
                    Map.insert waitId (EventWaiter guid method (Just params)) (sessionWaiters session)
                , sessionEventBuffer = Map.insert (guid, method) rest (sessionEventBuffer session)
                }
            )
          Seq.EmptyL ->
            insertWaitingWaiter session waitId guid method

registerFutureEventWaiter :: Connection -> Guid -> MethodName -> IO WaitId
registerFutureEventWaiter conn guid method =
  atomically $
    stateSession conn $ \session ->
      insertWaitingWaiter session (sessionNextWaitId session) guid method

insertWaitingWaiter :: Session -> WaitId -> Guid -> MethodName -> (WaitId, Session)
insertWaitingWaiter session waitId guid method =
  ( waitId
  , session
      { sessionNextWaitId = succ waitId
      , sessionWaiters =
          Map.insert waitId (EventWaiter guid method Nothing) (sessionWaiters session)
      , sessionEventWaiters = waitId : sessionEventWaiters session
      }
  )

waitForWaiter :: Connection -> WaitId -> IO Value
waitForWaiter conn waitId = do
  outcome <-
    atomically $
      withSession conn $ \session ->
        case waiterValue session waitId of
          Just params ->
            pure (Right params, removeWaiter waitId session)
          Nothing ->
            case sessionClosed session of
              Just err ->
                pure (Left err, removeWaiter waitId session)
              Nothing ->
                retry
  throwOutcome outcome

waitForWaiterUnlessClosed :: Connection -> WaitId -> Guid -> IO Value
waitForWaiterUnlessClosed conn waitId closedGuid = do
  already <- atomically (Set.member closedGuid . sessionClosedGuids <$> readSession conn)
  when already (throwIO targetClosedError)
  closeId <- registerEventWaiter conn closedGuid (MethodName "close")
  outcome <-
    atomically $
      withSession conn $ \session ->
        case sessionClosed session of
          Just err ->
            pure (Left err, removeWaiter waitId (removeWaiter closeId session))
          Nothing ->
            case waiterValue session waitId of
              Just params ->
                pure (Right params, removeWaiter waitId (removeWaiter closeId session))
              Nothing
                | Set.member closedGuid (sessionClosedGuids session) || waiterFilled session closeId ->
                    pure (Left targetClosedError, removeWaiter waitId (removeWaiter closeId session))
                | otherwise ->
                    retry
  throwOutcome outcome

waitForChannelEvent :: Connection -> Guid -> MethodName -> IO Value
waitForChannelEvent conn guid method = do
  waitId <- registerEventWaiter conn guid method
  waitForWaiter conn waitId

waitForChannelEventUnlessClosed :: Connection -> Guid -> MethodName -> Guid -> IO Value
waitForChannelEventUnlessClosed conn eventGuid method closedGuid = do
  waitId <- registerEventWaiter conn eventGuid method
  waitForWaiterUnlessClosed conn waitId closedGuid

waitForFutureChannelEvent :: Connection -> Guid -> MethodName -> IO Value
waitForFutureChannelEvent conn guid method = do
  waitId <- registerFutureEventWaiter conn guid method
  waitForWaiter conn waitId

waitForFutureChannelEventUnlessClosed :: Connection -> Guid -> MethodName -> IO Value
waitForFutureChannelEventUnlessClosed conn guid method = do
  already <- atomically (Set.member guid . sessionClosedGuids <$> readSession conn)
  when already (throwIO targetClosedError)
  waitId <- registerFutureEventWaiter conn guid method
  closeId <- registerEventWaiter conn guid (MethodName "close")
  outcome <-
    atomically $
      withSession conn $ \session ->
        case sessionClosed session of
          Just err ->
            pure (Left err, removeWaiter waitId (removeWaiter closeId session))
          Nothing ->
            case waiterValue session waitId of
              Just params ->
                pure (Right params, removeWaiter waitId (removeWaiter closeId session))
              Nothing
                | Set.member guid (sessionClosedGuids session) || waiterFilled session closeId ->
                    pure (Left targetClosedError, removeWaiter waitId (removeWaiter closeId session))
                | otherwise ->
                    retry
  throwOutcome outcome

throwOutcome :: Either PlaywrightError a -> IO a
throwOutcome outcome =
  case outcome of
    Left err ->
      throwIO err
    Right value ->
      pure value

waiterValue :: Session -> WaitId -> Maybe Value
waiterValue session waitId =
  case Map.lookup waitId (sessionWaiters session) of
    Just (EventWaiter _ _ (Just params)) ->
      Just params
    _ ->
      Nothing

waiterFilled :: Session -> WaitId -> Bool
waiterFilled session waitId =
  case waiterValue session waitId of
    Just _ ->
      True
    Nothing ->
      False

waitForLifecycle :: Connection -> Guid -> LifecycleEvent -> IO ()
waitForLifecycle conn guid wanted =
  atomically (waitForLifecycleSTM conn guid wanted)

waitForLifecycleSTM :: Connection -> Guid -> LifecycleEvent -> STM ()
waitForLifecycleSTM conn guid wanted = do
  session <- readSession conn
  case sessionClosed session of
    Just err ->
      throwSTM err
    Nothing ->
      case Map.lookup guid (sessionObjects session) of
        Nothing ->
          retry
        Just obj -> do
          initializer <- throwDecode (remoteInitializer obj)
          unless (wanted `elem` frameInitializerLoadStates initializer) retry

waitUntilUrl :: Connection -> Guid -> (Url -> Bool) -> IO ()
waitUntilUrl conn guid predicate =
  atomically (waitUntilUrlSTM conn guid predicate)

waitUntilRemote :: Connection -> Guid -> (RemoteObject -> STM a) -> IO a
waitUntilRemote conn guid useObject =
  atomically $ do
    session <- readSession conn
    case sessionClosed session of
      Just err ->
        throwSTM err
      Nothing ->
        case Map.lookup guid (sessionObjects session) of
          Nothing ->
            retry
          Just obj ->
            useObject obj

waitUntilUrlSTM :: Connection -> Guid -> (Url -> Bool) -> STM ()
waitUntilUrlSTM conn guid predicate = do
  session <- readSession conn
  case sessionClosed session of
    Just err ->
      throwSTM err
    Nothing ->
      case Map.lookup guid (sessionObjects session) of
        Nothing ->
          retry
        Just obj -> do
          initializer <- throwDecode (remoteInitializer obj)
          unless (predicate (frameInitializerUrl initializer)) retry

waitUntilRequestSettled :: Connection -> Guid -> IO ()
waitUntilRequestSettled conn guid =
  atomically $ do
    session <- readSession conn
    case sessionClosed session of
      Just err ->
        throwSTM err
      Nothing ->
        if Set.member guid (sessionFinishedRequests session)
          then pure ()
          else case Map.lookup guid (sessionObjects session) of
            Nothing ->
              throwSTM targetClosedError
            Just _ ->
              retry

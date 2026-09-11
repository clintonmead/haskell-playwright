module Playwright.Connection
  ( Connection
  , withConnection
  , sendCommand
  , guidsOfType
  , lookupObject
  , registerEventWaiter
  , waitForChannelEvent
  , waitForLifecycle
  , waitUntilUrl
  , closedGuids
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , object
  , requiredField'
  , (.=)
  )
import Control.Concurrent.Async
  ( withAsync
  )
import Control.Concurrent.STM
  ( STM
  , TMVar
  , TVar
  , atomically
  , modifyTVar'
  , newEmptyTMVarIO
  , newTVarIO
  , putTMVar
  , readTVar
  , retry
  , takeTMVar
  , throwSTM
  , writeTVar
  )
import Control.Exception
  ( finally
  , throwIO
  )
import Control.Monad
  ( unless
  )
import Data.Aeson
  ( FromJSON
  , Result (Error, Success)
  , ToJSON (toJSON)
  , Value (Object)
  , encode
  , fromJSON
  )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson qualified as Aeson
  ( object
  , (.=)
  )
import Data.ByteString.Lazy
  ( toStrict
  )
import Data.Map.Strict
  ( Map
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set
  )
import Data.Set qualified as Set
import Data.Sequence
  ( Seq
  )
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Playwright.Error
  ( PlaywrightError (ConnectionClosed, DecodeFailure, MissingObject, ProtocolRpc, UnexpectedResponse)
  , ProtocolFailure
  )
import Playwright.Guid
  ( Guid
  , rootGuid
  )
import Playwright.Incoming
  ( CreateParams (createGuid, createInitializer, createType)
  , Incoming (IncomingAdopt, IncomingCreate, IncomingDispose, IncomingEvent, IncomingResponse)
  , parseIncoming
  )
import Playwright.Json.Decode
  ( fromJsonValue
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( ChannelType (ChannelTypeRoot)
  , FrameInitializer (frameInitializerLoadStates, frameInitializerUrl)
  , FrameLoadstateEvent (frameLoadstateEventAdd, frameLoadstateEventRemove)
  , FrameNavigatedEvent (frameNavigatedEventName, frameNavigatedEventUrl)
  , LifecycleEvent
  )
import Playwright.RemoteObject
  ( RemoteObject (RemoteObject, remoteGuid, remoteInitializer, remoteParent, remoteType)
  )
import Playwright.RequestId
  ( RequestId (RequestId)
  )
import Playwright.Transport
  ( Transport (closeTransport, recvPayload, sendPayload)
  )
import Playwright.Url
  ( Url
  )

data Connection = Connection
  { connectionTransport :: Transport
  , connectionObjects :: TVar (Map Guid RemoteObject)
  , connectionCallbacks :: TVar (Map RequestId (TMVar (Either PlaywrightError Value)))
  , connectionNextId :: TVar RequestId
  , connectionClosed :: TVar (Maybe PlaywrightError)
  , connectionEventWaiters :: TVar [EventWaiter]
  , connectionEventBuffer :: TVar (Map (Guid, MethodName) (Seq Value))
  , connectionClosedGuids :: TVar (Set Guid)
  }

data EventWaiter = EventWaiter
  { eventWaiterGuid :: Guid
  , eventWaiterMethod :: MethodName
  , eventWaiterBox :: TMVar Value
  }

data OutgoingCall = OutgoingCall
  { outgoingCallId :: RequestId
  , outgoingCallGuid :: Guid
  , outgoingCallMethod :: MethodName
  , outgoingCallParams :: Value
  , outgoingCallMetadata :: Value
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec OutgoingCall)

instance HasCodec OutgoingCall where
  codec =
    object "OutgoingCall" $
      OutgoingCall
        <$> requiredField' "id" .= outgoingCallId
        <*> requiredField' "guid" .= outgoingCallGuid
        <*> requiredField' "method" .= outgoingCallMethod
        <*> requiredField' "params" .= outgoingCallParams
        <*> requiredField' "metadata" .= outgoingCallMetadata

withConnection :: Transport -> (Connection -> IO a) -> IO a
withConnection transport action = do
  objects <- newTVarIO (Map.singleton rootGuid rootObject)
  callbacks <- newTVarIO Map.empty
  nextId <- newTVarIO (RequestId 1)
  closed <- newTVarIO Nothing
  eventWaiters <- newTVarIO []
  eventBuffer <- newTVarIO Map.empty
  closedGuidsVar <- newTVarIO Set.empty
  let
    conn :: Connection
    conn =
      Connection
        { connectionTransport = transport
        , connectionObjects = objects
        , connectionCallbacks = callbacks
        , connectionNextId = nextId
        , connectionClosed = closed
        , connectionEventWaiters = eventWaiters
        , connectionEventBuffer = eventBuffer
        , connectionClosedGuids = closedGuidsVar
        }
  withAsync (readerLoop conn) $ \_ ->
    action conn `finally` closeTransport transport

rootObject :: RemoteObject
rootObject =
  RemoteObject
    { remoteType = ChannelTypeRoot
    , remoteGuid = rootGuid
    , remoteParent = rootGuid
    , remoteInitializer = Aeson.object []
    }

sendCommand ::
  (ToJSON params, FromJSON result) =>
  Connection ->
  Guid ->
  MethodName ->
  params ->
  IO result
sendCommand conn guid method params = do
  closed <- atomically (readTVar (connectionClosed conn))
  case closed of
    Just err ->
      throwIO err
    Nothing -> do
      reqId <- nextRequestId conn
      waiter <- newEmptyTMVarIO
      atomically $
        modifyTVar' (connectionCallbacks conn) (Map.insert reqId waiter)
      let
        outgoing :: OutgoingCall
        outgoing =
          OutgoingCall
            { outgoingCallId = reqId
            , outgoingCallGuid = guid
            , outgoingCallMethod = method
            , outgoingCallParams = toJSON params
            , outgoingCallMetadata = Aeson.object ["internal" Aeson..= False]
            }
      sendPayload (connectionTransport conn) (toStrict (encode outgoing))
      result <- atomically (takeTMVar waiter)
      case result of
        Left err ->
          throwIO err
        Right value ->
          case fromJSON value of
            Error err ->
              throwIO (DecodeFailure (Text.pack err))
            Success decoded ->
              pure decoded

lookupObject :: Connection -> Guid -> IO RemoteObject
lookupObject conn guid = do
  objects <- atomically (readTVar (connectionObjects conn))
  case Map.lookup guid objects of
    Nothing ->
      throwIO (MissingObject guid)
    Just obj ->
      pure obj

guidsOfType :: Connection -> ChannelType -> IO [Guid]
guidsOfType conn ty = do
  objects <- atomically (readTVar (connectionObjects conn))
  pure
    [ remoteGuid obj
    | obj <- Map.elems objects
    , remoteType obj == ty
    ]

closedGuids :: Connection -> IO (Set Guid)
closedGuids conn =
  atomically (readTVar (connectionClosedGuids conn))

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

nextRequestId :: Connection -> IO RequestId
nextRequestId conn =
  atomically $ do
    current <- readTVar (connectionNextId conn)
    writeTVar (connectionNextId conn) (succ current)
    pure current

readerLoop :: Connection -> IO ()
readerLoop conn = do
  payload <- recvPayload (connectionTransport conn)
  case payload of
    Nothing ->
      markClosed conn ConnectionClosed
    Just bytes ->
      case parseIncoming bytes of
        Left err ->
          markClosed conn err
        Right incoming -> do
          dispatch conn incoming
          readerLoop conn

dispatch :: Connection -> Incoming -> IO ()
dispatch conn incoming =
  case incoming of
    IncomingResponse reqId result ->
      completeCallback conn reqId result
    IncomingCreate parent params ->
      atomically $ do
        modifyTVar' (connectionClosedGuids conn) (Set.delete (createGuid params))
        modifyTVar' (connectionObjects conn) $
          Map.insert
            (createGuid params)
            RemoteObject
              { remoteType = createType params
              , remoteGuid = createGuid params
              , remoteParent = parent
              , remoteInitializer = createInitializer params
              }
    IncomingAdopt parent child ->
      atomically $
        modifyTVar' (connectionObjects conn) $
          Map.adjust (\obj -> obj {remoteParent = parent}) child
    IncomingDispose guid ->
      atomically $
        modifyTVar' (connectionObjects conn) (disposeTree guid)
    IncomingEvent guid method params ->
      handleEvent conn guid method params

handleEvent :: Connection -> Guid -> MethodName -> Value -> IO ()
handleEvent conn guid method params = do
  case method of
    MethodName "navigated" ->
      applyNavigated conn guid params
    MethodName "loadstate" ->
      applyLoadstate conn guid params
    MethodName "close" ->
      atomically (modifyTVar' (connectionClosedGuids conn) (Set.insert guid))
    _ ->
      pure ()
  deliverEvent conn guid method params

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

waitUntilUrl :: Connection -> Guid -> (Url -> Bool) -> IO ()
waitUntilUrl conn guid predicate =
  atomically (waitUntilUrlSTM conn guid predicate)

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

applyLoadstate :: Connection -> Guid -> Value -> IO ()
applyLoadstate conn guid params = do
  event <- either throwIO pure (fromJsonValue params)
  atomically $ do
    objects <- readTVar (connectionObjects conn)
    case Map.lookup guid objects of
      Nothing ->
        pure ()
      Just obj ->
        case fromJsonValue (remoteInitializer obj) of
          Left err ->
            throwSTM err
          Right initializer ->
            writeTVar (connectionObjects conn) $
              Map.insert
                guid
                obj
                  { remoteInitializer =
                      toJSON
                        ( initializer
                            { frameInitializerLoadStates =
                                updateLoadStates event (frameInitializerLoadStates initializer)
                            }
                        )
                  }
                objects

updateLoadStates :: FrameLoadstateEvent -> [LifecycleEvent] -> [LifecycleEvent]
updateLoadStates event states =
  let
    afterRemove :: [LifecycleEvent]
    afterRemove =
      case frameLoadstateEventRemove event of
        Nothing ->
          states
        Just removed ->
          filter (/= removed) states
  in
    case frameLoadstateEventAdd event of
      Nothing ->
        afterRemove
      Just added ->
        if added `elem` afterRemove
          then afterRemove
          else afterRemove <> [added]

applyNavigated :: Connection -> Guid -> Value -> IO ()
applyNavigated conn guid params = do
  event <- either throwIO pure (fromJsonValue params)
  atomically $
    modifyTVar' (connectionObjects conn) $
      Map.adjust
        (setInitializerNavigation (frameNavigatedEventName event) (frameNavigatedEventUrl event))
        guid

setInitializerNavigation :: Text.Text -> Url -> RemoteObject -> RemoteObject
setInitializerNavigation name url obj =
  obj {remoteInitializer = patchNavigation name url (remoteInitializer obj)}

patchNavigation :: Text.Text -> Url -> Value -> Value
patchNavigation name url value =
  case value of
    Object keys ->
      Object
        ( KeyMap.insert
            (Key.fromText "name")
            (toJSON name)
            (KeyMap.insert (Key.fromText "url") (toJSON url) keys)
        )
    _ ->
      value

completeCallback ::
  Connection ->
  RequestId ->
  Either ProtocolFailure Value ->
  IO ()
completeCallback conn reqId result = do
  waiter <- atomically $ do
    callbacks <- readTVar (connectionCallbacks conn)
    writeTVar (connectionCallbacks conn) (Map.delete reqId callbacks)
    pure (Map.lookup reqId callbacks)
  case waiter of
    Nothing ->
      throwIO (UnexpectedResponse reqId)
    Just tmvar ->
      atomically $
        putTMVar tmvar $
          case result of
            Left failure ->
              Left (ProtocolRpc failure)
            Right value ->
              Right value

disposeTree :: Guid -> Map Guid RemoteObject -> Map Guid RemoteObject
disposeTree guid objects =
  let
    children :: [Guid]
    children =
      Map.keys (Map.filter (\obj -> remoteParent obj == guid && remoteGuid obj /= guid) objects)
    withoutSelf :: Map Guid RemoteObject
    withoutSelf = Map.delete guid objects
  in
    foldr disposeTree withoutSelf children

markClosed :: Connection -> PlaywrightError -> IO ()
markClosed conn err = do
  waiters <- atomically $ do
    writeTVar (connectionClosed conn) (Just err)
    callbacks <- readTVar (connectionCallbacks conn)
    writeTVar (connectionCallbacks conn) Map.empty
    pure (Map.elems callbacks)
  mapM_ (\waiter -> atomically (putTMVar waiter (Left err))) waiters

module Playwright.Connection
  ( Connection
  , withConnection
  , sendCommand
  , lookupObject
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
  ( TMVar
  , TVar
  , atomically
  , modifyTVar'
  , newEmptyTMVarIO
  , newTVarIO
  , putTMVar
  , readTVar
  , takeTMVar
  , writeTVar
  )
import Control.Exception
  ( finally
  , throwIO
  )
import Data.Aeson
  ( FromJSON
  , Result (Error, Success)
  , ToJSON (toJSON)
  , Value
  , encode
  , fromJSON
  )
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
import Playwright.MethodName
  ( MethodName
  )
import Playwright.Protocol
  ( ChannelType (ChannelTypeRoot)
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

data Connection = Connection
  { connectionTransport :: Transport
  , connectionObjects :: TVar (Map Guid RemoteObject)
  , connectionCallbacks :: TVar (Map RequestId (TMVar (Either PlaywrightError Value)))
  , connectionNextId :: TVar RequestId
  , connectionClosed :: TVar (Maybe PlaywrightError)
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
  let
    conn :: Connection
    conn =
      Connection
        { connectionTransport = transport
        , connectionObjects = objects
        , connectionCallbacks = callbacks
        , connectionNextId = nextId
        , connectionClosed = closed
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
      atomically $
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
    IncomingEvent _ _ _ ->
      pure ()

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

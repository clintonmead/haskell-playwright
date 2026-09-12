module Playwright.Connection
  ( Connection
  , withConnection
  , sendCommand
  , guidsOfType
  , lookupObject
  , registerEventHandler
  , registerEventWaiter
  , waitForChannelEvent
  , waitForFutureChannelEvent
  , waitForFutureChannelEventUnlessClosed
  , registerFutureEventWaiter
  , rememberBrowserContext
  , lookupOwnedContextGuids
  , lookupContextBrowserGuid
  , markPageOwnsContext
  , pageOwnsContext
  , markContextHasOwnerPage
  , contextHasOwnerPage
  , rememberBrowserType
  , lookupBrowserTypeGuid
  , waitForLifecycle
  , waitUntilUrl
  , closedGuids
  , pushRouteHandler
  , removeRouteHandlers
  , clearRouteHandlers
  , routeHandlersFor
  , lookupRequestOverrides
  , writeRequestOverrides
  , lookupContextBaseUrl
  , writeContextBaseUrl
  , lookupTimeoutDefaults
  , lookupMainFramePage
  , registerMainFramePage
  , lookupRequestFailure
  , waitUntilRequestSettled
  , setTimeoutDefault
  , setTimeoutNavigation
  , waitUntilRemote
  , lookupSelectorEngines
  , addSelectorEngine
  , StoredLocatorHandler (..)
  , markLocatorHandlersListening
  , putLocatorHandler
  , beginLocatorHandler
  , deleteLocatorHandler
  , locatorHandlerUidsMatching
  ) where

import Control.Concurrent
  ( ThreadId
  )
import Control.Concurrent.Async
  ( withAsync
  )
import Control.Concurrent.STM
  ( atomically
  , modifyTVar'
  , newEmptyTMVarIO
  , newTVarIO
  , putTMVar
  , readTVar
  , takeTMVar
  , throwSTM
  , writeTVar
  )
import Control.Concurrent.STM.TQueue
  ( newTQueueIO
  )
import Control.Exception
  ( finally
  , throwIO
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
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Playwright.Connection.Handlers
  ( handlerLoop
  , registerEventHandler
  , runEventHandlers
  , waitForDispatchedHandlers
  )
import Playwright.Connection.Store
  ( addSelectorEngine
  , beginLocatorHandler
  , clearRouteHandlers
  , closedGuids
  , contextHasOwnerPage
  , deleteLocatorHandler
  , guidsOfType
  , locatorHandlerUidsMatching
  , lookupBrowserTypeGuid
  , lookupContextBaseUrl
  , lookupContextBrowserGuid
  , lookupMainFramePage
  , lookupObject
  , lookupOwnedContextGuids
  , lookupRequestFailure
  , lookupRequestOverrides
  , lookupSelectorEngines
  , lookupTimeoutDefaults
  , markLocatorHandlersListening
  , markContextHasOwnerPage
  , markPageOwnsContext
  , pageOwnsContext
  , pushRouteHandler
  , putLocatorHandler
  , registerMainFramePage
  , rememberBrowserContext
  , rememberBrowserType
  , removeRouteHandlers
  , routeHandlersFor
  , setTimeoutDefault
  , setTimeoutNavigation
  , writeContextBaseUrl
  , writeRequestOverrides
  )
import Playwright.Connection.Types
  ( Connection (Connection, connectionBrowserContexts, connectionBrowserTypes, connectionCallbacks, connectionClosed, connectionClosedGuids, connectionContextBaseUrls, connectionContextBrowsers, connectionContextsWithOwnerPage, connectionEventBuffer, connectionEventHandlers, connectionEventWaiters, connectionFinishedRequests, connectionHandlerQueue, connectionHandlerThreads, connectionLocatorHandlers, connectionLocatorListening, connectionMainFramePages, connectionNextId, connectionObjects, connectionPagesOwningContext, connectionPendingHandlers, connectionRequestFailures, connectionRequestOverrides, connectionRouteHandlers, connectionRouteListening, connectionSelectorEngines, connectionTimeoutDefaults, connectionTransport)
  , OutgoingCall (OutgoingCall, outgoingCallGuid, outgoingCallId, outgoingCallMetadata, outgoingCallMethod, outgoingCallParams)
  , StoredLocatorHandler (StoredLocatorHandler, storedLocatorHandlerFrame, storedLocatorHandlerRun, storedLocatorHandlerSelector, storedLocatorHandlerTimes)
  )
import Playwright.Connection.Wait
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
  )
import Playwright.Error
  ( PlaywrightError (ConnectionClosed, DecodeFailure, ProtocolRpc, UnexpectedResponse)
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
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.Json.Decode
  ( fromJsonValue
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( BrowserContextRequestFailedEvent (browserContextRequestFailedEventFailureText, browserContextRequestFailedEventRequest)
  , BrowserContextRequestFinishedEvent (browserContextRequestFinishedEventRequest)
  , ChannelType (ChannelTypeRoot)
  , FrameInitializer (frameInitializerLoadStates)
  , FrameLoadstateEvent (frameLoadstateEventAdd, frameLoadstateEventRemove)
  , FrameNavigatedEvent (frameNavigatedEventName, frameNavigatedEventUrl)
  , JSHandlePreviewUpdatedEvent (jSHandlePreviewUpdatedEventPreview)
  , LifecycleEvent
  , SelectorEngine
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

withConnection :: Transport -> (Connection -> IO a) -> IO a
withConnection transport action = do
  objects <- newTVarIO (Map.singleton rootGuid rootObject)
  callbacks <- newTVarIO Map.empty
  nextId <- newTVarIO (RequestId 1)
  closed <- newTVarIO Nothing
  eventWaiters <- newTVarIO []
  eventBuffer <- newTVarIO Map.empty
  closedGuidsVar <- newTVarIO Set.empty
  eventHandlers <- newTVarIO []
  routeHandlers <- newTVarIO Map.empty
  routeListening <- newTVarIO Set.empty
  requestOverrides <- newTVarIO Map.empty
  contextBaseUrls <- newTVarIO Map.empty
  browserContexts <- newTVarIO (Map.empty :: Map Guid [Guid])
  contextBrowsers <- newTVarIO (Map.empty :: Map Guid Guid)
  pagesOwningContext <- newTVarIO (Set.empty :: Set Guid)
  contextsWithOwnerPage <- newTVarIO (Set.empty :: Set Guid)
  browserTypes <- newTVarIO (Map.empty :: Map Guid Guid)
  selectorEngines <- newTVarIO ([] :: [SelectorEngine])
  timeoutDefaults <- newTVarIO Map.empty
  mainFramePages <- newTVarIO Map.empty
  finishedRequests <- newTVarIO (Set.empty :: Set Guid)
  requestFailures <- newTVarIO (Map.empty :: Map Guid Text)
  pendingHandlers <- newTVarIO (0 :: Int)
  handlerThreads <- newTVarIO (Set.empty :: Set ThreadId)
  handlerQueue <- newTQueueIO
  locatorHandlers <- newTVarIO (Map.empty :: Map Guid (Map Int StoredLocatorHandler))
  locatorListening <- newTVarIO Set.empty
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
        , connectionEventHandlers = eventHandlers
        , connectionRouteHandlers = routeHandlers
        , connectionRouteListening = routeListening
        , connectionRequestOverrides = requestOverrides
        , connectionContextBaseUrls = contextBaseUrls
        , connectionBrowserContexts = browserContexts
        , connectionContextBrowsers = contextBrowsers
        , connectionPagesOwningContext = pagesOwningContext
        , connectionContextsWithOwnerPage = contextsWithOwnerPage
        , connectionBrowserTypes = browserTypes
        , connectionSelectorEngines = selectorEngines
        , connectionTimeoutDefaults = timeoutDefaults
        , connectionMainFramePages = mainFramePages
        , connectionFinishedRequests = finishedRequests
        , connectionRequestFailures = requestFailures
        , connectionPendingHandlers = pendingHandlers
        , connectionHandlerThreads = handlerThreads
        , connectionHandlerQueue = handlerQueue
        , connectionLocatorHandlers = locatorHandlers
        , connectionLocatorListening = locatorListening
        }
  withAsync (readerLoop conn) $ \_ ->
    withAsync (handlerLoop conn) $ \_ ->
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
      waitForDispatchedHandlers conn
      case result of
        Left err ->
          throwIO err
        Right value ->
          case fromJSON value of
            Error err ->
              throwIO (DecodeFailure (Text.pack err))
            Success decoded ->
              pure decoded

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
    MethodName "previewUpdated" ->
      applyPreviewUpdated conn guid params
    MethodName "requestFailed" ->
      applyRequestFailed conn params
    MethodName "requestFinished" ->
      applyRequestFinished conn params
    MethodName "close" ->
      atomically $ do
        modifyTVar' (connectionClosedGuids conn) (Set.insert guid)
        modifyTVar' (connectionBrowserContexts conn) (Map.map (filter (/= guid)))
    _ ->
      pure ()
  deliverEvent conn guid method params
  runEventHandlers conn guid method params

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

applyPreviewUpdated :: Connection -> Guid -> Value -> IO ()
applyPreviewUpdated conn guid params = do
  event <- either throwIO pure (fromJsonValue params)
  atomically $
    modifyTVar' (connectionObjects conn) $
      Map.adjust
        (setInitializerPreview (jSHandlePreviewUpdatedEventPreview event))
        guid

applyRequestFailed :: Connection -> Value -> IO ()
applyRequestFailed conn params = do
  event <- either throwIO pure (fromJsonValue params)
  atomically $ do
    modifyTVar' (connectionFinishedRequests conn) (Set.insert (channelRefGuid (browserContextRequestFailedEventRequest event)))
    case browserContextRequestFailedEventFailureText event of
      Nothing ->
        pure ()
      Just text ->
        modifyTVar' (connectionRequestFailures conn) (Map.insert (channelRefGuid (browserContextRequestFailedEventRequest event)) text)

applyRequestFinished :: Connection -> Value -> IO ()
applyRequestFinished conn params = do
  event <- either throwIO pure (fromJsonValue params)
  atomically $
    modifyTVar' (connectionFinishedRequests conn) (Set.insert (channelRefGuid (browserContextRequestFinishedEventRequest event)))

setInitializerPreview :: Text -> RemoteObject -> RemoteObject
setInitializerPreview preview obj =
  obj {remoteInitializer = patchPreview preview (remoteInitializer obj)}

patchPreview :: Text -> Value -> Value
patchPreview preview value =
  case value of
    Object keys ->
      Object (KeyMap.insert (Key.fromText "preview") (toJSON preview) keys)
    _ ->
      value

setInitializerNavigation :: Text -> Url -> RemoteObject -> RemoteObject
setInitializerNavigation name url obj =
  obj {remoteInitializer = patchNavigation name url (remoteInitializer obj)}

patchNavigation :: Text -> Url -> Value -> Value
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

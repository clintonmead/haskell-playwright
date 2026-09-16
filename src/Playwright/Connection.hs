module Playwright.Connection
  ( Connection
  , withConnection
  , sendCommand
  , guidsOfType
  , lookupObject
  , registerEventHandler
  , registerEventWaiter
  , waitForChannelEvent
  , waitForChannelEventUnlessClosed
  , waitForFutureChannelEvent
  , waitForFutureChannelEventUnlessClosed
  , registerFutureEventWaiter
  , waitForWaiter
  , waitForWaiterUnlessClosed
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
  , lookupRequestEndTiming
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
  , storePlaywrightGuids
  , lookupPlaywrightGuid
  , lookupChromiumGuid
  , lookupFirefoxGuid
  , lookupWebkitGuid
  ) where

import Control.Concurrent.Async
  ( withAsync
  )
import Control.Concurrent.STM
  ( STM
  , atomically
  , newTVarIO
  , retry
  , throwSTM
  )
import Control.Concurrent.STM.TQueue
  ( newTQueueIO
  )
import Control.Exception
  ( finally
  , throwIO
  )
import Control.Monad
  ( unless
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Aeson
  ( FromJSON
  , ToJSON (toJSON)
  , Value (Object)
  , encode
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
import Data.Scientific
  ( Scientific
  )
import Data.Set
  ( Set
  )
import Data.Set qualified as Set
import Data.Text
  ( Text
  )
import Playwright.Connection.Handlers
  ( handlerLoop
  , runEventHandlers
  , waitForDispatchedHandlers
  )
import Playwright.Connection.Handlers qualified as Handlers
import Playwright.Connection.Store qualified as Store
import Playwright.Connection.Types
  ( Connection (Connection, connectionHandlerQueue, connectionSession, connectionTransport)
  , OutgoingCall (OutgoingCall, outgoingCallGuid, outgoingCallId, outgoingCallMetadata, outgoingCallMethod, outgoingCallParams)
  , modifySession
  , readSession
  , stateSession
  , withSession
  )
import Playwright.Connection.Wait
  ( deliverEvent
  )
import Playwright.Connection.Wait qualified as Wait
import Playwright.Error
  ( PlaywrightError (ConnectionClosed, DecodeFailure, ProtocolRpc, UnexpectedResponse)
  , ProtocolFailure
  )
import Playwright.Guid
  ( Guid
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
  ( throwDecode
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Monad
  ( Playwright
  , askConnection
  , runPlaywrightWith
  )
import Playwright.Protocol
  ( BrowserContextRequestFailedEvent (browserContextRequestFailedEventFailureText, browserContextRequestFailedEventRequest, browserContextRequestFailedEventResponseEndTiming)
  , BrowserContextRequestFinishedEvent (browserContextRequestFinishedEventRequest, browserContextRequestFinishedEventResponseEndTiming)
  , ChannelType
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
  ( RequestId
  )
import Playwright.RequestOverrides
  ( RequestOverrides
  )
import Playwright.Session
  ( Session (sessionBrowserContexts, sessionChromiumGuid, sessionClosed, sessionClosedGuids, sessionFinishedRequests, sessionFirefoxGuid, sessionNextId, sessionObjects, sessionPending, sessionPlaywrightGuid, sessionRequestEndTiming, sessionRequestFailures, sessionResults, sessionWebkitGuid)
  , emptySession
  )
import Playwright.StoredLocatorHandler
  ( StoredLocatorHandler (StoredLocatorHandler, storedLocatorHandlerFrame, storedLocatorHandlerRun, storedLocatorHandlerSelector, storedLocatorHandlerTimes)
  )
import Playwright.Timeout
  ( Timeout
  )
import Playwright.TimeoutDefaults
  ( TimeoutDefaults
  )
import Playwright.Transport
  ( Transport (closeTransport, recvPayload, sendPayload)
  )
import Playwright.Url
  ( Url
  )
import Playwright.UrlGlob
  ( UrlGlob
  )
import Playwright.WaitId
  ( WaitId
  )

withConnection :: Transport -> (Connection -> IO a) -> IO a
withConnection transport action = do
  session <- newTVarIO emptySession
  handlerQueue <- newTQueueIO
  let
    conn :: Connection
    conn =
      Connection
        { connectionTransport = transport
        , connectionSession = session
        , connectionHandlerQueue = handlerQueue
        }
  withAsync (readerLoop conn) $ \_ ->
    withAsync (handlerLoop conn) $ \_ ->
      action conn `finally` closeTransport transport

sendCommand ::
  (ToJSON params, FromJSON result) =>
  Guid ->
  MethodName ->
  params ->
  Playwright result
sendCommand guid method params = do
  conn <- askConnection
  liftIO (sendCommandIO conn guid method params)

inConnection :: (Connection -> IO a) -> Playwright a
inConnection action = do
  conn <- askConnection
  liftIO (action conn)

lookupObject :: Guid -> Playwright RemoteObject
lookupObject guid =
  inConnection (`Store.lookupObject` guid)

guidsOfType :: ChannelType -> Playwright [Guid]
guidsOfType ty =
  inConnection (`Store.guidsOfType` ty)

closedGuids :: Playwright (Set Guid)
closedGuids =
  inConnection Store.closedGuids

pushRouteHandler :: Guid -> UrlGlob -> (ChannelRef -> IO Bool) -> Playwright Bool
pushRouteHandler guid glob run =
  inConnection $ \conn -> Store.pushRouteHandler conn guid glob run

removeRouteHandlers :: Guid -> UrlGlob -> Playwright ()
removeRouteHandlers guid glob =
  inConnection $ \conn -> Store.removeRouteHandlers conn guid glob

clearRouteHandlers :: Guid -> Playwright ()
clearRouteHandlers guid =
  inConnection (`Store.clearRouteHandlers` guid)

routeHandlersFor :: Guid -> Playwright [(UrlGlob, ChannelRef -> IO Bool)]
routeHandlersFor guid =
  inConnection (`Store.routeHandlersFor` guid)

lookupRequestOverrides :: Guid -> Playwright RequestOverrides
lookupRequestOverrides guid =
  inConnection (`Store.lookupRequestOverrides` guid)

writeRequestOverrides :: Guid -> RequestOverrides -> Playwright ()
writeRequestOverrides guid overrides =
  inConnection $ \conn -> Store.writeRequestOverrides conn guid overrides

lookupContextBaseUrl :: Guid -> Playwright (Maybe Url)
lookupContextBaseUrl guid =
  inConnection (`Store.lookupContextBaseUrl` guid)

writeContextBaseUrl :: Guid -> Url -> Playwright ()
writeContextBaseUrl guid url =
  inConnection $ \conn -> Store.writeContextBaseUrl conn guid url

rememberBrowserContext :: Guid -> Guid -> Playwright ()
rememberBrowserContext browserGuid contextGuid =
  inConnection $ \conn -> Store.rememberBrowserContext conn browserGuid contextGuid

lookupOwnedContextGuids :: Guid -> Playwright [Guid]
lookupOwnedContextGuids browserGuid =
  inConnection (`Store.lookupOwnedContextGuids` browserGuid)

lookupContextBrowserGuid :: Guid -> Playwright (Maybe Guid)
lookupContextBrowserGuid contextGuid =
  inConnection (`Store.lookupContextBrowserGuid` contextGuid)

markPageOwnsContext :: Guid -> Playwright ()
markPageOwnsContext pageGuid =
  inConnection (`Store.markPageOwnsContext` pageGuid)

pageOwnsContext :: Guid -> Playwright Bool
pageOwnsContext pageGuid =
  inConnection (`Store.pageOwnsContext` pageGuid)

markContextHasOwnerPage :: Guid -> Playwright ()
markContextHasOwnerPage contextGuid =
  inConnection (`Store.markContextHasOwnerPage` contextGuid)

contextHasOwnerPage :: Guid -> Playwright Bool
contextHasOwnerPage contextGuid =
  inConnection (`Store.contextHasOwnerPage` contextGuid)

rememberBrowserType :: Guid -> Guid -> Playwright ()
rememberBrowserType browserGuid browserTypeGuid =
  inConnection $ \conn -> Store.rememberBrowserType conn browserGuid browserTypeGuid

lookupBrowserTypeGuid :: Guid -> Playwright (Maybe Guid)
lookupBrowserTypeGuid browserGuid =
  inConnection (`Store.lookupBrowserTypeGuid` browserGuid)

lookupSelectorEngines :: Playwright [SelectorEngine]
lookupSelectorEngines =
  inConnection Store.lookupSelectorEngines

addSelectorEngine :: SelectorEngine -> Playwright ()
addSelectorEngine engine =
  inConnection (`Store.addSelectorEngine` engine)

markLocatorHandlersListening :: Guid -> Playwright Bool
markLocatorHandlersListening guid =
  inConnection (`Store.markLocatorHandlersListening` guid)

putLocatorHandler :: Guid -> Int -> StoredLocatorHandler -> Playwright ()
putLocatorHandler pageGuid uid stored =
  inConnection $ \conn -> Store.putLocatorHandler conn pageGuid uid stored

beginLocatorHandler :: Guid -> Int -> Playwright (Maybe StoredLocatorHandler, Bool)
beginLocatorHandler pageGuid uid =
  inConnection $ \conn -> Store.beginLocatorHandler conn pageGuid uid

deleteLocatorHandler :: Guid -> Int -> Playwright ()
deleteLocatorHandler pageGuid uid =
  inConnection $ \conn -> Store.deleteLocatorHandler conn pageGuid uid

locatorHandlerUidsMatching :: Guid -> Text -> Guid -> Playwright [Int]
locatorHandlerUidsMatching pageGuid selector frameGuid =
  inConnection $ \conn -> Store.locatorHandlerUidsMatching conn pageGuid selector frameGuid

lookupTimeoutDefaults :: Guid -> Playwright TimeoutDefaults
lookupTimeoutDefaults guid =
  inConnection (`Store.lookupTimeoutDefaults` guid)

setTimeoutDefault :: Guid -> Maybe Timeout -> Playwright ()
setTimeoutDefault guid value =
  inConnection $ \conn -> Store.setTimeoutDefault conn guid value

setTimeoutNavigation :: Guid -> Maybe Timeout -> Playwright ()
setTimeoutNavigation guid value =
  inConnection $ \conn -> Store.setTimeoutNavigation conn guid value

registerMainFramePage :: Guid -> Guid -> Playwright ()
registerMainFramePage frameGuid pageGuid =
  inConnection $ \conn -> Store.registerMainFramePage conn frameGuid pageGuid

lookupMainFramePage :: Guid -> Playwright (Maybe Guid)
lookupMainFramePage frameGuid =
  inConnection (`Store.lookupMainFramePage` frameGuid)

lookupRequestFailure :: Guid -> Playwright (Maybe Text)
lookupRequestFailure guid =
  inConnection (`Store.lookupRequestFailure` guid)

lookupRequestEndTiming :: Guid -> Playwright (Maybe Scientific)
lookupRequestEndTiming guid =
  inConnection (`Store.lookupRequestEndTiming` guid)

registerEventWaiter :: Guid -> MethodName -> Playwright WaitId
registerEventWaiter guid method =
  inConnection $ \conn -> Wait.registerEventWaiter conn guid method

registerFutureEventWaiter :: Guid -> MethodName -> Playwright WaitId
registerFutureEventWaiter guid method =
  inConnection $ \conn -> Wait.registerFutureEventWaiter conn guid method

waitForWaiter :: WaitId -> Playwright Value
waitForWaiter waitId =
  inConnection (`Wait.waitForWaiter` waitId)

waitForWaiterUnlessClosed :: WaitId -> Guid -> Playwright Value
waitForWaiterUnlessClosed waitId closedGuid =
  inConnection $ \conn -> Wait.waitForWaiterUnlessClosed conn waitId closedGuid

waitForChannelEvent :: Guid -> MethodName -> Playwright Value
waitForChannelEvent guid method =
  inConnection $ \conn -> Wait.waitForChannelEvent conn guid method

waitForChannelEventUnlessClosed :: Guid -> MethodName -> Guid -> Playwright Value
waitForChannelEventUnlessClosed eventGuid method closedGuid =
  inConnection $ \conn -> Wait.waitForChannelEventUnlessClosed conn eventGuid method closedGuid

waitForFutureChannelEvent :: Guid -> MethodName -> Playwright Value
waitForFutureChannelEvent guid method =
  inConnection $ \conn -> Wait.waitForFutureChannelEvent conn guid method

waitForFutureChannelEventUnlessClosed :: Guid -> MethodName -> Playwright Value
waitForFutureChannelEventUnlessClosed guid method =
  inConnection $ \conn -> Wait.waitForFutureChannelEventUnlessClosed conn guid method

waitForLifecycle :: Guid -> LifecycleEvent -> Playwright ()
waitForLifecycle guid wanted =
  inConnection $ \conn -> Wait.waitForLifecycle conn guid wanted

waitUntilUrl :: Guid -> (Url -> Bool) -> Playwright ()
waitUntilUrl guid predicate =
  inConnection $ \conn -> Wait.waitUntilUrl conn guid predicate

waitUntilRemote :: Guid -> (RemoteObject -> STM a) -> Playwright a
waitUntilRemote guid useObject =
  inConnection $ \conn -> Wait.waitUntilRemote conn guid useObject

waitUntilRequestSettled :: Guid -> Playwright ()
waitUntilRequestSettled guid =
  inConnection (`Wait.waitUntilRequestSettled` guid)

registerEventHandler :: Guid -> MethodName -> (Value -> Playwright ()) -> Playwright ()
registerEventHandler guid method run = do
  conn <- askConnection
  liftIO $
    Handlers.registerEventHandler conn guid method $ \params ->
      runPlaywrightWith conn (run params)

storePlaywrightGuids :: Guid -> Guid -> Guid -> Guid -> Playwright ()
storePlaywrightGuids playwrightGuid chromiumGuid firefoxGuid webkitGuid =
  inConnection $ \conn ->
    atomically $
      modifySession conn $ \session ->
        session
          { sessionPlaywrightGuid = Just playwrightGuid
          , sessionChromiumGuid = Just chromiumGuid
          , sessionFirefoxGuid = Just firefoxGuid
          , sessionWebkitGuid = Just webkitGuid
          }

lookupPlaywrightGuid :: Playwright Guid
lookupPlaywrightGuid =
  requireSessionGuid "Playwright" sessionPlaywrightGuid

lookupChromiumGuid :: Playwright Guid
lookupChromiumGuid =
  requireSessionGuid "chromium" sessionChromiumGuid

lookupFirefoxGuid :: Playwright Guid
lookupFirefoxGuid =
  requireSessionGuid "firefox" sessionFirefoxGuid

lookupWebkitGuid :: Playwright Guid
lookupWebkitGuid =
  requireSessionGuid "webkit" sessionWebkitGuid

requireSessionGuid :: Text -> (Session -> Maybe Guid) -> Playwright Guid
requireSessionGuid label field = do
  session <- inConnection (atomically . readSession)
  case field session of
    Just guid ->
      pure guid
    Nothing ->
      liftIO (throwIO (DecodeFailure (label <> " was not initialized")))

sendCommandIO ::
  (ToJSON params, FromJSON result) =>
  Connection ->
  Guid ->
  MethodName ->
  params ->
  IO result
sendCommandIO conn guid method params = do
  closed <- atomically (sessionClosed <$> readSession conn)
  case closed of
    Just err ->
      throwIO err
    Nothing -> do
      reqId <-
        atomically $
          stateSession conn $ \session ->
            let
              nextId = sessionNextId session
            in
              ( nextId
              , session
                  { sessionNextId = succ nextId
                  , sessionPending = Set.insert nextId (sessionPending session)
                  }
              )
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
      result <-
        atomically $
          withSession conn $ \session ->
            case Map.lookup reqId (sessionResults session) of
              Just value ->
                pure (value, session {sessionResults = Map.delete reqId (sessionResults session)})
              Nothing ->
                case sessionClosed session of
                  Just err ->
                    throwSTM err
                  Nothing ->
                    retry
      waitForDispatchedHandlers conn
      case result of
        Left err ->
          throwIO err
        Right value ->
          throwDecode value

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
        modifySession conn $ \session ->
          session
            { sessionClosedGuids = Set.delete (createGuid params) (sessionClosedGuids session)
            , sessionObjects =
                Map.insert
                  (createGuid params)
                  RemoteObject
                    { remoteType = createType params
                    , remoteGuid = createGuid params
                    , remoteParent = parent
                    , remoteInitializer = createInitializer params
                    }
                  (sessionObjects session)
            }
    IncomingAdopt parent child ->
      atomically $
        modifySession conn $ \session ->
          session
            { sessionObjects =
                Map.adjust (\obj -> obj {remoteParent = parent}) child (sessionObjects session)
            }
    IncomingDispose guid ->
      atomically $
        modifySession conn $ \session ->
          session {sessionObjects = disposeTree guid (sessionObjects session)}
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
      atomically $
        modifySession conn $ \session ->
          session
            { sessionClosedGuids = Set.insert guid (sessionClosedGuids session)
            , sessionBrowserContexts = Map.map (filter (/= guid)) (sessionBrowserContexts session)
            }
    _ ->
      pure ()
  deliverEvent conn guid method params
  runEventHandlers conn guid method params

applyLoadstate :: Connection -> Guid -> Value -> IO ()
applyLoadstate conn guid params = do
  event <- throwDecode params
  atomically $
    withSession conn $ \session ->
      case Map.lookup guid (sessionObjects session) of
        Nothing ->
          pure ((), session)
        Just obj -> do
          initializer <- throwDecode (remoteInitializer obj)
          pure
            ( ()
            , session
                { sessionObjects =
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
                      (sessionObjects session)
                }
            )

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
  event <- throwDecode params
  atomically $
    modifySession conn $ \session ->
      session
        { sessionObjects =
            Map.adjust
              (setInitializerNavigation (frameNavigatedEventName event) (frameNavigatedEventUrl event))
              guid
              (sessionObjects session)
        }

applyPreviewUpdated :: Connection -> Guid -> Value -> IO ()
applyPreviewUpdated conn guid params = do
  event <- throwDecode params
  atomically $
    modifySession conn $ \session ->
      session
        { sessionObjects =
            Map.adjust
              (setInitializerPreview (jSHandlePreviewUpdatedEventPreview event))
              guid
              (sessionObjects session)
        }

applyRequestFailed :: Connection -> Value -> IO ()
applyRequestFailed conn params = do
  event <- throwDecode params
  atomically $
    modifySession conn $ \session ->
      let
        requestGuid = channelRefGuid (browserContextRequestFailedEventRequest event)
        finished = Set.insert requestGuid (sessionFinishedRequests session)
        failures =
          case browserContextRequestFailedEventFailureText event of
            Nothing ->
              sessionRequestFailures session
            Just text ->
              Map.insert requestGuid text (sessionRequestFailures session)
        endTiming =
          Map.insert
            requestGuid
            (browserContextRequestFailedEventResponseEndTiming event)
            (sessionRequestEndTiming session)
      in
        session
          { sessionFinishedRequests = finished
          , sessionRequestEndTiming = endTiming
          , sessionRequestFailures = failures
          }

applyRequestFinished :: Connection -> Value -> IO ()
applyRequestFinished conn params = do
  event <- throwDecode params
  atomically $
    modifySession conn $ \session ->
      let
        requestGuid = channelRefGuid (browserContextRequestFinishedEventRequest event)
      in
        session
          { sessionFinishedRequests =
              Set.insert requestGuid (sessionFinishedRequests session)
          , sessionRequestEndTiming =
              Map.insert
                requestGuid
                (browserContextRequestFinishedEventResponseEndTiming event)
                (sessionRequestEndTiming session)
          }

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
  found <-
    atomically $
      withSession conn $ \session ->
        if Set.member reqId (sessionPending session)
          then
            let
              decoded =
                case result of
                  Left failure ->
                    Left (ProtocolRpc failure)
                  Right value ->
                    Right value
            in
              pure
                ( True
                , session
                    { sessionPending = Set.delete reqId (sessionPending session)
                    , sessionResults = Map.insert reqId decoded (sessionResults session)
                    }
                )
          else
            pure (False, session)
  unless found (throwIO (UnexpectedResponse reqId))

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
markClosed conn err =
  atomically $
    modifySession conn $ \session ->
      session {sessionClosed = Just err}

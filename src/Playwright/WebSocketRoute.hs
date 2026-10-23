module Playwright.WebSocketRoute
  ( WebSocketRoute
  , WebSocketRouteServer
  , routeWebSocket
  , contextRouteWebSocket
  , webSocketRouteUrl
  , webSocketRouteProtocols
  , webSocketRouteOnMessage
  , webSocketRouteOnClose
  , webSocketRouteSend
  , webSocketRouteClose
  , webSocketRouteConnectToServer
  , webSocketRouteServerOnMessage
  , webSocketRouteServerOnClose
  , webSocketRouteServerSend
  , webSocketRouteServerClose
  ) where

import Control.Monad
  ( unless
  , when
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Aeson
  ( Value
  )
import Data.ByteString.Base64 qualified as Base64
import Data.IORef
  ( IORef
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.List
  ( find
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Data.Text.Encoding
  ( decodeUtf8
  , encodeUtf8
  )
import Playwright.BrowserContext
  ( BrowserContext (browserContextChannel)
  )
import Playwright.Channel
  ( Channel (Channel, channelGuid)
  , WebSocketRouteTag
  , send
  )
import Playwright.Connection
  ( lookupContextBaseUrl
  , lookupObject
  , pushWebSocketRouteHandler
  , registerEventHandler
  , webSocketRouteHandlersFor
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure
      ( ProtocolFailure
      , protocolFailureMessage
      , protocolFailureName
      , protocolFailureStack
      )
  , ignoreTargetClosed
  )
import Playwright.Guid
  ( Guid
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
import Playwright.Page
  ( Page (pageChannel, pageContextChannel)
  )
import Playwright.Protocol
  ( BrowserContextSetWebSocketInterceptionPatternsParamsPatterns (browserContextSetWebSocketInterceptionPatternsParamsPatternsGlob, browserContextSetWebSocketInterceptionPatternsParamsPatternsRegexFlags, browserContextSetWebSocketInterceptionPatternsParamsPatternsRegexSource)
  , BrowserContextSetWebSocketInterceptionPatternsResult
  , BrowserContextWebSocketRouteEvent (browserContextWebSocketRouteEventWebSocketRoute)
  , PageSetWebSocketInterceptionPatternsParamsPatterns (pageSetWebSocketInterceptionPatternsParamsPatternsGlob, pageSetWebSocketInterceptionPatternsParamsPatternsRegexFlags, pageSetWebSocketInterceptionPatternsParamsPatternsRegexSource)
  , PageSetWebSocketInterceptionPatternsResult
  , PageWebSocketRouteEvent (pageWebSocketRouteEventWebSocketRoute)
  , WebSocketRouteClosePageEvent (webSocketRouteClosePageEventCode, webSocketRouteClosePageEventReason, webSocketRouteClosePageEventWasClean)
  , WebSocketRouteClosePageParams (webSocketRouteClosePageParamsCode, webSocketRouteClosePageParamsReason)
  , WebSocketRouteCloseServerEvent (webSocketRouteCloseServerEventCode, webSocketRouteCloseServerEventReason, webSocketRouteCloseServerEventWasClean)
  , WebSocketRouteCloseServerParams (webSocketRouteCloseServerParamsCode, webSocketRouteCloseServerParamsReason)
  , WebSocketRouteConnectResult
  , WebSocketRouteEnsureOpenedResult
  , WebSocketRouteInitializer (webSocketRouteInitializerProtocols, webSocketRouteInitializerUrl)
  , WebSocketRouteMessageFromPageEvent (webSocketRouteMessageFromPageEventIsBase64, webSocketRouteMessageFromPageEventMessage)
  , WebSocketRouteMessageFromServerEvent (webSocketRouteMessageFromServerEventIsBase64, webSocketRouteMessageFromServerEventMessage)
  , WebSocketRouteSendToPageResult
  , WebSocketRouteSendToServerResult
  , WebSocketRouteCloseServerResult
  , WebSocketRouteClosePageResult
  , mkBrowserContextSetWebSocketInterceptionPatternsParams
  , mkBrowserContextSetWebSocketInterceptionPatternsParamsPatterns
  , mkPageSetWebSocketInterceptionPatternsParams
  , mkPageSetWebSocketInterceptionPatternsParamsPatterns
  , mkWebSocketRouteCloseServerParams
  , mkWebSocketRouteClosePageParams
  , mkWebSocketRouteConnectParams
  , mkWebSocketRouteEnsureOpenedParams
  , mkWebSocketRouteSendToPageParams
  , mkWebSocketRouteSendToServerParams
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
  )
import Playwright.Url
  ( Url
  , urlToText
  )
import Playwright.UrlMatch
  ( UrlMatch (UrlMatchGlob, UrlMatchPredicate, UrlMatchRegex)
  , matchesUrlMatch
  , resolveUrlMatch
  )
import Playwright.WebSocketFrame
  ( WebSocketFrame (WebSocketBinary, WebSocketText)
  )
import UnliftIO.Exception
  ( throwIO
  )

-- | A routed WebSocket as seen from the page. The handlers are kept in IORefs
-- captured by the event callbacks registered for this route's guid.
data WebSocketRoute = WebSocketRoute
  { routeChannel :: Channel WebSocketRouteTag
  , routeUrl :: Url
  , routeProtocols :: [Text]
  , routeState :: RouteState
  }

data RouteState = RouteState
  { stateConnected :: IORef Bool
  , statePageMessage :: IORef (Maybe (WebSocketFrame -> Playwright ()))
  , statePageClose :: IORef (Maybe (Maybe Int -> Maybe Text -> Playwright ()))
  , stateServerMessage :: IORef (Maybe (WebSocketFrame -> Playwright ()))
  , stateServerClose :: IORef (Maybe (Maybe Int -> Maybe Text -> Playwright ()))
  }

-- | The server side of a route, returned by 'webSocketRouteConnectToServer'.
newtype WebSocketRouteServer = WebSocketRouteServer WebSocketRoute

routeWebSocket :: Page -> UrlMatch -> (WebSocketRoute -> Playwright ()) -> Playwright ()
routeWebSocket page glob handler = do
  let
    guid = channelGuid (pageChannel page)
  first <- installHandler guid glob handler
  when first (registerEventHandler guid (MethodName "webSocketRoute") (dispatchPageRoute page))
  handlers <- webSocketRouteHandlersFor guid
  _ :: PageSetWebSocketInterceptionPatternsResult <-
    send
      (pageChannel page)
      (MethodName "setWebSocketInterceptionPatterns")
      (mkPageSetWebSocketInterceptionPatternsParams (fmap (pagePattern . fst) handlers))
  pure ()

contextRouteWebSocket :: BrowserContext -> UrlMatch -> (WebSocketRoute -> Playwright ()) -> Playwright ()
contextRouteWebSocket ctx glob handler = do
  let
    guid = channelGuid (browserContextChannel ctx)
  first <- installHandler guid glob handler
  when first (registerEventHandler guid (MethodName "webSocketRoute") (dispatchContextRoute ctx))
  handlers <- webSocketRouteHandlersFor guid
  _ :: BrowserContextSetWebSocketInterceptionPatternsResult <-
    send
      (browserContextChannel ctx)
      (MethodName "setWebSocketInterceptionPatterns")
      (mkBrowserContextSetWebSocketInterceptionPatternsParams (fmap (contextPattern . fst) handlers))
  pure ()

webSocketRouteUrl :: WebSocketRoute -> Text
webSocketRouteUrl route =
  urlToText (routeUrl route)

webSocketRouteProtocols :: WebSocketRoute -> [Text]
webSocketRouteProtocols =
  routeProtocols

webSocketRouteOnMessage :: WebSocketRoute -> (WebSocketFrame -> Playwright ()) -> Playwright ()
webSocketRouteOnMessage route handler =
  liftIO (writeIORef (statePageMessage (routeState route)) (Just handler))

webSocketRouteOnClose :: WebSocketRoute -> (Maybe Int -> Maybe Text -> Playwright ()) -> Playwright ()
webSocketRouteOnClose route handler =
  liftIO (writeIORef (statePageClose (routeState route)) (Just handler))

webSocketRouteSend :: WebSocketRoute -> WebSocketFrame -> Playwright ()
webSocketRouteSend route frame =
  ignoreTargetClosed $ do
    let
      (isBase64, message) = frameParts frame
    _ :: WebSocketRouteSendToPageResult <-
      send
        (routeChannel route)
        (MethodName "sendToPage")
        (mkWebSocketRouteSendToPageParams isBase64 message)
    pure ()

-- | Closes the page side of the socket with a clean close.
webSocketRouteClose :: WebSocketRoute -> Maybe Int -> Maybe Text -> Playwright ()
webSocketRouteClose route code reason =
  closePage route code reason True

webSocketRouteConnectToServer :: WebSocketRoute -> Playwright WebSocketRouteServer
webSocketRouteConnectToServer route = do
  connected <- liftIO (readIORef (stateConnected (routeState route)))
  when connected $
    throwIO
      ( ProtocolRpc
          ProtocolFailure
            { protocolFailureName = "Error"
            , protocolFailureMessage = "Already connected to the server"
            , protocolFailureStack = Nothing
            }
      )
  liftIO (writeIORef (stateConnected (routeState route)) True)
  connectRoute route
  pure (WebSocketRouteServer route)

webSocketRouteServerOnMessage :: WebSocketRouteServer -> (WebSocketFrame -> Playwright ()) -> Playwright ()
webSocketRouteServerOnMessage (WebSocketRouteServer route) handler =
  liftIO (writeIORef (stateServerMessage (routeState route)) (Just handler))

webSocketRouteServerOnClose :: WebSocketRouteServer -> (Maybe Int -> Maybe Text -> Playwright ()) -> Playwright ()
webSocketRouteServerOnClose (WebSocketRouteServer route) handler =
  liftIO (writeIORef (stateServerClose (routeState route)) (Just handler))

webSocketRouteServerSend :: WebSocketRouteServer -> WebSocketFrame -> Playwright ()
webSocketRouteServerSend (WebSocketRouteServer route) =
  sendToServer route

webSocketRouteServerClose :: WebSocketRouteServer -> Maybe Int -> Maybe Text -> Playwright ()
webSocketRouteServerClose (WebSocketRouteServer route) code reason =
  closeServer route code reason True

-- | Registers the route's callbacks and returns it. Each message or close from
-- the page or the server goes to the user handler when one is set, and is
-- passed through otherwise, as upstream does.
openRoute :: Guid -> Playwright WebSocketRoute
openRoute guid = do
  obj <- lookupObject guid
  initializer <- throwDecode (remoteInitializer obj)
  state <-
    liftIO $
      RouteState
        <$> newIORef False
        <*> newIORef Nothing
        <*> newIORef Nothing
        <*> newIORef Nothing
        <*> newIORef Nothing
  let
    route =
      WebSocketRoute
        { routeChannel = Channel guid
        , routeUrl = webSocketRouteInitializerUrl initializer
        , routeProtocols = webSocketRouteInitializerProtocols initializer
        , routeState = state
        }
  registerEventHandler guid (MethodName "messageFromPage") (onPageMessage route)
  registerEventHandler guid (MethodName "messageFromServer") (onServerMessage route)
  registerEventHandler guid (MethodName "closePage") (onPageClose route)
  registerEventHandler guid (MethodName "closeServer") (onServerClose route)
  pure route

onPageMessage :: WebSocketRoute -> Value -> Playwright ()
onPageMessage route params = do
  event <- throwDecode params
  let
    frame = frameFromParts (webSocketRouteMessageFromPageEventIsBase64 event) (webSocketRouteMessageFromPageEventMessage event)
  handler <- liftIO (readIORef (statePageMessage (routeState route)))
  case handler of
    Just run ->
      run frame
    Nothing -> do
      connected <- liftIO (readIORef (stateConnected (routeState route)))
      when connected (sendToServer route frame)

onServerMessage :: WebSocketRoute -> Value -> Playwright ()
onServerMessage route params = do
  event <- throwDecode params
  let
    frame = frameFromParts (webSocketRouteMessageFromServerEventIsBase64 event) (webSocketRouteMessageFromServerEventMessage event)
  handler <- liftIO (readIORef (stateServerMessage (routeState route)))
  case handler of
    Just run ->
      run frame
    Nothing ->
      webSocketRouteSend route frame

onPageClose :: WebSocketRoute -> Value -> Playwright ()
onPageClose route params = do
  event <- throwDecode params
  handler <- liftIO (readIORef (statePageClose (routeState route)))
  case handler of
    Just run ->
      run (webSocketRouteClosePageEventCode event) (webSocketRouteClosePageEventReason event)
    Nothing ->
      closeServer route (webSocketRouteClosePageEventCode event) (webSocketRouteClosePageEventReason event) (webSocketRouteClosePageEventWasClean event)

onServerClose :: WebSocketRoute -> Value -> Playwright ()
onServerClose route params = do
  event <- throwDecode params
  handler <- liftIO (readIORef (stateServerClose (routeState route)))
  case handler of
    Just run ->
      run (webSocketRouteCloseServerEventCode event) (webSocketRouteCloseServerEventReason event)
    Nothing ->
      closePage route (webSocketRouteCloseServerEventCode event) (webSocketRouteCloseServerEventReason event) (webSocketRouteCloseServerEventWasClean event)

connectRoute :: WebSocketRoute -> Playwright ()
connectRoute route =
  ignoreTargetClosed $ do
    _ :: WebSocketRouteConnectResult <-
      send (routeChannel route) (MethodName "connect") mkWebSocketRouteConnectParams
    pure ()

-- | Makes the socket open for the page when no server connection was made.
ensureOpened :: WebSocketRoute -> Playwright ()
ensureOpened route =
  ignoreTargetClosed $ do
    _ :: WebSocketRouteEnsureOpenedResult <-
      send (routeChannel route) (MethodName "ensureOpened") mkWebSocketRouteEnsureOpenedParams
    pure ()

sendToServer :: WebSocketRoute -> WebSocketFrame -> Playwright ()
sendToServer route frame =
  ignoreTargetClosed $ do
    let
      (isBase64, message) = frameParts frame
    _ :: WebSocketRouteSendToServerResult <-
      send
        (routeChannel route)
        (MethodName "sendToServer")
        (mkWebSocketRouteSendToServerParams isBase64 message)
    pure ()

closePage :: WebSocketRoute -> Maybe Int -> Maybe Text -> Bool -> Playwright ()
closePage route code reason wasClean =
  ignoreTargetClosed $ do
    _ :: WebSocketRouteClosePageResult <-
      send
        (routeChannel route)
        (MethodName "closePage")
        (mkWebSocketRouteClosePageParams wasClean)
          { webSocketRouteClosePageParamsCode = code
          , webSocketRouteClosePageParamsReason = reason
          }
    pure ()

closeServer :: WebSocketRoute -> Maybe Int -> Maybe Text -> Bool -> Playwright ()
closeServer route code reason wasClean =
  ignoreTargetClosed $ do
    _ :: WebSocketRouteCloseServerResult <-
      send
        (routeChannel route)
        (MethodName "closeServer")
        (mkWebSocketRouteCloseServerParams wasClean)
          { webSocketRouteCloseServerParamsCode = code
          , webSocketRouteCloseServerParamsReason = reason
          }
    pure ()

-- | Runs a route's handler the first time it matches. The callback is stored
-- as IO and re-enters the Playwright monad, as the HTTP route store does.
installHandler :: Guid -> UrlMatch -> (WebSocketRoute -> Playwright ()) -> Playwright Bool
installHandler guid glob handler = do
  conn <- askConnection
  pushWebSocketRouteHandler guid glob $ \routeGuid ->
    runPlaywrightWith conn $ do
      route <- openRoute routeGuid
      handler route
      connected <- liftIO (readIORef (stateConnected (routeState route)))
      unless connected (ensureOpened route)

dispatchPageRoute :: Page -> Value -> Playwright ()
dispatchPageRoute page params = do
  event <- throwDecode params
  let
    pageGuid = channelGuid (pageChannel page)
    contextGuid = channelGuid (pageContextChannel page)
  dispatchRoute contextGuid [pageGuid, contextGuid] (channelRefGuid (pageWebSocketRouteEventWebSocketRoute event))

dispatchContextRoute :: BrowserContext -> Value -> Playwright ()
dispatchContextRoute ctx params = do
  event <- throwDecode params
  let
    contextGuid = channelGuid (browserContextChannel ctx)
  dispatchRoute contextGuid [contextGuid] (channelRefGuid (browserContextWebSocketRouteEventWebSocketRoute event))

-- | Chooses the first matching handler, page before context, and otherwise
-- connects to the server so the socket behaves as if unrouted.
dispatchRoute :: Guid -> [Guid] -> Guid -> Playwright ()
dispatchRoute contextGuid owners routeGuid = do
  obj <- lookupObject routeGuid
  initializer <- throwDecode (remoteInitializer obj)
  base <- lookupContextBaseUrl contextGuid
  handlers <- concat <$> traverse webSocketRouteHandlersFor owners
  let
    url = webSocketRouteInitializerUrl initializer
  case find (\(glob, _) -> matchesUrlMatch (resolveUrlMatch base glob) url) handlers of
    Just (_, run) ->
      liftIO (run routeGuid)
    Nothing -> do
      route <- openRoute routeGuid
      liftIO (writeIORef (stateConnected (routeState route)) True)
      connectRoute route

frameParts :: WebSocketFrame -> (Bool, Text)
frameParts frame =
  case frame of
    WebSocketText text ->
      (False, text)
    WebSocketBinary bytes ->
      (True, decodeUtf8 (Base64.encode bytes))

frameFromParts :: Bool -> Text -> WebSocketFrame
frameFromParts isBase64 message
  | isBase64 = WebSocketBinary (either (const mempty) id (Base64.decode (encodeUtf8 message)))
  | otherwise = WebSocketText message

-- | What the protocol receives for a route. A predicate cannot be sent, so it
-- is registered as @**\/*@ and the handler filters the sockets itself.
protocolPattern :: UrlMatch -> (Maybe Text, Maybe (Text, Text))
protocolPattern match =
  case match of
    UrlMatchGlob glob ->
      (Just glob, Nothing)
    UrlMatchRegex source flags ->
      (Nothing, Just (source, Text.filter (`notElem` ("gy" :: String)) flags))
    UrlMatchPredicate _ ->
      (Just "**/*", Nothing)

pagePattern :: UrlMatch -> PageSetWebSocketInterceptionPatternsParamsPatterns
pagePattern match =
  mkPageSetWebSocketInterceptionPatternsParamsPatterns
    { pageSetWebSocketInterceptionPatternsParamsPatternsGlob = glob
    , pageSetWebSocketInterceptionPatternsParamsPatternsRegexSource = fmap fst regex
    , pageSetWebSocketInterceptionPatternsParamsPatternsRegexFlags = fmap snd regex
    }
  where
    (glob, regex) = protocolPattern match

contextPattern :: UrlMatch -> BrowserContextSetWebSocketInterceptionPatternsParamsPatterns
contextPattern match =
  mkBrowserContextSetWebSocketInterceptionPatternsParamsPatterns
    { browserContextSetWebSocketInterceptionPatternsParamsPatternsGlob = glob
    , browserContextSetWebSocketInterceptionPatternsParamsPatternsRegexSource = fmap fst regex
    , browserContextSetWebSocketInterceptionPatternsParamsPatternsRegexFlags = fmap snd regex
    }
  where
    (glob, regex) = protocolPattern match

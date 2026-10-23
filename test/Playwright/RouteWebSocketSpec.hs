module Playwright.RouteWebSocketSpec
  ( tests
  ) where

import Control.Exception
  ( SomeException
  , catch
  , displayException
  )
import Control.Monad
  ( forever
  , void
  )
import Data.ByteString
  ( ByteString
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Network.HTTP.Types
  ( hContentType
  , status200
  , status400
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
import Network.Wai.Handler.WebSockets
  ( websocketsOr
  )
import Network.WebSockets
  ( Connection
  , ServerApp
  , acceptRequest
  , defaultConnectionOptions
  , receiveData
  )
import Playwright
  ( Playwright
  , UrlMatch (UrlMatchRegex)
  , WebSocketFrame (WebSocketText)
  , closePage
  , contextRouteWebSocket
  , evaluateText
  , pageContext
  , routeWebSocket
  , webSocketRouteClose
  , webSocketRouteConnectToServer
  , webSocketRouteOnMessage
  , webSocketRouteSend
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , withChromiumPage
  , withLocalApp
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO
  ( try
  )
import UnliftIO.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , takeMVar
  )
import UnliftIO.Timeout
  ( timeout
  )

tests :: TestTree
tests =
  testGroup
    "RouteWebSocket"
    [ testCase "should work with ws.close" $
        withChromiumPage $ \page ->
          withLocalApp (wsApp keepListening) $ \prefix -> do
            routeVar <- newEmptyMVar
            routeWebSocket page (UrlMatchRegex ".*" "") $ \route -> do
              void (webSocketRouteConnectToServer route)
              putMVar routeVar route
            _ <- evaluateText page (openSocketJs (wsUrlFor prefix))
            route <- awaitValue routeVar
            webSocketRouteSend route (WebSocketText "hello")
            received <- evaluateText page "() => window.nextMessage"
            assertEqual "message from route" "hello" received
            webSocketRouteClose route (Just 3009) (Just "oops")
            closed <- evaluateText page closeWaitJs
            assertEqual "close from route" "code=3009 reason=oops wasClean=true" closed
    , testCase "should not throw with empty handler" $
        withChromiumPage $ \page ->
          withLocalApp (wsApp keepListening) $ \prefix -> do
            routeWebSocket page (UrlMatchRegex ".*" "") (\_ -> pure ())
            _ <- evaluateText page (openSocketJs (wsUrlFor prefix))
            opened <- evaluateText page "() => window.opened"
            assertEqual "open event" "open" opened
            _ <- evaluateText page "() => { window.ws.send('hi'); window.ws.send('hi2'); return 'sent'; }"
            state <- evaluateText page "() => String(window.ws.readyState)"
            assertEqual "still open after sends" "1" state
    , testCase "should route on context" $
        withChromiumPage $ \page ->
          withLocalApp (wsApp keepListening) $ \prefix -> do
            let context = pageContext page
            routeWebSocket page (UrlMatchRegex "ws1" "") $ \route ->
              webSocketRouteOnMessage route (\_ -> webSocketRouteSend route (WebSocketText "page-mock-1"))
            routeWebSocket page (UrlMatchRegex "ws1" "") $ \route ->
              webSocketRouteOnMessage route (\_ -> webSocketRouteSend route (WebSocketText "page-mock-2"))
            contextRouteWebSocket context (UrlMatchRegex ".*" "") $ \route -> do
              webSocketRouteOnMessage route (\_ -> webSocketRouteSend route (WebSocketText "context-mock-1"))
              webSocketRouteOnMessage route (\_ -> webSocketRouteSend route (WebSocketText "context-mock-2"))
            _ <- evaluateText page (logSocketsJs (Text.replace "http://" "ws://" prefix))
            firstLog <- evaluateText page (exchangeJs "ws1")
            assertEqual "page route wins" "ws1:page-mock-2" firstLog
            secondLog <- evaluateText page (exchangeJs "ws2")
            assertEqual "context route handles ws2" "ws1:page-mock-2,ws2:context-mock-2" secondLog
    , testCase "should not throw after page closure" $
        withChromiumPage $ \page ->
          withLocalApp (wsApp keepListening) $ \prefix -> do
            routeVar <- newEmptyMVar
            routeWebSocket page (UrlMatchRegex ".*" "") $ \route -> do
              void (webSocketRouteConnectToServer route)
              putMVar routeVar route
            _ <- evaluateText page (openSocketJs (wsUrlFor prefix))
            route <- awaitValue routeVar
            closePage page
            webSocketRouteSend route (WebSocketText "hello")
    , testCase "should throw when connecting twice" $
        withChromiumPage $ \page ->
          withLocalApp (wsApp keepListening) $ \prefix -> do
            errorVar <- newEmptyMVar
            routeWebSocket page (UrlMatchRegex ".*" "") $ \route -> do
              void (webSocketRouteConnectToServer route)
              result <- try (webSocketRouteConnectToServer route)
              putMVar
                errorVar
                (either (\e -> Text.pack (displayException (e :: SomeException))) (const "no error") result)
            _ <- evaluateText page (openSocketJs (wsUrlFor prefix))
            errorText <- awaitValue errorVar
            assertBool
              "second connectToServer reports already connected"
              (Text.pack "Already connected to the server" `Text.isInfixOf` errorText)
    -- Skipped: "should work with text message" - needs the server to pause the handshake
    -- (server.waitForUpgrade / doUpgrade) and hand back a server-side socket before the page
    -- is connected; the WAI harness has no hook to hold the upgrade open.
    -- Skipped: "should work with binaryType=blob" - same deferred-upgrade requirement as the
    -- text-message case; it also needs Blob send from the page, which is not asserted here.
    -- Skipped: "should work with binaryType=arraybuffer" - same deferred-upgrade requirement
    -- as the text-message case.
    -- Skipped: "should work when connection errors out" - needs the server to drop or refuse
    -- the TCP connection at the socket level; the WAI harness cannot destroy a raw socket.
    -- Skipped: "should work with error after successful open" - needs the server to fail the
    -- connection after the handshake succeeds; the harness can only close cleanly through
    -- websockets, so no error close is produced.
    -- Skipped: "should work with client-side close" - the upstream body was not reproduced in
    -- this pass; its client-close assertion depends on the server-side close observation.
    -- Skipped: "should pass through the required protocol" - needs a server that negotiates a
    -- WebSocket subprotocol; wsApp accepts without selecting one, and the websockets 0.13
    -- subprotocol API was not verified against this build.
    -- Skipped: "should work with relative WebSocket URL" - the upstream body was not
    -- reproduced in this pass; the relative-URL resolution it checks is not asserted here.
    -- Skipped: "should observe upstream handshake failure when connectToServer is used" -
    -- needs the server to reject the upgrade with a 403; the rejection path through
    -- websockets' rejectRequest was not verified for this harness.
    -- Skipped: "should observe multiple concurrent routed WebSockets with connectToServer" -
    -- the upstream body was not reproduced in this pass; it needs several concurrent server
    -- connections counted per route.
    -- Skipped: "should pattern match" - the upstream body was not reproduced in this pass;
    -- glob and regex matching for routeWebSocket is only covered by the UrlMatch tests.
    -- Skipped: "should work with server" - the upstream body was not reproduced in this pass;
    -- it needs the server-side route API end to end (server onMessage, server send).
    -- Skipped: "should work without server" - the upstream body was not reproduced in this
    -- pass; it relies on a route that answers without connectToServer.
    -- Skipped: "should emit close upon frame navigation" - needs the page to navigate a frame
    -- and observe the socket close; not reproduced in this pass.
    -- Skipped: "should emit close upon frame detach" - the harness has attachFrame but no
    -- detachFrame helper, so the iframe cannot be removed from the page as the case requires.
    -- Skipped: "should work with no trailing slash" - the upstream body was not reproduced in
    -- this pass; it checks the route URL form for a bare host, not asserted here.
    -- Skipped: "should work with baseURL" - needs a context created with a baseURL and a
    -- relative WebSocket URL; not reproduced in this pass.
    -- Skipped: "should work with baseURL regardless of scheme casing" - same baseURL setup as
    -- the baseURL case, with scheme casing; not reproduced in this pass.
    -- Skipped: "should expose protocols to the route handler" - needs the page to request a
    -- subprotocol list and the server to answer it; wsApp does not negotiate subprotocols.
    -- Skipped: "should expose protocols on server-side route" - needs the same subprotocol
    -- negotiation as the route-handler case; WebSocketRouteServer has no protocols accessor.
    ]

wsApp :: (Connection -> IO ()) -> Application
wsApp onConnect request respond =
  case pathInfo request of
    ["bogus-ws"] ->
      respond (responseLBS status400 [] "")
    _ ->
      websocketsOr defaultConnectionOptions server backup request respond
  where
    backup :: Application
    backup _request respondBackup =
      respondBackup (responseLBS status200 [(hContentType, "text/html")] "")
    server :: ServerApp
    server pending = do
      conn <- acceptRequest pending
      onConnect conn

keepListening :: Connection -> IO ()
keepListening conn =
  forever (void (receiveData conn :: IO ByteString)) `catch` ignoreException

ignoreException :: SomeException -> IO ()
ignoreException _ =
  pure ()

wsUrlFor :: Text -> Text
wsUrlFor prefix =
  Text.replace "http://" "ws://" prefix <> "/ws"

-- Opens a page WebSocket without blocking. window.opened, window.nextMessage and
-- window.closed are promises that resolve on the matching event.
openSocketJs :: Text -> Text
openSocketJs url =
  "() => { window.ws = new WebSocket('"
    <> url
    <> "'); window.opened = new Promise(resolve => window.ws.addEventListener('open', () => resolve('open'))); window.nextMessage = new Promise(resolve => window.ws.addEventListener('message', event => resolve(event.data))); window.closed = new Promise(resolve => window.ws.addEventListener('close', event => resolve(`code=${event.code} reason=${event.reason} wasClean=${event.wasClean}`))); window.ws.addEventListener('close', event => { window.closeInfo = `code=${event.code} reason=${event.reason} wasClean=${event.wasClean}`; }); return 'created'; }"

-- Resolves with window.closeInfo once the page socket's close event has been recorded,
-- or with "no close event" after 10 seconds so a missing event fails instead of hanging.
closeWaitJs :: Text
closeWaitJs =
  "() => new Promise(resolve => { const start = Date.now(); const check = () => { if (window.closeInfo) { resolve(window.closeInfo); } else if (Date.now() - start > 10000) { resolve('no close event'); } else { setTimeout(check, 10); } }; check(); })"

-- Opens ws1 and ws2 and records every message, in arrival order, into window.log.
logSocketsJs :: Text -> Text
logSocketsJs base =
  "() => { window.log = []; window.ws1 = new WebSocket('"
    <> base
    <> "/ws1'); window.ws1.addEventListener('message', event => window.log.push('ws1:' + event.data)); window.ws2 = new WebSocket('"
    <> base
    <> "/ws2'); window.ws2.addEventListener('message', event => window.log.push('ws2:' + event.data)); return 'created'; }"

-- Sends "request" on the named socket once it is open, then resolves with the full log
-- after the reply arrives.
exchangeJs :: Text -> Text
exchangeJs name =
  "() => new Promise(resolve => { const ws = window."
    <> name
    <> "; const go = () => { ws.addEventListener('message', () => resolve(window.log.join(','))); ws.send('request'); }; if (ws.readyState === 1) go(); else ws.addEventListener('open', go); })"

awaitValue :: MVar a -> Playwright a
awaitValue var = do
  result <- timeout 30000000 (takeMVar var)
  case result of
    Nothing ->
      assertFailure "timed out waiting for websocket route event"
    Just value ->
      pure value

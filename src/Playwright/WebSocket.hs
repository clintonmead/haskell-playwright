module Playwright.WebSocket
  ( WebSocket
  , isWebSocketClosed
  , onWebSocket
  , onWebSocketClose
  , onWebSocketError
  , onWebSocketFrameReceived
  , onWebSocketFrameSent
  , waitForWebSocket
  , waitForWebSocketClose
  , waitForWebSocketError
  , waitForWebSocketFrameReceived
  , waitForWebSocketFrameSent
  , webSocketUrl
  , webSocketsEqual
  ) where

import Data.Aeson
  ( Value
  )
import Data.ByteString.Base64
  ( decodeLenient
  )
import Data.Set qualified as Set
import Data.Text
  ( Text
  )
import Data.Text.Encoding
  ( encodeUtf8
  )
import Playwright.Channel
  ( Channel (Channel, channelGuid)
  , WebSocketTag
  )
import Playwright.Connection
  ( EventAbort (EventAbort)
  , closedGuids
  , lookupObject
  , registerEventHandler
  , waitForChannelEventUnlessAborted
  , waitForChannelEventUnlessClosed
  )
import Playwright.Error
  ( PlaywrightError (SocketClosed, SocketError)
  , targetClosedError
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
  )
import Playwright.Page
  ( Page (pageChannel)
  , pageActionTimeout
  )
import Playwright.Protocol
  ( PageWebSocketEvent (pageWebSocketEventWebSocket)
  , WebSocketFrameReceivedEvent (webSocketFrameReceivedEventData, webSocketFrameReceivedEventOpcode)
  , WebSocketFrameSentEvent (webSocketFrameSentEventData, webSocketFrameSentEventOpcode)
  , WebSocketInitializer (webSocketInitializerUrl)
  , WebSocketSocketErrorEvent (webSocketSocketErrorEventError)
  )
import Playwright.RaceTimeout
  ( waitWithTimeout
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
  )
import Playwright.Url
  ( Url
  )
import Playwright.WebSocketFrame
  ( WebSocketFrame (WebSocketBinary, WebSocketText)
  )

data WebSocket = WebSocket
  { webSocketChannel :: Channel WebSocketTag
  , webSocketPage :: Page
  }

webSocketFromRef :: Page -> ChannelRef -> WebSocket
webSocketFromRef page ref =
  WebSocket
    { webSocketChannel = Channel (channelRefGuid ref)
    , webSocketPage = page
    }

webSocketsEqual :: WebSocket -> WebSocket -> Bool
webSocketsEqual left right =
  channelGuid (webSocketChannel left) == channelGuid (webSocketChannel right)

webSocketUrl :: WebSocket -> Playwright Url
webSocketUrl socket = do
  obj <- lookupObject (channelGuid (webSocketChannel socket))
  initializer <- throwDecode (remoteInitializer obj)
  pure (webSocketInitializerUrl initializer)

isWebSocketClosed :: WebSocket -> Playwright Bool
isWebSocketClosed socket = do
  closed <- closedGuids
  pure (Set.member (channelGuid (webSocketChannel socket)) closed)

waitForWebSocket :: Page -> Playwright WebSocket
waitForWebSocket page = do
  timeout <- pageActionTimeout page
  waitWithTimeout timeout $ do
    let
      guid = channelGuid (pageChannel page)
    params <- waitForChannelEventUnlessClosed guid (MethodName "webSocket") guid
    event <- throwDecode params
    pure (webSocketFromRef page (pageWebSocketEventWebSocket event))

onWebSocket :: Page -> (WebSocket -> Playwright ()) -> Playwright ()
onWebSocket page run =
  registerEventHandler
    (channelGuid (pageChannel page))
    (MethodName "webSocket")
    ( \params -> do
        event <- throwDecode params
        run (webSocketFromRef page (pageWebSocketEventWebSocket event))
    )

waitForWebSocketFrameSent :: WebSocket -> Playwright WebSocketFrame
waitForWebSocketFrameSent socket = do
  event <- throwDecode =<< waitForSocketEvent socket (MethodName "frameSent")
  pure (frameFromOpcode (webSocketFrameSentEventOpcode event) (webSocketFrameSentEventData event))

waitForWebSocketFrameReceived :: WebSocket -> Playwright WebSocketFrame
waitForWebSocketFrameReceived socket = do
  event <- throwDecode =<< waitForSocketEvent socket (MethodName "frameReceived")
  pure
    ( frameFromOpcode
        (webSocketFrameReceivedEventOpcode event)
        (webSocketFrameReceivedEventData event)
    )

waitForWebSocketClose :: WebSocket -> Playwright ()
waitForWebSocketClose socket = do
  _ <- waitForSocketEvent socket (MethodName "close")
  pure ()

waitForWebSocketError :: WebSocket -> Playwright Text
waitForWebSocketError socket = do
  event <- throwDecode =<< waitForSocketEvent socket (MethodName "socketError")
  pure (webSocketSocketErrorEventError event)

onWebSocketFrameSent :: WebSocket -> (WebSocketFrame -> Playwright ()) -> Playwright ()
onWebSocketFrameSent socket run =
  registerEventHandler
    (channelGuid (webSocketChannel socket))
    (MethodName "frameSent")
    ( \params -> do
        event <- throwDecode params
        run (frameFromOpcode (webSocketFrameSentEventOpcode event) (webSocketFrameSentEventData event))
    )

onWebSocketFrameReceived :: WebSocket -> (WebSocketFrame -> Playwright ()) -> Playwright ()
onWebSocketFrameReceived socket run =
  registerEventHandler
    (channelGuid (webSocketChannel socket))
    (MethodName "frameReceived")
    ( \params -> do
        event <- throwDecode params
        run
          ( frameFromOpcode
              (webSocketFrameReceivedEventOpcode event)
              (webSocketFrameReceivedEventData event)
          )
    )

onWebSocketClose :: WebSocket -> (WebSocket -> Playwright ()) -> Playwright ()
onWebSocketClose socket run =
  registerEventHandler
    (channelGuid (webSocketChannel socket))
    (MethodName "close")
    (\_ -> run socket)

onWebSocketError :: WebSocket -> (Text -> Playwright ()) -> Playwright ()
onWebSocketError socket run =
  registerEventHandler
    (channelGuid (webSocketChannel socket))
    (MethodName "socketError")
    ( \params -> do
        event <- throwDecode params
        run (webSocketSocketErrorEventError event)
    )

waitForSocketEvent :: WebSocket -> MethodName -> Playwright Value
waitForSocketEvent socket method = do
  timeout <- pageActionTimeout (webSocketPage socket)
  waitWithTimeout timeout $
    waitForChannelEventUnlessAborted
      (channelGuid (webSocketChannel socket))
      method
      (socketAborts socket method)

socketAborts :: WebSocket -> MethodName -> [EventAbort]
socketAborts socket method =
  let
    wsGuid = channelGuid (webSocketChannel socket)
    pageGuid = channelGuid (pageChannel (webSocketPage socket))
    pageClose = EventAbort pageGuid (MethodName "close") targetClosedError
    socketClose = EventAbort wsGuid (MethodName "close") SocketClosed
    socketError = EventAbort wsGuid (MethodName "socketError") SocketError
  in
    pageClose
      : [abort | abort <- [socketClose], method /= MethodName "close"]
      ++ [abort | abort <- [socketError], method /= MethodName "socketError"]

frameFromOpcode :: Int -> Text -> WebSocketFrame
frameFromOpcode opcode payload =
  case opcode of
    2 ->
      WebSocketBinary (decodeLenient (encodeUtf8 payload))
    _ ->
      WebSocketText payload

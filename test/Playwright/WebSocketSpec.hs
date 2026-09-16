module Playwright.WebSocketSpec
  ( tests
  ) where

import Control.Exception
  ( SomeException
  , catch
  )
import Control.Monad
  ( forever
  , void
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as ByteString
import Data.Maybe
  ( fromMaybe
  )
import Data.Text
  ( Text
  , isInfixOf
  , stripPrefix
  )
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
  , sendCloseCode
  , sendTextData
  )
import Playwright
  ( Playwright
  , WebSocket
  , WebSocketFrame (WebSocketBinary, WebSocketText)
  , closePage
  , evaluate
  , evaluateTextWith
  , evaluateWith
  , isWebSocketClosed
  , onWebSocket
  , onWebSocketClose
  , onWebSocketError
  , onWebSocketFrameReceived
  , onWebSocketFrameSent
  , textArgument
  , urlToText
  , waitForWebSocket
  , waitForWebSocketFrameReceived
  , waitForWebSocketFrameSent
  , webSocketUrl
  , webSocketsEqual
  )
import Playwright.Error
  ( PlaywrightError (SocketClosed)
  , targetClosedError
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
import UnliftIO.Async
  ( wait
  , withAsync
  )
import UnliftIO.IORef
  ( modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
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
    "web-socket"
    [ testCase "should work" $
        withChromiumPage $ \page ->
          withLocalApp (wsApp (sendThenListen "incoming")) $ \prefix -> do
            value <-
              evaluateTextWith
                page
                "host => { let cb; const result = new Promise(f => cb = f); const ws = new WebSocket('ws://' + host + '/ws'); ws.addEventListener('message', data => { ws.close(); cb(data.data); }); return result; }"
                (textArgument (httpHost prefix))
            assertEqual "incoming" "incoming" value
    , testCase "should emit close events" $
        withChromiumPage $ \page ->
          withLocalApp (wsApp keepListening) $ \prefix -> do
            let
              host = httpHost prefix
            closed <- newEmptyMVar
            logRef <- newIORef ([] :: [Text])
            socketRef <- newIORef (Nothing :: Maybe WebSocket)
            onWebSocket page $ \ws -> do
              writeIORef socketRef (Just ws)
              onWebSocketClose ws $ \_ -> do
                modifyIORef' logRef (<> ["close"])
                putMVar closed ()
              url <- webSocketUrl ws
              modifyIORef' logRef (<> ["open<" <> urlToText url <> ">"])
            _ <-
              evaluateWith
                page
                "host => { const ws = new WebSocket('ws://' + host + '/ws'); ws.addEventListener('open', () => ws.close()); }"
                (textArgument host)
            awaitClose closed
            recorded <- readIORef logRef
            assertEqual "log" ["open<ws://" <> host <> "/ws>", "close"] recorded
            socket <- readIORef socketRef
            case socket of
              Nothing ->
                assertFailure "missing websocket"
              Just ws ->
                assertEqual "closed" True =<< isWebSocketClosed ws
    , testCase "should emit frame events" $
        withChromiumPage $ \page ->
          withLocalApp (wsApp (echoThenListen "incoming")) $ \prefix -> do
            closed <- newEmptyMVar
            logRef <- newIORef ([] :: [Text])
            onWebSocket page $ \ws -> do
              modifyIORef' logRef (<> ["open"])
              onWebSocketFrameSent ws $ \frame ->
                modifyIORef' logRef (<> ["sent<" <> frameText frame <> ">"])
              onWebSocketFrameReceived ws $ \frame ->
                modifyIORef' logRef (<> ["received<" <> frameText frame <> ">"])
              onWebSocketClose ws $ \_ -> do
                modifyIORef' logRef (<> ["close"])
                putMVar closed ()
            _ <-
              evaluateWith
                page
                "host => { const ws = new WebSocket('ws://' + host + '/ws'); ws.addEventListener('open', () => ws.send('outgoing')); ws.addEventListener('message', () => { ws.close(); }); window.ws = ws; }"
                (textArgument (httpHost prefix))
            awaitClose closed
            recorded <- readIORef logRef
            assertEqual "log" ["open", "sent<outgoing>", "received<incoming>", "close"] recorded
    , testCase "should filter out the close events when the server closes with a message" $
        withChromiumPage $ \page ->
          withLocalApp (wsApp closeAfterClientMessage) $ \prefix -> do
            closed <- newEmptyMVar
            logRef <- newIORef ([] :: [Text])
            onWebSocket page $ \ws -> do
              modifyIORef' logRef (<> ["open"])
              onWebSocketFrameSent ws $ \frame ->
                modifyIORef' logRef (<> ["sent<" <> frameText frame <> ">"])
              onWebSocketFrameReceived ws $ \frame ->
                modifyIORef' logRef (<> ["received<" <> frameText frame <> ">"])
              onWebSocketClose ws $ \_ -> do
                modifyIORef' logRef (<> ["close"])
                putMVar closed ()
            _ <-
              evaluateWith
                page
                "host => { const ws = new WebSocket('ws://' + host + '/ws'); ws.addEventListener('message', () => ws.send('outgoing')); window.ws = ws; }"
                (textArgument (httpHost prefix))
            awaitClose closed
            recorded <- readIORef logRef
            assertEqual "log" ["open", "received<incoming>", "sent<outgoing>", "close"] recorded
    , testCase "should pass self as argument to close event" $
        withChromiumPage $ \page ->
          withLocalApp (wsApp keepListening) $ \prefix -> do
            closed <- newEmptyMVar
            socketRef <- newIORef (Nothing :: Maybe WebSocket)
            onWebSocket page $ \ws -> do
              writeIORef socketRef (Just ws)
              onWebSocketClose ws $ \eventArg ->
                putMVar closed eventArg
            _ <-
              evaluateWith
                page
                "host => { const ws = new WebSocket('ws://' + host + '/ws'); ws.addEventListener('open', () => ws.close()); }"
                (textArgument (httpHost prefix))
            eventArg <- awaitValue closed
            socket <- readIORef socketRef
            case socket of
              Nothing ->
                assertFailure "missing websocket"
              Just ws ->
                assertEqual "same socket" True (webSocketsEqual eventArg ws)
    , testCase "should emit binary frame events" $
        withChromiumPage $ \page ->
          withLocalApp (wsApp keepListening) $ \prefix -> do
            closed <- newEmptyMVar
            sentRef <- newIORef ([] :: [WebSocketFrame])
            onWebSocket page $ \ws -> do
              onWebSocketClose ws $ \_ ->
                putMVar closed ()
              onWebSocketFrameSent ws $ \frame ->
                modifyIORef' sentRef (<> [frame])
            _ <-
              evaluateWith
                page
                "host => { const ws = new WebSocket('ws://' + host + '/ws'); ws.addEventListener('open', () => { const binary = new Uint8Array(5); for (let i = 0; i < 5; ++i) binary[i] = i; ws.send('text'); ws.send(binary); ws.close(); }); }"
                (textArgument (httpHost prefix))
            awaitClose closed
            sent <- readIORef sentRef
            case sent of
              [WebSocketText text, WebSocketBinary bytes] -> do
                assertEqual "text" "text" text
                assertEqual "binary" (ByteString.pack [0, 1, 2, 3, 4]) bytes
              other ->
                assertFailure ("unexpected frames: " <> show other)
    , testCase "should emit error" $
        withChromiumPage $ \page ->
          withLocalApp (wsApp keepListening) $ \prefix -> do
            messageVar <- newEmptyMVar
            onWebSocket page $ \ws ->
              onWebSocketError ws (putMVar messageVar)
            _ <-
              evaluateWith
                page
                "host => { new WebSocket('ws://' + host + '/bogus-ws'); }"
                (textArgument (httpHost prefix))
            message <- awaitValue messageVar
            assertBool "status" (": 400" `isInfixOf` message)
    , testCase "should not have stray error events" $
        withChromiumPage $ \page ->
          withLocalApp (wsApp (sendThenListen "incoming")) $ \prefix -> do
            errorRef <- newIORef (Nothing :: Maybe Text)
            onWebSocket page $ \ws ->
              onWebSocketError ws $ \err ->
                writeIORef errorRef (Just err)
            withAsync
              ( do
                  ws <- waitForWebSocket page
                  _ <- waitForWebSocketFrameReceived ws
                  pure ws
              )
              $ \handle -> do
                _ <-
                  evaluateWith
                    page
                    "host => { window.ws = new WebSocket('ws://' + host + '/ws'); }"
                    (textArgument (httpHost prefix))
                _ <- wait handle
                _ <- evaluate page "() => window.ws.close()"
                assertEqual "no error" Nothing =<< readIORef errorRef
    , testCase "should reject waitForEvent on socket close" $
        withChromiumPage $ \page ->
          withLocalApp (wsApp (sendThenListen "incoming")) $ \prefix ->
            withAsync
              ( do
                  ws <- waitForWebSocket page
                  _ <- waitForWebSocketFrameReceived ws
                  pure ws
              )
              $ \handle -> do
                _ <-
                  evaluateWith
                    page
                    "host => { window.ws = new WebSocket('ws://' + host + '/ws'); }"
                    (textArgument (httpHost prefix))
                ws <- wait handle
                withAsync (waitForWebSocketFrameSent ws) $ \sent -> do
                  _ <- evaluate page "() => window.ws.close()"
                  result <- try (wait sent)
                  case result of
                    Left SocketClosed ->
                      pure ()
                    Left err ->
                      assertFailure ("expected SocketClosed, got: " <> show err)
                    Right _ ->
                      assertFailure "expected SocketClosed"
    , testCase "should reject waitForEvent on page close" $
        withChromiumPage $ \page ->
          withLocalApp (wsApp (sendThenListen "incoming")) $ \prefix ->
            withAsync
              ( do
                  ws <- waitForWebSocket page
                  _ <- waitForWebSocketFrameReceived ws
                  pure ws
              )
              $ \handle -> do
                _ <-
                  evaluateWith
                    page
                    "host => { window.ws = new WebSocket('ws://' + host + '/ws'); }"
                    (textArgument (httpHost prefix))
                ws <- wait handle
                withAsync (waitForWebSocketFrameSent ws) $ \sent -> do
                  closePage page
                  result <- try (wait sent)
                  case result of
                    Left err
                      | err == targetClosedError ->
                          pure ()
                    Left err ->
                      assertFailure ("expected target closed, got: " <> show err)
                    Right _ ->
                      assertFailure "expected target closed"
    ]

httpHost :: Text -> Text
httpHost prefix =
  fromMaybe prefix (stripPrefix "http://" prefix)

frameText :: WebSocketFrame -> Text
frameText frame =
  case frame of
    WebSocketText text ->
      text
    WebSocketBinary _ ->
      "<binary>"

awaitClose :: MVar () -> Playwright ()
awaitClose closed =
  awaitValue closed

awaitValue :: MVar a -> Playwright a
awaitValue var = do
  result <- timeout 30000000 (takeMVar var)
  case result of
    Nothing ->
      assertFailure "timed out waiting for websocket event"
    Just value ->
      pure value

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

sendThenListen :: Text -> Connection -> IO ()
sendThenListen message conn = do
  sendTextData conn message
  keepListening conn

echoThenListen :: Text -> Connection -> IO ()
echoThenListen incoming conn = do
  _ <- receiveData conn :: IO Text
  sendTextData conn incoming
  keepListening conn

closeAfterClientMessage :: Connection -> IO ()
closeAfterClientMessage conn = do
  sendTextData conn ("incoming" :: Text)
  _ <- receiveData conn :: IO Text
  sendCloseCode conn 1003 ("closed by Playwright test-server" :: Text)

keepListening :: Connection -> IO ()
keepListening conn =
  forever (void (receiveData conn :: IO ByteString)) `catch` ignoreException

ignoreException :: SomeException -> IO ()
ignoreException _ =
  pure ()

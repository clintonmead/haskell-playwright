module Playwright.PageEventRequestSpec
  ( tests
  ) where

import Control.Concurrent.Async
  ( wait
  , withAsync
  )
import Control.Monad
  ( forM_
  )
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  )
import Data.Text
  ( Text
  , isInfixOf
  , pack
  , replace
  , unpack
  )
import Data.Text.Encoding
  ( decodeUtf8
  )
import Network.HTTP.Types
  ( hContentLength
  , hContentType
  , methodOptions
  , status200
  , status204
  , status404
  )
import Network.Wai
  ( Application
  , pathInfo
  , rawQueryString
  , requestHeaderHost
  , responseLBS
  )
import Network.Wai qualified as Wai
  ( requestMethod
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( Frame
  , HttpMethod (httpMethodToText)
  , Page
  , Request
  , ResourceType (resourceTypeToText)
  , UrlGlob (UrlGlob)
  , asElement
  , contentFrameHandle
  , defaultGotoOptions
  , evaluate
  , evaluateHandle
  , evaluateTextWith
  , goto
  , onRequest
  , requestFailure
  , requestIsNavigation
  , requestMethod
  , requestResourceType
  , requestResponse
  , requestUrl
  , requests
  , responseFinished
  , responseRequest
  , responseText
  , setContent
  , textArgument
  , url
  , urlFromText
  , urlToText
  , waitForRequest
  )
import Playwright.Harness
  ( requireHandle
  , withAssets
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , localOption
  , mkTimeout
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertBool
  , assertEqual
  , assertFailure
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-event-request"
    [ testCase "should fire for navigation requests" $
        withAssets $ \prefix page -> do
          seen <- newIORef (0 :: Int)
          onRequest page $ \_ -> modifyIORef' seen (+ 1)
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "count" 1 =<< readIORef seen
    , testCase "should fire for iframes" $
        withAssets $ \prefix page -> do
          seen <- newIORef (0 :: Int)
          onRequest page $ \_ -> modifyIORef' seen (+ 1)
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- attachFrame page prefix "frame1"
          assertEqual "count" 2 =<< readIORef seen
    , testCase "should fire for fetches" $
        withAssets $ \prefix page -> do
          seen <- newIORef (0 :: Int)
          onRequest page $ \_ -> modifyIORef' seen (+ 1)
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- evaluate page "() => fetch('/empty.html')"
          assertEqual "count" 2 =<< readIORef seen
    , testCase "should fire for fetches with keepalive: true" $
        withAssets $ \prefix page -> do
          seen <- newIORef (0 :: Int)
          onRequest page $ \_ -> modifyIORef' seen (+ 1)
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- evaluate page "() => fetch('/empty.html', { keepalive: true })"
          assertEqual "count" 2 =<< readIORef seen
    , testCase "should return response body when Cross-Origin-Opener-Policy is set" $
        withChromiumPage $ \page ->
          withApplication (pure coopApp) $ \port -> do
            let empty = "http://127.0.0.1:" <> pack (show port) <> "/empty.html"
            response <- goto page (urlFromText empty) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                assertEqual "url" empty . urlToText =<< url page
                responseFinished value
                failure <- requestFailure =<< responseRequest value
                case failure of
                  Nothing ->
                    pure ()
                  Just _ ->
                    assertFailure "expected no failure"
                body <- responseText value
                assertBool "hello" ("Hello there!" `isInfixOf` body)
    , testCase "main resource xhr should have type xhr" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync (waitForRequest page (UrlGlob (prefix <> "/title.html"))) $ \pending -> do
            _ <-
              evaluate
                page
                "() => { const x = new XMLHttpRequest(); x.open('GET', '/title.html', false); x.send(); }"
            request <- wait pending
            assertEqual "nav" False =<< requestIsNavigation request
            assertEqual "type" "xhr" . resourceTypeToText =<< requestResourceType request
    , testCase "<picture> resource should have type image" $
        withAssets $ \prefix page -> do
          let image = prefix <> "/digits/1.png"
          withAsync (waitForRequest page (UrlGlob image)) $ \pending -> do
            setContent
              page
              ("<picture><source><img src=\"" <> image <> "\"></source></picture>")
            request <- wait pending
            assertEqual "type" "image" . resourceTypeToText =<< requestResourceType request
    , testCase "should not expose preflight OPTIONS request" $
        withChromiumPage $ \page -> do
          serverHits <- newIORef ([] :: [Text])
          withApplication (pure (corsApp serverHits)) $ \port -> do
            let
              prefix = "http://127.0.0.1:" <> pack (show port)
              cross = replace "127.0.0.1" "localhost" prefix <> "/cors"
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            clientHits <- newIORef ([] :: [Text])
            onRequest page $ \request -> do
              method <- httpMethodToText <$> requestMethod request
              location <- urlToText <$> requestUrl request
              modifyIORef' clientHits (<> [method <> " " <> location])
            body <-
              evaluateTextWith
                page
                "async url => { const response = await fetch(url, { method: 'POST', body: '', headers: { 'Content-Type': 'application/json', 'X-Custom-Header': 'test-value' } }); return await response.text(); }"
                (textArgument cross)
            assertEqual "body" "Hello there!" body
            assertEqual
              "server"
              ["OPTIONS /cors", "POST /cors"]
              =<< readIORef serverHits
            assertEqual "client" ["POST " <> cross] =<< readIORef clientHits
    , localOption (mkTimeout 120000000) $
        testCase "should return last requests" lastRequestsTest
    ]

lastRequestsTest :: IO ()
lastRequestsTest =
  withChromiumPage $ \page ->
    withApplication (pure fetchLogApp) $ \port -> do
      let prefix = "http://127.0.0.1:" <> pack (show port)
      _ <- goto page (urlFromText (prefix <> "/title.html")) defaultGotoOptions
      forM_ [0 .. 98 :: Int] $ \i ->
        evaluate page ("() => fetch('" <> prefix <> "/fetch?" <> pack (show i) <> "').then(r => r.text())")
      first99 <- requests page
      let withoutNav = drop 1 first99
      forM_ [99 .. 198 :: Int] $ \i ->
        evaluate page ("() => fetch('" <> prefix <> "/fetch?" <> pack (show i) <> "').then(r => r.text())")
      last100 <- requests page
      received <- traverse requestSummary (withoutNav <> last100)
      let expected =
            [ (prefix <> "/fetch?" <> pack (show i), "url:" <> prefix <> "/fetch?" <> pack (show i))
            | i <- [0 .. 198 :: Int]
            ]
      assertEqual "all" expected received

requestSummary :: Request -> IO (Text, Text)
requestSummary request = do
  location <- urlToText <$> requestUrl request
  maybeResponse <- requestResponse request
  body <-
    case maybeResponse of
      Nothing ->
        assertFailure "missing response"
      Just response ->
        responseText response
  pure (location, body)

attachFrame :: Page -> Text -> Text -> IO Frame
attachFrame page prefix frameId = do
  handle <-
    evaluateHandle
      page
      ( "async () => { const frame = document.createElement('iframe'); frame.src = '"
          <> prefix
          <> "/empty.html'; frame.id = '"
          <> frameId
          <> "'; document.body.appendChild(frame); await new Promise(x => frame.onload = x); return frame; }"
      )
  element <- requireHandle =<< asElement handle
  frame <- contentFrameHandle element
  case frame of
    Nothing ->
      assertFailure "expected a frame"
    Just value ->
      pure value

coopApp :: Application
coopApp request respond =
  case pathInfo request of
    ["empty.html"] ->
      respond
        ( responseLBS
            status200
            [ (hContentType, "text/html")
            , ("Cross-Origin-Opener-Policy", "same-origin")
            ]
            "<div>Hello there!</div><script>window.onload = () => console.log('onload')</script>"
        )
    _ ->
      respond (responseLBS status404 [] "")

corsApp :: IORef [Text] -> Application
corsApp hits request respond =
  case pathInfo request of
    ["empty.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] "")
    ["cors"] -> do
      let
        method = decodeUtf8 (Wai.requestMethod request)
        label = method <> " /cors"
      modifyIORef' hits (<> [label])
      if Wai.requestMethod request == methodOptions
        then
          respond
            ( responseLBS
                status204
                [ ("Access-Control-Allow-Origin", "*")
                , ("Access-Control-Allow-Methods", "GET, POST, PUT, OPTIONS")
                , ("Access-Control-Allow-Headers", "*")
                ]
                ""
            )
        else
          respond
            ( responseLBS
                status200
                [ (hContentType, "text/plain")
                , ("Access-Control-Allow-Origin", "*")
                , (hContentLength, "12")
                ]
                (LBS8.pack "Hello there!")
            )
    _ ->
      respond (responseLBS status404 [] "")

fetchLogApp :: Application
fetchLogApp request respond =
  case pathInfo request of
    ["title.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] "<title>t</title>")
    ["fetch"] -> do
      let
        host = maybe "127.0.0.1" decodeUtf8 (requestHeaderHost request)
        location = "http://" <> host <> "/fetch" <> decodeUtf8 (rawQueryString request)
        body = "url:" <> location
        bytes = LBS8.pack (unpack body)
      respond
        ( responseLBS
            status200
            [ (hContentType, "text/plain")
            , (hContentLength, ByteString.Char8.pack (show (LBS8.length bytes)))
            ]
            bytes
        )
    _ ->
      respond (responseLBS status404 [] "")

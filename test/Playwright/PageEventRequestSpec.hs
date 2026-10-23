module Playwright.PageEventRequestSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad
  ( forM_
  , void
  , when
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import UnliftIO.Async
  ( race
  )
import UnliftIO.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , takeMVar
  , tryPutMVar
  )
import UnliftIO.Exception
  ( SomeException
  , try
  )
import Data.Maybe
  ( isNothing
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
import Playwright
  ( Frame
  , HttpMethod (httpMethodToText)
  , Page
  , Request
  , ResourceType (resourceTypeToText)
  , UrlMatch (UrlMatchGlob)
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
  , armRequest
  , continueRoute
  , requestServiceWorker
  , responseFromServiceWorker
  , responseUrl
  , route
  , routeRequest
  , armRequestFailed
  , evaluateBool
  , reload
  , requestFailureText
  , onRequestFailed
  , requestsEqual
  , Playwright
  )
import Playwright.Harness
  ( requireHandle
  , withAssets
  , withChromiumPage
  , withLocalApp
  , assertBool
  , assertEqual
  , assertFailure
  )
import Test.Tasty
  ( TestTree
  , localOption
  , mkTimeout
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

serviceWorkerNavigation :: Text -> Page -> Playwright ()
serviceWorkerNavigation prefix page = do
  let
    stub = prefix <> "/serviceworkers/stub/sw.html"
    failing = prefix <> "/serviceworkers/stub/error.html"
  _ <- goto page (urlFromText stub) defaultGotoOptions
  _ <- evaluate page "() => window['activationPromise']"
  reloaded <- reload page
  case reloaded of
    Nothing ->
      assertFailure "missing reload response"
    Just response ->
      assertEqual "reload url" stub . urlToText =<< responseUrl response
  assertBool "from sw" =<< evaluateBool page "() => window.fromSW"
  _ <- evaluate page "() => window['activationPromise']"
  pendingFailure <- armRequestFailed page
  _ <- try (evaluateTextWith page "name => { window.location.href = name; return 'navigated'; }" (textArgument "/serviceworkers/stub/error.html")) :: Playwright (Either SomeException Text)
  failed <- pendingFailure
  assertEqual "failed url" failing . urlToText =<< requestUrl failed
  failure <- requestFailure failed
  assertBool "failure text" (maybe False (isInfixOf "net::ERR_FAILED" . requestFailureText) failure)
  assertBool "failed no service worker" . isNothing =<< requestServiceWorker failed
  assertBool "failed no response" . isNothing =<< requestResponse failed

-- Stalled requests are aborted by the inline script, so each iframe fires
-- request and then requestfailed. Every request must be seen exactly once
-- per event, and requestfailed must arrive only after its request.
interceptRaceTest :: Text -> Page -> Playwright ()
interceptRaceTest empty page = do
  violations <- newIORef ([] :: [Text])
  alive <- newIORef ([] :: [Request])
  failures <- newIORef ([] :: [Request])
  counter <- newIORef (0 :: Int)
  done <- newEmptyMVar
  onRequest page $ \request -> do
    isAlive <- any (requestsEqual request) <$> readIORef alive
    isFailed <- any (requestsEqual request) <$> readIORef failures
    when (isAlive || isFailed) $
      modifyIORef' violations (<> ["request reported twice"])
    modifyIORef' alive (<> [request])
  onRequestFailed page $ \request -> do
    isAlive <- any (requestsEqual request) <$> readIORef alive
    isFailed <- any (requestsEqual request) <$> readIORef failures
    when (isFailed || not isAlive) $
      modifyIORef' violations (<> ["requestfailed out of order"])
    modifyIORef' alive (filter (not . requestsEqual request))
    modifyIORef' failures (<> [request])
    modifyIORef' counter (+ 1)
    seen <- readIORef counter
    when (seen == 10) $
      void (tryPutMVar done ())
  outcome <- race (liftIO (threadDelay 30000000)) $ do
    -- Stall requests to make sure we don't get requestfinished.
    route page (UrlMatchGlob "**/*") (\_ -> pure ())
    setContent page $
      mconcat (replicate 10 ("<iframe src=\"" <> empty <> "\"></iframe>\n"))
        <> "<script>\n"
        <> "function abortAll() {\n"
        <> "  const frames = document.querySelectorAll('iframe');\n"
        <> "  for (const frame of frames)\n"
        <> "    frame.src = 'about:blank';\n"
        <> "}\n"
        <> "abortAll();\n"
        <> "</script>\n"
    liftIO (takeMVar done)
  case outcome of
    Left () ->
      assertFailure "requestfailed count not reached"
    Right () ->
      pure ()
  assertEqual "violations" [] =<< readIORef violations

emptyApp :: Application
emptyApp request respond =
  case pathInfo request of
    ["empty.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] "")
    _ ->
      respond (responseLBS status404 [] "")

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
    , testCase "should report requests and responses handled by service worker" $
        withAssets $ \prefix page -> do
          let foo = prefix <> "/serviceworkers/fetchdummy/foo"
          _ <- goto page (urlFromText (prefix <> "/serviceworkers/fetchdummy/sw.html")) defaultGotoOptions
          _ <- evaluate page "() => window['activationPromise']"
          pending <- armRequest page (UrlMatchGlob foo)
          swResponse <- evaluateTextWith page "name => window['fetchDummy'](name)" (textArgument "foo")
          request <- pending
          assertEqual "sw response" "responseFromServiceWorker:foo" swResponse
          assertEqual "url" foo . urlToText =<< requestUrl request
          assertBool "no service worker" . isNothing =<< requestServiceWorker request
          response <-
            requestResponse request >>= \maybeResponse ->
              case maybeResponse of
                Nothing ->
                  assertFailure "missing response"
                Just value ->
                  pure value
          assertEqual "response url" foo . urlToText =<< responseUrl response
          assertBool "from service worker" =<< responseFromServiceWorker response
          assertEqual "text" "responseFromServiceWorker:foo" =<< responseText response
    , testCase "should report requests and responses handled by service worker with routing" $
        withAssets $ \prefix page -> do
          let
            sw = prefix <> "/serviceworkers/fetchdummy/sw.html"
            foo = prefix <> "/serviceworkers/fetchdummy/foo"
            failing = prefix <> "/serviceworkers/fetchdummy/error"
          intercepted <- newIORef ([] :: [Text])
          route page (UrlMatchGlob "**/*") $ \handled -> do
            seenUrl <- urlToText <$> (routeRequest handled >>= requestUrl)
            modifyIORef' intercepted (<> [seenUrl])
            continueRoute handled
          _ <- goto page (urlFromText sw) defaultGotoOptions
          _ <- evaluate page "() => window['activationPromise']"
          pending <- armRequest page (UrlMatchGlob foo)
          swResponse <- evaluateTextWith page "name => window['fetchDummy'](name)" (textArgument "foo")
          request <- pending
          assertEqual "sw response" "responseFromServiceWorker:foo" swResponse
          assertEqual "url" foo . urlToText =<< requestUrl request
          assertBool "no service worker" . isNothing =<< requestServiceWorker request
          response <-
            requestResponse request >>= \maybeResponse ->
              case maybeResponse of
                Nothing ->
                  assertFailure "missing response"
                Just value ->
                  pure value
          assertEqual "response url" foo . urlToText =<< responseUrl response
          assertBool "from service worker" =<< responseFromServiceWorker response
          assertEqual "text" "responseFromServiceWorker:foo" =<< responseText response
          pendingFailure <- armRequestFailed page
          _ <- try (evaluateTextWith page "name => window['fetchDummy'](name)" (textArgument "error")) :: Playwright (Either SomeException Text)
          failed <- pendingFailure
          assertEqual "failed url" failing . urlToText =<< requestUrl failed
          assertBool "failure" . not . isNothing =<< requestFailure failed
          assertBool "failed no service worker" . isNothing =<< requestServiceWorker failed
          assertBool "failed no response" . isNothing =<< requestResponse failed
          assertEqual "intercepted" [sw] =<< readIORef intercepted
    , testCase "should report navigation requests and responses handled by service worker" $
        withAssets $ \prefix page -> serviceWorkerNavigation prefix page
    , testCase "should report navigation requests and responses handled by service worker with routing" $
        withAssets $ \prefix page -> do
          route page (UrlMatchGlob "**/*") continueRoute
          serviceWorkerNavigation prefix page
    , testCase "should return response body when Cross-Origin-Opener-Policy is set" $
        withChromiumPage $ \page ->
          withLocalApp coopApp $ \prefix -> do
            let empty = prefix <> "/empty.html"
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
          armRequest page (UrlMatchGlob (prefix <> "/title.html")) >>= \pending -> do
            _ <-
              evaluate
                page
                "() => { const x = new XMLHttpRequest(); x.open('GET', '/title.html', false); x.send(); }"
            request <- pending
            assertEqual "nav" False =<< requestIsNavigation request
            assertEqual "type" "xhr" . resourceTypeToText =<< requestResourceType request
    , testCase "<picture> resource should have type image" $
        withAssets $ \prefix page -> do
          let image = prefix <> "/digits/1.png"
          armRequest page (UrlMatchGlob image) >>= \pending -> do
            setContent
              page
              ("<picture><source><img src=\"" <> image <> "\"></source></picture>")
            request <- pending
            assertEqual "type" "image" . resourceTypeToText =<< requestResourceType request
    , testCase "should not expose preflight OPTIONS request" $
        withChromiumPage $ \page -> do
          serverHits <- newIORef ([] :: [Text])
          withLocalApp (corsApp serverHits) $ \prefix -> do
            let
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
    , testCase "should not expose preflight OPTIONS request with network interception" $
        withChromiumPage $ \page -> do
          serverHits <- newIORef ([] :: [Text])
          withLocalApp (corsApp serverHits) $ \prefix -> do
            let
              cross = replace "127.0.0.1" "localhost" prefix <> "/cors"
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/*") continueRoute
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
            assertEqual "server" ["POST /cors"] =<< readIORef serverHits
            assertEqual "client" ["POST " <> cross] =<< readIORef clientHits
    , testCase "should fire requestfailed when intercepting race" $
        withChromiumPage $ \page ->
          withLocalApp emptyApp $ \prefix ->
            interceptRaceTest (prefix <> "/empty.html") page
    , localOption (mkTimeout 120000000) $
        testCase "should return last requests" lastRequestsTest
    -- Skipped: "should finish 204 request". Upstream marks it `fixme` on Chromium, and this
    -- suite runs only Chromium.
    -- "should finish 204 request"
    ]

lastRequestsTest :: IO ()
lastRequestsTest =
  withChromiumPage $ \page ->
    withLocalApp fetchLogApp $ \prefix -> do
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

requestSummary :: Request -> Playwright (Text, Text)
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

attachFrame :: Page -> Text -> Text -> Playwright Frame
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

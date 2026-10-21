module Playwright.PageRequestContinueSpec
  ( tests
  ) where

import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe
  ( fromMaybe
  , isJust
  )
import Control.Concurrent
  ( threadDelay
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Text
  ( isInfixOf
  , isSuffixOf
  , pack
  , toLower
  , unpack
  )
import Data.Text.Encoding
  ( encodeUtf8
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , responseLBS
  )
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( ContinueOptions (continueHeaders, continueMethod, continuePostData, continueUrl)
  , HeaderName (HeaderName)
  , HeaderValue (HeaderValue)
  , HttpHeader (HttpHeader)
  , HttpMethod (HttpMethod)
  , RequestFailure (requestFailureText)
  , UrlMatch (UrlMatchGlob)
  , armRequestFailed
  , continueRoute
  , continueRouteWith
  , defaultContinueOptions
  , defaultGotoOptions
  , evaluate
  , evaluateText
  , goto
  , requestFailure
  , requestHeaders
  , responseOk
  , responseUrl
  , route
  , routeRequest
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( assertBool
  , withLocalApp
  , withAssets
  , withChromiumPage
  , assertEqual
  , assertFailure
  )
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
  , putMVar
  , takeMVar
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-request-continue"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          route page (UrlMatchGlob "**/*") continueRoute
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "ok" True =<< responseOk value
    , testCase "should amend HTTP headers" $
        withApplication (pure echoFooHeader) $ \port ->
          withChromiumPage $ \page -> do
            route page (UrlMatchGlob "**/*") $ \handled -> do
              headers <- requestHeaders =<< routeRequest handled
              continueRouteWith
                handled
                defaultContinueOptions
                  { continueHeaders =
                      Just (headers <> [HttpHeader (HeaderName "foo") (HeaderValue "bar")])
                  }
            _ <-
              goto
                page
                (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
                defaultGotoOptions
            assertEqual "foo" "bar" =<< evaluateText page "() => document.body.innerText"    , testCase "should amend method" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/*") $ \handled ->
              continueRouteWith handled defaultContinueOptions {continueMethod = Just (HttpMethod "POST")}
            _ <- evaluateText page "() => fetch('/sleep.zzz').then(r => r.text())"
            seen <- readIORef requests
            assertEqual "method" (Just "POST") (methodOf "/sleep.zzz" seen)
    , testCase "should amend method on main request" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            route page (UrlMatchGlob "**/*") $ \handled ->
              continueRouteWith handled defaultContinueOptions {continueMethod = Just (HttpMethod "POST")}
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            seen <- readIORef requests
            assertEqual "method" (Just "POST") (methodOf "/empty.html" seen)
    , testCase "should override request url" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            route page (UrlMatchGlob "**/foo") $ \handled ->
              continueRouteWith handled defaultContinueOptions {continueUrl = Just (urlFromText (prefix <> "/global-var.html"))}
            response <- goto page (urlFromText (prefix <> "/foo")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                finalUrl <- responseUrl value
                assertBool "response url" ("/global-var.html" `isSuffixOf` urlToText finalUrl)
            seen <- readIORef requests
            assertEqual "server saw the overridden path" True (isJust (methodOf "/global-var.html" seen))
    , testCase "should override method along with url" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            route page (UrlMatchGlob "**/foo") $ \handled ->
              continueRouteWith
                handled
                defaultContinueOptions
                  { continueUrl = Just (urlFromText (prefix <> "/empty.html"))
                  , continueMethod = Just (HttpMethod "POST")
                  }
            _ <- goto page (urlFromText (prefix <> "/foo")) defaultGotoOptions
            seen <- readIORef requests
            assertEqual "method" (Just "POST") (methodOf "/empty.html" seen)
    , testCase "should amend post data" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/*") $ \handled ->
              continueRouteWith handled defaultContinueOptions {continuePostData = Just "doggo"}
            _ <- evaluateText page "() => fetch('/sleep.zzz', { method: 'POST', body: 'birdy' }).then(r => r.text())"
            seen <- readIORef requests
            assertEqual "body" (Just "doggo") (bodyOf "/sleep.zzz" seen)
    , testCase "should amend utf8 post data" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/*") $ \handled ->
              continueRouteWith handled defaultContinueOptions {continuePostData = Just (encodeUtf8 "你好")}
            _ <- evaluateText page "() => fetch('/sleep.zzz', { method: 'POST', body: 'birdy' }).then(r => r.text())"
            seen <- readIORef requests
            assertEqual "body" (Just (encodeUtf8 "你好")) (bodyOf "/sleep.zzz" seen)
    , testCase "should amend longer post data" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            let longBody = BS8.replicate 7500 'a'
            route page (UrlMatchGlob "**/*") $ \handled ->
              continueRouteWith handled defaultContinueOptions {continuePostData = Just longBody}
            _ <- evaluateText page "() => fetch('/sleep.zzz', { method: 'POST', body: 'birdy' }).then(r => r.text())"
            seen <- readIORef requests
            assertEqual "body" (Just longBody) (bodyOf "/sleep.zzz" seen)
    , testCase "should amend binary post data" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            let binary = BS.pack [0, 1, 2, 127, 128, 255]
            route page (UrlMatchGlob "**/*") $ \handled ->
              continueRouteWith handled defaultContinueOptions {continuePostData = Just binary}
            _ <- evaluateText page "() => fetch('/sleep.zzz', { method: 'POST', body: 'birdy' }).then(r => r.text())"
            seen <- readIORef requests
            assertEqual "body" (Just binary) (bodyOf "/sleep.zzz" seen)

    , testCase "should use content-type from original request" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/title.html") $ \handled ->
              continueRouteWith handled defaultContinueOptions {continuePostData = Just "{\"b\":2}"}
            _ <-
              evaluateText
                page
                ( "() => fetch('" <> prefix
                    <> "/title.html', { method: 'POST', body: '{\"a\":1}', headers: { 'content-type': 'application/json' } }).then(r => r.text())"
                )
            seen <- readIORef requests
            assertEqual "content-type" (Just "application/json") (headerOf "/title.html" "content-type" seen)
            assertEqual "body" (Just "{\"b\":2}") (bodyOf "/title.html" seen)
    , testCase "should not allow to override unsafe HTTP headers" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            route page (UrlMatchGlob "**/*") $ \handled ->
              continueRouteWith
                handled
                defaultContinueOptions
                  { continueHeaders =
                      Just
                        [ HttpHeader (HeaderName "host") (HeaderValue "bar")
                        , HttpHeader (HeaderName "trailer") (HeaderValue "baz")
                        ]
                  }
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            seen <- readIORef requests
            assertEqual "trailer" Nothing (headerOf "/empty.html" "trailer" seen)
            assertEqual "host" (Just (BS8.pack (drop 7 (unpack prefix)))) (headerOf "/empty.html" "host" seen)
    , testCase "should compute content-length from post data" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            let longBody = BS8.replicate 7500 'a'
            route page (UrlMatchGlob "**/*") $ \handled ->
              continueRouteWith handled defaultContinueOptions {continuePostData = Just longBody}
            _ <- evaluateText page "() => fetch('/sleep.zzz', { method: 'PATCH', body: 'birdy' }).then(r => r.text())"
            seen <- readIORef requests
            assertEqual "body" (Just longBody) (bodyOf "/sleep.zzz" seen)
    , testCase "postData should return empty string when overriding body with empty string" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/*") $ \handled ->
              continueRouteWith handled defaultContinueOptions {continuePostData = Just ""}
            _ <- evaluateText page "() => fetch('/sleep.zzz', { method: 'POST', body: 'original' }).then(r => r.text())"
            seen <- readIORef requests
            assertEqual "body" (Just "") (bodyOf "/sleep.zzz" seen)
    , testCase "should not throw if request was cancelled by the page" $
        withAssets $ \prefix page -> do
          intercepted <- newEmptyMVar
          route page (UrlMatchGlob "**/data.json") $ \handled -> putMVar intercepted handled
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <-
            evaluate
              page
              "() => { globalThis.controller = new AbortController(); fetch('/data.json', { signal: globalThis.controller.signal }).catch(() => {}); }"
          -- Bounded: a route that never fires must fail the test, not hang the run.
          outcome <- race (liftIO (threadDelay 30000000)) (takeMVar intercepted)
          case outcome of
            Left () ->
              assertFailure "route never intercepted data.json"
            Right handled -> do
              takeFailure <- armRequestFailed page
              _ <- evaluate page "() => globalThis.controller.abort()"
              failed <- takeFailure
              failure <- requestFailure failed
              case failure of
                Nothing ->
                  assertFailure "missing failure"
                Just value ->
                  assertBool
                    "cancelled or aborted"
                    (any (`isInfixOf` toLower (requestFailureText value)) ["cancelled", "aborted"])
              -- Continuing a route whose request was cancelled must not throw.
              continueRoute handled
    ]

echoFooHeader :: Application
echoFooHeader req respond =
  respond
    ( responseLBS
        status200
        [(hContentType, BS8.pack "text/plain")]
        (LBS8.fromStrict (fromMaybe BS8.empty (lookup "foo" (Wai.requestHeaders req))))
    )

-- | One request the recording server saw: method, path, and body.
data Recorded = Recorded
  { recordedMethod :: BS8.ByteString
  , recordedPath :: BS8.ByteString
  , recordedBody :: BS8.ByteString
  , recordedHeaders :: [(BS8.ByteString, BS8.ByteString)]
  }

-- | Records every request's method, path, and body, and answers with an
-- empty page.
recordingApp :: IORef [Recorded] -> Application
recordingApp requests request respond = do
  body <- Wai.strictRequestBody request
  modifyIORef' requests
    ( Recorded
        (Wai.requestMethod request)
        (Wai.rawPathInfo request)
        (LBS8.toStrict body)
        (map (\(name, value) -> (CI.foldedCase name, value)) (Wai.requestHeaders request))
        :
    )
  respond (responseLBS status200 [(hContentType, "text/html")] "")

-- | A header the last request to the path carried.
headerOf :: BS8.ByteString -> BS8.ByteString -> [Recorded] -> Maybe BS8.ByteString
headerOf path name requests =
  lookup name . recordedHeaders =<< lastFor path requests

-- | The method of the last request to the path.
methodOf :: BS8.ByteString -> [Recorded] -> Maybe BS8.ByteString
methodOf path requests =
  recordedMethod <$> lastFor path requests

-- | The body of the last request to the path.
bodyOf :: BS8.ByteString -> [Recorded] -> Maybe BS8.ByteString
bodyOf path requests =
  recordedBody <$> lastFor path requests

-- | The most recent request to the path. The list is newest first.
lastFor :: BS8.ByteString -> [Recorded] -> Maybe Recorded
lastFor path requests =
  case filter ((== path) . recordedPath) requests of
    latest : _ ->
      Just latest
    [] ->
      Nothing

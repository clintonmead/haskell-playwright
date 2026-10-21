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
  , isNothing
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
  , Text
  , replace
  , toLower
  , unpack
  )
import Data.Text.Encoding
  ( encodeUtf8
  )
import Network.HTTP.Types
  ( hContentType
  , status204
  , status301
  , hLocation
  , status302
  , status200
  )
import Network.Wai
  ( Application
  , Request
  , pathInfo
  , responseLBS
  )
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , staticApp
  )
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( ContinueOptions (continueHeaders, continueMethod, continuePostData, continueUrl)
  , HeaderName (HeaderName, headerNameToText)
  , HeaderValue (HeaderValue, headerValueToText)
  , HttpHeader (HttpHeader, httpHeaderName, httpHeaderValue)
  , HttpMethod (HttpMethod)
  , RequestFailure (requestFailureText)
  , UrlMatch (UrlMatchGlob)
  , armRequestFailed
  , continueRoute
  , continueRouteWith
  , defaultContinueOptions
  , Page
  , Playwright
  , defaultGotoOptions
  , waitForFunction
  , waitForTimeout
  , evaluate
  , evaluateNumber
  , evaluateText
  , goto
  , requestFailure
  , requestHeaders
  , requestUrl
  , requestAllHeaders
  , fallbackRouteWith
  , url
  , onRequest
  , onRequestFailed
  , onRequestFinished
  , onResponse
  , responseRequest
  , responseFinished
  , responseOk
  , responseUrl
  , route
  , routeRequest
  , urlFromText
  , urlToText
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( assertBool
  , requireTimeout
  , testAssetsDir
  , withLocalApp
  , withAssets
  , withChromiumPage
  , assertEqual
  , assertFailure
  )
import UnliftIO.Exception
  ( try
  )
import UnliftIO.Async
  ( race
  , withAsync
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
            assertEqual "foo" "bar" =<< evaluateText page "() => document.body.innerText"
    , testCase "should amend method" $ do
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
    , testCase "should intercept css variable with background url" $
        withChromiumPage $ \page -> do
          assets <- testAssetsDir
          withLocalApp (cssVariableApp assets) $ \prefix -> do
            counter <- newIORef (0 :: Int)
            route page (UrlMatchGlob (prefix <> "/pptr.png")) $ \handled -> do
              modifyIORef' counter (+ 1)
              continueRoute handled
            _ <- goto page (urlFromText (prefix <> "/test.html")) defaultGotoOptions
            assertEqual "text" "Yo!" =<< evaluateText page "() => document.querySelector('div').textContent"
            waitForTimeout page =<< requireTimeout 1000
            assertEqual "intercepted once" 1 =<< readIORef counter
    , testCase "continue should not change multipart/form-data body" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            before <- sendUpload page
            assertEqual "status before" 200 before
            seenBefore <- bodyOf "/upload" <$> readIORef requests
            assertBool "body before continue" (maybe False (uploadPart `BS8.isInfixOf`) seenBefore)
            route page (UrlMatchGlob "**/*") $ \handled -> continueRoute handled
            after <- sendUpload page
            assertEqual "status after" 200 after
            seenAfter <- bodyOf "/upload" <$> readIORef requests
            assertBool "body after continue" (maybe False (uploadPart `BS8.isInfixOf`) seenAfter)
    , testCase "continue should propagate headers to redirects" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (redirectingApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/redirect") $ \handled ->
              continueRouteWith
                handled
                defaultContinueOptions
                  {continueHeaders = Just [HttpHeader (HeaderName "custom") (HeaderValue "value")]}
            _ <- goto page (urlFromText (prefix <> "/redirect")) defaultGotoOptions
            seen <- readIORef requests
            assertEqual "custom header" (Just "value") (headerOf "/empty.html" "custom" seen)
    , testCase "continue should drop content-length on redirects" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (redirectingApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/redirect") $ \handled ->
              continueRouteWith
                handled
                defaultContinueOptions
                  {continueHeaders = Just [HttpHeader (HeaderName "custom") (HeaderValue "value")]}
            _ <- evaluateNumber page "() => fetch('/redirect', { method: 'POST', body: 'foo' }).then(r => r.status)"
            seen <- readIORef requests
            assertEqual "method" (Just "GET") (recordedMethod <$> lastFor "/empty.html" seen)
            assertEqual "content-length" Nothing (headerOf "/empty.html" "content-length" seen)
            assertEqual "content-type" Nothing (headerOf "/empty.html" "content-type" seen)
            assertEqual "custom header" (Just "value") (headerOf "/empty.html" "custom" seen)
    , testCase "should not forward Host header on cross-origin redirect" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (crossOriginRedirectApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/*") $ \handled -> do
              headers <- requestHeaders =<< routeRequest handled
              assertBool "route sees no host header" (all ((/= "host") . toLower . headerNameToText . httpHeaderName) headers)
              continueRouteWith handled defaultContinueOptions {continueHeaders = Just headers}
            _ <- goto page (urlFromText (prefix <> "/redirect")) defaultGotoOptions
            seen <- readIORef requests
            assertBool "redirected to the localhost origin" (maybe False ("localhost" `BS8.isPrefixOf`) (headerOf "/final" "host" seen))
    , testCase "should not allow changing protocol when overriding url" $
        withAssets $ \prefix page -> do
          results <- newEmptyMVar
          route page (UrlMatchGlob "**/*") $ \handled -> do
            result <-
              try (continueRouteWith handled defaultContinueOptions {continueUrl = Just (urlFromText "file:///tmp/foo")})
                :: Playwright (Either PlaywrightError ())
            putMVar results result
          withAsync (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions) $ \_ -> do
            result <- takeMVar results
            case result of
              Left (ProtocolRpc err) ->
                assertBool
                  "message"
                  ("New URL must have same protocol as overridden URL" `isInfixOf` protocolFailureMessage err)
              Left err ->
                assertFailure ("expected ProtocolRpc, got: " <> show err)
              Right () ->
                assertFailure "expected the override to fail"
    , testCase "should not delete the origin header" $ do
        requests <- newIORef []
        origins <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (corsApp requests) $ \prefix -> do
            let
              cross = replace "127.0.0.1" "localhost" prefix
              isOrigin = (== "origin") . toLower . headerNameToText . httpHeaderName
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob (cross <> "/something")) $ \handled -> do
              headers <- requestHeaders =<< routeRequest handled
              modifyIORef' origins (<> [headerValueToText (httpHeaderValue header) | header <- headers, isOrigin header])
              continueRouteWith handled defaultContinueOptions {continueHeaders = Just (filter (not . isOrigin) headers)}
            text <- evaluateText page ("async () => (await fetch('" <> cross <> "/something')).text()")
            assertEqual "text" "done" text
            assertEqual "intercepted origin" [prefix] =<< readIORef origins
            assertEqual "server origin" (Just (encodeUtf8 prefix)) . headerOf "/something" "origin" =<< readIORef requests
    , testCase "should continue preload link requests" $ do
        requests <- newIORef []
        assets <- testAssetsDir
        withChromiumPage $ \page ->
          withLocalApp (recordingAssetsApp assets requests) $ \prefix -> do
            route page (UrlMatchGlob "**/one-style.css") $ \handled -> do
              headers <- requestHeaders =<< routeRequest handled
              continueRouteWith
                handled
                defaultContinueOptions
                  {continueHeaders = Just (headers <> [HttpHeader (HeaderName "custom") (HeaderValue "value")])}
            _ <- goto page (urlFromText (prefix <> "/preload.html")) defaultGotoOptions
            _ <- waitForFunction page "() => window.preloadedStyles"
            assertEqual "custom header" (Just "value") . headerOf "/one-style.css" "custom" =<< readIORef requests
            assertEqual "color" "rgb(255, 192, 203)" =<< evaluateText page "() => window.getComputedStyle(document.body).backgroundColor"
    , testCase "should work with Cross-Origin-Opener-Policy" $ do
        requests <- newIORef []
        intercepted <- newIORef []
        events <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (coopApp requests) $ \prefix -> do
            let
              pageUrl = prefix <> "/empty.html"
            route page (UrlMatchGlob "**/*") $ \handled -> do
              request <- routeRequest handled
              modifyIORef' intercepted . (<>) . pure . urlToText =<< requestUrl request
              continueRouteWith handled defaultContinueOptions {continueHeaders = Just [HttpHeader (HeaderName "foo") (HeaderValue "bar")]}
            onRequest page (eventRecorder events "request")
            onResponse page (eventRecorder events "response")
            onRequestFailed page (eventRecorder events "requestfailed")
            onRequestFinished page (eventRecorder events "requestfinished")
            response <- goto page (urlFromText pageUrl) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                responseFinished value
                assertEqual "intercepted" [pageUrl] =<< readIORef intercepted
                assertEqual "server requests" ["/empty.html"] . map recordedPath =<< readIORef requests
                assertEqual "server header" (Just "bar") . headerOf "/empty.html" "foo" =<< readIORef requests
                assertEqual "url" pageUrl . urlToText =<< url page
                assertEqual "events" ["request", "response", "requestfinished"] =<< readIORef events
                failure <- requestFailure =<< responseRequest value
                assertBool "no failure" (isNothing failure)
    , testCase "redirected requests should report overridden headers" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (redirectingApp requests) $ \prefix -> do
            route page (UrlMatchGlob "**/redirect") $ \handled -> do
              headers <- requestHeaders =<< routeRequest handled
              fallbackRouteWith
                handled
                defaultContinueOptions
                  {continueHeaders = Just (headers <> [HttpHeader (HeaderName "custom") (HeaderValue "value")])}
            response <- goto page (urlFromText (prefix <> "/redirect")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                assertEqual "server custom" (Just "value") . headerOf "/empty.html" "custom" =<< readIORef requests
                request <- responseRequest value
                assertEqual "url" (prefix <> "/empty.html") . urlToText =<< requestUrl request
                assertBool "custom header" . any (headerIs "custom" "value") =<< requestHeaders request
                assertBool "all headers" . any (headerIs "custom" "value") =<< requestAllHeaders request
    , testCase "continue should delete headers on redirects" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (redirectingCorsApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/redirect") $ \handled -> do
              headers <- requestHeaders =<< routeRequest handled
              continueRouteWith handled defaultContinueOptions {continueHeaders = Just (filter (not . hasName "foo") headers)}
            text <- evaluateText page ("async () => (await fetch('" <> prefix <> "/redirect', {headers: {foo: 'a'}})).text()")
            assertEqual "text" "done" text
            assertEqual "server foo" Nothing . headerOf "/something" "foo" =<< readIORef requests
    , testCase "propagate headers cross origin" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (corsCredentialsApp requests) $ \prefix -> do
            let
              cross = replace "127.0.0.1" "localhost" prefix
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            text <-
              evaluateText
                page
                ( "async () => (await fetch('" <> cross <> "/something', {headers: {authorization: 'credentials', custom: 'foo'}, credentials: 'include'})).text()"
                )
            assertEqual "text" "done" text
            assertEqual "authorization" (Just "credentials") . headerOf "/something" "authorization" =<< readIORef requests
            assertEqual "custom" (Just "foo") . headerOf "/something" "custom" =<< readIORef requests
    , testCase "propagate headers cross origin redirect" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (redirectingCorsCredentialsApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            _ <- evaluateText page "() => { document.cookie = 'a=b'; return document.cookie; }"
            text <-
              evaluateText
                page
                ( "async () => (await fetch('" <> prefix <> "/redirect', {headers: {authorization: 'credentials', custom: 'foo'}, credentials: 'include'})).text()"
                )
            assertEqual "text" "done" text
            assertEqual "authorization" Nothing . headerOf "/something" "authorization" =<< readIORef requests
            assertEqual "cookie" Nothing . headerOf "/something" "cookie" =<< readIORef requests
            assertEqual "custom" (Just "foo") . headerOf "/something" "custom" =<< readIORef requests
    , testCase "propagate headers cross origin redirect after interception" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (redirectingCorsCredentialsApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            _ <- evaluateText page "() => { document.cookie = 'a=b'; return document.cookie; }"
            route page (UrlMatchGlob "**/redirect") $ \handled -> do
              headers <- requestHeaders =<< routeRequest handled
              continueRouteWith
                handled
                defaultContinueOptions
                  { continueHeaders =
                      Just
                        ( filter (not . hasName "authorization") headers
                            <> [ HttpHeader (HeaderName "authorization") (HeaderValue "credentials")
                               , HttpHeader (HeaderName "custom") (HeaderValue "foo")
                               ]
                        )
                  }
            text <-
              evaluateText
                page
                ( "async () => (await fetch('" <> prefix <> "/redirect', {headers: {authorization: 'none'}, credentials: 'include'})).text()"
                )
            assertEqual "text" "done" text
            assertEqual "authorization" (Just "credentials") . headerOf "/something" "authorization" =<< readIORef requests
            assertEqual "cookie" Nothing . headerOf "/something" "cookie" =<< readIORef requests
            assertEqual "custom" (Just "foo") . headerOf "/something" "custom" =<< readIORef requests
    ]

-- | Redirects /redirect to /something, which answers with a body another origin
-- may read. Every request is recorded.
redirectingCorsApp :: IORef [Recorded] -> Application
redirectingCorsApp requests request respond = do
  recordRequest requests request
  case pathInfo request of
    ["redirect"] ->
      respond (responseLBS status302 [(hLocation, "/something")] LBS8.empty)
    _ ->
      respond (responseLBS status200 [(hContentType, "text/plain"), ("Access-Control-Allow-Origin", "*")] "done")

hasName :: Text -> HttpHeader -> Bool
hasName name header =
  toLower (headerNameToText (httpHeaderName header)) == name

headerIs :: Text -> Text -> HttpHeader -> Bool
headerIs name value header =
  hasName name header && headerValueToText (httpHeaderValue header) == value

-- | Serves a page with a Cross-Origin-Opener-Policy header, recording requests.
coopApp :: IORef [Recorded] -> Application
coopApp requests request respond = do
  recordRequest requests request
  respond
    ( responseLBS
        status200
        [(hContentType, "text/html"), ("Cross-Origin-Opener-Policy", "same-origin")]
        "<div>Hello there!</div><script>window.onload = () => console.log('onload')</script>"
    )

-- | Appends the name of each event to the list, in the order they arrive.
eventRecorder :: IORef [Text] -> Text -> a -> Playwright ()
eventRecorder events name _ =
  modifyIORef' events (<> [name])

-- | Serves a page whose div takes its background from a CSS variable and an
-- animation, and serves every other path from the assets directory.
cssVariableApp :: FilePath -> Application
cssVariableApp assets request respond =
  case pathInfo request of
    ["test.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] cssPage)
    _ ->
      staticApp (defaultFileServerSettings assets) request respond
  where
    cssPage =
      LBS8.pack "<style>@keyframes JNDzq { 0% { background-position: 0 0 } to { background-position: 100 0 } } div { --background: url(/pptr.png); background-image: var(--background); animation: JNDzq 1s linear infinite; }</style><div>Yo!</div>"

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
  recordRequest requests request
  respond (responseLBS status200 [(hContentType, "text/html")] "")

recordRequest :: IORef [Recorded] -> Request -> IO ()
recordRequest requests request = do
  body <- Wai.strictRequestBody request
  modifyIORef' requests
    ( Recorded
        (Wai.requestMethod request)
        (Wai.rawPathInfo request)
        (LBS8.toStrict body)
        (map (\(name, value) -> (CI.foldedCase name, value)) (Wai.requestHeaders request))
        :
    )

-- | Records every request, and answers with a body another origin may read.
corsApp :: IORef [Recorded] -> Application
corsApp requests request respond = do
  recordRequest requests request
  respond (responseLBS status200 [(hContentType, "text/html"), ("Access-Control-Allow-Origin", "*")] "done")

-- | Records every request, then serves the file from the assets directory.
recordingAssetsApp :: FilePath -> IORef [Recorded] -> Application
recordingAssetsApp assets requests request respond = do
  recordRequest requests request
  staticApp (defaultFileServerSettings assets) request respond

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

-- | Posts a one-file multipart form from the page and returns the status.
sendUpload :: Page -> Playwright Double
sendUpload page =
  evaluateNumber
    page
    "() => { const newFile = new File(['file content'], 'file.txt'); const formData = new FormData(); formData.append('file', newFile); return fetch('/upload', { method: 'POST', credentials: 'include', body: formData }).then(r => r.status); }"

-- | The file part of the upload, as the server receives it.
uploadPart :: BS8.ByteString
uploadPart =
  BS8.intercalate
    "\r\n"
    [ "Content-Disposition: form-data; name=\"file\"; filename=\"file.txt\""
    , "Content-Type: application/octet-stream"
    , ""
    , "file content"
    , "------"
    ]

-- | Redirects /redirect to /empty.html. Every other request is recorded.
redirectingApp :: IORef [Recorded] -> Application
redirectingApp requests request respond =
  case pathInfo request of
    ["redirect"] ->
      respond (responseLBS status302 [(hLocation, "/empty.html")] LBS8.empty)
    _ ->
      recordingApp requests request respond

-- | Redirects /redirect to the same server under localhost, which is another
-- origin. Every other request is recorded.
crossOriginRedirectApp :: IORef [Recorded] -> Application
crossOriginRedirectApp requests request respond =
  case pathInfo request of
    ["redirect"] ->
      respond (responseLBS status302 [(hLocation, "http://" <> crossHost <> "/final")] LBS8.empty)
    _ ->
      recordingApp requests request respond
  where
    host = fromMaybe BS8.empty (lookup "host" (Wai.requestHeaders request))
    crossHost =
      if BS8.isPrefixOf "127.0.0.1" host
        then "localhost" <> BS8.drop 9 host
        else host

-- | Answers the preflight of a cross-origin request, then the request itself, for
-- the page's origin with credentials allowed. Every request is recorded.
corsCredentialsApp :: IORef [Recorded] -> Application
corsCredentialsApp requests request respond = do
  recordRequest requests request
  if Wai.requestMethod request == "OPTIONS"
    then
      respond
        ( responseLBS
            status204
            [ ("Access-Control-Allow-Origin", origin)
            , ("Access-Control-Allow-Credentials", "true")
            , ("Access-Control-Allow-Methods", "POST, GET, OPTIONS, DELETE")
            , ("Access-Control-Allow-Headers", "authorization,custom")
            ]
            LBS8.empty
        )
    else
      respond
        ( responseLBS
            status200
            [ (hContentType, "text/plain")
            , ("Access-Control-Allow-Origin", origin)
            , ("Access-Control-Allow-Credentials", "true")
            ]
            "done"
        )
  where
    host = fromMaybe BS8.empty (lookup "host" (Wai.requestHeaders request))
    origin = "http://" <> otherOrigin host

-- | The same host and port under the other name, since 127.0.0.1 and localhost
-- are different origins.
otherOrigin :: BS8.ByteString -> BS8.ByteString
otherOrigin host
  | "127.0.0.1" `BS8.isPrefixOf` host = "localhost" <> BS8.drop 9 host
  | "localhost" `BS8.isPrefixOf` host = "127.0.0.1" <> BS8.drop 9 host
  | otherwise = host

-- | Redirects /redirect with a 301 to /something on the other origin. Every other
-- request goes to 'corsCredentialsApp'.
redirectingCorsCredentialsApp :: IORef [Recorded] -> Application
redirectingCorsCredentialsApp requests request respond =
  case pathInfo request of
    ["redirect"] -> do
      recordRequest requests request
      respond (responseLBS status301 [(hLocation, "http://" <> otherOrigin host <> "/something")] LBS8.empty)
    _ ->
      corsCredentialsApp requests request respond
  where
    host = fromMaybe BS8.empty (lookup "host" (Wai.requestHeaders request))

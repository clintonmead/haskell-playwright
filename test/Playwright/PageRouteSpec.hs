module Playwright.PageRouteSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad
  ( replicateM
  , replicateM_
  , void
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString
  ( ByteString
  )
import Data.Maybe
  ( isJust
  , isNothing
  )
import Data.Text
  ( Text
  , isInfixOf
  , isSuffixOf
  , takeWhileEnd
  , toLower
  )
import Network.HTTP.Types
  ( hContentType
  , hLocation
  , hReferer
  , status200
  , status302
  , status404
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
import Network.Wai qualified as Wai
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , staticApp
  )
import Playwright
  ( ContentType (ContentType)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType, fulfillHeaders, fulfillStatus)
  , HeaderName (HeaderName, headerNameToText)
  , HeaderValue (HeaderValue, headerValueToText)
  , HttpHeader (HttpHeader, httpHeaderName, httpHeaderValue)
  , HttpStatus (HttpStatus)
  , HttpMethod (httpMethodToText)
  , Playwright
  , Request
  , RequestFailure (requestFailureText)
  , ResourceType (resourceTypeToText)
  , RouteOptions (routeTimes)
  , RouteTimes (RouteTimes)
  , UrlMatch (UrlMatchGlob, UrlMatchPredicate, UrlMatchRegex)
  , abortRoute
  , abortRouteWith
  , armRequestFailed
  , continueRoute
  , currentUrl
  , defaultFulfillOptions
  , defaultGotoOptions
  , defaultRouteOptions
  , evaluate
  , evaluateText
  , evaluateWith
  , fallbackRoute
  , framesEqual
  , armResponse
  , fulfillRoute
  , requestAllHeaders
  , requestResponse
  , responseHeaders
  , goto
  , mainFrame
  , requestFailure
  , requestFrame
  , requestHeaders
  , requestIsNavigation
  , requestMethod
  , requestPostData
  , requestRedirectedFrom
  , requestResourceType
  , requestUrl
  , responseOk
  , responseRequest
  , responseUrl
  , route
  , routeRequest
  , routeWith
  , setExtraHTTPHeaders
  , textArgument
  , unroute
  , unrouteAll
  , urlFromText
  , urlToText
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , assertProtocolContains
  , withAssets
  , withChromiumPage
  , testAssetsDir
  , withLocalApp
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( poll
  , wait
  , withAsync
  )
import UnliftIO.Exception
  ( try
  )
import UnliftIO.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , putMVar
  , takeMVar
  )

tests :: TestTree
tests =
  testGroup
    "page-route"
    [ testCase "should intercept" $
        withAssets $ \prefix page -> do
          captured <- newIORef False
          route page (UrlMatchGlob "**/empty.html") $ \handled -> do
            request <- routeRequest handled
            url <- requestUrl request
            assertBool "empty.html" ("empty.html" `isInfixOf` urlToText url)
            headers <- requestHeaders request
            assertBool "user-agent" (isJust (headerValue "user-agent" headers))
            method <- requestMethod request
            assertEqual "method" "GET" (httpMethodToText method)
            postData <- requestPostData request
            assertBool "postData" (isNothing postData)
            navigation <- requestIsNavigation request
            assertEqual "navigation" True navigation
            resourceType <- requestResourceType request
            assertEqual "resource" "document" (resourceTypeToText resourceType)
            frame <- requestFrame request
            case frame of
              Nothing ->
                assertFailure "expected frame"
              Just actual -> do
                assertEqual "main frame" True (framesEqual actual (mainFrame page))
                blank <- currentUrl actual
                assertEqual "blank" "about:blank" (urlToText blank)
            writeIORef captured True
            continueRoute handled
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "ok" True =<< responseOk value
          assertEqual "intercepted" True =<< readIORef captured
    , testCase "should show custom HTTP headers" $
        withAssets $ \prefix page -> do
          setExtraHTTPHeaders
            page
            [HttpHeader (HeaderName "foo") (HeaderValue "bar")]
          route page (UrlMatchGlob "**/*") $ \handled -> do
            request <- routeRequest handled
            headers <- requestHeaders request
            assertEqual "foo" (Just "bar") (headerValue "foo" headers)
            continueRoute handled
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "ok" True =<< responseOk value
    , testCase "should contain referer header" $
        withAssets $ \prefix page -> do
          captured <- newIORef []
          route page (UrlMatchGlob "**/*") $ \handled -> do
            request <- routeRequest handled
            modifyIORef' captured (request :)
            continueRoute handled
          _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
          requests <- reverse <$> readIORef captured
          case requests of
            (_ : css : _) -> do
              url <- requestUrl css
              assertBool "css" ("/one-style.css" `isInfixOf` urlToText url)
              headers <- requestHeaders css
              case headerValue "referer" headers of
                Nothing ->
                  assertFailure "missing referer"
                Just referer ->
                  assertBool "html" ("/one-style.html" `isInfixOf` referer)
            _ ->
              assertFailure "expected stylesheet request"
    , testCase "should unroute" $
        withAssets $ \prefix page -> do
          intercepted <- newIORef ([] :: [Int])
          route page (UrlMatchGlob "**/*") $ \handled -> do
            modifyIORef' intercepted (1 :)
            fallbackRoute handled
          route page (UrlMatchGlob "**/empty.html") $ \handled -> do
            modifyIORef' intercepted (2 :)
            fallbackRoute handled
          route page (UrlMatchGlob "**/empty.html") $ \handled -> do
            modifyIORef' intercepted (3 :)
            fallbackRoute handled
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "before" [3, 2, 1] . reverse =<< readIORef intercepted
          writeIORef intercepted []
          unroute page (UrlMatchGlob "**/empty.html")
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "after" [1] . reverse =<< readIORef intercepted
    , testCase "page.unrouteAll removes all routes" $
        withAssets $ \prefix page -> do
          route page (UrlMatchGlob "**/*") abortRoute
          route page (UrlMatchGlob "**/empty.html") abortRoute
          unrouteAll page
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "ok" True =<< responseOk value
    , testCase "should be abortable" $
        withAssets $ \prefix page -> do
          route page (UrlMatchGlob "**/*.css") abortRoute
          takeFailed <- armRequestFailed page
          response <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              assertEqual "ok" True =<< responseOk value
              request <- responseRequest value
              failure <- requestFailure request
              assertEqual "document not failed" Nothing (requestFailureText <$> failure)
          failed <- takeFailed
          url <- requestUrl failed
          assertBool "stylesheet failed" (".css" `isInfixOf` urlToText url)
    , testCase "should be abortable with custom error codes" $
        withAssets $ \prefix page -> do
          route page (UrlMatchGlob "**/*") (\handled -> abortRouteWith handled "internetdisconnected")
          takeFailed <- armRequestFailed page
          attempt (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
          failed <- takeFailed
          reason <- requestFailure failed
          assertEqual "error text" (Just "net::ERR_INTERNET_DISCONNECTED") (requestFailureText <$> reason)
    , testCase "should fail navigation when aborting main resource" $
        withAssets $ \prefix page -> do
          route page (UrlMatchGlob "**/*") abortRoute
          assertProtocolContains
            "net::ERR_FAILED"
            (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
    , testCase "should not throw if request was cancelled by the page" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          captured <- newEmptyMVar
          route page (UrlMatchGlob "**/data.json") (putMVar captured)
          withAsync
            ( attempt
                ( evaluateWith
                    page
                    "url => { globalThis.controller = new AbortController(); return fetch(url, { signal: globalThis.controller.signal }); }"
                    (textArgument (prefix <> "/data.json"))
                )
            )
            $ \fetching -> do
              pending <- takeMVar captured
              takeFailed <- armRequestFailed page
              _ <- evaluate page "() => globalThis.controller.abort()"
              failed <- takeFailed
              reason <- requestFailure failed
              assertBool "cancelled" (maybe False (isCancelled . requestFailureText) reason)
              abortRoute pending
              wait fetching
    , testCase "should send referer" $ do
        captured <- newIORef Nothing
        withChromiumPage $ \page ->
          withLocalApp (refererRecordingApp captured) $ \prefix -> do
            setExtraHTTPHeaders
              page
              [HttpHeader (HeaderName "referer") (HeaderValue "http://google.com/")]
            route page (UrlMatchGlob "**/*") continueRoute
            _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
            assertEqual "server referer" (Just "http://google.com/") =<< readIORef captured
    , testCase "should support the times parameter with route matching" $
        withAssets $ \prefix page -> do
          intercepted <- newIORef (0 :: Int)
          routeWith
            page
            (UrlMatchGlob "**/empty.html")
            defaultRouteOptions {routeTimes = Just (RouteTimes 1)}
            $ \handled -> do
              modifyIORef' intercepted (+ 1)
              continueRoute handled
          replicateM_ 3 (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
          assertEqual "intercepted once" (1 :: Int) =<< readIORef intercepted
    , testCase "should support async handler w/ times" $
        withAssets $ \prefix page -> do
          routeWith
            page
            (UrlMatchGlob "**/empty.html")
            defaultRouteOptions {routeTimes = Just (RouteTimes 1)}
            $ \handled -> do
              liftIO (threadDelay 100000)
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillBody = Just (FulfillText "<html>intercepted</html>")
                  , fulfillContentType = Just (ContentType "text/html")
                  }
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "first" "intercepted" =<< evaluateText page "() => document.body.textContent"
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          second <- evaluateText page "() => document.body.textContent"
          assertEqual "second" False ("intercepted" == second)
    , testCase "should navigate to dataURL and not fire dataURL requests" $
        withChromiumPage $ \page -> do
          seen <- newIORef (0 :: Int)
          route page (UrlMatchGlob "**/*") $ \handled -> do
            modifyIORef' seen (+ 1)
            continueRoute handled
          response <- goto page (urlFromText "data:text/html,<div>yo</div>") defaultGotoOptions
          assertEqual "no response" True (isNothing response)
          assertEqual "requests" (0 :: Int) =<< readIORef seen
    , testCase "should work with equal requests" $ do
        counter <- newIORef (1 :: Int)
        withChromiumPage $ \page ->
          withLocalApp (countingApp counter) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            alternate <- newIORef False
            route page (UrlMatchGlob "**/*") $ \handled -> do
              cancel <- readIORef alternate
              writeIORef alternate (not cancel)
              if cancel
                then abortRoute handled
                else continueRoute handled
            results <-
              replicateM 3 (evaluateText page "() => fetch('/zzz').then(response => response.text()).catch(() => 'FAILED')")
            assertEqual "results" ["11", "FAILED", "22"] results
    , testCase "should not work with redirects" $ do
        withChromiumPage $ \page ->
          withLocalApp redirectChainApp $ \prefix -> do
            intercepted <- newIORef (0 :: Int)
            route page (UrlMatchGlob "**/*") $ \handled -> do
              modifyIORef' intercepted (+ 1)
              continueRoute handled
            response <- goto page (urlFromText (prefix <> "/non-existing-page.html")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                assertEqual "ok" True =<< responseOk value
                finalUrl <- responseUrl value
                assertBool "final url" ("/empty.html" `isInfixOf` urlToText finalUrl)
                assertEqual "intercepted once" (1 :: Int) =<< readIORef intercepted
                chain <- responseRequest value >>= redirectChain
                assertEqual "chain length" (5 :: Int) (length chain)
                paths <- mapM (fmap (pathOf . urlToText) . requestUrl) chain
                assertEqual
                  "chain"
                  [ "/empty.html"
                  , "/non-existing-page-4.html"
                  , "/non-existing-page-3.html"
                  , "/non-existing-page-2.html"
                  , "/non-existing-page.html"
                  ]
                  paths
    , testCase "should work with regular expression" $
        withAssets $ \prefix page -> do
          route page (UrlMatchRegex "empty\\.html$" "") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions {fulfillBody = Just (FulfillText "intercepted")}
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "body" "intercepted" =<< evaluateText page "() => document.body.textContent"
    , testCase "should work with predicate" $
        withAssets $ \prefix page -> do
          let wanted = prefix <> "/digits/2.png"
          route page (UrlMatchPredicate (\url -> urlToText url == wanted)) $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions {fulfillBody = Just (FulfillText "intercepted")}
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "matched" "intercepted"
            =<< evaluateText page "() => fetch('/digits/2.png').then(r => r.text())"
          other <- evaluateText page "() => fetch('/digits/1.png').then(r => r.text())"
          assertBool "other request not intercepted" (other /= "intercepted")
    , testCase "should not support ? in glob pattern" $
        withAssets $ \prefix page -> do
          -- In a glob, ? is a literal question mark, so ?hello matches only index?hello.
          route page (UrlMatchGlob "**/index?hello") $ \handled ->
            fulfillRoute handled defaultFulfillOptions {fulfillBody = Just (FulfillText "any character")}
          route page (UrlMatchGlob "**/index\\?hello") $ \handled ->
            fulfillRoute handled defaultFulfillOptions {fulfillBody = Just (FulfillText "question mark")}
          _ <- goto page (urlFromText (prefix <> "/index?hello")) defaultGotoOptions
          assertEqual "question mark" "question mark" =<< evaluateText page "() => document.body.textContent"
          _ <- goto page (urlFromText (prefix <> "/index1hello")) defaultGotoOptions
          body <- evaluateText page "() => document.body.textContent"
          assertBool "index1hello is not intercepted" (body `notElem` ["any character", "question mark"])
    , testCase "should work with redirect inside sync XHR" $ do
        assets <- testAssetsDir
        withChromiumPage $ \page ->
          withLocalApp (redirectingAssetsApp assets) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/*") continueRoute
            status <-
              evaluateText
                page
                "() => { const request = new XMLHttpRequest(); request.open('GET', '/logo.png', false); request.send(null); return String(request.status); }"
            assertEqual "status" "200" status
    , testCase "should contain raw request header" $
        withAssets $ \prefix page -> do
          headers <- newIORef []
          route page (UrlMatchGlob "**/*") $ \handled -> do
            request <- routeRequest handled
            writeIORef headers =<< requestAllHeaders request
            continueRoute handled
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertBool "accept" . isJust . headerValue "accept" =<< readIORef headers
    , testCase "should contain raw response header" $
        withAssets $ \prefix page -> do
          requests <- newIORef Nothing
          route page (UrlMatchGlob "**/*") $ \handled -> do
            request <- routeRequest handled
            writeIORef requests (Just request)
            continueRoute handled
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertContentTypeOfResponse =<< readIORef requests
    , testCase "should contain raw response header after fulfill" $
        withAssets $ \prefix page -> do
          requests <- newIORef Nothing
          route page (UrlMatchGlob "**/*") $ \handled -> do
            request <- routeRequest handled
            writeIORef requests (Just request)
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillBody = Just (FulfillText "Hello")
                , fulfillContentType = Just (ContentType "text/html")
                }
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertContentTypeOfResponse =<< readIORef requests
    , testCase "should support cors with GET" $
        withAssets $ \prefix page -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/cars*") $ \handled -> do
              request <- routeRequest handled
              allowed <- isSuffixOf "allow" . urlToText <$> requestUrl request
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillStatus = Just (HttpStatus 200)
                  , fulfillHeaders =
                      [HttpHeader (HeaderName "access-control-allow-origin") (HeaderValue (if allowed then "*" else "none"))]
                  , fulfillContentType = Just (ContentType "application/json")
                  , fulfillBody = Just (FulfillText "[\"electric\", \"gas\"]")
                  }
            allowed <-
              evaluateText page "async () => JSON.stringify(await (await fetch('https://example.com/cars?allow', {mode: 'cors'})).json())"
            assertEqual "allowed" "[\"electric\",\"gas\"]" allowed
            rejected <- try (evaluateText page "async () => (await fetch('https://example.com/cars?reject', {mode: 'cors'})).json()")
            case rejected of
              Left (ProtocolRpc failure) ->
                assertBool "rejected" ("Failed" `isInfixOf` protocolFailureMessage failure)
              Left err ->
                assertFailure ("expected ProtocolRpc, got: " <> show err)
              Right _ ->
                assertFailure "expected the rejected fetch to fail"
    , testCase "should add Access-Control-Allow-Origin by default when fulfill" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          route page (UrlMatchGlob "**/cars") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus 200)
                , fulfillContentType = Just (ContentType "application/json")
                , fulfillBody = Just (FulfillText "[\"electric\", \"gas\"]")
                }
          takeResponse <- armResponse page (UrlMatchGlob "https://example.com/cars")
          result <-
            evaluateText
              page
              "async () => JSON.stringify(await (await fetch('https://example.com/cars', {method: 'POST', headers: {'Content-Type': 'application/json'}, mode: 'cors', body: JSON.stringify({number: 1})})).json())"
          assertEqual "result" "[\"electric\",\"gas\"]" result
          response <- takeResponse
          headers <- responseHeaders response
          assertEqual "allow origin" (Just prefix) (headerValue "access-control-allow-origin" headers)
    , testCase "should allow null origin for about:blank" $
        withChromiumPage $ \page -> do
          route page (UrlMatchGlob "**/something") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus 200)
                , fulfillContentType = Just (ContentType "text/plain")
                , fulfillBody = Just (FulfillText "done")
                }
          takeResponse <- armResponse page (UrlMatchGlob "https://example.com/something")
          text <-
            evaluateText
              page
              "async () => (await fetch('https://example.com/something', {method: 'GET', headers: {'X-PINGOTHER': 'pingpong'}})).text()"
          assertEqual "text" "done" text
          response <- takeResponse
          headers <- responseHeaders response
          assertEqual "allow origin" (Just "null") (headerValue "access-control-allow-origin" headers)
    , testCase "should support cors with POST" $
        withAssets $ \_ page -> do
          route page (UrlMatchGlob "**/cars") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus 200)
                , fulfillContentType = Just (ContentType "application/json")
                , fulfillHeaders = [HttpHeader (HeaderName "Access-Control-Allow-Origin") (HeaderValue "*")]
                , fulfillBody = Just (FulfillText "[\"electric\", \"gas\"]")
                }
          result <- evaluateText page (postCarsScript "")
          assertEqual "result" "[\"electric\",\"gas\"]" result
    , testCase "should support cors with credentials" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          route page (UrlMatchGlob "**/cars") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus 200)
                , fulfillContentType = Just (ContentType "application/json")
                , fulfillHeaders =
                    [ HttpHeader (HeaderName "Access-Control-Allow-Origin") (HeaderValue prefix)
                    , HttpHeader (HeaderName "Access-Control-Allow-Credentials") (HeaderValue "true")
                    ]
                , fulfillBody = Just (FulfillText "[\"electric\", \"gas\"]")
                }
          result <- evaluateText page (postCarsScript "credentials: 'include'")
          assertEqual "result" "[\"electric\",\"gas\"]" result
    , testCase "should reject cors with disallowed credentials" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          route page (UrlMatchGlob "**/cars") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus 200)
                , fulfillContentType = Just (ContentType "application/json")
                , fulfillHeaders = [HttpHeader (HeaderName "Access-Control-Allow-Origin") (HeaderValue prefix)]
                , fulfillBody = Just (FulfillText "[\"electric\", \"gas\"]")
                }
          rejected <- try (evaluateText page (postCarsScript "credentials: 'include'")) :: Playwright (Either PlaywrightError Text)
          case rejected of
            Left _ ->
              pure ()
            Right _ ->
              assertFailure "expected the credentialed fetch to be rejected"
    , testCase "should support cors for different methods" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          route page (UrlMatchGlob "**/cars") $ \handled -> do
            request <- routeRequest handled
            method <- httpMethodToText <$> requestMethod request
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus 200)
                , fulfillContentType = Just (ContentType "application/json")
                , fulfillHeaders = [HttpHeader (HeaderName "Access-Control-Allow-Origin") (HeaderValue "*")]
                , fulfillBody = Just (FulfillText ("[\"" <> method <> "\", \"electric\", \"gas\"]"))
                }
          posted <- evaluateText page (postCarsScript "")
          assertEqual "post" "[\"POST\",\"electric\",\"gas\"]" posted
          deleted <-
            evaluateText
              page
              "async () => JSON.stringify(await (await fetch('https://example.com/cars', {method: 'DELETE', headers: {}, mode: 'cors', body: ''})).json())"
          assertEqual "delete" "[\"DELETE\",\"electric\",\"gas\"]" deleted
    , testCase "should pause intercepted XHR until continue" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          routes <- newEmptyMVar
          route page (UrlMatchGlob "**/global-var.html") $ \handled -> putMVar routes handled
          withAsync
            ( evaluateText
                page
                "() => { const request = new XMLHttpRequest(); request.open('GET', '/global-var.html', false); request.send(null); return String(request.status); }"
            )
            $ \pending -> do
              handled <- takeMVar routes
              liftIO (threadDelay 500000)
              finished <- poll pending
              assertBool "paused" (isNothing finished)
              continueRoute handled
              assertEqual "status" "200" =<< wait pending
    , testCase "should pause intercepted fetch request until continue" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          routes <- newEmptyMVar
          route page (UrlMatchGlob "**/global-var.html") $ \handled -> putMVar routes handled
          withAsync
            (evaluateText page "async () => String((await fetch('/global-var.html')).status)")
            $ \pending -> do
              handled <- takeMVar routes
              liftIO (threadDelay 500000)
              finished <- poll pending
              assertBool "paused" (isNothing finished)
              continueRoute handled
              assertEqual "status" "200" =<< wait pending
    ]

-- | A Playwright error from an action that is expected to fail, ignored here.
attempt :: Playwright a -> Playwright ()
attempt action = do
  outcome <- try (void action) :: Playwright (Either PlaywrightError ())
  case outcome of
    Left _ ->
      pure ()
    Right () ->
      pure ()

-- | The last path segment of a URL, with its leading slash.
pathOf :: Text -> Text
pathOf url =
  "/" <> takeWhileEnd (/= '/') url

-- | Whether a failure reason says the request was cancelled or aborted.
isCancelled :: Text -> Bool
isCancelled reason =
  any (`isInfixOf` toLower reason) ["cancelled", "aborted"]

-- | Walks redirectedFrom back to the first request of the chain.
redirectChain :: Request -> Playwright [Request]
redirectChain request = do
  previous <- requestRedirectedFrom request
  case previous of
    Nothing ->
      pure [request]
    Just before ->
      (request :) <$> redirectChain before

-- | Serves the test assets, counting each /zzz request.
countingApp :: IORef Int -> Application
countingApp counter request respond =
  case pathInfo request of
    ["zzz"] -> do
      n <- readIORef counter
      writeIORef counter (n + 1)
      respond (responseLBS status200 [(hContentType, "text/plain")] (LBS8.pack (show (n * 11))))
    ["empty.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] "")
    _ ->
      respond (responseLBS status404 [] "")

-- | Redirects /non-existing-page.html through three more pages to /empty.html.
redirectChainApp :: Application
redirectChainApp request respond =
  case pathInfo request of
    ["non-existing-page.html"] -> redirectTo "non-existing-page-2.html"
    ["non-existing-page-2.html"] -> redirectTo "non-existing-page-3.html"
    ["non-existing-page-3.html"] -> redirectTo "non-existing-page-4.html"
    ["non-existing-page-4.html"] -> redirectTo "empty.html"
    ["empty.html"] -> respond (responseLBS status200 [(hContentType, "text/html")] "")
    _ -> respond (responseLBS status404 [] "")
  where
    redirectTo target =
      respond (responseLBS status302 [(hLocation, target)] "")

headerValue :: Text -> [HttpHeader] -> Maybe Text
headerValue name headers =
  case fmap (headerValueToText . httpHeaderValue)
    ( filter
        ( \header ->
            toLower (headerNameToText (httpHeaderName header)) == name
        )
        headers
    ) of
    [value] ->
      Just value
    _ ->
      Nothing

-- | Serves grid.html and records the Referer header the server received.
refererRecordingApp :: IORef (Maybe ByteString) -> Application
refererRecordingApp captured request respond =
  case pathInfo request of
    ["grid.html"] -> do
      writeIORef captured (lookup hReferer (Wai.requestHeaders request))
      respond (responseLBS status200 [(hContentType, "text/html")] "")
    _ ->
      respond (responseLBS status404 [] "")

-- | Redirects /logo.png to /pptr.png, and serves every other path from the assets
-- directory.
redirectingAssetsApp :: FilePath -> Application
redirectingAssetsApp assets request respond =
  case pathInfo request of
    ["logo.png"] ->
      respond (responseLBS status302 [(hLocation, "/pptr.png")] "")
    _ ->
      staticApp (defaultFileServerSettings assets) request respond

-- | Checks that the response of the recorded request has a content type header.
assertContentTypeOfResponse :: Maybe Request -> Playwright ()
assertContentTypeOfResponse recorded =
  case recorded of
    Nothing ->
      assertFailure "no request was routed"
    Just request -> do
      response <- requestResponse request
      case response of
        Nothing ->
          assertFailure "no response"
        Just value ->
          assertBool "content-type" . isJust . headerValue "content-type" =<< responseHeaders value

-- | A POST to example.com/cars with a JSON body, which resolves to the parsed
-- JSON, with extra fetch options such as credentials.
postCarsScript :: Text -> Text
postCarsScript options =
  "async () => JSON.stringify(await (await fetch('https://example.com/cars', {method: 'POST', headers: {'Content-Type': 'application/json'}, mode: 'cors', body: JSON.stringify({number: 1}), "
    <> options
    <> "})).json())"

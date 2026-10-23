module Playwright.PageRoute2Spec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Maybe
  ( isNothing
  )
import Data.Text
  ( Text
  , breakOn
  , isInfixOf
  , replace
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
  ( responseLBS
  )
import Playwright
  ( ContentType (ContentType)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType, fulfillHeaders, fulfillStatus)
  , HeaderName (HeaderName, headerNameToText)
  , HeaderValue (HeaderValue)
  , HttpHeader (HttpHeader, httpHeaderName)
  , HttpStatus (HttpStatus, httpStatusToInt)
  , HttpMethod (httpMethodToText)
  , Playwright
  , Request
  , browserVersion
  , withPage
  , UrlMatch (UrlMatchGlob)
  , abortRoute
  , continueRoute
  , defaultFulfillOptions
  , defaultGotoOptions
  , evaluate
  , evaluateText
  , evaluateWith
  , armResponse
  , fulfillRoute
  , ContinueOptions (continueHeaders)
  , CookieName (CookieName)
  , CookieValue (CookieValue)
  , addCookies
  , cookieForUrl
  , continueRouteWith
  , defaultContinueOptions
  , pageContext
  , reload
  , responseOk
  , responseStatus
  , setContent
  , setExtraHTTPHeaders
  , requestAllHeaders
  , requestResponse
  , responseHeaders
  , goto
  , requestHeaders
  , requestMethod
  , requestPostData
  , responseUrl
  , route
  , routeRequest
  , setExtraHTTPHeaders
  , textArgument
  , urlFromText
  , urlToText
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( Recorded (recordedHeaders, recordedMethod, recordedPath)
  , assertBool
  , assertEqual
  , assertFailure
  , withAssets
  , withAssetsPrefix
  , withChromiumBrowser
  , withChromiumPage
  , withLocalApp
  )
import Playwright.PageRouteSupport
  ( crossProcessPrefix
  , headerValue
  , postCarsScript
  , optionsScript
  , optionsApp
  , recordingOkApp
  , notFoundApp
  , malformedApp
  )
import Test.Tasty
  ( testGroup
  , TestTree
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
  ( newIORef
  , readIORef
  , writeIORef
  , modifyIORef'
  )
import UnliftIO.MVar
  ( modifyMVar_
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , takeMVar
  )

tests :: TestTree
tests =
  testGroup
    "page-route"
    [ testCase "should add Access-Control-Allow-Origin by default when fulfill" $
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
    , testCase "should respect cors overrides" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          route page (UrlMatchGlob "**/something") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus 200)
                , fulfillContentType = Just (ContentType "text/plain")
                , fulfillHeaders = [HttpHeader (HeaderName "Access-Control-Allow-Origin") (HeaderValue "http://non-existent")]
                , fulfillBody = Just (FulfillText "done")
                }
          let
            cross = replace "127.0.0.1" "localhost" prefix <> "/something"
          rejected <-
            try
              ( evaluateText
                  page
                  ( "async () => (await (await fetch('" <> cross <> "', {method: 'GET', headers: {'X-PINGOTHER': 'pingpong'}})).text())"
                  )
              )
              :: Playwright (Either PlaywrightError Text)
          case rejected of
            Left (ProtocolRpc failure) ->
              assertBool "failed to fetch" ("Failed to fetch" `isInfixOf` protocolFailureMessage failure)
            Left err ->
              assertFailure ("expected ProtocolRpc, got: " <> show err)
            Right _ ->
              assertFailure "expected the fetch to be rejected"
    , testCase "should not auto-intercept non-preflight OPTIONS without network interception" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (optionsApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            writeIORef requests []
            texts <- evaluateText page (optionsScript (replace "127.0.0.1" "localhost" prefix <> "/something"))
            assertEqual "texts" "[\"Hello\",\"World\"]" texts
            seen <- readIORef requests
            assertEqual "requests" [("OPTIONS", "/something"), ("OPTIONS", "/something"), ("GET", "/something")] (map (\request -> (recordedMethod request, recordedPath request)) (reverse seen))
    , testCase "should not auto-intercept non-preflight OPTIONS with network interception" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (optionsApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/something") continueRoute
            writeIORef requests []
            texts <- evaluateText page (optionsScript (replace "127.0.0.1" "localhost" prefix <> "/something"))
            assertEqual "texts" "[\"Hello\",\"World\"]" texts
            seen <- readIORef requests
            assertEqual "requests" [("OPTIONS", "/something"), ("GET", "/something")] (map (\request -> (recordedMethod request, recordedPath request)) (reverse seen))
    , testCase "should properly return navigation response when URL has cookies" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          addCookies
            (pageContext page)
            [cookieForUrl (urlFromText (prefix <> "/empty.html")) (CookieName "foo") (CookieValue "bar")]
          route page (UrlMatchGlob "**/*") continueRoute
          response <- reload page
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "ok" True =<< responseOk value
    , testCase "should not override cookie header" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingOkApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            _ <- evaluateText page "() => { document.cookie = 'original=value'; return document.cookie; }"
            seen <- newIORef Nothing
            route page (UrlMatchGlob "**") $ \handled -> do
              request <- routeRequest handled
              headers <- requestAllHeaders request
              writeIORef seen (headerValue "cookie" headers)
              continueRouteWith
                handled
                defaultContinueOptions
                  { continueHeaders =
                      Just
                        ( filter ((/= "cookie") . toLower . headerNameToText . httpHeaderName) headers
                            <> [HttpHeader (HeaderName "cookie") (HeaderValue "overridden=value")]
                        )
                  }
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            assertEqual "route cookie" (Just "original=value") =<< readIORef seen
            recorded <- readIORef requests
            case recorded of
              latest : _ ->
                assertEqual "server cookie" (Just "original=value") (lookup "cookie" (recordedHeaders latest))
              [] ->
                assertFailure "the server saw no request"
    , testCase "should be able to remove headers" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingOkApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            seen <- newIORef Nothing
            route page (UrlMatchGlob "**/*") $ \handled -> do
              request <- routeRequest handled
              headers <- requestAllHeaders request
              writeIORef seen (headerValue "foo" headers)
              continueRouteWith
                handled
                defaultContinueOptions
                  { continueHeaders =
                      Just (filter ((/= "foo") . toLower . headerNameToText . httpHeaderName) headers)
                  }
            _ <-
              evaluateText
                page
                ( "async () => { await fetch('"
                    <> prefix
                    <> "/title.html', { headers: { foo: 'bar' } }); return 'done'; }"
                )
            assertEqual "route foo" (Just "bar") =<< readIORef seen
            recorded <- readIORef requests
            case recorded of
              latest : _ ->
                assertEqual "server foo" Nothing (lookup "foo" (recordedHeaders latest))
              [] ->
                assertFailure "the server saw no request"
    , testCase "should fulfill with redirect status" $
        withChromiumPage $ \page ->
          withLocalApp (\_ respond -> respond (responseLBS status200 [(hContentType, "text/plain")] "foo")) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/redirect_this") $ \handled ->
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillStatus = Just (HttpStatus 301)
                  , fulfillHeaders = [HttpHeader (HeaderName "location") (HeaderValue "/final")]
                  }
            text <-
              evaluateText
                page
                ( "async () => { const response = await fetch('"
                    <> prefix
                    <> "/redirect_this'); return response.text(); }"
                )
            assertEqual "text" "foo" text
    , testCase "should work with custom referer headers" $
        withAssetsPrefix $ \prefix ->
          withChromiumBrowser $ \browser -> do
            version <- browserVersion browser
            let referer = prefix <> "/empty.html"
                major = read (unpack (fst (breakOn "." version))) :: Int
                expected = if major < 154 then referer <> ", " <> referer else referer
            withPage browser $ \page -> do
              seen <- newIORef []
              setExtraHTTPHeaders page [HttpHeader (HeaderName "referer") (HeaderValue referer)]
              route page (UrlMatchGlob "**/*") $ \handled -> do
                request <- routeRequest handled
                headers <- requestAllHeaders request
                modifyIORef' seen (headerValue "referer" headers :)
                continueRoute handled
              response <- goto page (urlFromText referer) defaultGotoOptions
              case response of
                Nothing ->
                  assertFailure "missing response"
                Just value ->
                  assertEqual "ok" True =<< responseOk value
              values <- readIORef seen
              assertBool "requests seen" (not (null values))
              assertEqual "referers" [] (filter (/= Just expected) values)
    , testCase "should navigate to URL with hash and fire requests without hash" $
        withChromiumPage $ \page ->
          withLocalApp (\_ respond -> respond (responseLBS status200 [(hContentType, "text/html")] "")) $ \prefix -> do
            seen <- newIORef (0 :: Int)
            route page (UrlMatchGlob "**/*") $ \handled -> do
              modifyIORef' seen (+ 1)
              continueRoute handled
            response <- goto page (urlFromText (prefix <> "/empty.html#hash")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                assertEqual "ok" True =<< responseOk value
                url <- responseUrl value
                assertEqual "url" (prefix <> "/empty.html") (urlToText url)
            assertEqual "route requests" (1 :: Int) =<< readIORef seen
    , testCase "should work with encoded server" $
        withChromiumPage $ \page ->
          withLocalApp notFoundApp $ \prefix -> do
            route page (UrlMatchGlob "**/*") continueRoute
            response <- goto page (urlFromText (prefix <> "/some nonexisting page")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value ->
                assertEqual "status" 404 . httpStatusToInt =<< responseStatus value
    , testCase "should work with badly encoded server" $
        withChromiumPage $ \page ->
          withLocalApp malformedApp $ \prefix -> do
            route page (UrlMatchGlob "**/*") continueRoute
            response <- goto page (urlFromText (prefix <> "/malformed?rnd=%911")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value ->
                assertEqual "status" 200 . httpStatusToInt =<< responseStatus value
    , testCase "should work with encoded server - 2" $
        withChromiumPage $ \page ->
          withLocalApp notFoundApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            requests <- newIORef ([] :: [Request])
            route page (UrlMatchGlob "**/*") $ \handled -> do
              request <- routeRequest handled
              continueRoute handled
              modifyIORef' requests (<> [request])
            setContent page ("<link rel=\"stylesheet\" href=\"" <> prefix <> "/fonts?helvetica|arial\"/>")
            seen <- readIORef requests
            assertEqual "requests" (1 :: Int) (length seen)
            case seen of
              [request] -> do
                response <- requestResponse request
                case response of
                  Nothing ->
                    assertFailure "no response"
                  Just value ->
                    assertEqual "status" 404 . httpStatusToInt =<< responseStatus value
              _ ->
                assertFailure "unexpected requests"
    , testCase "should show custom HTTP headers" $
        withAssets $ \prefix page -> do
          setExtraHTTPHeaders page [HttpHeader (HeaderName "foo") (HeaderValue "bar")]
          seen <- newIORef Nothing
          route page (UrlMatchGlob "**/*") $ \handled -> do
            request <- routeRequest handled
            headers <- requestHeaders request
            writeIORef seen (headerValue "foo" headers)
            continueRoute handled
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "foo" (Just "bar") =<< readIORef seen
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "ok" True =<< responseOk value
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
    , testCase "should intercept when postData is more than 1MB" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          captured <- newEmptyMVar
          route page (UrlMatchGlob "**/404.html") $ \handled -> do
            request <- routeRequest handled
            postData <- requestPostData request
            abortRoute handled
            putMVar captured postData
          let
            body = pack (replicate (2 * 1024 * 1024) '0')
          _ <- evaluateWith page "text => fetch('/404.html', { method: 'POST', body: text }).catch(() => {})" (textArgument body)
          assertEqual "post data" (Just (encodeUtf8 body)) =<< takeMVar captured
    , testCase "should intercept main resource during cross-process navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          intercepted <- newEmptyMVar
          let
            target = crossProcessPrefix prefix <> "/empty.html"
          route page (UrlMatchGlob target) $ \handled -> do
            putMVar intercepted ()
            continueRoute handled
          response <- goto page (urlFromText target) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "ok" True =<< responseOk value
          takeMVar intercepted
    , testCase "should be able to intercept every navigation to a page controlled by service worker" $
        withAssets $ \prefix page -> do
          interceptions <- newMVar (0 :: Int)
          let
            target = prefix <> "/serviceworkers/bug-33561/index.html"
          route page (UrlMatchGlob target) $ \handled -> do
            modifyMVar_ interceptions (pure . (+ 1))
            continueRoute handled
          _ <- goto page (urlFromText target) defaultGotoOptions
          _ <- evaluate page "() => window['activationPromise']"
          _ <- goto page (urlFromText target) defaultGotoOptions
          assertEqual "interceptions" 2 =<< readMVar interceptions
    ]

module Playwright.PageNetworkRequestSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad
  ( filterM
  , when
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Aeson
  ( object
  , (.=)
  )
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.CaseInsensitive qualified as CI
import Data.List
  ( find
  , sort
  )
import Data.Maybe
  ( fromMaybe
  , isJust
  )
import Data.Text
  ( Text
  , isInfixOf
  , isPrefixOf
  , pack
  , replace
  , stripPrefix
  , takeWhileEnd
  , toLower
  , unpack
  )
import Network.HTTP.Types
  ( Header
  , hContentType
  , hLocation
  , status200
  , status204
  , status302
  , status304
  )
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , staticApp
  )
import Network.Wai
  ( Application
  , responseLBS
  )
import Network.Wai qualified as Wai
import Playwright
  ( ContentType (ContentType)
  , ContinueOptions (continueHeaders)
  , FulfillBody (FulfillText, FulfillBytes)
  , FulfillOptions (fulfillBody, fulfillContentType)
  , HeaderName (HeaderName, headerNameToText)
  , HeaderValue (HeaderValue, headerValueToText)
  , HttpHeader (httpHeaderName, httpHeaderValue)
  , HttpMethod (httpMethodToText)
  , HttpStatus (httpStatusToInt)
  , Playwright
  , Request
  , RequestFailure (requestFailureText)
  , ResourceType (resourceTypeToText)
  , UrlMatch (UrlMatchGlob)
  , armPopup
  , armRequest
  , armRequestFailed
  , click
  , continueRoute
  , continueRouteWith
  , defaultContinueOptions
  , defaultFulfillOptions
  , defaultGotoOptions
  , evaluate
  , evaluateText
  , evaluateTextWith
  , frames
  , framesEqual
  , fulfillRoute
  , goto
  , mainFrame
  , onContextRequest
  , onRequest
  , reload
  , requestAllHeaders
  , requestFailure
  , requestFrame
  , requestHeaderValue
  , requestHeaders
  , requestHeadersArray
  , requestIsNavigation
  , requestMethod
  , requestPostData
  , requestPostDataJSON
  , requestRedirectedFrom
  , requestResourceType
  , requestResponse
  , requestUrl
  , responseAllHeaders
  , responseRequest
  , responseStatus
  , responseStatusText
  , responseText
  , route
  , routeRequest
  , setContent
  , textArgument
  , url
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , locate
  , readTestAsset
  , testAssetsDir
  , withAssets
  , withChromiumContext
  , withChromiumPage
  , withLocalApp
  )
import UnliftIO.IORef
  ( IORef
  , modifyIORef
  , newIORef
  , readIORef
  , writeIORef
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
    "page-network-request"
    [ testCase "should work for main frame navigation request" $
        withAssets $ \prefix page -> do
          armRequest page (UrlMatchGlob (prefix <> "/empty.html")) >>= \handle -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            request <- handle
            frame <- requestFrame request
            case frame of
              Nothing ->
                assertFailure "expected frame"
              Just actual ->
                assertEqual "main frame" True (framesEqual actual (mainFrame page))
    , testCase "should work for subframe navigation request" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          armRequest page (UrlMatchGlob (prefix <> "/frames/frame.html")) >>= \handle -> do
            _ <-
              evaluate
                page
                "() => { const frame = document.createElement('iframe'); frame.src = '/frames/frame.html'; document.body.appendChild(frame); }"
            request <- handle
            frame <- requestFrame request
            pageFrames <- frames page
            case (frame, drop 1 pageFrames) of
              (Just actual, child : _) ->
                assertBool "subframe" (framesEqual actual child)
              _ ->
                assertFailure "expected subframe"
    , testCase "should work for fetch requests" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          armRequest page (UrlMatchGlob (prefix <> "/digits/1.png")) >>= \handle -> do
            _ <- evaluate page "() => fetch('/digits/1.png')"
            request <- handle
            frame <- requestFrame request
            case frame of
              Nothing ->
                assertFailure "expected frame"
              Just actual ->
                assertEqual "main frame" True (framesEqual actual (mainFrame page))
    , testCase "should work for a redirect" $ do
        assets <- testAssetsDir
        withChromiumPage $ \page ->
          withLocalApp (redirectApp assets) $ \prefix -> do
            seen <- newIORef ([] :: [Text])
            onRequest page (recordUrl seen)
            _ <- goto page (urlFromText (prefix <> "/foo.html")) defaultGotoOptions
            assertEqual "requests" [prefix <> "/foo.html", prefix <> "/empty.html"] =<< readIORef seen
    , testCase "should not work for a redirect and interception" $ do
        assets <- testAssetsDir
        withChromiumPage $ \page ->
          withLocalApp (redirectApp assets) $ \prefix -> do
            seen <- newIORef ([] :: [Text])
            route page (UrlMatchGlob (prefix <> "/**")) $ \intercepted -> do
              request <- routeRequest intercepted
              recordUrl seen request
              continueRoute intercepted
            _ <- goto page (urlFromText (prefix <> "/foo.html")) defaultGotoOptions
            current <- url page
            assertEqual "page url" (prefix <> "/empty.html") (urlToText current)
            requests <- readIORef seen
            assertEqual "foo requests" 1 (length (filter (isInfixOf "/foo.html") requests))
    , testCase "should return headers" $
        withAssets $ \prefix page -> do
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              request <- responseRequest value
              headers <- requestHeaders request
              let userAgent =
                    fmap (headerValueToText . httpHeaderValue)
                      ( filter
                          ( \header ->
                              toLower (headerNameToText (httpHeaderName header)) == "user-agent"
                          )
                          headers
                      )
              case userAgent of
                [agent] ->
                  assertBool "Chrome" ("Chrome" `isInfixOf` agent)
                _ ->
                  assertFailure "missing user-agent"
    , testCase "should expose request url after navigation" $
        withAssets $ \prefix page -> do
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              request <- responseRequest value
              requestedUrl <- requestUrl request
              assertEqual "url" (prefix <> "/empty.html") (urlToText requestedUrl)
    , testCase "should return postData" $
        withChromiumPage $ \page ->
          withLocalApp emptyApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            seen <- newIORef (Nothing :: Maybe Request)
            onRequest page (recordPost prefix seen)
            _ <- evaluate page "() => fetch('./post', { method: 'POST', body: JSON.stringify({ foo: 'bar' }) })"
            readIORef seen >>= \found -> case found of
              Nothing ->
                assertFailure "missing post request"
              Just request ->
                assertEqual "postData" (Just (BS8.pack "{\"foo\":\"bar\"}")) =<< requestPostData request
    , testCase "should get undefined with postData() when there is no post data" $
        withChromiumPage $ \page ->
          withLocalApp emptyApp $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                request <- responseRequest value
                assertEqual "postData" Nothing =<< requestPostData request
    , testCase "should get |undefined| with postDataJSON() when there is no post data" $
        withChromiumPage $ \page ->
          withLocalApp emptyApp $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                request <- responseRequest value
                assertEqual "json" Nothing =<< requestPostDataJSON request
    , testCase "should parse the json post data" $
        withChromiumPage $ \page ->
          withLocalApp emptyApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            seen <- newIORef (Nothing :: Maybe Request)
            onRequest page (recordPost prefix seen)
            _ <- evaluate page "() => fetch('./post', { method: 'POST', body: JSON.stringify({ foo: 'bar' }) })"
            readIORef seen >>= \found -> case found of
              Nothing ->
                assertFailure "missing post request"
              Just request ->
                assertEqual "json" (Just (object ["foo" .= ("bar" :: Text)])) =<< requestPostDataJSON request
    , testCase "should override post data content type" $
        withChromiumPage $ \page ->
          withLocalApp echoContentTypeApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/post") $ \intercepted -> do
              request <- routeRequest intercepted
              headers <- requestHeaders request
              continueRouteWith
                intercepted
                defaultContinueOptions {continueHeaders = Just (map overrideContentType headers)}
            body <-
              evaluateText
                page
                "async () => { const response = await fetch('/post', { method: 'POST', body: JSON.stringify({ foo: 'bar' }) }); return await response.text(); }"
            assertEqual "content type" "application/x-www-form-urlencoded; charset=UTF-8" body
    , testCase "should parse the data if content-type is application/x-www-form-urlencoded" $
        withChromiumPage $ \page ->
          withLocalApp emptyApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            seen <- newIORef (Nothing :: Maybe Request)
            onRequest page (recordPost prefix seen)
            setContent
              page
              "<form method='POST' action='/post'><input type='text' name='foo' value='bar'><input type='number' name='baz' value='123'><input type='submit'></form>"
            click (locate page "input[type=submit]")
            readIORef seen >>= \found -> case found of
              Nothing ->
                assertFailure "missing post request"
              Just request ->
                assertEqual
                  "json"
                  (Just (object ["foo" .= ("bar" :: Text), "baz" .= ("123" :: Text)]))
                  =<< requestPostDataJSON request
    , testCase "should parse the data if content-type is application/x-www-form-urlencoded; charset=UTF-8" $
        withChromiumPage $ \page ->
          withLocalApp emptyApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            seen <- newIORef (Nothing :: Maybe Request)
            onRequest page (recordPost prefix seen)
            _ <-
              evaluate
                page
                "() => fetch('./post', { method: 'POST', headers: { 'content-type': 'application/x-www-form-urlencoded; charset=UTF-8' }, body: 'foo=bar&baz=123' })"
            readIORef seen >>= \found -> case found of
              Nothing ->
                assertFailure "missing post request"
              Just request ->
                assertEqual
                  "json"
                  (Just (object ["foo" .= ("bar" :: Text), "baz" .= ("123" :: Text)]))
                  =<< requestPostDataJSON request
    , testCase "should return event source" $
        withChromiumPage $ \page ->
          withLocalApp sseApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            seen <- newIORef ([] :: [Request])
            onRequest page (\request -> modifyIORef seen (<> [request]))
            _ <-
              evaluate
                page
                "() => new Promise(resolve => { const source = new EventSource('/sse'); source.onmessage = () => { source.close(); resolve(); }; })"
            requests <- readIORef seen
            case requests of
              first : _ ->
                assertEqual "resource type" "eventsource" . resourceTypeToText =<< requestResourceType first
              [] ->
                assertFailure "missing event source request"
    , testCase "should return navigation bit" $ do
        assets <- testAssetsDir
        withChromiumPage $ \page ->
          withLocalApp (redirectApp assets) $ \prefix -> do
            seen <- newIORef ([] :: [(Text, Request)])
            onRequest page (recordRequest seen)
            _ <- goto page (urlFromText (prefix <> "/rrredirect")) defaultGotoOptions
            assertNavigation seen "rrredirect" True
            assertNavigation seen "one-frame.html" True
            assertNavigation seen "frame.html" True
            assertNavigation seen "script.js" False
            assertNavigation seen "style.css" False
    , testCase "should return navigation bit when navigating to image" $ do
        assets <- testAssetsDir
        withChromiumPage $ \page ->
          withLocalApp (redirectApp assets) $ \prefix -> do
            seen <- newIORef ([] :: [(Text, Request)])
            onRequest page (recordRequest seen)
            _ <- goto page (urlFromText (prefix <> "/pptr.png")) defaultGotoOptions
            entries <- readIORef seen
            case entries of
              (_, first) : _ ->
                assertBool "navigation" =<< requestIsNavigation first
              [] ->
                assertFailure "missing request"
    , testCase "page.reload return 304 status code" $ do
        counter <- newIORef (0 :: Int)
        withChromiumPage $ \page ->
          withLocalApp (reloadApp counter) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/test.html")) defaultGotoOptions
            response <- reload page
            assertEqual "requests" 2 =<< readIORef counter
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                status <- responseStatus value
                assertEqual "status" 200 (httpStatusToInt status)
                assertEqual "status text" "OK" =<< responseStatusText value
                assertEqual "text" "<div>Test</div>" =<< responseText value
    , testCase "should get the same headers as the server" $ do
        seen <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordHeadersApp seen ["empty.html"] [] "done") $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                request <- responseRequest value
                clientHeaders <- requestAllHeaders request
                served <- readIORef seen
                assertEqual
                  "headers"
                  (normalizeHeaders (waiHeaderPairs served))
                  (normalizeHeaders (httpHeaderPairs clientHeaders))
    , testCase "should not return allHeaders() until they are available" $ do
        seen <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordHeadersApp seen ["empty.html"] [("foo", "bar")] "done") $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                request <- responseRequest value
                clientHeaders <- requestAllHeaders request
                served <- readIORef seen
                assertEqual
                  "request headers"
                  (normalizeHeaders (waiHeaderPairs served))
                  (normalizeHeaders (httpHeaderPairs clientHeaders))
                responseHeaders <- responseAllHeaders value
                assertEqual "foo" (Just "bar") (lookupHeader "foo" responseHeaders)
    , testCase "should get the same headers as the server CORS" $ do
        seen <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordHeadersApp seen ["something"] [("Access-Control-Allow-Origin", "*")] "done") $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            let crossUrl = crossOriginPrefix prefix <> "/something"
            handle <- armRequest page (UrlMatchGlob crossUrl)
            text <- evaluateTextWith page crossOriginFetchScript (textArgument crossUrl)
            assertEqual "text" "done" text
            request <- handle
            clientHeaders <- requestAllHeaders request
            served <- readIORef seen
            assertEqual
              "headers"
              (normalizeHeaders (waiHeaderPairs served))
              (normalizeHeaders (httpHeaderPairs clientHeaders))
    , testCase "should not get preflight CORS requests when intercepting" $ do
        methods <- newIORef ([] :: [Text])
        withChromiumPage $ \page ->
          withLocalApp (preflightApp methods) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            let crossUrl = crossOriginPrefix prefix <> "/something"
            -- First check the browser sends a preflight request when interception is off.
            writeIORef methods []
            text <- evaluateTextWith page deleteFetchScript (textArgument crossUrl)
            assertEqual "text" "done" text
            assertEqual "preflight" ["OPTIONS", "DELETE"] =<< readIORef methods
            -- Now check the browser does not send a preflight request when interception is on.
            writeIORef methods []
            routed <- newIORef ([] :: [Text])
            route page (UrlMatchGlob "**/something") $ \intercepted -> do
              method <- requestMethod =<< routeRequest intercepted
              modifyIORef routed (<> [httpMethodToText method])
              continueRoute intercepted
            routedText <- evaluateTextWith page deleteFetchScript (textArgument crossUrl)
            assertEqual "text" "done" routedText
            assertEqual "routed" ["DELETE"] =<< readIORef routed
            assertEqual "server" ["DELETE"] =<< readIORef methods
    , testCase "should work with binary post data" $
        withChromiumPage $ \page ->
          withLocalApp emptyApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            seen <- newIORef (Nothing :: Maybe Request)
            onRequest page (recordPost prefix seen)
            _ <- evaluate page binaryPostScript
            readIORef seen >>= \found -> case found of
              Nothing ->
                assertFailure "missing post request"
              Just request ->
                assertEqual "binary postData" (Just allBytes) =<< requestPostData request
    , testCase "should work with binary post data and interception" $
        withChromiumPage $ \page ->
          withLocalApp emptyApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            seen <- newIORef (Nothing :: Maybe Request)
            route page (UrlMatchGlob "**/post") continueRoute
            onRequest page (recordPost prefix seen)
            _ <- evaluate page binaryPostScript
            readIORef seen >>= \found -> case found of
              Nothing ->
                assertFailure "missing post request"
              Just request ->
                assertEqual "binary postData" (Just allBytes) =<< requestPostData request
    , testCase "should return multipart/form-data" $
        withChromiumPage $ \page ->
          withLocalApp emptyApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/*") continueRoute
            handle <- armRequest page (UrlMatchGlob (prefix <> "/post"))
            _ <- evaluate page multipartScript
            request <- handle
            contentType <- requestHeaderValue request (HeaderName "Content-Type")
            case contentType >>= stripPrefix "multipart/form-data; boundary=" of
              Nothing ->
                assertFailure ("content type " <> show contentType)
              Just boundary -> do
                body <- requestPostData request
                assertEqual "multipart body" (Just (BS8.pack (multipartBody (unpack boundary)))) body
    , testCase "should report raw headers" $ do
        seen <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordHeadersApp seen ["headers"] [] "") $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            handle <- armRequest page (UrlMatchGlob (prefix <> "/headers"))
            _ <- evaluate page rawHeadersScript
            request <- handle
            served <- readIORef seen
            rawHeaders <- requestHeadersArray request
            -- The browser adds its own headers after the raw list is taken, so compare the custom ones.
            let customOnly :: [(Text, Text)] -> [(Text, Text)]
                customOnly = filter (isPrefixOf "header-" . fst)
            assertEqual
              "raw headers"
              (normalizeHeaders (customOnly (waiHeaderPairs served)))
              (normalizeHeaders (customOnly (httpHeaderPairs rawHeaders)))
            headerA <- requestHeaderValue request (HeaderName "header-a")
            assertEqual "header-a" (Just "value-a, value-a-1, value-a-2") headerA
            missing <- requestHeaderValue request (HeaderName "not-there")
            assertEqual "not-there" Nothing missing
    , testCase "should report raw response headers in redirects" $
        withChromiumPage $ \page ->
          withLocalApp redirectChainApp $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/redirect/1.html")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                request <- responseRequest value
                chain <- collectChain request
                assertEqual
                  "redirect chain"
                  [ (prefix <> "/redirect/1.html", Just "1.html")
                  , (prefix <> "/redirect/2.html", Just "2.html")
                  , (prefix <> "/empty.html", Just "empty.html")
                  ]
                  chain
    , testCase "should report all cookies in one header" $
        withChromiumPage $ \page ->
          withLocalApp emptyApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            _ <-
              evaluate
                page
                "() => { document.cookie = 'myCookie=myValue'; document.cookie = 'myOtherCookie=myOtherValue'; }"
            response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                request <- responseRequest value
                headers <- requestAllHeaders request
                assertEqual
                  "cookie"
                  (Just "myCookie=myValue; myOtherCookie=myOtherValue")
                  (lookupHeader "cookie" headers)
    , testCase "should not allow to access frame on popup main request" $
        withChromiumContext $ \ctx page ->
          withLocalApp emptyApp $ \prefix -> do
            seen <- newIORef ([] :: [Request])
            onContextRequest ctx (\request -> modifyIORef seen (<> [request]))
            setContent page ("<a target=_blank href=\"" <> prefix <> "/empty.html\">click me</a>")
            popup <- armPopup page
            click (locate page "a")
            popupPage <- popup
            request <- awaitRecorded seen (prefix <> "/empty.html")
            assertBool "navigation" =<< requestIsNavigation request
            frame <- requestFrame request
            -- The Haskell API returns Maybe rather than throwing, so the popup main frame is asserted directly.
            assertBool "popup main frame" (maybe False (\actual -> framesEqual actual (mainFrame popupPage)) frame)
    , testCase "should handle mixed-content blocked requests" $
        withChromiumPage $ \page -> do
          route page (UrlMatchGlob "**/mixedcontent.html") $ \intercepted ->
            fulfillRoute
              intercepted
              defaultFulfillOptions
                { fulfillContentType = Just (ContentType "text/html")
                , fulfillBody = Just (FulfillText mixedContentPage)
                }
          route page (UrlMatchGlob "**/iconfont.woff2") $ \intercepted -> do
            body <- readTestAsset "webfont/iconfont.woff2"
            fulfillRoute intercepted defaultFulfillOptions {fulfillBody = Just (FulfillBytes body)}
          failed <- armRequestFailed page
          _ <- goto page (urlFromText "https://example.com/mixedcontent.html") defaultGotoOptions
          request <- failed
          headers <- requestAllHeaders request
          assertBool "origin header" (isJust (lookupHeader "origin" headers))
          failure <- requestFailure request
          assertEqual "failure text" (Just "mixed-content") (fmap requestFailureText failure)
    ]

-- | Records the request to the local /post path, for the post-data cases.
recordPost :: Text -> IORef (Maybe Request) -> Request -> Playwright ()
recordPost prefix seen request = do
  target <- urlToText <$> requestUrl request
  when (target == prefix <> "/post") (writeIORef seen (Just request))

-- | Records every request URL, in the order the requests were made.
recordUrl :: IORef [Text] -> Request -> Playwright ()
recordUrl seen request = do
  target <- urlToText <$> requestUrl request
  modifyIORef seen (<> [target])

-- | Records each request under the last segment of its URL.
recordRequest :: IORef [(Text, Request)] -> Request -> Playwright ()
recordRequest seen request = do
  target <- urlToText <$> requestUrl request
  modifyIORef seen (<> [(takeWhileEnd (/= '/') target, request)])

-- | Checks whether the recorded request with the given last segment is a navigation.
assertNavigation :: IORef [(Text, Request)] -> Text -> Bool -> Playwright ()
assertNavigation seen name expected = do
  entries <- readIORef seen
  case lookup name entries of
    Nothing ->
      assertFailure ("missing request " <> show name)
    Just request ->
      assertEqual ("navigation " <> show name) expected =<< requestIsNavigation request

-- | Answers every path with an empty 200, so the post-data cases get a response.
emptyApp :: Application
emptyApp _ respond =
  respond (responseLBS status200 [] LBS8.empty)

-- | Redirects /foo.html to /empty.html and /rrredirect to /frames/one-frame.html,
-- and serves everything else from the assets directory.
redirectApp :: FilePath -> Application
redirectApp assets req respond =
  case Wai.pathInfo req of
    ["foo.html"] ->
      respond (responseLBS status302 [(hLocation, "/empty.html")] LBS8.empty)
    ["rrredirect"] ->
      respond (responseLBS status302 [(hLocation, "/frames/one-frame.html")] LBS8.empty)
    _ ->
      staticApp (defaultFileServerSettings assets) req respond

-- | Answers /post with the Content-Type the request arrived with.
echoContentTypeApp :: Application
echoContentTypeApp req respond =
  case Wai.pathInfo req of
    ["post"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/plain")]
            (LBS8.fromStrict (fromMaybe "<none>" (lookup hContentType (Wai.requestHeaders req))))
        )
    _ ->
      emptyApp req respond

-- | Replaces the Content-Type header with the form-urlencoded type, and leaves the others alone.
overrideContentType :: HttpHeader -> HttpHeader
overrideContentType header
  | toLower (headerNameToText (httpHeaderName header)) == "content-type" =
      header {httpHeaderValue = HeaderValue "application/x-www-form-urlencoded; charset=UTF-8"}
  | otherwise =
      header

-- | Sends one server-sent event at /sse, and answers every other path with an empty 200.
sseApp :: Application
sseApp req respond =
  case Wai.pathInfo req of
    ["sse"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/event-stream"), ("Cache-Control", "no-cache")]
            (LBS8.pack "data: {\"foo\":\"bar\"}\n\n")
        )
    _ ->
      emptyApp req respond

-- | Validator headers that let Chromium revalidate the page on reload.
reloadHeaders :: [Header]
reloadHeaders =
  [ ("cf-cache-status", "DYNAMIC")
  , (hContentType, "text/html;charset=UTF-8")
  , ("Last-Modified", "Fri, 05 Jan 2024 01:56:20 GMT")
  , ("Vary", "Access-Control-Request-Headers")
  ]

-- | Answers /test.html with the page on the first request and 304 after that.
reloadApp :: IORef Int -> Application
reloadApp counter req respond =
  case Wai.pathInfo req of
    ["test.html"] -> do
      count <- readIORef counter
      writeIORef counter (count + 1)
      respond
        ( if count == 0
            then responseLBS status200 reloadHeaders (LBS8.pack "<div>Test</div>")
            else responseLBS status304 reloadHeaders (LBS8.pack "<div>Test</div>")
        )
    _ ->
      emptyApp req respond

-- | Waits until a request with the given URL has been recorded, polling for up to five seconds.
awaitRecorded :: IORef [Request] -> Text -> Playwright Request
awaitRecorded seen target = poll (50 :: Int)
  where
    poll :: Int -> Playwright Request
    poll 0 =
      assertFailure ("missing request " <> show target)
    poll remaining = do
      requests <- readIORef seen
      matching <- filterM (\request -> (== target) . urlToText <$> requestUrl request) requests
      case matching of
        first : _ ->
          pure first
        [] -> do
          liftIO (threadDelay 100000)
          poll (remaining - 1)

-- | Lowercases header names and sorts the pairs, so header sets compare regardless of order and case.
normalizeHeaders :: [(Text, Text)] -> [(Text, Text)]
normalizeHeaders =
  sort . map (\(name, value) -> (toLower name, value))

-- | Converts the headers the server saw into text pairs.
waiHeaderPairs :: [Header] -> [(Text, Text)]
waiHeaderPairs =
  map (\(name, value) -> (pack (BS8.unpack (CI.foldedCase name)), pack (BS8.unpack value)))

-- | Converts Playwright headers into text pairs.
httpHeaderPairs :: [HttpHeader] -> [(Text, Text)]
httpHeaderPairs =
  map (\header -> (headerNameToText (httpHeaderName header), headerValueToText (httpHeaderValue header)))

-- | Finds the value of a header by case-insensitive name.
lookupHeader :: Text -> [HttpHeader] -> Maybe Text
lookupHeader name headers =
  headerValueToText . httpHeaderValue
    <$> find (\header -> toLower (headerNameToText (httpHeaderName header)) == toLower name) headers

-- | Walks the redirect chain that ends at the request, oldest first, pairing each URL with its sec-test-header.
collectChain :: Request -> Playwright [(Text, Maybe Text)]
collectChain request = do
  target <- urlToText <$> requestUrl request
  response <- requestResponse request
  header <- case response of
    Nothing ->
      pure Nothing
    Just value ->
      lookupHeader "sec-test-header" <$> responseAllHeaders value
  previous <- requestRedirectedFrom request
  older <- maybe (pure []) collectChain previous
  pure (older <> [(target, header)])

-- | Stores the headers of the request to one path, and answers every request with the given headers and body.
recordHeadersApp :: IORef [Header] -> [Text] -> [Header] -> String -> Application
recordHeadersApp seen segments extra body req respond = do
  when (Wai.pathInfo req == segments) (writeIORef seen (Wai.requestHeaders req))
  respond (responseLBS status200 extra (LBS8.pack body))

-- | Answers /something with CORS headers, records its method, and answers OPTIONS with a 204 preflight.
preflightApp :: IORef [Text] -> Application
preflightApp methods req respond
  | Wai.pathInfo req /= ["something"] =
      emptyApp req respond
  | method == "OPTIONS" = do
      modifyIORef methods (<> [method])
      respond
        ( responseLBS
            status204
            [ ("Access-Control-Allow-Origin", "*")
            , ("Access-Control-Allow-Methods", "POST, GET, OPTIONS, DELETE")
            , ("Access-Control-Allow-Headers", "*")
            , ("Cache-Control", "no-cache")
            ]
            LBS8.empty
        )
  | otherwise = do
      modifyIORef methods (<> [method])
      respond (responseLBS status200 [("Access-Control-Allow-Origin", "*")] (LBS8.pack "done"))
  where
    method = pack (BS8.unpack (Wai.requestMethod req))

-- | Redirects /redirect/1.html to /redirect/2.html and then to /empty.html, tagging each response.
redirectChainApp :: Application
redirectChainApp req respond =
  case Wai.pathInfo req of
    ["redirect", "1.html"] ->
      respond (responseLBS status302 [(hLocation, "/redirect/2.html"), ("sec-test-header", "1.html")] LBS8.empty)
    ["redirect", "2.html"] ->
      respond (responseLBS status302 [(hLocation, "/empty.html"), ("sec-test-header", "2.html")] LBS8.empty)
    ["empty.html"] ->
      respond (responseLBS status200 [("sec-test-header", "empty.html")] LBS8.empty)
    _ ->
      emptyApp req respond

-- | The page URL on localhost rather than 127.0.0.1, so requests from the page are cross-origin.
crossOriginPrefix :: Text -> Text
crossOriginPrefix =
  replace "127.0.0.1" "localhost"

-- | Fetches the URL from the page and returns the body text.
crossOriginFetchScript :: Text
crossOriginFetchScript =
  "async url => { const data = await fetch(url); return await data.text(); }"

-- | Sends a DELETE with a custom header, which triggers a CORS preflight.
deleteFetchScript :: Text
deleteFetchScript =
  "async url => { const data = await fetch(url, { method: 'DELETE', headers: { 'X-PINGOTHER': 'pingpong' } }); return await data.text(); }"

-- | Sends a POST whose body is the bytes 0 through 255.
binaryPostScript :: Text
binaryPostScript =
  "async () => { await fetch('./post', { method: 'POST', body: new Uint8Array(Array.from(Array(256).keys())) }); }"

-- | The bytes 0 through 255, as posted by binaryPostScript.
allBytes :: BS.ByteString
allBytes =
  BS.pack [0 .. 255]

-- | Sends a multipart form with two text fields and a file.
multipartScript :: Text
multipartScript =
  "async () => { const body = new FormData(); body.set('name1', 'value1'); body.set('file', new File(['file-value'], 'foo.txt')); body.set('name2', 'value2'); body.append('name2', 'another-value2'); await fetch('/post', { method: 'POST', body }); }"

-- | The multipart body the browser should send for multipartScript, given its boundary.
multipartBody :: String -> String
multipartBody boundary =
  concat
    [ "--" <> boundary <> "\r\nContent-Disposition: form-data; name=\"name1\"\r\n\r\nvalue1\r\n"
    , "--" <> boundary <> "\r\nContent-Disposition: form-data; name=\"file\"; filename=\"foo.txt\"\r\nContent-Type: application/octet-stream\r\n\r\nfile-value\r\n"
    , "--" <> boundary <> "\r\nContent-Disposition: form-data; name=\"name2\"\r\n\r\nvalue2\r\n"
    , "--" <> boundary <> "\r\nContent-Disposition: form-data; name=\"name2\"\r\n\r\nanother-value2\r\n"
    , "--" <> boundary <> "--\r\n"
    ]

-- | Sends a fetch with repeated header names, so the raw header list keeps every entry.
rawHeadersScript :: Text
rawHeadersScript =
  "() => fetch('/headers', { headers: [['header-a', 'value-a'], ['header-b', 'value-b'], ['header-a', 'value-a-1'], ['header-a', 'value-a-2']] })"

-- | A page whose font is served from another origin, so Chromium blocks it as mixed content.
mixedContentPage :: Text
mixedContentPage =
  pack
    ( unlines
        [ "<!doctype html>"
        , "<meta charset=\"utf-8\">"
        , "<style>"
        , "@font-face {"
        , "  font-family: 'pwtest-iconfont';"
        , "  src: url('http://another.com/iconfont.woff2') format('woff2');"
        , "}"
        , "body {"
        , "  font-family: 'pwtest-iconfont';"
        , "}"
        , "</style>"
        , "<span>+-</span>"
        ]
    )


module Playwright.PageNetworkRequestSpec
  ( tests
  ) where

import Control.Monad
  ( when
  )
import Data.Aeson
  ( object
  , (.=)
  )
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe
  ( fromMaybe
  )
import Data.Text
  ( Text
  , isInfixOf
  , takeWhileEnd
  , toLower
  )
import Network.HTTP.Types
  ( Header
  , hContentType
  , hLocation
  , status200
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
  ( ContinueOptions (continueHeaders)
  , HeaderName (headerNameToText)
  , HeaderValue (HeaderValue, headerValueToText)
  , HttpHeader (httpHeaderName, httpHeaderValue)
  , HttpStatus (httpStatusToInt)
  , Playwright
  , Request
  , ResourceType (resourceTypeToText)
  , UrlMatch (UrlMatchGlob)
  , click
  , continueRoute
  , continueRouteWith
  , defaultContinueOptions
  , defaultGotoOptions
  , evaluate
  , evaluateText
  , frames
  , framesEqual
  , goto
  , mainFrame
  , onRequest
  , reload
  , requestFrame
  , requestHeaders
  , requestIsNavigation
  , requestPostData
  , requestPostDataJSON
  , requestResourceType
  , requestUrl
  , responseRequest
  , responseStatus
  , responseStatusText
  , responseText
  , route
  , routeRequest
  , setContent
  , url
  , urlFromText
  , urlToText
  , armRequest
  )
import Playwright.Harness
  ( locate
  , testAssetsDir
  , withAssets
  , withChromiumPage
  , withLocalApp
  , assertBool
  , assertEqual
  , assertFailure
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

module Playwright.PageRequestInterceptSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Concurrent.MVar
  ( MVar
  , newEmptyMVar
  , takeMVar
  , tryPutMVar
  )
import Control.Monad
  ( forever
  )
import Data.Aeson
  ( object
  , (.=)
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.Maybe
  ( fromMaybe
  )
import Data.Text
  ( Text
  , isInfixOf
  , isSuffixOf
  , pack
  , toLower
  )
import Data.Text.Encoding
  ( decodeUtf8
  )
import Network.HTTP.Types
  ( hContentType
  , hLocation
  , status200
  , status302
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  , strictRequestBody
  )
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , staticApp
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( FetchOptions (fetchMaxRedirects, fetchPostData, fetchTimeout, fetchUrl)
  , FetchPostData (FetchPostJson)
  , ContentType (ContentType)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType, fulfillHeaders, fulfillResponse, fulfillStatus)
  , GotoOptions (gotoOptionsTimeout)
  , HeaderName (HeaderName, headerNameToText)
  , HeaderValue (HeaderValue, headerValueToText)
  , HttpHeader (HttpHeader, httpHeaderName, httpHeaderValue)
  , HttpMethod (httpMethodToText)
  , HttpStatus (HttpStatus, httpStatusToInt)
  , Page
  , APIResponse
  , Timeout
  , UrlGlob (UrlGlob)
  , apiResponseHeaders
  , apiResponseHeadersArray
  , apiResponseOk
  , apiResponseStatus
  , apiResponseStatusText
  , apiResponseText
  , apiResponseUrl
  , click
  , content
  , defaultFetchOptions
  , defaultFulfillOptions
  , defaultGotoOptions
  , evaluateText
  , evaluateTextWith
  , fetchGet
  , fetchRequest
  , fulfillRoute
  , getByTestId
  , goto
  , milliseconds
  , pageRequest
  , requestMethod
  , requestPostData
  , responseHeaders
  , responseStatus
  , responseText
  , route
  , routeFetch
  , routeFetchWith
  , routeRequest
  , setContent
  , textArgument
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( assertProtocolContains
  , readTestAsset
  , testAssetsDir
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
  localOption (mkTimeout 30000000) $
    testGroup
      "page-request-intercept"
      [ testCase "should fulfill intercepted response" $
          withAssets $ \prefix page -> do
            ctx <- pageRequest page
            route page (UrlGlob "**/*") $ \handled -> do
              response <- fetchRequest ctx =<< routeRequest handled
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillResponse = Just response
                  , fulfillStatus = Just (HttpStatus 201)
                  , fulfillHeaders = [HttpHeader (HeaderName "foo") (HeaderValue "bar")]
                  , fulfillContentType = Just (ContentType "text/plain")
                  , fulfillBody = Just (FulfillText "Yo, page!")
                  }
            navigated <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            case navigated of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                status <- responseStatus value
                assertEqual "status" 201 (httpStatusToInt status)
                headers <- responseHeaders value
                assertEqual "foo" (Just "bar") (headerValue "foo" headers)
                assertEqual "content-type" (Just "text/plain") (headerValue "content-type" headers)
            body <- evaluateText page "() => document.body.textContent"
            assertEqual "body" "Yo, page!" body
      , testCase "should fulfill response with empty body" $
          withAssets $ \prefix page -> do
            ctx <- pageRequest page
            route page (UrlGlob "**/*") $ \handled -> do
              response <- fetchRequest ctx =<< routeRequest handled
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillResponse = Just response
                  , fulfillHeaders = [HttpHeader (HeaderName "content-length") (HeaderValue "0")]
                  , fulfillStatus = Just (HttpStatus 201)
                  , fulfillBody = Just (FulfillText "")
                  }
            navigated <- goto page (urlFromText (prefix <> "/title.html")) defaultGotoOptions
            case navigated of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                status <- responseStatus value
                assertEqual "status" 201 (httpStatusToInt status)
                assertEqual "body" "" =<< responseText value
      , testCase "should override with defaults when intercepted response not provided" $
          withEmptyFooApp $ \prefix page -> do
            ctx <- pageRequest page
            route page (UrlGlob "**/*") $ \handled -> do
              _ <- fetchRequest ctx =<< routeRequest handled
              fulfillRoute
                handled
                defaultFulfillOptions {fulfillStatus = Just (HttpStatus 201)}
            navigated <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            case navigated of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                status <- responseStatus value
                assertEqual "status" 201 (httpStatusToInt status)
                assertEqual "body" "" =<< responseText value
                headers <- responseHeaders value
                assertEqual "headers" [] headers
      , testCase "should fulfill with any response" $
          withSampleApp $ \prefix page -> do
            ctx <- pageRequest page
            sample <- fetchGet ctx (urlFromText (prefix <> "/sample"))
            route page (UrlGlob "**/*") $ \handled ->
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillResponse = Just sample
                  , fulfillStatus = Just (HttpStatus 201)
                  , fulfillContentType = Just (ContentType "text/plain")
                  }
            navigated <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            case navigated of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                status <- responseStatus value
                assertEqual "status" 201 (httpStatusToInt status)
                assertEqual "body" "Woo-hoo" =<< responseText value
                headers <- responseHeaders value
                assertEqual "foo" (Just "bar") (headerValue "foo" headers)
      , testCase "should support fulfill after intercept" $
          withAssets $ \prefix page -> do
            ctx <- pageRequest page
            original <- fmap decodeUtf8 (readTestAsset "title.html")
            route page (UrlGlob "**") $ \handled -> do
              response <- fetchRequest ctx =<< routeRequest handled
              fulfillRoute handled defaultFulfillOptions {fulfillResponse = Just response}
            navigated <- goto page (urlFromText (prefix <> "/title.html")) defaultGotoOptions
            case navigated of
              Nothing ->
                assertFailure "missing response"
              Just value ->
                assertEqual "body" original =<< responseText value
      , testCase "should give access to the intercepted response" $
          withAssets $ \prefix page -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            ctx <- pageRequest page
            seen <- newIORef (Nothing :: Maybe APIResponse)
            route page (UrlGlob "**/title.html") $ \handled -> do
              response <- fetchRequest ctx =<< routeRequest handled
              writeIORef seen (Just response)
              fulfillRoute handled defaultFulfillOptions {fulfillResponse = Just response}
            _ <-
              evaluateTextWith
                page
                "url => fetch(url).then(r => r.text())"
                (textArgument (prefix <> "/title.html"))
            response <-
              maybe (assertFailure "missing intercepted response") pure =<< readIORef seen
            assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
            assertEqual "statusText" "OK" (apiResponseStatusText response)
            assertEqual "ok" True (apiResponseOk response)
            assertBool "url" ("/title.html" `isSuffixOf` urlToText (apiResponseUrl response))
            assertBool
              "content-type"
              (maybe False ("text/html" `isInfixOf`) (headerValue "content-type" (apiResponseHeaders response)))
            assertBool
              "headersArray"
              (maybe False ("text/html" `isInfixOf`) (headerValue "content-type" (apiResponseHeadersArray response)))
      , testCase "should give access to the intercepted response body" $
          withAssets $ \prefix page -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            ctx <- pageRequest page
            route page (UrlGlob "**/simple.json") $ \handled -> do
              response <- fetchRequest ctx =<< routeRequest handled
              assertEqual "body" "{\"foo\": \"bar\"}\n" =<< apiResponseText response
              fulfillRoute handled defaultFulfillOptions {fulfillResponse = Just response}
            _ <-
              evaluateTextWith
                page
                "url => fetch(url).then(r => r.text()).catch(() => '')"
                (textArgument (prefix <> "/simple.json"))
            pure ()
      , testCase "should fulfill intercepted response using alias" $
          withAssets $ \prefix page -> do
            route page (UrlGlob "**/*") $ \handled -> do
              response <- routeFetch handled
              fulfillRoute handled defaultFulfillOptions {fulfillResponse = Just response}
            navigated <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            case navigated of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                status <- responseStatus value
                assertEqual "status" 200 (httpStatusToInt status)
                headers <- responseHeaders value
                case headerValue "content-type" headers of
                  Nothing ->
                    assertFailure "missing content-type"
                  Just contentType ->
                    assertBool "html" ("text/html" `isInfixOf` contentType)
      , testCase "should support timeout option in route.fetch" $
          withSlowApp $ \prefix page -> do
            timeout <- requireTimeout 1000
            navTimeout <- requireTimeout 2000
            route page (UrlGlob "**/*") $ \handled ->
              assertProtocolContains
                "Timeout 1000ms exceeded"
                (routeFetchWith handled defaultFetchOptions {fetchTimeout = Just timeout})
            assertProtocolContains
              "Timeout 2000ms exceeded"
              ( goto
                  page
                  (urlFromText (prefix <> "/slow"))
                  defaultGotoOptions {gotoOptionsTimeout = Just navTimeout}
              )
      , testCase "should not follow redirects when maxRedirects is set to 0 in route.fetch" $
          withRedirectApp $ \prefix page -> do
            route page (UrlGlob "**/*") $ \handled -> do
              response <-
                routeFetchWith handled defaultFetchOptions {fetchMaxRedirects = Just 0}
              assertEqual "location" (Just "/empty.html") (headerValue "location" (apiResponseHeaders response))
              assertEqual "status" 302 (httpStatusToInt (apiResponseStatus response))
              fulfillRoute handled defaultFulfillOptions {fulfillBody = Just (FulfillText "hello")}
            _ <- goto page (urlFromText (prefix <> "/foo")) defaultGotoOptions
            html <- content page
            assertBool "hello" ("hello" `isInfixOf` html)
      , testCase "should intercept with url override" $
          withAssets $ \prefix page -> do
            route page (UrlGlob "**/*.html") $ \handled -> do
              response <-
                routeFetchWith
                  handled
                  defaultFetchOptions {fetchUrl = Just (urlFromText (prefix <> "/one-style.html"))}
              fulfillRoute handled defaultFulfillOptions {fulfillResponse = Just response}
            navigated <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            case navigated of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                status <- responseStatus value
                assertEqual "status" 200 (httpStatusToInt status)
                body <- responseText value
                assertBool "css" ("one-style.css" `isInfixOf` body)
      , testCase "should intercept with post data override" $
          withCapturedEmpty $ \prefix captured page -> do
            route page (UrlGlob "**/*.html") $ \handled -> do
              response <-
                routeFetchWith
                  handled
                  defaultFetchOptions {fetchPostData = Just (FetchPostJson (object ["foo" .= ("bar" :: Text)]))}
              fulfillRoute handled defaultFulfillOptions {fulfillResponse = Just response}
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            body <- takeMVar captured
            assertEqual "post" "{\"foo\":\"bar\"}" (decodeUtf8 body)
      , testCase "request.postData is not null when fetching FormData with a Blob" $
          withAssets $ \prefix page -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            posted <- newEmptyMVar
            route page (UrlGlob (prefix <> "/upload")) $ \handled -> do
              request <- routeRequest handled
              assertEqual "method" "POST" . httpMethodToText =<< requestMethod request
              payload <- requestPostData request
              _ <- tryPutMVar posted (fromMaybe mempty payload)
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillStatus = Just (HttpStatus 200)
                  , fulfillBody = Just (FulfillText "ok")
                  }
            setContent
              page
              "<script>function doStuff() { const formData = new FormData(); formData.append('file', new Blob(['hello'], { type: 'text/plain' })); fetch('/upload', { method: 'POST', body: formData }); }</script><body><button onclick=\"doStuff()\" data-testid=\"click-me\">Click me!</button></body>"
            click (getByTestId page "click-me")
            postData <- fmap decodeUtf8 (takeMVar posted)
            assertBool "disposition" ("Content-Disposition: form-data; name=\"file\"; filename=\"blob\"" `isInfixOf` postData)
            assertBool "hello" ("\r\nhello\r\n" `isInfixOf` postData)
      , testCase "should abort favicon requests if interception is enabled" $
          withFaviconApp $ \prefix hits page -> do
            route page (UrlGlob "**/*") $ \handled ->
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillStatus = Just (HttpStatus 200)
                  , fulfillBody = Just (FulfillText "Hello, world!")
                  }
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            fetched <-
              evaluateText
                page
                "() => fetch('/favicon.ico').then(r => r.text()).catch(() => 'load failed')"
            assertEqual "favicon" "load failed" fetched
            threadDelay 1000000
            assertEqual "server hits" (0 :: Int) =<< readIORef hits
      ]

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

requireTimeout :: Double -> IO Timeout
requireTimeout n =
  case milliseconds n of
    Nothing ->
      assertFailure ("milliseconds " <> show n)
    Just timeout ->
      pure timeout

withEmptyFooApp :: (Text -> Page -> IO a) -> IO a
withEmptyFooApp action =
  withApplication (pure emptyFooApp) $ \port ->
    withChromiumPage $ \page ->
      action ("http://127.0.0.1:" <> pack (show port)) page

emptyFooApp :: Application
emptyFooApp request respond =
  case pathInfo request of
    ["empty.html"] ->
      respond (responseLBS status200 [("foo", "bar")] "my content")
    _ ->
      respond (responseLBS status200 [(hContentType, "text/html")] "<html><body></body></html>")

withSampleApp :: (Text -> Page -> IO a) -> IO a
withSampleApp action = do
  assets <- testAssetsDir
  withApplication (pure (sampleApp assets)) $ \port ->
    withChromiumPage $ \page ->
      action ("http://127.0.0.1:" <> pack (show port)) page

sampleApp :: FilePath -> Application
sampleApp assets request respond =
  case pathInfo request of
    ["sample"] ->
      respond (responseLBS status200 [("foo", "bar")] "Woo-hoo")
    _ ->
      staticApp (defaultFileServerSettings assets) request respond

withSlowApp :: (Text -> Page -> IO a) -> IO a
withSlowApp action =
  withApplication (pure slowApp) $ \port ->
    withChromiumPage $ \page ->
      action ("http://127.0.0.1:" <> pack (show port)) page

slowApp :: Application
slowApp request respond =
  case pathInfo request of
    ["slow"] ->
      forever (threadDelay 1000000)
    _ ->
      respond (responseLBS status200 [(hContentType, "text/html")] "<html></html>")

withRedirectApp :: (Text -> Page -> IO a) -> IO a
withRedirectApp action = do
  assets <- testAssetsDir
  withApplication (pure (redirectApp assets)) $ \port ->
    withChromiumPage $ \page ->
      action ("http://127.0.0.1:" <> pack (show port)) page

redirectApp :: FilePath -> Application
redirectApp assets request respond =
  case pathInfo request of
    ["foo"] ->
      respond (responseLBS status302 [(hLocation, "/empty.html")] "")
    _ ->
      staticApp (defaultFileServerSettings assets) request respond

withCapturedEmpty :: (Text -> MVar ByteString -> Page -> IO a) -> IO a
withCapturedEmpty action = do
  captured <- newEmptyMVar
  assets <- testAssetsDir
  withApplication (pure (captureEmpty assets captured)) $ \port ->
    withChromiumPage $ \page ->
      action ("http://127.0.0.1:" <> pack (show port)) captured page

captureEmpty :: FilePath -> MVar ByteString -> Application
captureEmpty assets captured request respond =
  case pathInfo request of
    ["empty.html"] -> do
      body <- fmap LBS.toStrict (strictRequestBody request)
      _ <- tryPutMVar captured body
      staticApp (defaultFileServerSettings assets) request respond
    _ ->
      staticApp (defaultFileServerSettings assets) request respond

withFaviconApp :: (Text -> IORef Int -> Page -> IO a) -> IO a
withFaviconApp action = do
  hits <- newIORef (0 :: Int)
  assets <- testAssetsDir
  withApplication (pure (faviconApp assets hits)) $ \port ->
    withChromiumPage $ \page ->
      action ("http://127.0.0.1:" <> pack (show port)) hits page

faviconApp :: FilePath -> IORef Int -> Application
faviconApp assets hits request respond =
  case pathInfo request of
    ["favicon.ico"] -> do
      atomicModifyIORef' hits (\n -> (n + 1, ()))
      respond (responseLBS status200 [(hContentType, "text/plain")] "my content")
    _ ->
      staticApp (defaultFileServerSettings assets) request respond

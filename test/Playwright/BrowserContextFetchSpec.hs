module Playwright.BrowserContextFetchSpec
  ( tests
  ) where

import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Zlib qualified as Zlib
import Control.Concurrent
  ( Chan
  , newChan
  , readChan
  , threadDelay
  , writeChan
  )
import Control.Concurrent.Async
  ( wait
  , withAsync
  )
import Control.Concurrent.MVar
  ( MVar
  , newEmptyMVar
  , takeMVar
  , tryPutMVar
  )
import Control.Exception
  ( try
  )
import Control.Monad
  ( forM_
  , forever
  , replicateM
  )
import Data.Aeson
  ( Value
  , eitherDecodeStrict
  , object
  , (.=)
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , newIORef
  )
import Data.Maybe
  ( fromMaybe
  )
import Data.List
  ( sort
  )
import Data.Text
  ( Text
  , breakOnEnd
  , isInfixOf
  , pack
  , splitOn
  , strip
  , toLower
  )
import Data.Text.Encoding
  ( decodeUtf8
  , encodeUtf8
  )
import Network.HTTP.Types
  ( RequestHeaders
  , hContentType
  , hLocation
  , parseQuery
  , status200
  , status301
  , status302
  , status401
  , status404
  , urlDecode
  )
import Network.HTTP.Types qualified as HTTP
import Network.Wai
  ( Application
  , pathInfo
  , rawQueryString
  , requestHeaders
  , requestMethod
  , responseLBS
  , responseRaw
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
  ( APIRequestContext
  , APIResponse
  , BrowserContext
  , Cookie (cookieDomain, cookieExpires, cookieHttpOnly, cookieName, cookiePath, cookieSameSite, cookieSecure, cookieValue)
  , CookieDomain (cookieDomainToText)
  , CookieExpires (cookieExpiresToScientific)
  , CookieHttpOnly (cookieHttpOnlyToBool)
  , CookieName (CookieName, cookieNameToText)
  , CookiePath (cookiePathToText)
  , CookieSameSite (CookieSameSiteLax, CookieSameSiteStrict)
  , CookieSecure (cookieSecureToBool)
  , CookieValue (CookieValue, cookieValueToText)
  , FetchFormField (FetchFormField)
  , FetchOptions (fetchEncodedParams, fetchFailOnStatusCode, fetchFormData, fetchHeaders, fetchMaxRetries, fetchMethod, fetchPostData, fetchQueryParams, fetchTimeout)
  , FetchPostData (FetchPostBytes, FetchPostJson, FetchPostText)
  , FetchQueryParam (FetchQueryParam)
  , HeaderName (HeaderName, headerNameToText)
  , HeaderValue (HeaderValue, headerValueToText)
  , HttpCredentials (HttpCredentials, httpCredentialsOrigin, httpCredentialsPassword, httpCredentialsUsername)
  , HttpHeader (HttpHeader, httpHeaderName, httpHeaderValue)
  , HttpMethod (HttpMethod)
  , HttpPassword (HttpPassword)
  , HttpStatus (httpStatusToInt)
  , HttpUsername (HttpUsername)
  , NewContextOptions (newContextBaseURL)
  , Page
  , StorageState (storageStateCookies)
  , Timeout
  , Url
  , UrlGlob (UrlGlob)
  , addCookies
  , apiRequestStorageState
  , apiResponseBody
  , apiResponseDispose
  , apiResponseHeaders
  , apiResponseHeadersArray
  , apiResponseJson
  , apiResponseOk
  , apiResponseStatus
  , apiResponseStatusText
  , apiResponseText
  , apiResponseUrl
  , closeContext
  , closeContextWith
  , contextRequest
  , cookieForUrl
  , cookies
  , defaultFetchOptions
  , defaultGotoOptions
  , defaultNewContextOptions
  , disposeAPIRequestContext
  , evaluateText
  , fetchDeleteWith
  , fetchFromUrl
  , fetchFromUrlWith
  , fetchGet
  , fetchGetWith
  , fetchHeadWith
  , fetchPatchWith
  , fetchPostWith
  , fetchPutWith
  , fetchRequestWith
  , goto
  , milliseconds
  , newContextWith
  , pageRequest
  , sessionExpires
  , setContextDefaultTimeout
  , setContextExtraHTTPHeaders
  , setHTTPCredentials
  , storageState
  , urlFromText
  , urlToText
  , waitForRequest
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure, ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( assertProtocolContains
  , testAssetsDir
  , withAssetsContextWith
  , withAssetsPrefix
  , withChromiumBrowser
  , withChromiumContext
  , withChromiumContextWith
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
      "browsercontext-fetch"
      [ testCase "get should work" $
          withAssetsContext $ \prefix ctx -> do
            request <- contextRequest ctx
            response <- fetchGet request (simpleJson prefix)
            assertSimpleJson prefix response
      , testCase "fetch should work" $
          withAssetsContext $ \prefix ctx -> do
            request <- contextRequest ctx
            response <- fetchFromUrl request (simpleJson prefix)
            assertSimpleJson prefix response
      , testCase "should throw on network error" $
          withHangupApp $ \prefix ctx -> do
            request <- contextRequest ctx
            assertNetworkError (fetchGet request (urlFromText (prefix <> "/test")))
      , testCase "should throw on network error after redirect" $
          withHangupApp $ \prefix ctx -> do
            request <- contextRequest ctx
            assertNetworkError (fetchGet request (urlFromText (prefix <> "/redirect")))
      , testCase "should follow redirects" $
          withRedirectJsonApp $ \prefix ctx -> do
            request <- contextRequest ctx
            response <- fetchGet request (urlFromText (prefix <> "/redirect1"))
            assertEqual "url" (urlToText (simpleJson prefix)) (urlToText (apiResponseUrl response))
            assertEqual "json" (object ["foo" .= ("bar" :: Text)]) =<< apiResponseJson response
      , testCase "should add cookies from Set-Cookie header" $
          withSetCookieApp $ \prefix ctx page -> do
            request <- contextRequest ctx
            _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
            jar <- cookies ctx
            assertEqual
              "cookies"
              [("foo", "bar"), ("session", "value")]
              (sort (fmap cookiePair jar))
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            documentCookie <- evaluateText page "() => document.cookie"
            assertEqual
              "document"
              ["foo=bar", "session=value"]
              (sort (fmap trim (splitSemi documentCookie)))
      , testCase "should support timeout option" $
          withSlowApp $ \prefix ctx -> do
            request <- contextRequest ctx
            timeout <- requireTimeout 10
            assertProtocolContains
              "Timeout 10ms exceeded"
              (fetchGetWith request (urlFromText (prefix <> "/slow")) defaultFetchOptions {fetchTimeout = Just timeout})
      , testCase "should dispose" $
          withAssetsContext $ \prefix ctx -> do
            request <- contextRequest ctx
            response <- fetchGet request (simpleJson prefix)
            _ <- apiResponseBody response
            apiResponseDispose response
            result <- try (apiResponseBody response)
            case result of
              Left (DecodeFailure message) ->
                assertBool "disposed" ("disposed" `isInfixOf` message)
              Left err ->
                assertFailure ("expected disposed, got: " <> show err)
              Right _ ->
                assertFailure "expected disposed response body"
      , testCase "should support failOnStatusCode" $
          withMissingApp $ \prefix ctx -> do
            request <- contextRequest ctx
            assertProtocolContains
              "404"
              ( fetchGetWith
                  request
                  (urlFromText (prefix <> "/does-not-exist.html"))
                  defaultFetchOptions {fetchFailOnStatusCode = Just True}
              )
      , testCase "delete should support post data" $
          methodPostDataTest "DELETE" fetchDeleteWith
      , testCase "get should support post data" $
          methodPostDataTest "GET" fetchGetWith
      , testCase "head should support post data" $
          methodPostDataTest "HEAD" fetchHeadWith
      , testCase "patch should support post data" $
          methodPostDataTest "PATCH" fetchPatchWith
      , testCase "post should support post data" $
          methodPostDataTest "POST" fetchPostWith
      , testCase "put should support post data" $
          methodPostDataTest "PUT" fetchPutWith
      , testCase "fetch should support params passed as object" $
          objectParamsTest fetchFromUrlWith
      , testCase "delete should support params passed as object" $
          objectParamsTest fetchDeleteWith
      , testCase "get should support params passed as object" $
          objectParamsTest fetchGetWith
      , testCase "head should support params passed as object" $
          objectParamsTest fetchHeadWith
      , testCase "patch should support params passed as object" $
          objectParamsTest fetchPatchWith
      , testCase "post should support params passed as object" $
          objectParamsTest fetchPostWith
      , testCase "put should support params passed as object" $
          objectParamsTest fetchPutWith
      , testCase "fetch should support params passed as URLSearchParams" $
          duplicateParamsTest fetchFromUrlWith
      , testCase "get should support params passed as URLSearchParams" $
          duplicateParamsTest fetchGetWith
      , testCase "fetch should support params passed as string" $
          encodedParamsTest fetchFromUrlWith
      , testCase "get should support params passed as string" $
          encodedParamsTest fetchGetWith
      , testCase "delete should support params passed as URLSearchParams" $
          duplicateParamsTest fetchDeleteWith
      , testCase "head should support params passed as URLSearchParams" $
          duplicateParamsTest fetchHeadWith
      , testCase "patch should support params passed as URLSearchParams" $
          duplicateParamsTest fetchPatchWith
      , testCase "post should support params passed as URLSearchParams" $
          duplicateParamsTest fetchPostWith
      , testCase "put should support params passed as URLSearchParams" $
          duplicateParamsTest fetchPutWith
      , testCase "delete should support params passed as string" $
          encodedParamsTest fetchDeleteWith
      , testCase "head should support params passed as string" $
          encodedParamsTest fetchHeadWith
      , testCase "patch should support params passed as string" $
          encodedParamsTest fetchPatchWith
      , testCase "post should support params passed as string" $
          encodedParamsTest fetchPostWith
      , testCase "put should support params passed as string" $
          encodedParamsTest fetchPutWith
      , testCase "fetch should support failOnStatusCode" $
          failOnStatusTest fetchFromUrlWith True
      , testCase "delete should support failOnStatusCode" $
          failOnStatusTest fetchDeleteWith True
      , testCase "get should support failOnStatusCode" $
          failOnStatusTest fetchGetWith True
      , testCase "head should support failOnStatusCode" $
          failOnStatusTest fetchHeadWith False
      , testCase "patch should support failOnStatusCode" $
          failOnStatusTest fetchPatchWith True
      , testCase "post should support failOnStatusCode" $
          failOnStatusTest fetchPostWith True
      , testCase "put should support failOnStatusCode" $
          failOnStatusTest fetchPutWith True
      , testCase "should throw on network error when sending body" $
          withPartialBodyApp $ \prefix ctx -> do
            request <- contextRequest ctx
            assertNetworkError (fetchGet request (urlFromText (prefix <> "/test")))
      , testCase "should throw on network error when sending body after redirect" $
          withPartialBodyApp $ \prefix ctx -> do
            request <- contextRequest ctx
            assertNetworkError (fetchGet request (urlFromText (prefix <> "/redirect")))
      , testCase "should not add context cookie if cookie header passed as a parameter" $
          withCookieCapture $ \prefix captured ctx -> do
            addCookies
              ctx
              [cookieForUrl (urlFromText (prefix <> "/empty.html")) (CookieName "username") (CookieValue "John Doe")]
            request <- contextRequest ctx
            _ <-
              fetchGetWith
                request
                (urlFromText (prefix <> "/empty.html"))
                defaultFetchOptions
                  { fetchHeaders = Just [HttpHeader (HeaderName "Cookie") (HeaderValue "foo=bar")]
                  }
            cookie <- takeMVar captured
            assertEqual "cookie" (Just "foo=bar") cookie
      , testCase "should add session cookies to request" $
          withCookieCapture $ \prefix captured ctx -> do
            addCookies
              ctx
              [cookieForUrl (urlFromText (prefix <> "/empty.html")) (CookieName "username") (CookieValue "John Doe")]
            request <- contextRequest ctx
            _ <- fetchGet request (urlFromText (prefix <> "/empty.html"))
            cookie <- takeMVar captured
            assertEqual "cookie" (Just "username=John Doe") cookie
      , testCase "should follow redirects correctly when Location header contains UTF-8 characters" $
          withUtf8RedirectApp $ \prefix ctx -> do
            request <- contextRequest ctx
            response <- fetchGet request (urlFromText (prefix <> "/redirect"))
            assertEqual
              "query"
              [("message", "マスクПривет")]
              (decodeQueryText (urlToText (apiResponseUrl response)))
      , testCase "should preserve cookie order from Set-Cookie header" $
          withOrderedCookieApp $ \prefix captured ctx page -> do
            request <- pageRequest page
            _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
            jar <- cookies ctx
            assertEqual "cookies" [("cookie.0", "foo"), ("cookie.1", "bar")] (fmap cookiePair jar)
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            assertEqual "document" "cookie.0=foo; cookie.1=bar" =<< evaluateText page "() => document.cookie"
            _ <- fetchGet request (urlFromText (prefix <> "/empty.html"))
            cookie <- takeMVar captured
            assertEqual "header" (Just "cookie.0=foo; cookie.1=bar") cookie
      , testCase "should support cookie with empty value" $
          withEmptyCookieApp $ \prefix ctx page -> do
            request <- contextRequest ctx
            _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            assertEqual "document" "first=" =<< evaluateText page "() => document.cookie"
            jar <- cookies ctx
            assertEqual "cookies" [("first", "")] (fmap cookiePair jar)
      , testCase "should not lose body while handling Set-Cookie header" $
          withSetCookieBodyApp $ \prefix ctx -> do
            request <- contextRequest ctx
            response <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
            assertEqual "body" "text content" =<< apiResponseText response
      , testCase "should remove cookie with negative max-age" $
          withNegativeMaxAgeApp $ \prefix captured ctx -> do
            request <- contextRequest ctx
            _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
            _ <- fetchGet request (urlFromText (prefix <> "/removecookie.html"))
            _ <- fetchGet request (urlFromText (prefix <> "/empty.html"))
            cookie <- takeMVar captured
            assertEqual "cookie" (Just "c=v") cookie
      , testCase "should remove cookie with expires far in the past" $
          withPastExpiresApp $ \prefix captured ctx -> do
            request <- contextRequest ctx
            _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
            _ <- fetchGet request (urlFromText (prefix <> "/removecookie.html"))
            _ <- fetchGet request (urlFromText (prefix <> "/empty.html"))
            cookie <- takeMVar captured
            assertEqual "cookie" Nothing cookie
      , testCase "should handle cookies on redirects" $
          cookieRedirectsTest
      , testCase "should return raw headers" $
          withRawHeadersApp $ \prefix ctx -> do
            request <- contextRequest ctx
            response <- fetchGet request (urlFromText (prefix <> "/headers"))
            assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
            assertEqual
              "array"
              [ HttpHeader (HeaderName "Name-A") (HeaderValue "v1")
              , HttpHeader (HeaderName "name-b") (HeaderValue "v4")
              , HttpHeader (HeaderName "Name-a") (HeaderValue "v2")
              , HttpHeader (HeaderName "name-A") (HeaderValue "v3")
              ]
              (filter (headerNameContains "name-") (apiResponseHeadersArray response))
            assertEqual "name-a" (Just "v1, v2, v3") (headerValue "name-a" (apiResponseHeaders response))
            assertEqual "name-b" (Just "v4") (headerValue "name-b" (apiResponseHeaders response))
      , testCase "should work with http credentials" $
          withAuthApp $ \prefix captured ctx -> do
            request <- contextRequest ctx
            response <-
              fetchGetWith
                request
                (urlFromText (prefix <> "/empty.html"))
                defaultFetchOptions
                  { fetchHeaders =
                      Just [HttpHeader (HeaderName "authorization") (HeaderValue "Basic dXNlcjpwYXNz")]
                  }
            auth <- takeMVar captured
            assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
            assertEqual "authorization" (Just "Basic dXNlcjpwYXNz") auth
      , testCase "should work with setHTTPCredentials" $
          withAuthApp $ \prefix _captured ctx -> do
            request <- contextRequest ctx
            first <- fetchGet request (urlFromText (prefix <> "/empty.html"))
            assertEqual "unauthorized" 401 (httpStatusToInt (apiResponseStatus first))
            setHTTPCredentials
              ctx
              HttpCredentials
                { httpCredentialsUsername = HttpUsername "user"
                , httpCredentialsPassword = HttpPassword "pass"
                , httpCredentialsOrigin = Nothing
                }
            second <- fetchGet request (urlFromText (prefix <> "/empty.html"))
            assertEqual "authorized" 200 (httpStatusToInt (apiResponseStatus second))
      , testCase "should return error with wrong credentials" $
          withAuthApp $ \prefix _captured ctx -> do
            request <- contextRequest ctx
            setHTTPCredentials
              ctx
              HttpCredentials
                { httpCredentialsUsername = HttpUsername "user"
                , httpCredentialsPassword = HttpPassword "wrong"
                , httpCredentialsOrigin = Nothing
                }
            response <- fetchGet request (urlFromText (prefix <> "/empty.html"))
            assertEqual "status" 401 (httpStatusToInt (apiResponseStatus response))
      , testCase "should add default headers" $
          withHeaderCapture $ \prefix captured ctx page -> do
            request <- contextRequest ctx
            _ <- fetchGet request (urlFromText (prefix <> "/empty.html"))
            headers <- takeMVar captured
            userAgent <- evaluateText page "() => navigator.userAgent"
            assertEqual "accept" (Just "*/*") (lookupHeader "accept" headers)
            assertEqual "user-agent" (Just (encodeUtf8 userAgent)) (lookupHeader "user-agent" headers)
            assertEqual "accept-encoding" (Just "gzip,deflate,br") (lookupHeader "accept-encoding" headers)
      , testCase "should send content-length" $
          withHeaderCapture $ \prefix captured ctx _page -> do
            request <- contextRequest ctx
            _ <-
              fetchPostWith
                request
                (urlFromText (prefix <> "/empty.html"))
                defaultFetchOptions {fetchPostData = Just (FetchPostBytes (ByteString.pack [0 .. 255]))}
            headers <- takeMVar captured
            assertEqual "content-length" (Just "256") (lookupHeader "content-length" headers)
            assertEqual "content-type" (Just "application/octet-stream") (lookupHeader "content-type" headers)
      , testCase "should add default headers to redirects" $
          withRedirectHeaderCapture $ \prefix captured ctx page -> do
            request <- contextRequest ctx
            _ <- fetchGet request (urlFromText (prefix <> "/redirect"))
            headers <- takeMVar captured
            userAgent <- evaluateText page "() => navigator.userAgent"
            assertEqual "accept" (Just "*/*") (lookupHeader "accept" headers)
            assertEqual "user-agent" (Just (encodeUtf8 userAgent)) (lookupHeader "user-agent" headers)
            assertEqual "accept-encoding" (Just "gzip,deflate,br") (lookupHeader "accept-encoding" headers)
      , testCase "should allow to override default headers" $
          withHeaderCapture $ \prefix captured ctx _page -> do
            request <- contextRequest ctx
            _ <-
              fetchGetWith
                request
                (urlFromText (prefix <> "/empty.html"))
                defaultFetchOptions
                  { fetchHeaders =
                      Just
                        [ HttpHeader (HeaderName "User-Agent") (HeaderValue "Playwright")
                        , HttpHeader (HeaderName "Accept") (HeaderValue "text/html")
                        , HttpHeader (HeaderName "Accept-Encoding") (HeaderValue "br")
                        ]
                  }
            headers <- takeMVar captured
            assertEqual "accept" (Just "text/html") (lookupHeader "accept" headers)
            assertEqual "user-agent" (Just "Playwright") (lookupHeader "user-agent" headers)
            assertEqual "accept-encoding" (Just "br") (lookupHeader "accept-encoding" headers)
      , testCase "should propagate custom headers with redirects" $
          withPropagatedRedirects $ \prefix captured ctx -> do
            request <- contextRequest ctx
            _ <-
              fetchGetWith
                request
                (urlFromText (prefix <> "/a/redirect1"))
                defaultFetchOptions {fetchHeaders = Just [HttpHeader (HeaderName "foo") (HeaderValue "bar")]}
            got <- drainCaptured captured 3
            assertEqual "foo" ["bar", "bar", "bar"] (fmap (fromMaybe "" . lookupHeader "foo") got)
      , testCase "should propagate extra http headers with redirects" $
          withPropagatedRedirects $ \prefix captured ctx -> do
            setContextExtraHTTPHeaders ctx [HttpHeader (HeaderName "My-Secret") (HeaderValue "Value")]
            request <- contextRequest ctx
            _ <- fetchGet request (urlFromText (prefix <> "/a/redirect1"))
            got <- drainCaptured captured 3
            assertEqual "secret" ["Value", "Value", "Value"] (fmap (fromMaybe "" . lookupHeader "my-secret") got)
      , testCase "should throw on invalid header value" $
          withAssetsContext $ \prefix ctx -> do
            request <- contextRequest ctx
            assertProtocolContains
              "Invalid character in header content"
              ( fetchGetWith
                  request
                  (urlFromText (prefix <> "/empty.html"))
                  defaultFetchOptions
                    { fetchHeaders = Just [HttpHeader (HeaderName "foo") (HeaderValue "недопустимое значение")]
                    }
              )
      , testCase "should throw on non-http(s) protocol" $
          withChromiumContext $ \ctx _page -> do
            request <- contextRequest ctx
            assertProtocolContains "Protocol \"data:\" not supported" (fetchGet request (urlFromText "data:text/plain,test"))
            assertProtocolContains "Protocol \"file:\" not supported" (fetchGet request (urlFromText "file:///tmp/foo"))
      , testCase "should resolve url relative to baseURL" $
          withAssetsPrefix $ \prefix ->
            withChromiumContextWith defaultNewContextOptions {newContextBaseURL = Just (urlFromText prefix)} $ \ctx _page -> do
              request <- contextRequest ctx
              response <- fetchGet request (urlFromText "/empty.html")
              assertEqual "url" (prefix <> "/empty.html") (urlToText (apiResponseUrl response))
      , testCase "should support gzip compression" $
          compressionTest "gzip" (GZip.compress "Hello, world!")
      , testCase "should throw informative error on corrupted gzip body" $
          corruptedCompressionTest "gzip"
      , testCase "should support deflate compression" $
          compressionTest "deflate" (Zlib.compress "Hello, world!")
      , testCase "should throw informative error on corrupted deflate body" $
          corruptedCompressionTest "deflate"
      , testCase "should support a timeout of 0" $
          withSlowThenDoneApp $ \prefix ctx -> do
            request <- contextRequest ctx
            timeout <- requireTimeout 0
            response <-
              fetchGetWith
                request
                (urlFromText (prefix <> "/slow"))
                defaultFetchOptions {fetchTimeout = Just timeout}
            assertEqual "body" "done" =<< apiResponseText response
      , testCase "should respect timeout after redirects" $
          withSlowRedirectApp $ \prefix ctx -> do
            timeout <- requireTimeout 100
            setContextDefaultTimeout ctx timeout
            request <- contextRequest ctx
            assertProtocolContains
              "Timeout 100ms exceeded"
              (fetchGet request (urlFromText (prefix <> "/redirect")))
      , testCase "should dispose when context closes" $
          disposeAfterContextClose
      , testCase "should override request parameters" $
          overrideRequestParameters
      , testCase "should support application/x-www-form-urlencoded" $
          withBodyCapture $ \prefix captured ctx -> do
            request <- contextRequest ctx
            _ <-
              fetchPostWith
                request
                (urlFromText (prefix <> "/empty.html"))
                defaultFetchOptions
                  { fetchFormData =
                      Just
                        [ FetchFormField "firstName" "John"
                        , FetchFormField "lastName" "Doe"
                        , FetchFormField "file" "f.js"
                        ]
                  }
            (headers, body) <- takeMVar captured
            assertEqual "content-type" (Just "application/x-www-form-urlencoded") (lookupHeader "content-type" headers)
            assertEqual
              "body"
              [("firstName", "John"), ("lastName", "Doe"), ("file", "f.js")]
              (decodeQuery body)
      , testCase "should support application/x-www-form-urlencoded with param lists" $
          withBodyCapture $ \prefix captured ctx -> do
            request <- contextRequest ctx
            _ <-
              fetchPostWith
                request
                (urlFromText (prefix <> "/empty.html"))
                defaultFetchOptions
                  { fetchFormData =
                      Just
                        [ FetchFormField "foo" "1"
                        , FetchFormField "foo" "2"
                        ]
                  }
            (headers, body) <- takeMVar captured
            assertEqual "content-type" (Just "application/x-www-form-urlencoded") (lookupHeader "content-type" headers)
            assertEqual "body" [("foo", "1"), ("foo", "2")] (decodeQuery body)
      , testCase "should encode to application/json by default" $
          withBodyCapture $ \prefix captured ctx -> do
            request <- contextRequest ctx
            _ <-
              fetchPostWith
                request
                (urlFromText (prefix <> "/empty.html"))
                defaultFetchOptions {fetchPostData = Just (FetchPostJson sampleJson)}
            (headers, body) <- takeMVar captured
            assertEqual "content-type" (Just "application/json") (lookupHeader "content-type" headers)
            assertEqual "json" sampleJson =<< decodeBodyJson body
      , testCase "should serialize data to json regardless of content-type" $
          withBodyCapture $ \prefix captured ctx -> do
            request <- contextRequest ctx
            _ <-
              fetchPostWith
                request
                (urlFromText (prefix <> "/empty.html"))
                defaultFetchOptions
                  { fetchHeaders = Just [HttpHeader (HeaderName "content-type") (HeaderValue "unknown")]
                  , fetchPostData = Just (FetchPostJson (object ["firstName" .= ("John" :: Text), "lastName" .= ("Doe" :: Text)]))
                  }
            (headers, body) <- takeMVar captured
            assertEqual "content-type" (Just "unknown") (lookupHeader "content-type" headers)
            assertEqual
              "json"
              (object ["firstName" .= ("John" :: Text), "lastName" .= ("Doe" :: Text)])
              =<< decodeBodyJson body
      , testCase "context request should export same storage state as context" $
          withSetCookieApp $ \prefix ctx page -> do
            request <- contextRequest ctx
            pageReq <- pageRequest page
            _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
            contextState <- storageState ctx
            requestState <- apiRequestStorageState request
            pageState <- apiRequestStorageState pageReq
            assertEqual "count" 2 (length (storageStateCookies contextState))
            assertEqual "request" contextState requestState
            assertEqual "page" contextState pageState
      , testCase "should send secure cookie over http for localhost" $
          secureLocalhostCookieTest
      , testCase "should set domain=localhost cookie" $
          localhostDomainCookieTest
      , testCase "fetch should not throw on long set-cookie value" $
          longSetCookieTest
      , testCase "should support set-cookie with SameSite and without Secure attribute over HTTP" $
          sameSiteOverHttpTest
      , testCase "should abort requests when browser context closes" $
          abortOnContextClose
      , testCase "should not work after dispose" $
          withAssetsContext $ \prefix ctx -> do
            request <- contextRequest ctx
            disposeAPIRequestContext request
            assertProtocolContains
              "Target page, context or browser has been closed"
              (fetchGet request (urlFromText (prefix <> "/empty.html")))
      , testCase "should not work after context dispose" $
          disposeAfterContextReason
      , testCase "should retry on ECONNRESET" $
          retryHangupTest 3 4 "Hello!"
      , testCase "should retry ECONNRESET on compressed response" $
          retryGzipTest
      , testCase "should update host header on redirect" $
          hostHeaderRedirectTest
      ]

assertSimpleJson :: Text -> APIResponse -> IO ()
assertSimpleJson prefix response = do
  assertEqual "url" (urlToText (simpleJson prefix)) (urlToText (apiResponseUrl response))
  assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
  assertEqual "statusText" "OK" (apiResponseStatusText response)
  assertEqual "ok" True (apiResponseOk response)
  assertBool
    "content-type"
    (maybe False ("application/json" `isInfixOf`) (headerValue "content-type" (apiResponseHeaders response)))
  assertEqual "body" "{\"foo\": \"bar\"}\n" =<< apiResponseText response

methodPostDataTest ::
  Text ->
  (APIRequestContext -> Url -> FetchOptions -> IO APIResponse) ->
  IO ()
methodPostDataTest methodName send =
  withCapturedSimple $ \prefix captured ctx -> do
    request <- contextRequest ctx
    response <-
      send
        request
        (simpleJson prefix)
        defaultFetchOptions {fetchPostData = Just (FetchPostText "My request")}
    assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
    (gotMethod, body) <- takeMVar captured
    assertEqual "method" methodName (decodeUtf8 gotMethod)
    assertEqual "body" "My request" (decodeUtf8 body)
    assertEqual "path" "/simple.json" (dropOrigin (urlToText (apiResponseUrl response)))

assertNetworkError :: IO a -> IO ()
assertNetworkError action = do
  result <- try action
  case result of
    Left (ProtocolRpc failure) ->
      assertBool
        "network error"
        (protocolFailureMessage failure /= "")
    Left err ->
      assertFailure ("expected ProtocolRpc, got: " <> show err)
    Right _ ->
      assertFailure "expected network error"

withAssetsContext :: (Text -> BrowserContext -> IO a) -> IO a
withAssetsContext action =
  withAssetsContextWith defaultNewContextOptions $ \prefix ctx _page ->
    action prefix ctx

withHangupApp :: (Text -> BrowserContext -> IO a) -> IO a
withHangupApp action =
  withApplication (pure hangupApp) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) ctx

hangupApp :: Application
hangupApp request respond =
  case pathInfo request of
    ["redirect"] ->
      respond (responseLBS status302 [(hLocation, "/test")] "")
    ["test"] ->
      respond (responseRaw (\_ _ -> pure ()) (responseLBS status200 [] ""))
    _ ->
      respond (responseLBS status404 [] "")

withRedirectJsonApp :: (Text -> BrowserContext -> IO a) -> IO a
withRedirectJsonApp action = do
  assets <- testAssetsDir
  withApplication (pure (redirectJsonApp assets)) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) ctx

redirectJsonApp :: FilePath -> Application
redirectJsonApp assets request respond =
  case pathInfo request of
    ["redirect1"] ->
      respond (responseLBS status302 [(hLocation, "/redirect2")] "")
    ["redirect2"] ->
      respond (responseLBS status302 [(hLocation, "/simple.json")] "")
    _ ->
      staticApp (defaultFileServerSettings assets) request respond

withSetCookieApp :: (Text -> BrowserContext -> Page -> IO a) -> IO a
withSetCookieApp action =
  withApplication (pure setCookieApp) $ \port ->
    withChromiumContext $ \ctx page ->
      action ("http://127.0.0.1:" <> pack (show port)) ctx page

setCookieApp :: Application
setCookieApp request respond =
  case pathInfo request of
    ["setcookie.html"] ->
      respond
        ( responseLBS
            status200
            [("Set-Cookie", "session=value"), ("Set-Cookie", "foo=bar; max-age=3600")]
            ""
        )
    ["empty.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] "<html><body></body></html>")
    _ ->
      respond (responseLBS status404 [] "")

withSlowApp :: (Text -> BrowserContext -> IO a) -> IO a
withSlowApp action =
  withApplication (pure slowApp) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) ctx

slowApp :: Application
slowApp request respond =
  case pathInfo request of
    ["slow"] ->
      forever (threadDelay 1000000)
    _ ->
      respond (responseLBS status404 [] "")

withMissingApp :: (Text -> BrowserContext -> IO a) -> IO a
withMissingApp action =
  withApplication (pure missingApp) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) ctx

missingApp :: Application
missingApp _request respond =
  respond (responseLBS status404 [(hContentType, "text/plain")] "File not found: nope")

withCapturedSimple :: (Text -> MVar (ByteString, ByteString) -> BrowserContext -> IO a) -> IO a
withCapturedSimple action = do
  captured <- newEmptyMVar
  withApplication (pure (captureSimple captured)) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) captured ctx

captureSimple :: MVar (ByteString, ByteString) -> Application
captureSimple captured request respond = do
  body <- fmap LBS.toStrict (strictRequestBody request)
  _ <- tryPutMVar captured (requestMethod request, body)
  respond
    ( responseLBS
        status200
        [(hContentType, "application/json")]
        "{\"foo\": \"bar\"}\n"
    )

objectParamsTest ::
  (APIRequestContext -> Url -> FetchOptions -> IO APIResponse) ->
  IO ()
objectParamsTest send =
  queryParamsTest
    send
    defaultFetchOptions
      { fetchQueryParams =
          Just
            [ FetchQueryParam "param1" "value1"
            , FetchQueryParam "парам2" "знач2"
            ]
      }
    [("param1", "value1"), ("парам2", "знач2")]

duplicateParamsTest ::
  (APIRequestContext -> Url -> FetchOptions -> IO APIResponse) ->
  IO ()
duplicateParamsTest send =
  queryParamsTest
    send
    defaultFetchOptions
      { fetchQueryParams =
          Just
            [ FetchQueryParam "param1" "value1"
            , FetchQueryParam "param1" "value2"
            , FetchQueryParam "парам2" "знач2"
            ]
      }
    [("param1", "value1"), ("param1", "value2"), ("парам2", "знач2")]

encodedParamsTest ::
  (APIRequestContext -> Url -> FetchOptions -> IO APIResponse) ->
  IO ()
encodedParamsTest send =
  queryParamsTest
    send
    defaultFetchOptions {fetchEncodedParams = Just "?param1=value1&param1=value2&парам2=знач2"}
    [("param1", "value1"), ("param1", "value2"), ("парам2", "знач2")]

queryParamsTest ::
  (APIRequestContext -> Url -> FetchOptions -> IO APIResponse) ->
  FetchOptions ->
  [(Text, Text)] ->
  IO ()
queryParamsTest send opts expected =
  withQueryCapture $ \prefix captured ctx -> do
    request <- contextRequest ctx
    response <- send request (urlFromText (prefix <> "/empty.html")) opts
    raw <- takeMVar captured
    assertEqual "request query" expected (decodeQuery raw)
    assertEqual "response query" expected (decodeQueryText (urlToText (apiResponseUrl response)))

withQueryCapture :: (Text -> MVar ByteString -> BrowserContext -> IO a) -> IO a
withQueryCapture action = do
  captured <- newEmptyMVar
  withApplication (pure (queryApp captured)) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) captured ctx

queryApp :: MVar ByteString -> Application
queryApp captured request respond = do
  _ <- tryPutMVar captured (rawQueryString request)
  respond (responseLBS status200 [(hContentType, "text/html")] "<html><body></body></html>")

decodeQueryText :: Text -> [(Text, Text)]
decodeQueryText url =
  case breakOnEnd "?" url of
    ("", _) ->
      []
    (_, query) ->
      decodeQuery (encodeUtf8 ("?" <> query))

decodeQuery :: ByteString -> [(Text, Text)]
decodeQuery raw =
  fmap decodePair (parseQuery (stripQuestion raw))

stripQuestion :: ByteString -> ByteString
stripQuestion raw =
  case ByteString.uncons raw of
    Just (63, rest) ->
      rest
    _ ->
      raw

decodePair :: (ByteString, Maybe ByteString) -> (Text, Text)
decodePair (name, value) =
  ( decodeUtf8 (urlDecode True name)
  , decodeUtf8 (urlDecode True (fromMaybe ByteString.empty value))
  )

failOnStatusTest ::
  (APIRequestContext -> Url -> FetchOptions -> IO APIResponse) ->
  Bool ->
  IO ()
failOnStatusTest send expectBody =
  withMissingApp $ \prefix ctx -> do
    request <- contextRequest ctx
    result <-
      try
        ( send
            request
            (urlFromText (prefix <> "/does-not-exist.html"))
            defaultFetchOptions {fetchFailOnStatusCode = Just True}
        )
    case result of
      Left (ProtocolRpc failure) -> do
        let message = protocolFailureMessage failure
        assertBool "404" ("404" `isInfixOf` message)
        if expectBody
          then assertBool "body" ("File not found" `isInfixOf` message)
          else pure ()
      Left err ->
        assertFailure ("expected ProtocolRpc, got: " <> show err)
      Right _ ->
        assertFailure "expected failOnStatusCode error"

withPartialBodyApp :: (Text -> BrowserContext -> IO a) -> IO a
withPartialBodyApp action =
  withApplication (pure partialBodyApp) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) ctx

partialBodyApp :: Application
partialBodyApp request respond =
  case pathInfo request of
    ["redirect"] ->
      respond (responseLBS status302 [(hLocation, "/test")] "")
    ["test"] ->
      respond
        ( responseRaw
            ( \_recv send ->
                send "HTTP/1.1 200 OK\r\nContent-Length: 4096\r\nContent-Type: text/html\r\n\r\n<title>A"
            )
            (responseLBS status200 [] "")
        )
    _ ->
      respond (responseLBS status404 [] "")

withCookieCapture :: (Text -> MVar (Maybe ByteString) -> BrowserContext -> IO a) -> IO a
withCookieCapture action = do
  captured <- newEmptyMVar
  withApplication (pure (cookieCaptureApp captured)) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) captured ctx

cookieCaptureApp :: MVar (Maybe ByteString) -> Application
cookieCaptureApp captured request respond = do
  _ <- tryPutMVar captured (lookupHeader "cookie" (requestHeaders request))
  respond (responseLBS status200 [(hContentType, "text/html")] "<html><body></body></html>")

withUtf8RedirectApp :: (Text -> BrowserContext -> IO a) -> IO a
withUtf8RedirectApp action =
  withApplication (pure utf8RedirectApp) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) ctx

utf8RedirectApp :: Application
utf8RedirectApp request respond =
  case pathInfo request of
    ["redirect"] ->
      respond
        ( responseRaw
            ( \_recv send ->
                send (encodeUtf8 "HTTP/1.1 301 Moved Permanently\r\nLocation: /empty.html?message=マスクПривет\r\n\r\n")
            )
            (responseLBS status200 [] "")
        )
    ["empty.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] "<html><body></body></html>")
    _ ->
      respond (responseLBS status404 [] "")

withOrderedCookieApp :: (Text -> MVar (Maybe ByteString) -> BrowserContext -> Page -> IO a) -> IO a
withOrderedCookieApp action = do
  captured <- newEmptyMVar
  withApplication (pure (orderedCookieApp captured)) $ \port ->
    withChromiumContext $ \ctx page ->
      action ("http://127.0.0.1:" <> pack (show port)) captured ctx page

orderedCookieApp :: MVar (Maybe ByteString) -> Application
orderedCookieApp captured request respond =
  case pathInfo request of
    ["setcookie.html"] ->
      respond
        ( responseLBS
            status200
            [("Set-Cookie", "cookie.0=foo"), ("Set-Cookie", "cookie.1=bar")]
            ""
        )
    ["empty.html"] -> do
      _ <- tryPutMVar captured (lookupHeader "cookie" (requestHeaders request))
      respond (responseLBS status200 [(hContentType, "text/html")] "<html><body></body></html>")
    _ ->
      respond (responseLBS status404 [] "")

withEmptyCookieApp :: (Text -> BrowserContext -> Page -> IO a) -> IO a
withEmptyCookieApp action =
  withApplication (pure emptyCookieApp) $ \port ->
    withChromiumContext $ \ctx page ->
      action ("http://127.0.0.1:" <> pack (show port)) ctx page

emptyCookieApp :: Application
emptyCookieApp request respond =
  case pathInfo request of
    ["setcookie.html"] ->
      respond (responseLBS status200 [("Set-Cookie", "first=")] "")
    ["empty.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] "<html><body></body></html>")
    _ ->
      respond (responseLBS status404 [] "")

withSetCookieBodyApp :: (Text -> BrowserContext -> IO a) -> IO a
withSetCookieBodyApp action =
  withApplication (pure setCookieBodyApp) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) ctx

setCookieBodyApp :: Application
setCookieBodyApp _request respond =
  respond
    ( responseLBS
        status200
        [("Set-Cookie", "session=value"), ("Set-Cookie", "foo=bar; max-age=3600")]
        "text content"
    )

withNegativeMaxAgeApp :: (Text -> MVar (Maybe ByteString) -> BrowserContext -> IO a) -> IO a
withNegativeMaxAgeApp action = do
  captured <- newEmptyMVar
  withApplication (pure (negativeMaxAgeApp captured)) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) captured ctx

negativeMaxAgeApp :: MVar (Maybe ByteString) -> Application
negativeMaxAgeApp captured request respond =
  case pathInfo request of
    ["setcookie.html"] ->
      respond
        ( responseLBS
            status200
            [("Set-Cookie", "a=v; max-age=100000"), ("Set-Cookie", "b=v; max-age=100000"), ("Set-Cookie", "c=v")]
            ""
        )
    ["removecookie.html"] ->
      respond
        ( responseLBS
            status200
            [("Set-Cookie", "a=v; max-age=-2000000000"), ("Set-Cookie", "b=v; max-age=-1")]
            ""
        )
    ["empty.html"] -> do
      _ <- tryPutMVar captured (lookupHeader "cookie" (requestHeaders request))
      respond (responseLBS status200 [(hContentType, "text/html")] "<html><body></body></html>")
    _ ->
      respond (responseLBS status404 [] "")

withPastExpiresApp :: (Text -> MVar (Maybe ByteString) -> BrowserContext -> IO a) -> IO a
withPastExpiresApp action = do
  captured <- newEmptyMVar
  withApplication (pure (pastExpiresApp captured)) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) captured ctx

pastExpiresApp :: MVar (Maybe ByteString) -> Application
pastExpiresApp captured request respond =
  case pathInfo request of
    ["setcookie.html"] ->
      respond (responseLBS status200 [("Set-Cookie", "a=v; max-age=1000000")] "")
    ["removecookie.html"] ->
      respond
        ( responseLBS
            status200
            [("Set-Cookie", "a=v; expires=Wed, 01 Jan 1000 00:00:00 GMT")]
            ""
        )
    ["empty.html"] -> do
      _ <- tryPutMVar captured (lookupHeader "cookie" (requestHeaders request))
      respond (responseLBS status200 [(hContentType, "text/html")] "<html><body></body></html>")
    _ ->
      respond (responseLBS status404 [] "")

cookieRedirectsTest :: IO ()
cookieRedirectsTest = do
  assets <- testAssetsDir
  captured <- newChan
  withApplication (pure (cookieRedirectApp captured assets)) $ \port ->
    withChromiumContext $ \ctx _page -> do
      let prefix = "http://127.0.0.1:" <> pack (show port)
      request <- contextRequest ctx
      _ <- fetchGet request (urlFromText (prefix <> "/redirect1"))
      first <- drainCookieChan captured 3
      assertEqual "first cookies" [Nothing, Just "r1=v1", Just "r1=v1"] (fmap snd first)
      _ <- fetchGet request (urlFromText (prefix <> "/redirect1"))
      second <- drainCookieChan captured 3
      case fmap snd second of
        [secondFirst, secondSecond, secondThird] -> do
          assertEqual "second first hop" (Just "r1=v1") secondFirst
          assertEqual
            "second second hop"
            ["r1=v1", "r2=v2"]
            (sort (splitSemi (decodeUtf8 (fromMaybe "" secondSecond))))
          assertEqual "second third hop" (Just "r1=v1") secondThird
        _ ->
          assertFailure ("expected 3 hops, got " <> show (length second))
      jar <- cookies ctx
      assertEqual
        "cookie fields"
        [ ("r1", "v1", "/", CookieSameSiteLax)
        , ("r2", "v2", "/a/b", CookieSameSiteLax)
        ]
        (sort (fmap cookieRedirectPair jar))
      forM_ jar $ \cookie -> do
        assertEqual "domain" "127.0.0.1" (cookieDomainToText (cookieDomain cookie))
        assertEqual "expires" (cookieExpiresToScientific sessionExpires) (cookieExpiresToScientific (cookieExpires cookie))
        assertEqual "httpOnly" False (cookieHttpOnlyToBool (cookieHttpOnly cookie))
        assertEqual "secure" False (cookieSecureToBool (cookieSecure cookie))

cookieRedirectApp :: Chan ([Text], Maybe ByteString) -> FilePath -> Application
cookieRedirectApp captured assets request respond = do
  writeChan captured (pathInfo request, lookupHeader "cookie" (requestHeaders request))
  case pathInfo request of
    ["redirect1"] ->
      respond
        ( responseLBS
            status301
            [("Set-Cookie", "r1=v1;SameSite=Lax"), (hLocation, "/a/b/redirect2")]
            ""
        )
    ["a", "b", "redirect2"] ->
      respond
        ( responseLBS
            status302
            [("Set-Cookie", "r2=v2;SameSite=Lax"), (hLocation, "/title.html")]
            ""
        )
    _ ->
      staticApp (defaultFileServerSettings assets) request respond

drainCookieChan :: Chan ([Text], Maybe ByteString) -> Int -> IO [([Text], Maybe ByteString)]
drainCookieChan captured n =
  replicateM n (readChan captured)

cookieRedirectPair :: Cookie -> (Text, Text, Text, CookieSameSite)
cookieRedirectPair cookie =
  ( cookieNameToText (cookieName cookie)
  , cookieValueToText (cookieValue cookie)
  , cookiePathToText (cookiePath cookie)
  , cookieSameSite cookie
  )

withRawHeadersApp :: (Text -> BrowserContext -> IO a) -> IO a
withRawHeadersApp action =
  withApplication (pure rawHeadersApp) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) ctx

rawHeadersApp :: Application
rawHeadersApp _request respond =
  respond
    ( responseRaw
        ( \_recv send ->
            send "HTTP/1.1 200 OK\r\nName-A: v1\r\nname-b: v4\r\nName-a: v2\r\nname-A: v3\r\n\r\n"
        )
        (responseLBS status200 [] "")
    )

headerNameContains :: Text -> HttpHeader -> Bool
headerNameContains needle header =
  needle `isInfixOf` toLower (headerNameToText (httpHeaderName header))

withAuthApp :: (Text -> MVar (Maybe ByteString) -> BrowserContext -> IO a) -> IO a
withAuthApp action = do
  captured <- newEmptyMVar
  withApplication (pure (authApp captured)) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) captured ctx

authApp :: MVar (Maybe ByteString) -> Application
authApp captured request respond = do
  let authorization = lookupHeader "authorization" (requestHeaders request)
  _ <- tryPutMVar captured authorization
  if authorization == Just "Basic dXNlcjpwYXNz"
    then respond (responseLBS status200 [(hContentType, "text/html")] "<html><title>ok</title></html>")
    else respond (responseLBS status401 [("WWW-Authenticate", "Basic realm=\"test\"")] "")

withHeaderCapture :: (Text -> MVar RequestHeaders -> BrowserContext -> Page -> IO a) -> IO a
withHeaderCapture action = do
  captured <- newEmptyMVar
  withApplication (pure (headerCaptureApp captured)) $ \port ->
    withChromiumContext $ \ctx page ->
      action ("http://127.0.0.1:" <> pack (show port)) captured ctx page

headerCaptureApp :: MVar RequestHeaders -> Application
headerCaptureApp captured request respond = do
  _ <- tryPutMVar captured (requestHeaders request)
  respond (responseLBS status200 [(hContentType, "text/html")] "<html><body></body></html>")

withRedirectHeaderCapture :: (Text -> MVar RequestHeaders -> BrowserContext -> Page -> IO a) -> IO a
withRedirectHeaderCapture action = do
  captured <- newEmptyMVar
  withApplication (pure (redirectHeaderCaptureApp captured)) $ \port ->
    withChromiumContext $ \ctx page ->
      action ("http://127.0.0.1:" <> pack (show port)) captured ctx page

redirectHeaderCaptureApp :: MVar RequestHeaders -> Application
redirectHeaderCaptureApp captured request respond =
  case pathInfo request of
    ["redirect"] ->
      respond (responseLBS status302 [(hLocation, "/empty.html")] "")
    _ -> do
      _ <- tryPutMVar captured (requestHeaders request)
      respond (responseLBS status200 [(hContentType, "text/html")] "<html><body></body></html>")

withPropagatedRedirects :: (Text -> Chan RequestHeaders -> BrowserContext -> IO a) -> IO a
withPropagatedRedirects action = do
  captured <- newChan
  assets <- testAssetsDir
  withApplication (pure (propagatedRedirectApp captured assets)) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) captured ctx

propagatedRedirectApp :: Chan RequestHeaders -> FilePath -> Application
propagatedRedirectApp captured assets request respond = do
  writeChan captured (requestHeaders request)
  case pathInfo request of
    ["a", "redirect1"] ->
      respond (responseLBS status302 [(hLocation, "/b/c/redirect2")] "")
    ["b", "c", "redirect2"] ->
      respond (responseLBS status302 [(hLocation, "/simple.json")] "")
    _ ->
      staticApp (defaultFileServerSettings assets) request respond

drainCaptured :: Chan RequestHeaders -> Int -> IO [RequestHeaders]
drainCaptured captured n =
  replicateM n (readChan captured)

lookupHeader :: HTTP.HeaderName -> RequestHeaders -> Maybe ByteString
lookupHeader =
  lookup

compressionTest :: ByteString -> LBS.ByteString -> IO ()
compressionTest encoding body =
  withApplication (pure (compressedApp encoding body)) $ \port ->
    withChromiumContext $ \ctx _page -> do
      request <- contextRequest ctx
      response <- fetchGet request (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/compressed"))
      assertEqual "body" "Hello, world!" =<< apiResponseText response

corruptedCompressionTest :: ByteString -> IO ()
corruptedCompressionTest encoding =
  withApplication (pure (compressedApp encoding "Hello, world!")) $ \port ->
    withChromiumContext $ \ctx _page -> do
      request <- contextRequest ctx
      assertProtocolContains
        ("failed to decompress '" <> decodeUtf8 encoding <> "' encoding")
        (fetchGet request (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/compressed")))

compressedApp :: ByteString -> LBS.ByteString -> Application
compressedApp encoding body _request respond =
  respond
    ( responseLBS
        status200
        [("Content-Encoding", encoding), (hContentType, "text/plain")]
        body
    )

withSlowThenDoneApp :: (Text -> BrowserContext -> IO a) -> IO a
withSlowThenDoneApp action =
  withApplication (pure slowThenDoneApp) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) ctx

slowThenDoneApp :: Application
slowThenDoneApp request respond =
  case pathInfo request of
    ["slow"] -> do
      threadDelay 50000
      respond (responseLBS status200 [(hContentType, "text/html")] "done")
    _ ->
      respond (responseLBS status404 [] "")

withSlowRedirectApp :: (Text -> BrowserContext -> IO a) -> IO a
withSlowRedirectApp action =
  withApplication (pure slowRedirectApp) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) ctx

slowRedirectApp :: Application
slowRedirectApp request respond =
  case pathInfo request of
    ["redirect"] ->
      respond (responseLBS status302 [(hLocation, "/slow")] "")
    ["slow"] ->
      forever (threadDelay 1000000)
    _ ->
      respond (responseLBS status404 [] "")

disposeAfterContextClose :: IO ()
disposeAfterContextClose =
  withAssetsPrefix $ \prefix ->
    withChromiumBrowser $ \browser -> do
      ctx <- newContextWith browser defaultNewContextOptions
      request <- contextRequest ctx
      response <- fetchGet request (simpleJson prefix)
      _ <- apiResponseJson response
      closeContext ctx
      result <- try (apiResponseBody response)
      case result of
        Left (DecodeFailure message) ->
          assertBool "disposed" ("disposed" `isInfixOf` message)
        Left (ProtocolRpc failure) ->
          assertBool
            "disposed"
            ( "disposed" `isInfixOf` protocolFailureMessage failure
                || "closed" `isInfixOf` protocolFailureMessage failure
            )
        Left err ->
          assertFailure ("expected disposed, got: " <> show err)
        Right _ ->
          assertFailure "expected disposed response body"

overrideRequestParameters :: IO ()
overrideRequestParameters = do
  captured <- newEmptyMVar
  withApplication (pure (postCaptureApp captured)) $ \port ->
    withChromiumContext $ \ctx page -> do
      let prefix = "http://127.0.0.1:" <> pack (show port)
      withAsync (waitForRequest page (UrlGlob (prefix <> "/empty.html"))) $ \handle -> do
        _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
        pageReq <- wait handle
        request <- contextRequest ctx
        _ <-
          fetchRequestWith
            request
            pageReq
            defaultFetchOptions
              { fetchMethod = Just (HttpMethod "POST")
              , fetchHeaders = Just [HttpHeader (HeaderName "foo") (HeaderValue "bar")]
              , fetchPostData = Just (FetchPostText "data")
              }
        (headers, body) <- takeMVar captured
        assertEqual "foo" (Just "bar") (lookupHeader "foo" headers)
        assertEqual "body" "data" (decodeUtf8 body)

postCaptureApp :: MVar (RequestHeaders, ByteString) -> Application
postCaptureApp captured request respond = do
  body <- fmap LBS.toStrict (strictRequestBody request)
  _ <-
    if requestMethod request == "POST"
      then tryPutMVar captured (requestHeaders request, body)
      else pure False
  respond (responseLBS status200 [(hContentType, "text/html")] "<html><body></body></html>")

withBodyCapture :: (Text -> MVar (RequestHeaders, ByteString) -> BrowserContext -> IO a) -> IO a
withBodyCapture action = do
  captured <- newEmptyMVar
  withApplication (pure (bodyCaptureApp captured)) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) captured ctx

bodyCaptureApp :: MVar (RequestHeaders, ByteString) -> Application
bodyCaptureApp captured request respond = do
  body <- fmap LBS.toStrict (strictRequestBody request)
  _ <- tryPutMVar captured (requestHeaders request, body)
  respond (responseLBS status200 [(hContentType, "text/html")] "<html><body></body></html>")

sampleJson :: Value
sampleJson =
  object
    [ "firstName" .= ("John" :: Text)
    , "lastName" .= ("Doe" :: Text)
    , "file" .= object ["name" .= ("f.js" :: Text)]
    ]

decodeBodyJson :: ByteString -> IO Value
decodeBodyJson body =
  case eitherDecodeStrict body of
    Left err ->
      assertFailure ("json: " <> err)
    Right value ->
      pure value

secureLocalhostCookieTest :: IO ()
secureLocalhostCookieTest = do
  captured <- newEmptyMVar
  withApplication (pure (secureCookieApp captured)) $ \port ->
    withChromiumContext $ \ctx _page -> do
      let prefix = "http://localhost:" <> pack (show port)
      request <- contextRequest ctx
      _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
      _ <- fetchGet request (urlFromText (prefix <> "/empty.html"))
      cookie <- takeMVar captured
      assertEqual "cookie" (Just "a=v") cookie

secureCookieApp :: MVar (Maybe ByteString) -> Application
secureCookieApp captured request respond =
  case pathInfo request of
    ["setcookie.html"] ->
      respond (responseLBS status200 [("Set-Cookie", "a=v; secure")] "")
    ["empty.html"] -> do
      _ <- tryPutMVar captured (lookupHeader "cookie" (requestHeaders request))
      respond (responseLBS status200 [(hContentType, "text/html")] "<html><body></body></html>")
    _ ->
      respond (responseLBS status404 [] "")

localhostDomainCookieTest :: IO ()
localhostDomainCookieTest =
  withApplication (pure localhostDomainApp) $ \port ->
    withChromiumContext $ \ctx _page -> do
      let prefix = "http://localhost:" <> pack (show port)
      request <- contextRequest ctx
      _ <- fetchGet request (urlFromText (prefix <> "/empty.html"))
      jar <- cookies ctx
      case jar of
        [cookie] -> do
          assertEqual "name" "name" (cookieNameToText (cookieName cookie))
          assertEqual "value" "val" (cookieValueToText (cookieValue cookie))
        _ ->
          assertFailure ("expected one cookie, got " <> show (length jar))

localhostDomainApp :: Application
localhostDomainApp _request respond =
  respond
    ( responseLBS
        status200
        [("Set-Cookie", "name=val; Domain=localhost; Path=/;"), (hContentType, "text/html")]
        "<html></html>"
    )

longSetCookieTest :: IO ()
longSetCookieTest =
  withApplication (pure longSetCookieApp) $ \port ->
    withChromiumContext $ \ctx _page -> do
      request <- contextRequest ctx
      timeout <- requireTimeout 5000
      _ <-
        fetchGetWith
          request
          (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
          defaultFetchOptions {fetchTimeout = Just timeout}
      jar <- cookies ctx
      assertBool "bar" ("bar" `elem` fmap (cookieNameToText . cookieName) jar)

longSetCookieApp :: Application
longSetCookieApp _request respond =
  respond
    ( responseLBS
        status200
        [ ("Set-Cookie", "foo=" <> ByteString.replicate 4100 97 <> "; path=/;")
        , ("Set-Cookie", "bar=val")
        , (hContentType, "text/html")
        ]
        ""
    )

sameSiteOverHttpTest :: IO ()
sameSiteOverHttpTest =
  forM_ [("None", Nothing), ("Lax", Just CookieSameSiteLax), ("Strict", Just CookieSameSiteStrict)] $ \(value, expected) ->
    withApplication (pure (sameSiteApp value)) $ \port ->
      withChromiumContext $ \ctx _page -> do
        request <- contextRequest ctx
        _ <- fetchGet request (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
        jar <- cookies ctx
        case (expected, jar) of
          (Nothing, []) ->
            pure ()
          (Just sameSite, [cookie]) ->
            assertEqual ("sameSite " <> show value) sameSite (cookieSameSite cookie)
          _ ->
            assertFailure ("SameSite=" <> show value <> " cookies=" <> show (length jar))

sameSiteApp :: ByteString -> Application
sameSiteApp value _request respond =
  respond
    ( responseLBS
        status200
        [("Set-Cookie", "SID=2022; Path=/; SameSite=" <> value), (hContentType, "text/html")]
        ""
    )

abortOnContextClose :: IO ()
abortOnContextClose = do
  arrived <- newEmptyMVar
  withApplication (pure (hangForever arrived)) $ \port ->
    withChromiumBrowser $ \browser -> do
      ctx <- newContextWith browser defaultNewContextOptions
      request <- contextRequest ctx
      withAsync
        ( try (fetchGet request (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html")))
        )
        $ \pending -> do
          _ <- takeMVar arrived
          closeContext ctx
          result <- wait pending
          case result of
            Left (ProtocolRpc failure) ->
              assertBool
                "closed"
                ( "disposed" `isInfixOf` protocolFailureMessage failure
                    || "closed" `isInfixOf` protocolFailureMessage failure
                )
            Left err ->
              assertFailure ("expected ProtocolRpc, got: " <> show err)
            Right _ ->
              assertFailure "expected abort on context close"

hangForever :: MVar () -> Application
hangForever arrived _request _respond = do
  _ <- tryPutMVar arrived ()
  forever (threadDelay 1000000)

disposeAfterContextReason :: IO ()
disposeAfterContextReason =
  withAssetsPrefix $ \prefix ->
    withChromiumBrowser $ \browser -> do
      ctx <- newContextWith browser defaultNewContextOptions
      request <- contextRequest ctx
      closeContextWith ctx (Just "Test ended.")
      assertProtocolContains
        "Target page, context or browser has been closed"
        (fetchGet request (urlFromText (prefix <> "/empty.html")))

retryHangupTest :: Int -> Int -> Text -> IO ()
retryHangupTest retries expectedHits body = do
  count <- newIORef (0 :: Int)
  withApplication (pure (retryHangupApp count body)) $ \port ->
    withChromiumContext $ \ctx _page -> do
      request <- contextRequest ctx
      response <-
        fetchGetWith
          request
          (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/test"))
          defaultFetchOptions {fetchMaxRetries = Just retries}
      hits <- atomicModifyIORef' count (\n -> (n, n))
      assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
      assertEqual "body" body =<< apiResponseText response
      assertEqual "hits" expectedHits hits

retryHangupApp :: IORef Int -> Text -> Application
retryHangupApp count body _request respond = do
  n <- atomicModifyIORef' count (\x -> (x + 1, x + 1))
  if n <= 3
    then respond (responseRaw (\_ _ -> pure ()) (responseLBS status200 [] ""))
    else respond (responseLBS status200 [(hContentType, "text/plain")] (LBS.fromStrict (encodeUtf8 body)))

retryGzipTest :: IO ()
retryGzipTest = do
  count <- newIORef (0 :: Int)
  withApplication (pure (retryGzipApp count)) $ \port ->
    withChromiumContext $ \ctx _page -> do
      request <- contextRequest ctx
      response <-
        fetchGetWith
          request
          (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/test-gzip"))
          defaultFetchOptions {fetchMaxRetries = Just 3}
      hits <- atomicModifyIORef' count (\n -> (n, n))
      assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
      assertEqual "body" "compressed-retry-ok" =<< apiResponseText response
      assertEqual "hits" 3 hits

retryGzipApp :: IORef Int -> Application
retryGzipApp count _request respond = do
  n <- atomicModifyIORef' count (\x -> (x + 1, x + 1))
  if n <= 2
    then respond (responseRaw (\_ _ -> pure ()) (responseLBS status200 [] ""))
    else
      respond
        ( responseLBS
            status200
            [("Content-Encoding", "gzip"), (hContentType, "text/plain")]
            (GZip.compress "compressed-retry-ok")
        )

hostHeaderRedirectTest :: IO ()
hostHeaderRedirectTest = do
  count <- newIORef (0 :: Int)
  capturedHost <- newEmptyMVar
  withApplication (pure (hostRedirectApp count capturedHost)) $ \port ->
    withChromiumContext $ \ctx _page -> do
      let localPrefix = "http://localhost:" <> pack (show port)
          hostLocal = "localhost:" <> pack (show port)
          hostCross = "127.0.0.1:" <> pack (show port)
      request <- contextRequest ctx
      response <-
        fetchGetWith
          request
          (urlFromText (localPrefix <> "/redirect"))
          defaultFetchOptions {fetchHeaders = Just [HttpHeader (HeaderName "host") (HeaderValue hostLocal)]}
      redirects <- atomicModifyIORef' count (\n -> (n, n))
      host <- takeMVar capturedHost
      assertEqual "redirects" 2 redirects
      assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
      assertEqual "body" "Hello!" =<< apiResponseText response
      assertEqual "host" (Just (encodeUtf8 hostCross)) host

hostRedirectApp :: IORef Int -> MVar (Maybe ByteString) -> Application
hostRedirectApp count capturedHost request respond =
  case pathInfo request of
    ["redirect"] -> do
      _ <- atomicModifyIORef' count (\n -> (n + 1, ()))
      let
        host :: ByteString
        host = fromMaybe ByteString.empty (lookupHeader "host" (requestHeaders request))
        locationPath :: ByteString
        locationPath =
          if "localhost" `ByteString.isPrefixOf` host
            then "/redirect"
            else "/test"
        portSuffix :: ByteString
        portSuffix = ByteString.dropWhile (/= 58) host
      respond
        ( responseLBS
            status302
            [(hLocation, "http://127.0.0.1" <> portSuffix <> locationPath)]
            ""
        )
    ["test"] -> do
      _ <- tryPutMVar capturedHost (lookupHeader "host" (requestHeaders request))
      respond (responseLBS status200 [(hContentType, "text/plain")] "Hello!")
    _ ->
      respond (responseLBS status404 [] "")

simpleJson :: Text -> Url
simpleJson prefix =
  urlFromText (prefix <> "/simple.json")

cookiePair :: Cookie -> (Text, Text)
cookiePair cookie =
  (cookieNameToText (cookieName cookie), cookieValueToText (cookieValue cookie))

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

splitSemi :: Text -> [Text]
splitSemi text =
  filter (/= "") (fmap trim (splitOn ";" text))

trim :: Text -> Text
trim =
  strip

dropOrigin :: Text -> Text
dropOrigin text =
  case breakOnEnd "/" text of
    (_, name) ->
      "/" <> name

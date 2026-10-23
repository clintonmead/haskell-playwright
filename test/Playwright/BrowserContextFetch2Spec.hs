module Playwright.BrowserContextFetch2Spec
  ( tests
  ) where

import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Zlib qualified as Zlib
import Data.Aeson
  ( object
  , (.=)
  )
import Data.ByteString qualified as ByteString
import Data.Maybe
  ( fromMaybe
  )
import Data.Text
  ( Text
  )
import Data.Text.Encoding
  ( encodeUtf8
  )
import Playwright
  ( FetchFormField (FetchFormField)
  , FetchOptions (fetchFormData, fetchHeaders, fetchPostData, fetchTimeout)
  , FetchPostData (FetchPostBytes, FetchPostJson)
  , HeaderName (HeaderName)
  , HeaderValue (HeaderValue)
  , HttpCredentials (HttpCredentials, httpCredentialsOrigin, httpCredentialsPassword, httpCredentialsSend, httpCredentialsUsername)
  , HttpHeader (HttpHeader)
  , HttpPassword (HttpPassword)
  , HttpStatus (httpStatusToInt)
  , HttpUsername (HttpUsername)
  , NewContextOptions (newContextBaseURL)
  , StorageState (storageStateCookies)
  , apiRequestStorageState
  , apiResponseStatus
  , apiResponseText
  , apiResponseUrl
  , contextRequest
  , defaultFetchOptions
  , defaultNewContextOptions
  , disposeAPIRequestContext
  , evaluateText
  , fetchGet
  , fetchGetWith
  , fetchPostWith
  , pageRequest
  , setContextDefaultTimeout
  , setContextExtraHTTPHeaders
  , setHTTPCredentials
  , storageState
  , urlFromText
  , urlToText
  )
import Playwright.BrowserContextFetchSupport
  ( withAssetsContext
  , withSetCookieApp
  , decodeQuery
  , withAuthApp
  , withHeaderCapture
  , withRedirectHeaderCapture
  , withPropagatedRedirects
  , drainCaptured
  , lookupHeader
  , compressionTest
  , corruptedCompressionTest
  , withSlowThenDoneApp
  , withSlowRedirectApp
  , disposeAfterContextClose
  , overrideRequestParameters
  , withBodyCapture
  , sampleJson
  , decodeBodyJson
  , secureLocalhostCookieTest
  , localhostDomainCookieTest
  , longSetCookieTest
  , sameSiteOverHttpTest
  , abortOnContextClose
  , disposeAfterContextReason
  , retryHangupTest
  , retryGzipTest
  , hostHeaderRedirectTest
  , httpCredentialsSendTest
  , multipartFileTest
  , multipartAssetTest
  , multipartOrderTest
  , multipartRepeatingTest
  )
import Playwright.Harness
  ( requireTimeout
  , assertProtocolContains
  , withAssetsPrefix
  , withChromiumContext
  , withChromiumContextWith
  , assertEqual
  )
import Test.Tasty
  ( localOption
  , mkTimeout
  , testGroup
  , TestTree
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.MVar
  ( takeMVar
  )

tests :: TestTree
tests =
  localOption (mkTimeout 30000000) $
    testGroup
      "browsercontext-fetch"
      [ testCase "should return error with wrong credentials" $
          withAuthApp $ \prefix _captured ctx -> do
            request <- contextRequest ctx
            setHTTPCredentials
              ctx
              HttpCredentials
                { httpCredentialsUsername = HttpUsername "user"
                , httpCredentialsPassword = HttpPassword "wrong"
                , httpCredentialsOrigin = Nothing
                , httpCredentialsSend = Nothing
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
      , testCase "should support HTTPCredentials.send for newContext" $
          httpCredentialsSendTest
      , testCase "should support multipart/form-data" $
          multipartFileTest
      , testCase "should support multipart/form-data with file bytes" $
          multipartAssetTest
      , testCase "should support multipart/form-data and keep the order" $
          multipartOrderTest
      , testCase "should support repeating names in multipart/form-data" $
          multipartRepeatingTest
      ]

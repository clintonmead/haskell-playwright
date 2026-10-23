module Playwright.BrowserContextFetch1Spec
  ( tests
  ) where

import Data.Aeson
  ( object
  , (.=)
  )
import Data.List
  ( sort
  )
import Data.Text
  ( Text
  , isInfixOf
  )
import Playwright
  ( CookieName (CookieName)
  , CookieValue (CookieValue)
  , FetchOptions (fetchFailOnStatusCode, fetchHeaders, fetchQueryParams, fetchTimeout)
  , FetchQueryParam (FetchQueryParam)
  , HeaderName (HeaderName)
  , HeaderValue (HeaderValue)
  , HttpCredentials (HttpCredentials, httpCredentialsOrigin, httpCredentialsPassword, httpCredentialsSend, httpCredentialsUsername)
  , HttpHeader (HttpHeader)
  , HttpPassword (HttpPassword)
  , HttpStatus (httpStatusToInt)
  , HttpUsername (HttpUsername)
  , addCookies
  , apiResponseBody
  , apiResponseDispose
  , apiResponseHeaders
  , apiResponseHeadersArray
  , apiResponseJson
  , apiResponseStatus
  , apiResponseText
  , apiResponseUrl
  , contextRequest
  , cookieForUrl
  , cookies
  , defaultFetchOptions
  , defaultGotoOptions
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
  , goto
  , pageRequest
  , setHTTPCredentials
  , urlFromText
  , urlToText
  )
import Playwright.BrowserContextFetchSupport
  ( assertSimpleJson
  , methodPostDataTest
  , assertNetworkError
  , withAssetsContext
  , withHangupApp
  , withRedirectJsonApp
  , withSetCookieApp
  , withSlowApp
  , withMissingApp
  , objectParamsTest
  , duplicateParamsTest
  , encodedParamsTest
  , queryParamsTest
  , decodeQueryText
  , failOnStatusTest
  , withPartialBodyApp
  , withCookieCapture
  , withUtf8RedirectApp
  , withOrderedCookieApp
  , withEmptyCookieApp
  , withSetCookieBodyApp
  , withNegativeMaxAgeApp
  , withPastExpiresApp
  , cookieRedirectsTest
  , withRawHeadersApp
  , headerNameContains
  , withAuthApp
  , simpleJson
  , cookiePair
  , headerValue
  , splitSemi
  , trim
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  )
import Playwright.Harness
  ( requireTimeout
  , assertProtocolContains
  , assertBool
  , assertEqual
  , assertFailure
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
import UnliftIO.Exception
  ( try
  )
import UnliftIO.MVar
  ( takeMVar
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
                , httpCredentialsSend = Nothing
                }
            second <- fetchGet request (urlFromText (prefix <> "/empty.html"))
            assertEqual "authorized" 200 (httpStatusToInt (apiResponseStatus second))
      , testCase "should accept bool and numeric params and filter out undefined" $
          queryParamsTest
            fetchGetWith
            defaultFetchOptions
              { fetchQueryParams =
                  Just
                    [ FetchQueryParam "str" "s"
                    , FetchQueryParam "num" "10"
                    , FetchQueryParam "bool" "true"
                    , FetchQueryParam "bool2" "false"
                    ]
              }
            [("str", "s"), ("num", "10"), ("bool", "true"), ("bool2", "false")]
      ]

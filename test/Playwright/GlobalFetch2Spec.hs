module Playwright.GlobalFetch2Spec
  ( tests
  ) where

import Data.ByteString qualified as ByteString
import Data.Text
  ( replace
  , toUpper
  )
import Playwright
  ( NewRequestOptions (newRequestBaseURL)
  , apiResponseBody
  , apiResponseSecurityDetails
  , apiResponseText
  , apiResponseUrl
  , defaultNewRequestOptions
  , disposeAPIRequestContext
  , fetchGet
  , newRequestContext
  , newRequestContextWith
  , urlFromText
  , urlToText
  , runPlaywright
  )
import Playwright.GlobalFetchSupport
  ( credentialsOriginTest
  , credentialsOriginPortTest
  , wwwAuthenticateBasicTest
  , httpCredentialsSendTest
  , serverAddrTest
  , playwrightUserAgentTest
  , abortWhenDisposed
  , abortRedirectWhenDisposed
  , stripContentLengthOnRedirect
  , jsonSerializationTests
  , serializedBufferJsonTest
  , emptyEncodedBodyTest
  , failingRequestBodyTest
  , maxRedirectsExceededTest
  , maxRedirectsZeroTest
  , maxRedirectsNegativeTest
  , maxRedirectsZeroContextTest
  , maxRedirectsContextLimitTest
  , maxRedirectsFetchOverrideTest
  , headerCapitalizationTest
  , retryHangupTest
  , contextFailOnStatus
  )
import Playwright.Harness
  ( assertProtocolContains
  , withAssetsPrefix
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

tests :: TestTree
tests =
  localOption (mkTimeout 30000000) $
    testGroup
      "global-fetch"
      ( [ testCase "should work with correct credentials and matching origin" $
            credentialsOriginTest id
        , testCase "should work with correct credentials and matching origin case insensitive" $
            credentialsOriginTest toUpper
        , testCase "should return error with correct credentials and mismatching scheme" $
            credentialsOriginTest (replace "http://" "https://")
        , testCase "should return error with correct credentials and mismatching hostname" $
            credentialsOriginTest (replace "127.0.0.1" "mismatching-hostname")
        , testCase "should return error with correct credentials and mismatching port" $
            credentialsOriginPortTest
        , testCase "should support WWW-Authenticate: Basic" $
            wwwAuthenticateBasicTest
        , testCase "should support HTTPCredentials.send" $
            httpCredentialsSendTest
        , testCase "should return server address from response" $
            serverAddrTest
        , testCase "should return null security details for http response" $
            withAssetsPrefix $ \prefix ->
              runPlaywright $ do
                request <- newRequestContext
                response <- fetchGet request (urlFromText (prefix <> "/empty.html"))
                assertEqual "securityDetails" Nothing (apiResponseSecurityDetails response)
                disposeAPIRequestContext request
        , testCase "should resolve url relative to global baseURL option" $
            withAssetsPrefix $ \prefix ->
              runPlaywright $ do
                request <-
                  newRequestContextWith
                    defaultNewRequestOptions {newRequestBaseURL = Just (urlFromText prefix)}
                response <- fetchGet request (urlFromText "/empty.html")
                assertEqual "url" (prefix <> "/empty.html") (urlToText (apiResponseUrl response))
                disposeAPIRequestContext request
        , testCase "should fallback to given URL if baseURL is bogus" $
            withAssetsPrefix $ \prefix ->
              runPlaywright $ do
                request <-
                  newRequestContextWith
                    defaultNewRequestOptions {newRequestBaseURL = Just (urlFromText "bogus")}
                response <- fetchGet request (urlFromText (prefix <> "/empty.html"))
                assertEqual "url" (prefix <> "/empty.html") (urlToText (apiResponseUrl response))
                disposeAPIRequestContext request
        , testCase "should set playwright as user-agent" $
            playwrightUserAgentTest
        , testCase "should return empty body" $
            withAssetsPrefix $ \prefix ->
              runPlaywright $ do
                request <- newRequestContext
                response <- fetchGet request (urlFromText (prefix <> "/empty.html"))
                body <- apiResponseBody response
                assertEqual "length" 0 (ByteString.length body)
                assertEqual "text" "" =<< apiResponseText response
                disposeAPIRequestContext request
        , testCase "should abort requests when context is disposed" $
            abortWhenDisposed
        , testCase "should abort redirected requests when context is disposed" $
            abortRedirectWhenDisposed
        , testCase "should remove content-length from redirected post requests" $
            stripContentLengthOnRedirect
        , testCase "should accept already serialized data as Buffer when content-type is application/json" $
            serializedBufferJsonTest
        , testCase "should not fail on empty body with encoding" $
            emptyEncodedBodyTest
        , testCase "should return body for failing requests" $
            failingRequestBodyTest
        , testCase "should throw an error when maxRedirects is exceeded" $
            maxRedirectsExceededTest
        , testCase "should not follow redirects when maxRedirects is set to 0" $
            maxRedirectsZeroTest
        , testCase "should throw an error when maxRedirects is less than 0" $
            maxRedirectsNegativeTest
        , testCase "should not follow redirects when maxRedirects is set to 0 in newContext" $
            maxRedirectsZeroContextTest
        , testCase "should follow redirects up to maxRedirects limit set in newContext" $
            maxRedirectsContextLimitTest
        , testCase "should use maxRedirects from fetch when provided, overriding newContext" $
            maxRedirectsFetchOverrideTest
        , testCase "should keep headers capitalization" $
            headerCapitalizationTest
        , testCase "should throw after dispose" $
            withAssetsPrefix $ \prefix ->
              runPlaywright $ do
                request <- newRequestContext
                disposeAPIRequestContext request
                assertProtocolContains
                  "Target page, context or browser has been closed"
                  (fetchGet request (urlFromText (prefix <> "/empty.html")))
        , testCase "should retry ECONNRESET" $
            retryHangupTest
        , testCase "should throw when failOnStatusCode is set to true inside APIRequest context options" $
            contextFailOnStatus True
        , testCase "should not throw when failOnStatusCode is set to false inside APIRequest context options" $
            contextFailOnStatus False
        ]
          <> jsonSerializationTests
      )

module Playwright.GlobalFetch1Spec
  ( tests
  ) where

import Data.Aeson
  ( object
  , (.=)
  )
import Data.Text
  ( Text
  , isInfixOf
  )
import Playwright
  ( HttpCredentials (HttpCredentials, httpCredentialsOrigin, httpCredentialsPassword, httpCredentialsSend, httpCredentialsUsername)
  , HttpPassword (HttpPassword)
  , HttpStatus (httpStatusToInt)
  , HttpUsername (HttpUsername)
  , NewRequestOptions (newRequestHttpCredentials, newRequestTimeout, newRequestUserAgent)
  , UserAgent (UserAgent)
  , apiResponseJson
  , apiResponseOk
  , apiResponseStatus
  , apiResponseText
  , apiResponseUrl
  , defaultNewRequestOptions
  , disposeAPIRequestContext
  , fetchDelete
  , fetchFromUrl
  , fetchGet
  , fetchHead
  , fetchPatch
  , fetchPost
  , fetchPut
  , newRequestContext
  , newRequestContextWith
  , urlFromText
  , urlToText
  , runPlaywright
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure, ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.GlobalFetchSupport
  ( methodSmoke
  , withGlobalSimple
  , withHeaderCapture
  , takeCaptured
  , withHangApp
  , extraHeadersRedirects
  , authorizationRedirectTest
  , withAuthApp
  , validCredentials
  , lookupHeader
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

tests :: TestTree
tests =
  localOption (mkTimeout 30000000) $
    testGroup
      "global-fetch"
      [ testCase "fetch should work" $
            methodSmoke fetchFromUrl False
        , testCase "delete should work" $
            methodSmoke fetchDelete False
        , testCase "get should work" $
            methodSmoke fetchGet False
        , testCase "head should work" $
            methodSmoke fetchHead True
        , testCase "patch should work" $
            methodSmoke fetchPatch False
        , testCase "post should work" $
            methodSmoke fetchPost False
        , testCase "put should work" $
            methodSmoke fetchPut False
        , testCase "should dispose global request" $
            withGlobalSimple $ \request url -> do
              response <- fetchGet request url
              assertEqual "json" (object ["foo" .= ("bar" :: Text)]) =<< apiResponseJson response
              disposeAPIRequestContext request
              result <- try (apiResponseText response)
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
              disposeAPIRequestContext request
        , testCase "should support global userAgent option" $
            withHeaderCapture $ \prefix captured ->
              runPlaywright $ do
                request <-
                  newRequestContextWith
                    defaultNewRequestOptions {newRequestUserAgent = Just (UserAgent "My Agent")}
                response <- fetchGet request (urlFromText (prefix <> "/empty.html"))
                headers <- takeCaptured captured
                assertEqual "ok" True (apiResponseOk response)
                assertEqual "url" (prefix <> "/empty.html") (urlToText (apiResponseUrl response))
                assertEqual "user-agent" (Just "My Agent") (lookupHeader "user-agent" headers)
                disposeAPIRequestContext request
        , testCase "should support global timeout option" $
            withHangApp $ \prefix ->
              runPlaywright $ do
                timeout <- requireTimeout 100
                request <-
                  newRequestContextWith
                    defaultNewRequestOptions {newRequestTimeout = Just timeout}
                assertProtocolContains
                  "Timeout 100ms exceeded"
                  (fetchGet request (urlFromText (prefix <> "/empty.html")))
                disposeAPIRequestContext request
        , testCase "should propagate extra http headers with redirects" $
            extraHeadersRedirects
        , testCase "should preserve authorization on same-origin redirect but strip on cross-origin" $
            authorizationRedirectTest
        , testCase "should support global httpCredentials option" $
            withAuthApp $ \prefix ->
              runPlaywright $ do
                request1 <- newRequestContext
                first <- fetchGet request1 (urlFromText (prefix <> "/empty.html"))
                assertEqual "unauthorized" 401 (httpStatusToInt (apiResponseStatus first))
                disposeAPIRequestContext request1
                request2 <-
                  newRequestContextWith
                    defaultNewRequestOptions {newRequestHttpCredentials = Just validCredentials}
                second <- fetchGet request2 (urlFromText (prefix <> "/empty.html"))
                assertEqual "authorized" 200 (httpStatusToInt (apiResponseStatus second))
                disposeAPIRequestContext request2
        , testCase "should return error with wrong credentials" $
            withAuthApp $ \prefix ->
              runPlaywright $ do
                request <-
                  newRequestContextWith
                    defaultNewRequestOptions
                      { newRequestHttpCredentials =
                          Just
                            HttpCredentials
                              { httpCredentialsUsername = HttpUsername "user"
                              , httpCredentialsPassword = HttpPassword "wrong"
                              , httpCredentialsOrigin = Nothing
                              , httpCredentialsSend = Nothing
                              }
                      }
                response <- fetchGet request (urlFromText (prefix <> "/empty.html"))
                assertEqual "status" 401 (httpStatusToInt (apiResponseStatus response))
                disposeAPIRequestContext request
        ]

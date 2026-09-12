module Playwright.GlobalFetchSpec
  ( tests
  ) where

import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )
import Control.Concurrent
  ( Chan
  , newChan
  , readChan
  , writeChan
  )
import UnliftIO.MVar
  ( MVar
  , newEmptyMVar
  , takeMVar
  , tryPutMVar
  )
import UnliftIO.Concurrent
  ( threadDelay
  )
import UnliftIO.Async
  ( wait
  , withAsync
  )
import UnliftIO.Exception
  ( try
  )
import Control.Monad
  ( forM_
  , forever
  , replicateM
  )
import Data.Aeson
  ( Value (Bool, Null, Number, String)
  , encode
  , object
  , toJSON
  , (.=)
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import UnliftIO.IORef
  ( IORef
  , atomicModifyIORef'
  , newIORef
  )
import Data.Maybe
  ( fromMaybe
  )
import Data.Text
  ( Text
  , breakOnEnd
  , isInfixOf
  , isPrefixOf
  , pack
  , replace
  , toLower
  , toUpper
  , unpack
  )
import Data.Text.Encoding
  ( decodeUtf8
  , encodeUtf8
  )
import Network.HTTP.Types
  ( RequestHeaders
  , hContentType
  , hLocation
  , status200
  , status302
  , status401
  , status404
  )
import Network.HTTP.Types qualified as HTTP
import Network.Wai
  ( Application
  , pathInfo
  , requestHeaders
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
  , FetchOptions (fetchHeaders, fetchMaxRedirects, fetchMaxRetries, fetchMethod, fetchPostData)
  , FetchPostData (FetchPostBytes, FetchPostJson, FetchPostText)
  , HeaderName (HeaderName, headerNameToText)
  , HeaderValue (HeaderValue, headerValueToText)
  , HttpCredentials (HttpCredentials, httpCredentialsOrigin, httpCredentialsPassword, httpCredentialsSend, httpCredentialsUsername)
  , HttpCredentialsSend (HttpCredentialsSendAlways)
  , HttpHeader (HttpHeader, httpHeaderName, httpHeaderValue)
  , HttpMethod (HttpMethod)
  , HttpPassword (HttpPassword)
  , HttpStatus (httpStatusToInt)
  , HttpUsername (HttpUsername)
  , NewRequestOptions (newRequestBaseURL, newRequestExtraHTTPHeaders, newRequestFailOnStatusCode, newRequestHttpCredentials, newRequestMaxRedirects, newRequestTimeout, newRequestUserAgent)
  , RemoteAddr (remoteAddrIpAddress, remoteAddrPort)
  , Timeout
  , Url
  , UserAgent (UserAgent)
  , apiResponseBody
  , apiResponseHeaders
  , apiResponseJson
  , apiResponseOk
  , apiResponseSecurityDetails
  , apiResponseServerAddr
  , apiResponseStatus
  , apiResponseStatusText
  , apiResponseText
  , apiResponseUrl
  , defaultFetchOptions
  , defaultNewRequestOptions
  , disposeAPIRequestContext
  , fetchDelete
  , fetchFromUrl
  , fetchFromUrlWith
  , fetchGet
  , fetchGetWith
  , fetchHead
  , fetchPatch
  , fetchPost
  , fetchPostWith
  , fetchPut
  , milliseconds
  , newRequestContext
  , newRequestContextWith
  , urlFromText
  , urlToText
  , runPlaywright
  , Playwright
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure, ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( assertProtocolContains
  , testAssetsDir
  , withAssetsPrefix
  , assertBool
  , assertEqual
  , assertFailure
  )
import Test.Tasty
  ( TestTree
  , localOption
  , mkTimeout
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import Text.Read
  ( readMaybe
  )

tests :: TestTree
tests =
  localOption (mkTimeout 30000000) $
    testGroup
      "global-fetch"
      ( [ testCase "fetch should work" $
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
        , testCase "should work with correct credentials and matching origin" $
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

methodSmoke :: (APIRequestContext -> Url -> Playwright APIResponse) -> Bool -> IO ()
methodSmoke send isHead =
  withGlobalSimple $ \request url -> do
    response <- send request url
    assertEqual "url" (urlToText url) (urlToText (apiResponseUrl response))
    assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
    assertEqual "statusText" "OK" (apiResponseStatusText response)
    assertEqual "ok" True (apiResponseOk response)
    if isHead
      then do
        result <- try (apiResponseText response)
        case result of
          Left (DecodeFailure _) ->
            pure ()
          Right text ->
            assertEqual "body" "" text
          Left err ->
            assertFailure ("unexpected head body error: " <> show err)
      else
        assertEqual "body" "{\"foo\": \"bar\"}\n" =<< apiResponseText response
    disposeAPIRequestContext request

withGlobalSimple :: (APIRequestContext -> Url -> Playwright a) -> IO a
withGlobalSimple action =
  withApplication (pure simpleJsonApp) $ \port ->
    runPlaywright $ do
      request <- newRequestContext
      action request (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/simple.json"))

simpleJsonApp :: Application
simpleJsonApp _request respond =
  respond
    ( responseLBS
        status200
        [(hContentType, "application/json")]
        "{\"foo\": \"bar\"}\n"
    )

withHeaderCapture :: (Text -> Chan RequestHeaders -> IO a) -> IO a
withHeaderCapture action = do
  captured <- newChan
  withApplication (pure (headerCaptureApp captured)) $ \port ->
    action ("http://127.0.0.1:" <> pack (show port)) captured

headerCaptureApp :: Chan RequestHeaders -> Application
headerCaptureApp captured request respond = do
  writeChan captured (requestHeaders request)
  respond (responseLBS status200 [(hContentType, "text/html")] "<html><body></body></html>")

takeCaptured :: (MonadIO m) => Chan RequestHeaders -> m RequestHeaders
takeCaptured captured =
  liftIO (readChan captured)

withHangApp :: (Text -> IO a) -> IO a
withHangApp action =
  withApplication (pure hangApp) $ \port ->
    action ("http://127.0.0.1:" <> pack (show port))

hangApp :: Application
hangApp _request _respond =
  forever (threadDelay 1000000)

extraHeadersRedirects :: IO ()
extraHeadersRedirects = do
  captured <- newChan
  assets <- testAssetsDir
  withApplication (pure (redirectApp captured assets)) $ \port ->
    runPlaywright $ do
      let prefix = "http://127.0.0.1:" <> pack (show port)
      request <-
        newRequestContextWith
          defaultNewRequestOptions
            { newRequestExtraHTTPHeaders =
                Just [HttpHeader (HeaderName "My-Secret") (HeaderValue "Value")]
            }
      _ <- fetchGet request (urlFromText (prefix <> "/a/redirect1"))
      got <- replicateM 3 (liftIO (readChan captured))
      assertEqual "secret" ["Value", "Value", "Value"] (fmap (fromMaybe "" . lookupHeader "my-secret") got)
      disposeAPIRequestContext request

redirectApp :: Chan RequestHeaders -> FilePath -> Application
redirectApp captured assets request respond = do
  writeChan captured (requestHeaders request)
  case pathInfo request of
    ["a", "redirect1"] ->
      respond (responseLBS status302 [(hLocation, "/b/c/redirect2")] "")
    ["b", "c", "redirect2"] ->
      respond (responseLBS status302 [(hLocation, "/simple.json")] "")
    _ ->
      staticApp (defaultFileServerSettings assets) request respond

authorizationRedirectTest :: IO ()
authorizationRedirectTest = do
  captured <- newChan
  withApplication (pure (authorizationRedirectApp captured)) $ \port ->
    runPlaywright $ do
      let localPrefix = "http://127.0.0.1:" <> pack (show port)
      request <-
        newRequestContextWith
          defaultNewRequestOptions
            { newRequestExtraHTTPHeaders =
                Just [HttpHeader (HeaderName "Authorization") (HeaderValue "Bearer secret")]
            }
      _ <- fetchGet request (urlFromText (localPrefix <> "/same/redirect"))
      sameHeaders <- liftIO (readChan captured)
      _ <- fetchGet request (urlFromText (localPrefix <> "/cross/redirect"))
      crossHeaders <- liftIO (readChan captured)
      assertEqual "same-origin" (Just "Bearer secret") (lookupHeader "authorization" sameHeaders)
      assertEqual "cross-origin" Nothing (lookupHeader "authorization" crossHeaders)
      disposeAPIRequestContext request

authorizationRedirectApp :: Chan RequestHeaders -> Application
authorizationRedirectApp captured request respond =
  case pathInfo request of
    ["same", "redirect"] ->
      respond (responseLBS status302 [(hLocation, "/same/dest")] "")
    ["cross", "redirect"] -> do
      host <- maybe (assertFailure "missing host") (pure . decodeUtf8) (lookupHeader "host" (requestHeaders request))
      let
        dest :: Text
        dest =
          "http://localhost:" <> snd (breakOnEnd ":" host) <> "/cross/dest"
      respond (responseLBS status302 [(hLocation, encodeUtf8 dest)] "")
    ["same", "dest"] -> do
      writeChan captured (requestHeaders request)
      respond (responseLBS status200 [(hContentType, "text/html")] "<html></html>")
    ["cross", "dest"] -> do
      writeChan captured (requestHeaders request)
      respond (responseLBS status200 [(hContentType, "text/html")] "<html></html>")
    _ ->
      respond (responseLBS status200 [(hContentType, "text/html")] "<html></html>")

withAuthApp :: (Text -> IO a) -> IO a
withAuthApp action =
  withApplication (pure authApp) $ \port ->
    action ("http://127.0.0.1:" <> pack (show port))

authApp :: Application
authApp request respond =
  case lookupHeader "authorization" (requestHeaders request) of
    Just "Basic dXNlcjpwYXNz" ->
      respond (responseLBS status200 [(hContentType, "text/html")] "<html></html>")
    _ ->
      respond (responseLBS status401 [("WWW-Authenticate", "Basic realm=\"test\"")] "")

validCredentials :: HttpCredentials
validCredentials =
  HttpCredentials
    { httpCredentialsUsername = HttpUsername "user"
    , httpCredentialsPassword = HttpPassword "pass"
    , httpCredentialsOrigin = Nothing
    , httpCredentialsSend = Nothing
    }

credentialsOriginTest :: (Text -> Text) -> IO ()
credentialsOriginTest originMap =
  withAuthApp $ \prefix ->
    runPlaywright $ do
      request <-
        newRequestContextWith
          defaultNewRequestOptions
            { newRequestHttpCredentials =
                Just
                  validCredentials {httpCredentialsOrigin = Just (urlFromText (originMap prefix))}
            }
      response <- fetchGet request (urlFromText (prefix <> "/empty.html"))
      let
        expected :: Int
        expected =
          if originMap prefix == prefix || originMap prefix == toUpper prefix
            then 200
            else 401
      assertEqual "status" expected (httpStatusToInt (apiResponseStatus response))
      disposeAPIRequestContext request

credentialsOriginPortTest :: IO ()
credentialsOriginPortTest =
  withAuthApp $ \prefix -> do
    origin <- bumpPort prefix
    credentialsOriginWith prefix origin

credentialsOriginWith :: Text -> Text -> IO ()
credentialsOriginWith prefix origin =
  runPlaywright $ do
    request <-
      newRequestContextWith
        defaultNewRequestOptions
          { newRequestHttpCredentials =
              Just validCredentials {httpCredentialsOrigin = Just (urlFromText origin)}
          }
    response <- fetchGet request (urlFromText (prefix <> "/empty.html"))
    assertEqual "status" 401 (httpStatusToInt (apiResponseStatus response))
    disposeAPIRequestContext request

bumpPort :: Text -> IO Text
bumpPort prefix =
  case breakOnEnd ":" prefix of
    (host, portText) ->
      case readMaybe (unpack portText) of
        Just (n :: Int) ->
          pure (host <> pack (show (n + 1)))
        Nothing ->
          assertFailure ("could not parse port from " <> unpack prefix)

wwwAuthenticateBasicTest :: IO ()
wwwAuthenticateBasicTest = do
  captured <- newChan
  withApplication (pure (wwwAuthenticateApp captured)) $ \port ->
    runPlaywright $ do
      request <-
        newRequestContextWith
          defaultNewRequestOptions {newRequestHttpCredentials = Just validCredentials}
      response <- fetchGet request (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
      auth <- lookupHeader "authorization" <$> liftIO (readChan captured)
      assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
      assertEqual "auth" (Just "Basic dXNlcjpwYXNz") auth
      disposeAPIRequestContext request

wwwAuthenticateApp :: Chan RequestHeaders -> Application
wwwAuthenticateApp captured request respond =
  case lookupHeader "authorization" (requestHeaders request) of
    Just "Basic dXNlcjpwYXNz" -> do
      writeChan captured (requestHeaders request)
      respond (responseLBS status200 [(hContentType, "text/plain")] "")
    _ ->
      respond (responseLBS status401 [("WWW-Authenticate", "Basic")] "HTTP Error 401 Unauthorized: Access is denied")

httpCredentialsSendTest :: IO ()
httpCredentialsSendTest = do
  captured <- newChan
  withApplication (pure (headerCaptureApp captured)) $ \port ->
    runPlaywright $ do
      let
        localPrefix = "http://localhost:" <> pack (show port)
        crossPrefix = "http://127.0.0.1:" <> pack (show port)
      request <-
        newRequestContextWith
          defaultNewRequestOptions
            { newRequestHttpCredentials =
                Just
                  HttpCredentials
                    { httpCredentialsUsername = HttpUsername "user"
                    , httpCredentialsPassword = HttpPassword "pass"
                    , httpCredentialsOrigin = Just (urlFromText (toUpper localPrefix))
                    , httpCredentialsSend = Just HttpCredentialsSendAlways
                    }
            }
      responseLocal <- fetchGet request (urlFromText (localPrefix <> "/empty.html"))
      authLocal <- lookupHeader "authorization" <$> liftIO (readChan captured)
      responseCross <- fetchGet request (urlFromText (crossPrefix <> "/empty.html"))
      authCross <- lookupHeader "authorization" <$> liftIO (readChan captured)
      assertEqual "local status" 200 (httpStatusToInt (apiResponseStatus responseLocal))
      assertEqual "local auth" (Just "Basic dXNlcjpwYXNz") authLocal
      assertEqual "cross status" 200 (httpStatusToInt (apiResponseStatus responseCross))
      assertEqual "cross auth" Nothing authCross
      disposeAPIRequestContext request

serverAddrTest :: IO ()
serverAddrTest =
  withAssetsPrefix $ \prefix ->
    runPlaywright $ do
      request <- newRequestContext
      expectedPort <- portFromPrefix prefix
      let
        checkOnce :: Playwright ()
        checkOnce = do
          response <- fetchGet request (urlFromText (prefix <> "/empty.html"))
          case apiResponseServerAddr response of
            Nothing ->
              -- Playwright 1.61.1 omits this under Node happy-eyeballs.
              pure ()
            Just addr -> do
              assertBool
                "ip"
                ( remoteAddrIpAddress addr == "127.0.0.1"
                    || remoteAddrIpAddress addr == "::1"
                    || remoteAddrIpAddress addr == "::ffff:127.0.0.1"
                )
              assertEqual "port" expectedPort (remoteAddrPort addr)
      checkOnce
      checkOnce
      disposeAPIRequestContext request

portFromPrefix :: (MonadIO m) => Text -> m Int
portFromPrefix prefix =
  case breakOnEnd ":" prefix of
    (_, portText) ->
      case readMaybe (unpack portText) of
        Just n ->
          pure n
        Nothing ->
          assertFailure ("could not parse port from " <> unpack prefix)

playwrightUserAgentTest :: IO ()
playwrightUserAgentTest =
  withHeaderCapture $ \prefix captured ->
    runPlaywright $ do
      request <- newRequestContext
      _ <- fetchGet request (urlFromText (prefix <> "/empty.html"))
      headers <- takeCaptured captured
      ua <- maybe (assertFailure "missing user-agent") (pure . decodeUtf8) (lookupHeader "user-agent" headers)
      assertBool "playwright" ("Playwright/" `isPrefixOf` ua)
      assertBool "platform" ("(" `isInfixOf` ua)
      disposeAPIRequestContext request

abortWhenDisposed :: IO ()
abortWhenDisposed = do
  arrived <- newEmptyMVar
  withApplication (pure (hangForever arrived)) $ \port ->
    runPlaywright $ do
      request <- newRequestContext
      let url = urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html")
      withAsync (try (fetchGet request url)) $ \pendingGet ->
        withAsync (try (fetchPost request url)) $ \pendingPost ->
          withAsync (try (fetchDelete request url)) $ \pendingDelete -> do
            _ <- takeMVar arrived
            disposeAPIRequestContext request
            assertAborted =<< wait pendingGet
            assertAborted =<< wait pendingPost
            assertAborted =<< wait pendingDelete

abortRedirectWhenDisposed :: IO ()
abortRedirectWhenDisposed = do
  arrived <- newEmptyMVar
  withApplication (pure (redirectHangApp arrived)) $ \port ->
    runPlaywright $ do
      request <- newRequestContext
      withAsync
        (try (fetchGet request (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/redirect"))))
        $ \pending -> do
          _ <- takeMVar arrived
          disposeAPIRequestContext request
          assertAborted =<< wait pending

assertAborted :: Either PlaywrightError APIResponse -> Playwright ()
assertAborted result =
  case result of
    Left (ProtocolRpc failure) ->
      assertBool
        "disposed"
        ( "disposed" `isInfixOf` protocolFailureMessage failure
            || "closed" `isInfixOf` protocolFailureMessage failure
        )
    Left (DecodeFailure message) ->
      assertBool
        "disposed"
        ("disposed" `isInfixOf` message || "closed" `isInfixOf` message)
    Left err ->
      assertFailure ("expected abort, got: " <> show err)
    Right _ ->
      assertFailure "expected abort on dispose"

hangForever :: MVar () -> Application
hangForever arrived _request _respond = do
  _ <- tryPutMVar arrived ()
  forever (threadDelay 1000000)

redirectHangApp :: MVar () -> Application
redirectHangApp arrived request respond =
  case pathInfo request of
    ["redirect"] ->
      respond (responseLBS status302 [(hLocation, "/test")] "")
    ["test"] ->
      hangForever arrived request respond
    _ ->
      respond (responseLBS status404 [] "")

stripContentLengthOnRedirect :: IO ()
stripContentLengthOnRedirect = do
  captured <- newChan
  withApplication (pure (contentLengthRedirectApp captured)) $ \port ->
    runPlaywright $ do
      request <- newRequestContext
      response <-
        fetchPostWith
          request
          (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/redirect"))
          defaultFetchOptions {fetchPostData = Just (FetchPostJson (object ["foo" .= ("bar" :: Text)]))}
      firstHeaders <- liftIO (readChan captured)
      secondHeaders <- liftIO (readChan captured)
      assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
      assertEqual "first content-length" (Just "13") (lookupHeader "content-length" firstHeaders)
      assertEqual "second content-length" Nothing (lookupHeader "content-length" secondHeaders)
      disposeAPIRequestContext request

contentLengthRedirectApp :: Chan RequestHeaders -> Application
contentLengthRedirectApp captured request respond = do
  _ <- strictRequestBody request
  writeChan captured (requestHeaders request)
  case pathInfo request of
    ["redirect"] ->
      respond (responseLBS status302 [(hLocation, "/empty.html")] "")
    _ ->
      respond (responseLBS status200 [(hContentType, "text/html")] "")

jsonSerializationTests :: [TestTree]
jsonSerializationTests =
  concatMap jsonTypeTests jsonBodies

jsonBodies :: [(Text, Value)]
jsonBodies =
  [ ("object", object ["foo" .= ("bar" :: Text)])
  , ("array", toJSON [String "foo", String "bar", Number 2021])
  , ("string", String "foo")
  , ("string (falsey)", String "")
  , ("bool", Bool True)
  , ("bool (false)", Bool False)
  , ("number", Number 2021)
  , ("number (falsey)", Number 0)
  , ("null", Null)
  , ("literal string undefined", String "undefined")
  ]

jsonTypeTests :: (Text, Value) -> [TestTree]
jsonTypeTests (label, value) =
  [ testCase ("should json stringify " <> unpack label <> " body when content-type is application/json") $
      jsonBodyTest (FetchPostJson value) (LBS.toStrict (encode value))
  , testCase ("should not double stringify " <> unpack label <> " body when content-type is application/json") $
      jsonBodyTest (FetchPostText (decodeUtf8 (LBS.toStrict (encode value)))) (LBS.toStrict (encode value))
  ]

jsonBodyTest :: FetchPostData -> ByteString -> IO ()
jsonBodyTest payload expected = do
  captured <- newChan
  withApplication (pure (bodyCaptureApp captured)) $ \port ->
    runPlaywright $ do
      request <- newRequestContext
      _ <-
        fetchPostWith
          request
          (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
          defaultFetchOptions
            { fetchHeaders = Just [HttpHeader (HeaderName "content-type") (HeaderValue "application/json")]
            , fetchPostData = Just payload
            }
      body <- liftIO (readChan captured)
      assertEqual "body" expected body
      disposeAPIRequestContext request

serializedBufferJsonTest :: IO ()
serializedBufferJsonTest = do
  let
    value :: ByteString
    value =
      LBS.toStrict (encode (String (decodeUtf8 (LBS.toStrict (encode (object ["foo" .= ("bar" :: Text)]))))))
  jsonBodyTest (FetchPostBytes value) value

bodyCaptureApp :: Chan ByteString -> Application
bodyCaptureApp captured request respond = do
  body <- fmap LBS.toStrict (strictRequestBody request)
  writeChan captured body
  respond (responseLBS status200 [(hContentType, "text/html")] "")

emptyEncodedBodyTest :: IO ()
emptyEncodedBodyTest =
  withApplication (pure encodedEmptyApp) $ \port ->
    runPlaywright $ do
      request <- newRequestContext
      let url = urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html")
      forM_ encodedEmptyMethods $ \send ->
        forM_ ["br", "gzip", "deflate"] $ \encoding -> do
          response <-
            send
              request
              url
              defaultFetchOptions {fetchHeaders = Just [HttpHeader (HeaderName "x-encoding") (HeaderValue encoding)]}
          assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
          assertEmptyBody response
      disposeAPIRequestContext request

encodedEmptyMethods :: [APIRequestContext -> Url -> FetchOptions -> Playwright APIResponse]
encodedEmptyMethods =
  [ fetchHeadWith'
  , fetchPutWith'
  ]

fetchHeadWith' :: APIRequestContext -> Url -> FetchOptions -> Playwright APIResponse
fetchHeadWith' =
  fetchFromUrlWithMethod "HEAD"

fetchPutWith' :: APIRequestContext -> Url -> FetchOptions -> Playwright APIResponse
fetchPutWith' =
  fetchFromUrlWithMethod "PUT"

fetchFromUrlWithMethod :: Text -> APIRequestContext -> Url -> FetchOptions -> Playwright APIResponse
fetchFromUrlWithMethod method ctx url opts =
  fetchFromUrlWith ctx url (opts {fetchMethod = Just (HttpMethod method)})

encodedEmptyApp :: Application
encodedEmptyApp request respond = do
  let
    encoding :: ByteString
    encoding =
      fromMaybe "gzip" (lookupHeader "x-encoding" (requestHeaders request))
  respond
    ( responseLBS
        status200
        [("Content-Encoding", encoding), (hContentType, "text/plain")]
        ""
    )

assertEmptyBody :: APIResponse -> Playwright ()
assertEmptyBody response = do
  result <- try (apiResponseBody response)
  case result of
    Left (DecodeFailure _) ->
      pure ()
    Right body ->
      assertEqual "empty" 0 (ByteString.length body)
    Left err ->
      assertFailure ("unexpected body error: " <> show err)

failingRequestBodyTest :: IO ()
failingRequestBodyTest =
  withApplication (pure notFoundApp) $ \port ->
    runPlaywright $ do
      request <- newRequestContext
      let url = urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html")
      forM_ ["HEAD", "PUT", "TRACE"] $ \method -> do
        response <- fetchFromUrlWithMethod method request url defaultFetchOptions
        assertEqual "status" 404 (httpStatusToInt (apiResponseStatus response))
        if method == "HEAD"
          then assertEmptyBody response
          else assertEqual "body" "Not found." =<< apiResponseText response
      disposeAPIRequestContext request

notFoundApp :: Application
notFoundApp _request respond =
  respond
    ( responseLBS
        status404
        [("Content-Length", "10"), (hContentType, "text/plain")]
        "Not found."
    )

maxRedirectsExceededTest :: IO ()
maxRedirectsExceededTest =
  withRedirectChain $ \prefix ->
    runPlaywright $ do
      request <- newRequestContext
      forM_ httpMethods $ \method ->
        forM_ [1, 2, 3] $ \limit ->
          assertMessageContains
            "Max redirect count exceeded"
            ( fetchFromUrlWith
                request
                (urlFromText (prefix <> "/a/redirect1"))
                defaultFetchOptions
                  { fetchMethod = Just method
                  , fetchMaxRedirects = Just limit
                  }
            )
      disposeAPIRequestContext request

maxRedirectsZeroTest :: IO ()
maxRedirectsZeroTest =
  withRedirectChain $ \prefix ->
    runPlaywright $ do
      request <- newRequestContext
      forM_ httpMethods $ \method -> do
        response <-
          fetchFromUrlWith
            request
            (urlFromText (prefix <> "/a/redirect1"))
            defaultFetchOptions
              { fetchMethod = Just method
              , fetchMaxRedirects = Just 0
              }
        assertEqual "location" (Just "/b/c/redirect2") (headerValue "location" (apiResponseHeaders response))
        assertEqual "status" 302 (httpStatusToInt (apiResponseStatus response))
      disposeAPIRequestContext request

maxRedirectsNegativeTest :: IO ()
maxRedirectsNegativeTest =
  withRedirectChain $ \prefix ->
    runPlaywright $ do
      request <- newRequestContext
      forM_ httpMethods $ \method ->
        assertMessageContains
          "'maxRedirects' must be greater than or equal to '0'"
          ( fetchFromUrlWith
              request
              (urlFromText (prefix <> "/a/redirect1"))
              defaultFetchOptions
                { fetchMethod = Just method
                , fetchMaxRedirects = Just (-1)
                }
          )
      disposeAPIRequestContext request

maxRedirectsZeroContextTest :: IO ()
maxRedirectsZeroContextTest =
  withRedirectChain $ \prefix ->
    runPlaywright $ do
      request <-
        newRequestContextWith
          defaultNewRequestOptions {newRequestMaxRedirects = Just 0}
      forM_ httpMethods $ \method -> do
        response <-
          fetchFromUrlWith
            request
            (urlFromText (prefix <> "/a/redirect1"))
            defaultFetchOptions {fetchMethod = Just method}
        assertEqual "location" (Just "/b/c/redirect2") (headerValue "location" (apiResponseHeaders response))
        assertEqual "status" 302 (httpStatusToInt (apiResponseStatus response))
      disposeAPIRequestContext request

maxRedirectsContextLimitTest :: IO ()
maxRedirectsContextLimitTest =
  withRedirectChain $ \prefix ->
    runPlaywright $
      forM_ [1, 2, 3, 4] $ \limit -> do
        request <-
          newRequestContextWith
            defaultNewRequestOptions {newRequestMaxRedirects = Just limit}
        forM_ httpMethods $ \method ->
          if limit < 4
            then
              assertMessageContains
                "Max redirect count exceeded"
                ( fetchFromUrlWith
                    request
                    (urlFromText (prefix <> "/a/redirect1"))
                    defaultFetchOptions {fetchMethod = Just method}
                )
            else do
              response <-
                fetchFromUrlWith
                  request
                  (urlFromText (prefix <> "/a/redirect1"))
                  defaultFetchOptions {fetchMethod = Just method}
              assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
        disposeAPIRequestContext request

maxRedirectsFetchOverrideTest :: IO ()
maxRedirectsFetchOverrideTest =
  withRedirectChain $ \prefix ->
    runPlaywright $ do
      request <-
        newRequestContextWith
          defaultNewRequestOptions {newRequestMaxRedirects = Just 1}
      forM_ httpMethods $ \method -> do
        response <-
          fetchFromUrlWith
            request
            (urlFromText (prefix <> "/a/redirect1"))
            defaultFetchOptions
              { fetchMethod = Just method
              , fetchMaxRedirects = Just 4
              }
        assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
      disposeAPIRequestContext request

httpMethods :: [HttpMethod]
httpMethods =
  fmap HttpMethod ["GET", "PUT", "POST", "OPTIONS", "HEAD", "PATCH"]

withRedirectChain :: (Text -> IO a) -> IO a
withRedirectChain action =
  withApplication (pure redirectChainApp) $ \port ->
    action ("http://127.0.0.1:" <> pack (show port))

redirectChainApp :: Application
redirectChainApp request respond =
  case pathInfo request of
    ["a", "redirect1"] ->
      respond (responseLBS status302 [(hLocation, "/b/c/redirect2")] "")
    ["b", "c", "redirect2"] ->
      respond (responseLBS status302 [(hLocation, "/b/c/redirect3")] "")
    ["b", "c", "redirect3"] ->
      respond (responseLBS status302 [(hLocation, "/b/c/redirect4")] "")
    ["b", "c", "redirect4"] ->
      respond (responseLBS status302 [(hLocation, "/simple.json")] "")
    ["simple.json"] ->
      simpleJsonApp request respond
    _ ->
      respond (responseLBS status404 [] "")

headerCapitalizationTest :: IO ()
headerCapitalizationTest =
  withHeaderCapture $ \prefix captured ->
    runPlaywright $ do
      request <- newRequestContext
      response <-
        fetchGetWith
          request
          (urlFromText (prefix <> "/empty.html"))
          defaultFetchOptions
            { fetchHeaders = Just [HttpHeader (HeaderName "X-fOo") (HeaderValue "vaLUE")]
            }
      headers <- takeCaptured captured
      assertEqual "ok" True (apiResponseOk response)
      assertBool "name" (elem "X-fOo" (fmap (CI.original . fst) headers))
      assertBool "value" (elem "vaLUE" (fmap snd headers))
      disposeAPIRequestContext request

retryHangupTest :: IO ()
retryHangupTest = do
  count <- newIORef (0 :: Int)
  withApplication (pure (retryHangupApp count)) $ \port ->
    runPlaywright $ do
      request <- newRequestContext
      response <-
        fetchFromUrlWith
          request
          (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/test"))
          defaultFetchOptions {fetchMaxRetries = Just 3}
      hits <- atomicModifyIORef' count (\n -> (n, n))
      assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
      assertEqual "body" "Hello!" =<< apiResponseText response
      assertEqual "hits" 4 hits
      disposeAPIRequestContext request

retryHangupApp :: IORef Int -> Application
retryHangupApp count _request respond = do
  n <- atomicModifyIORef' count (\x -> (x + 1, x + 1))
  if n <= 3
    then respond (responseRaw (\_ _ -> pure ()) (responseLBS status200 [] ""))
    else respond (responseLBS status200 [(hContentType, "text/plain")] "Hello!")

contextFailOnStatus :: Bool -> IO ()
contextFailOnStatus shouldFail =
  withApplication (pure notFoundApp) $ \port ->
    runPlaywright $ do
      request <-
        newRequestContextWith
          defaultNewRequestOptions {newRequestFailOnStatusCode = Just shouldFail}
      let url = urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html")
      if shouldFail
        then assertMessageContains "404 Not Found" (fetchFromUrl request url)
        else do
          response <- fetchFromUrl request url
          assertEqual "status" 404 (httpStatusToInt (apiResponseStatus response))
      disposeAPIRequestContext request

assertMessageContains :: Text -> Playwright a -> Playwright ()
assertMessageContains needle action = do
  result <- try action
  case result of
    Left (ProtocolRpc failure) ->
      if needle `isInfixOf` protocolFailureMessage failure
        then pure ()
        else
          assertFailure
            ( "expected protocol error to contain "
                <> unpack needle
                <> ", got: "
                <> unpack (protocolFailureMessage failure)
            )
    Left (DecodeFailure message) ->
      if needle `isInfixOf` message
        then pure ()
        else
          assertFailure
            ( "expected error to contain "
                <> unpack needle
                <> ", got: "
                <> unpack message
            )
    Left err ->
      assertFailure ("expected error containing " <> unpack needle <> ", got: " <> show err)
    Right _ ->
      assertFailure ("expected error containing " <> unpack needle)

lookupHeader :: HTTP.HeaderName -> RequestHeaders -> Maybe ByteString
lookupHeader =
  lookup

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

requireTimeout :: (MonadIO m) => Double -> m Timeout
requireTimeout n =
  case milliseconds n of
    Nothing ->
      assertFailure ("milliseconds " <> show n)
    Just timeout ->
      pure timeout

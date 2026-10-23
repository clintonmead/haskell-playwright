module Playwright.BrowserContextFetchSupport
  ( assertSimpleJson
  , methodPostDataTest
  , assertNetworkError
  , withAssetsContext
  , withHangupApp
  , hangupApp
  , withRedirectJsonApp
  , redirectJsonApp
  , withSetCookieApp
  , setCookieApp
  , withSlowApp
  , slowApp
  , withMissingApp
  , missingApp
  , withCapturedSimple
  , captureSimple
  , objectParamsTest
  , duplicateParamsTest
  , encodedParamsTest
  , queryParamsTest
  , withQueryCapture
  , queryApp
  , decodeQueryText
  , decodeQuery
  , stripQuestion
  , decodePair
  , failOnStatusTest
  , withPartialBodyApp
  , apiTimingTest
  , partialBodyApp
  , withCookieCapture
  , cookieCaptureApp
  , withUtf8RedirectApp
  , utf8RedirectApp
  , withOrderedCookieApp
  , orderedCookieApp
  , withEmptyCookieApp
  , emptyCookieApp
  , withSetCookieBodyApp
  , setCookieBodyApp
  , withNegativeMaxAgeApp
  , negativeMaxAgeApp
  , withPastExpiresApp
  , pastExpiresApp
  , cookieRedirectsTest
  , cookieRedirectApp
  , drainCookieChan
  , cookieRedirectPair
  , withRawHeadersApp
  , rawHeadersApp
  , headerNameContains
  , withAuthApp
  , authApp
  , withHeaderCapture
  , headerCaptureApp
  , withRedirectHeaderCapture
  , redirectHeaderCaptureApp
  , withPropagatedRedirects
  , propagatedRedirectApp
  , drainCaptured
  , lookupHeader
  , compressionTest
  , corruptedCompressionTest
  , brotliHello
  , brotliRangeTest
  , compressedApp
  , withSlowThenDoneApp
  , slowThenDoneApp
  , withSlowRedirectApp
  , slowRedirectApp
  , disposeAfterContextClose
  , overrideRequestParameters
  , postCaptureApp
  , withBodyCapture
  , bodyCaptureApp
  , sampleJson
  , decodeBodyJson
  , secureLocalhostCookieTest
  , secureCookieApp
  , localhostDomainCookieTest
  , localhostDomainApp
  , longSetCookieTest
  , longSetCookieApp
  , sameSiteOverHttpTest
  , sameSiteApp
  , abortOnContextClose
  , hangForever
  , disposeAfterContextReason
  , retryHangupTest
  , retryHangupApp
  , retryGzipTest
  , retryGzipApp
  , retryMidstreamGzipTest
  , retryMidstreamGzipApp
  , hostHeaderRedirectTest
  , hostRedirectApp
  , httpCredentialsSendTest
  , headerOnlyAuthApp
  , httpCredentialsSendPageTest
  , multipartFileTest
  , multipartAssetTest
  , multipartOrderTest
  , fieldOffset
  , multipartRepeatingTest
  , simpleJson
  , cookiePair
  , headerValue
  , splitSemi
  , trim
  , dropOrigin
  ) where

import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )
import Codec.Compression.GZip qualified as GZip
import Control.Concurrent
  ( Chan
  , newChan
  , readChan
  , writeChan
  )
import UnliftIO.Concurrent
  ( threadDelay
  )
import UnliftIO.Async
  ( wait
  , withAsync
  )
import UnliftIO.MVar
  ( MVar
  , newEmptyMVar
  , takeMVar
  , tryPutMVar
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
import UnliftIO.IORef
  ( IORef
  , newIORef
  , atomicModifyIORef'
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
  , toUpper
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
  , CookieName (cookieNameToText)
  , CookiePath (cookiePathToText)
  , CookieSameSite (CookieSameSiteLax, CookieSameSiteStrict)
  , CookieSecure (cookieSecureToBool)
  , CookieValue (cookieValueToText)
  , FetchMultipartField (FetchMultipartField, FetchMultipartFileField)
  , FetchMultipartFile (FetchMultipartFile, fetchMultipartFileBytes, fetchMultipartFileMimeType, fetchMultipartFileName)
  , FetchOptions (fetchEncodedParams, fetchFailOnStatusCode, fetchHeaders, fetchMaxRetries, fetchMethod, fetchMultipart, fetchPostData, fetchQueryParams, fetchTimeout)
  , FetchPostData (FetchPostText)
  , FetchQueryParam (FetchQueryParam)
  , HeaderName (HeaderName, headerNameToText)
  , HeaderValue (HeaderValue, headerValueToText)
  , HttpCredentials (HttpCredentials, httpCredentialsOrigin, httpCredentialsPassword, httpCredentialsSend, httpCredentialsUsername)
  , HttpCredentialsSend (HttpCredentialsSendAlways)
  , HttpHeader (HttpHeader, httpHeaderName, httpHeaderValue)
  , HttpMethod (HttpMethod)
  , HttpPassword (HttpPassword)
  , HttpStatus (httpStatusToInt)
  , HttpUsername (HttpUsername)
  , NewContextOptions (newContextHttpCredentials)
  , Page
  , Url
  , UrlMatch (UrlMatchGlob)
  , apiResponseBody
  , apiResponseHeaders
  , apiResponseJson
  , apiResponseOk
  , apiResponseStatus
  , apiResponseStatusText
  , apiResponseText
  , apiResponseTiming
  , apiResponseUrl
  , resourceTimingConnectEnd
  , resourceTimingConnectStart
  , resourceTimingDomainLookupEnd
  , resourceTimingDomainLookupStart
  , resourceTimingRequestStart
  , resourceTimingResponseEnd
  , resourceTimingResponseStart
  , resourceTimingSecureConnectionStart
  , resourceTimingStartTime
  , closeContext
  , closeContextWith
  , contextRequest
  , cookies
  , defaultFetchOptions
  , defaultGotoOptions
  , defaultNewContextOptions
  , fetchGet
  , fetchGetWith
  , fetchPostWith
  , fetchRequestWith
  , goto
  , newContextWith
  , newPage
  , pageRequest
  , sessionExpires
  , urlFromText
  , urlToText
  , armRequest
  , Playwright
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure, ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Data.Time.Clock.POSIX
  ( getPOSIXTime
  )
import Playwright.Harness
  ( requireTimeout
  , assertProtocolContains
  , readTestAsset
  , testAssetsDir
  , withAssetsContextWith
  , withAssetsPrefix
  , withChromiumBrowser
  , withChromiumContext
  , withChromiumContextWith
  , assertBool
  , assertEqual
  , assertFailure
  )

assertSimpleJson :: Text -> APIResponse -> Playwright ()
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
  (APIRequestContext -> Url -> FetchOptions -> Playwright APIResponse) ->
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

assertNetworkError :: Playwright a -> Playwright ()
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

withAssetsContext :: (Text -> BrowserContext -> Playwright a) -> IO a
withAssetsContext action =
  withAssetsContextWith defaultNewContextOptions $ \prefix ctx _page ->
    action prefix ctx

withHangupApp :: (Text -> BrowserContext -> Playwright a) -> IO a
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

withRedirectJsonApp :: (Text -> BrowserContext -> Playwright a) -> IO a
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

withSetCookieApp :: (Text -> BrowserContext -> Page -> Playwright a) -> IO a
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

withSlowApp :: (Text -> BrowserContext -> Playwright a) -> IO a
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

withMissingApp :: (Text -> BrowserContext -> Playwright a) -> IO a
withMissingApp action =
  withApplication (pure missingApp) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) ctx

missingApp :: Application
missingApp _request respond =
  respond (responseLBS status404 [(hContentType, "text/plain")] "File not found: nope")

withCapturedSimple :: (Text -> MVar (ByteString, ByteString) -> BrowserContext -> Playwright a) -> IO a
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
  (APIRequestContext -> Url -> FetchOptions -> Playwright APIResponse) ->
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
  (APIRequestContext -> Url -> FetchOptions -> Playwright APIResponse) ->
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
  (APIRequestContext -> Url -> FetchOptions -> Playwright APIResponse) ->
  IO ()
encodedParamsTest send =
  queryParamsTest
    send
    defaultFetchOptions {fetchEncodedParams = Just "?param1=value1&param1=value2&парам2=знач2"}
    [("param1", "value1"), ("param1", "value2"), ("парам2", "знач2")]

queryParamsTest ::
  (APIRequestContext -> Url -> FetchOptions -> Playwright APIResponse) ->
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

withQueryCapture :: (Text -> MVar ByteString -> BrowserContext -> Playwright a) -> IO a
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
  (APIRequestContext -> Url -> FetchOptions -> Playwright APIResponse) ->
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

withPartialBodyApp :: (Text -> BrowserContext -> Playwright a) -> IO a
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

withCookieCapture :: (Text -> MVar (Maybe ByteString) -> BrowserContext -> Playwright a) -> IO a
withCookieCapture action = do
  captured <- newEmptyMVar
  withApplication (pure (cookieCaptureApp captured)) $ \port ->
    withChromiumContext $ \ctx _page ->
      action ("http://127.0.0.1:" <> pack (show port)) captured ctx

cookieCaptureApp :: MVar (Maybe ByteString) -> Application
cookieCaptureApp captured request respond = do
  _ <- tryPutMVar captured (lookupHeader "cookie" (requestHeaders request))
  respond (responseLBS status200 [(hContentType, "text/html")] "<html><body></body></html>")

withUtf8RedirectApp :: (Text -> BrowserContext -> Playwright a) -> IO a
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

withOrderedCookieApp :: (Text -> MVar (Maybe ByteString) -> BrowserContext -> Page -> Playwright a) -> IO a
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

withEmptyCookieApp :: (Text -> BrowserContext -> Page -> Playwright a) -> IO a
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

withSetCookieBodyApp :: (Text -> BrowserContext -> Playwright a) -> IO a
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

withNegativeMaxAgeApp :: (Text -> MVar (Maybe ByteString) -> BrowserContext -> Playwright a) -> IO a
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

withPastExpiresApp :: (Text -> MVar (Maybe ByteString) -> BrowserContext -> Playwright a) -> IO a
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

drainCookieChan :: (MonadIO m) => Chan ([Text], Maybe ByteString) -> Int -> m [([Text], Maybe ByteString)]
drainCookieChan captured n =
  replicateM n (liftIO (readChan captured))

cookieRedirectPair :: Cookie -> (Text, Text, Text, CookieSameSite)
cookieRedirectPair cookie =
  ( cookieNameToText (cookieName cookie)
  , cookieValueToText (cookieValue cookie)
  , cookiePathToText (cookiePath cookie)
  , cookieSameSite cookie
  )

withRawHeadersApp :: (Text -> BrowserContext -> Playwright a) -> IO a
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

withAuthApp :: (Text -> MVar (Maybe ByteString) -> BrowserContext -> Playwright a) -> IO a
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

withHeaderCapture :: (Text -> MVar RequestHeaders -> BrowserContext -> Page -> Playwright a) -> IO a
withHeaderCapture action = do
  captured <- newEmptyMVar
  withApplication (pure (headerCaptureApp captured)) $ \port ->
    withChromiumContext $ \ctx page ->
      action ("http://127.0.0.1:" <> pack (show port)) captured ctx page

headerCaptureApp :: MVar RequestHeaders -> Application
headerCaptureApp captured request respond = do
  _ <- tryPutMVar captured (requestHeaders request)
  respond (responseLBS status200 [(hContentType, "text/html")] "<html><body></body></html>")

withRedirectHeaderCapture :: (Text -> MVar RequestHeaders -> BrowserContext -> Page -> Playwright a) -> IO a
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

withPropagatedRedirects :: (Text -> Chan RequestHeaders -> BrowserContext -> Playwright a) -> IO a
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

drainCaptured :: (MonadIO m) => Chan RequestHeaders -> Int -> m [RequestHeaders]
drainCaptured captured n =
  replicateM n (liftIO (readChan captured))

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

-- | Brotli encoding of "Hello, world!", precomputed (the Haskell test
-- suite has no brotli compressor).
brotliHello :: LBS.ByteString
brotliHello =
  LBS.pack [0x0b, 0x06, 0x80, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x2c, 0x20, 0x77, 0x6f, 0x72, 0x6c, 0x64, 0x21, 0x03]

-- | Sends a 206 whose Content-Length is 1 but whose body is two bytes of
-- brotli data, so the trailing byte is read as the next response.
brotliRangeTest :: IO ()
brotliRangeTest =
  withApplication (pure brotliRangeApp) $ \port ->
    withChromiumContext $ \ctx _page -> do
      request <- contextRequest ctx
      assertProtocolContains
        "Parse Error: Expected HTTP/"
        ( fetchGetWith
            request
            (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/brotli"))
            defaultFetchOptions
              { fetchHeaders = Just [HttpHeader (HeaderName "range") (HeaderValue "bytes=0-2")]
              }
        )

brotliRangeApp :: Application
brotliRangeApp _request respond =
  respond
    ( responseRaw
        (\_ write -> write brotliRangeResponse)
        (responseLBS status200 [] "")
    )

brotliRangeResponse :: ByteString
brotliRangeResponse =
  "HTTP/1.1 206 Partial Content\r\nContent-Type: text/plain\r\nContent-Length: 1\r\nContent-Encoding: br\r\nContent-Range: bytes 0-2/5\r\nAccept-Ranges: bytes\r\n\r\n"
    <> ByteString.pack [0x0b, 0x00]

withSlowThenDoneApp :: (Text -> BrowserContext -> Playwright a) -> IO a
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

withSlowRedirectApp :: (Text -> BrowserContext -> Playwright a) -> IO a
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
      armRequest page (UrlMatchGlob (prefix <> "/empty.html")) >>= \handle -> do
        _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
        pageReq <- handle
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

withBodyCapture :: (Text -> MVar (RequestHeaders, ByteString) -> BrowserContext -> Playwright a) -> IO a
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

decodeBodyJson :: (MonadIO m) => ByteString -> m Value
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

-- Sends the gzip response headers and then drops the connection before any
-- body arrives, so the client fails mid-stream and must retry.
retryMidstreamGzipTest :: IO ()
retryMidstreamGzipTest = do
  count <- newIORef (0 :: Int)
  withApplication (pure (retryMidstreamGzipApp count)) $ \port ->
    withChromiumContext $ \ctx _page -> do
      request <- contextRequest ctx
      response <-
        fetchGetWith
          request
          (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/test-gzip-midstream"))
          defaultFetchOptions {fetchMaxRetries = Just 3}
      hits <- atomicModifyIORef' count (\n -> (n, n))
      assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
      assertEqual "body" "midstream-retry-ok" =<< apiResponseText response
      assertEqual "hits" 3 hits

retryMidstreamGzipApp :: IORef Int -> Application
retryMidstreamGzipApp count _request respond = do
  n <- atomicModifyIORef' count (\x -> (x + 1, x + 1))
  if n <= 2
    then
      respond
        ( responseRaw
            ( \_ write ->
                write "HTTP/1.1 200 OK\r\nContent-Encoding: gzip\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n"
            )
            (responseLBS status200 [] "")
        )
    else
      respond
        ( responseLBS
            status200
            [("Content-Encoding", "gzip"), (hContentType, "text/plain")]
            (GZip.compress "midstream-retry-ok")
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

httpCredentialsSendTest :: IO ()
httpCredentialsSendTest = do
  captured <- newChan
  withApplication (pure (headerOnlyAuthApp captured)) $ \port -> do
    let
      localPrefix = "http://localhost:" <> pack (show port)
      crossPrefix = "http://127.0.0.1:" <> pack (show port)
    withChromiumContextWith
      defaultNewContextOptions
        { newContextHttpCredentials =
            Just
              HttpCredentials
                { httpCredentialsUsername = HttpUsername "user"
                , httpCredentialsPassword = HttpPassword "pass"
                , httpCredentialsOrigin = Just (urlFromText (toUpper localPrefix))
                , httpCredentialsSend = Just HttpCredentialsSendAlways
                }
        }
      $ \ctx _page -> do
        request <- contextRequest ctx
        responseLocal <- fetchGet request (urlFromText (localPrefix <> "/empty.html"))
        authLocal <- liftIO (readChan captured)
        responseCross <- fetchGet request (urlFromText (crossPrefix <> "/empty.html"))
        authCross <- liftIO (readChan captured)
        assertEqual "local status" 200 (httpStatusToInt (apiResponseStatus responseLocal))
        assertEqual "local auth" (Just "Basic dXNlcjpwYXNz") authLocal
        assertEqual "cross status" 200 (httpStatusToInt (apiResponseStatus responseCross))
        assertEqual "cross auth" Nothing authCross

httpCredentialsSendPageTest :: IO ()
httpCredentialsSendPageTest = do
  captured <- newChan
  withApplication (pure (headerOnlyAuthApp captured)) $ \port -> do
    let
      localPrefix = "http://localhost:" <> pack (show port)
      crossPrefix = "http://127.0.0.1:" <> pack (show port)
    withChromiumBrowser $ \browser -> do
      ctx <-
        newContextWith
          browser
          defaultNewContextOptions
            { newContextHttpCredentials =
                Just
                  HttpCredentials
                    { httpCredentialsUsername = HttpUsername "user"
                    , httpCredentialsPassword = HttpPassword "pass"
                    , httpCredentialsOrigin = Just (urlFromText (toUpper localPrefix))
                    , httpCredentialsSend = Just HttpCredentialsSendAlways
                    }
            }
      page <- newPage ctx
      request <- pageRequest page
      responseLocal <- fetchGet request (urlFromText (localPrefix <> "/empty.html"))
      authLocal <- liftIO (readChan captured)
      responseCross <- fetchGet request (urlFromText (crossPrefix <> "/empty.html"))
      authCross <- liftIO (readChan captured)
      assertEqual "local status" 200 (httpStatusToInt (apiResponseStatus responseLocal))
      assertEqual "local auth" (Just "Basic dXNlcjpwYXNz") authLocal
      assertEqual "cross status" 200 (httpStatusToInt (apiResponseStatus responseCross))
      assertEqual "cross auth" Nothing authCross

headerOnlyAuthApp :: Chan (Maybe ByteString) -> Application
headerOnlyAuthApp captured request respond = do
  writeChan captured (lookupHeader "authorization" (requestHeaders request))
  respond (responseLBS status200 [(hContentType, "text/html")] "<html><body></body></html>")

multipartFileTest :: IO ()
multipartFileTest =
  withBodyCapture $ \prefix captured ctx -> do
    request <- contextRequest ctx
    response <-
      fetchPostWith
        request
        (urlFromText (prefix <> "/empty.html"))
        defaultFetchOptions
          { fetchMultipart =
              Just
                [ FetchMultipartField "firstName" "John"
                , FetchMultipartField "lastName" "Doe"
                , FetchMultipartFileField
                    "file"
                    FetchMultipartFile
                      { fetchMultipartFileName = "f.js"
                      , fetchMultipartFileMimeType = Just "text/javascript"
                      , fetchMultipartFileBytes = "var x = 10;\r\n;console.log(x);"
                      }
                ]
          }
    (headers, body) <- takeMVar captured
    assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
    assertBool "content-type" (maybe False ("multipart/form-data" `ByteString.isInfixOf`) (lookupHeader "content-type" headers))
    assertBool "firstName" ("name=\"firstName\"" `ByteString.isInfixOf` body && "John" `ByteString.isInfixOf` body)
    assertBool "lastName" ("name=\"lastName\"" `ByteString.isInfixOf` body && "Doe" `ByteString.isInfixOf` body)
    assertBool "file" ("filename=\"f.js\"" `ByteString.isInfixOf` body)
    assertBool "mime" ("text/javascript" `ByteString.isInfixOf` body)
    assertBool "payload" ("var x = 10;\r\n;console.log(x);" `ByteString.isInfixOf` body)

multipartAssetTest :: IO ()
multipartAssetTest = do
  asset <- readTestAsset "simplezip.json"
  withBodyCapture $ \prefix captured ctx -> do
    request <- contextRequest ctx
    response <-
      fetchPostWith
        request
        (urlFromText (prefix <> "/empty.html"))
        defaultFetchOptions
          { fetchMultipart =
              Just
                [ FetchMultipartField "firstName" "John"
                , FetchMultipartField "lastName" "Doe"
                , FetchMultipartFileField
                    "readStream"
                    FetchMultipartFile
                      { fetchMultipartFileName = "simplezip.json"
                      , fetchMultipartFileMimeType = Just "application/json"
                      , fetchMultipartFileBytes = asset
                      }
                ]
          }
    (headers, body) <- takeMVar captured
    assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
    assertBool "content-type" (maybe False ("multipart/form-data" `ByteString.isInfixOf`) (lookupHeader "content-type" headers))
    assertBool "filename" ("filename=\"simplezip.json\"" `ByteString.isInfixOf` body)
    assertBool "asset" (asset `ByteString.isInfixOf` body)

multipartOrderTest :: IO ()
multipartOrderTest =
  withBodyCapture $ \prefix captured ctx -> do
    request <- contextRequest ctx
    response <-
      fetchPostWith
        request
        (urlFromText (prefix <> "/empty.html"))
        defaultFetchOptions
          { fetchMultipart =
              Just
                [ FetchMultipartField "firstName" "John"
                , FetchMultipartField "lastName" "Doe"
                , FetchMultipartField "age" "27"
                , FetchMultipartField "foo" "bar"
                ]
          }
    (_, body) <- takeMVar captured
    assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
    assertBool
      "order"
      ( fieldOffset "firstName" body
          < fieldOffset "lastName" body
          && fieldOffset "lastName" body < fieldOffset "age" body
          && fieldOffset "age" body < fieldOffset "foo" body
      )

fieldOffset :: ByteString -> ByteString -> Int
fieldOffset name body =
  ByteString.length (fst (ByteString.breakSubstring ("name=\"" <> name <> "\"") body))

multipartRepeatingTest :: IO ()
multipartRepeatingTest =
  withBodyCapture $ \prefix captured ctx -> do
    request <- contextRequest ctx
    response <-
      fetchPostWith
        request
        (urlFromText (prefix <> "/empty.html"))
        defaultFetchOptions
          { fetchMultipart =
              Just
                [ FetchMultipartField "name" "John"
                , FetchMultipartField "name" "Doe"
                , FetchMultipartFileField
                    "file"
                    FetchMultipartFile
                      { fetchMultipartFileName = "f1.js"
                      , fetchMultipartFileMimeType = Just "text/javascript"
                      , fetchMultipartFileBytes = "var x = 10;\r\n;console.log(x);"
                      }
                , FetchMultipartFileField
                    "file"
                    FetchMultipartFile
                      { fetchMultipartFileName = "custom_f2.txt"
                      , fetchMultipartFileMimeType = Just "text/plain"
                      , fetchMultipartFileBytes = "hello"
                      }
                , FetchMultipartFileField
                    "file"
                    FetchMultipartFile
                      { fetchMultipartFileName = "blob"
                      , fetchMultipartFileMimeType = Just "text/plain"
                      , fetchMultipartFileBytes = "boo"
                      }
                ]
          }
    (_, body) <- takeMVar captured
    assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
    assertBool "john" ("name=\"name\"\r\n\r\nJohn" `ByteString.isInfixOf` body)
    assertBool "doe" ("name=\"name\"\r\n\r\nDoe" `ByteString.isInfixOf` body)
    assertBool "f1" ("filename=\"f1.js\"" `ByteString.isInfixOf` body)
    assertBool "f2" ("filename=\"custom_f2.txt\"" `ByteString.isInfixOf` body)
    assertBool "blob" ("filename=\"blob\"" `ByteString.isInfixOf` body)
    assertBool "boo" ("boo" `ByteString.isInfixOf` body)

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

splitSemi :: Text -> [Text]
splitSemi text =
  filter (/= "") (fmap trim (splitOn ";" text))

-- | Port of "should return timing": a fresh server guarantees a new connection,
-- so the dns/connect phases are reported.
apiTimingTest :: IO ()
apiTimingTest =
  withApplication (pure timingApp) $ \port ->
    withChromiumContext $ \ctx _page -> do
      request <- contextRequest ctx
      response <-
        fetchGet request (urlFromText ("http://localhost:" <> pack (show port) <> "/"))
      assertBool "response is ok" (apiResponseOk response)
      now <- liftIO getPOSIXTime
      let
        timing = apiResponseTiming response
        startMs = realToFrac (resourceTimingStartTime timing) :: Double
        nowMs = realToFrac now * 1000 :: Double
        domainLookupStart = resourceTimingDomainLookupStart timing
        domainLookupEnd = resourceTimingDomainLookupEnd timing
        connectStart = resourceTimingConnectStart timing
        secureConnectionStart = resourceTimingSecureConnectionStart timing
        connectEnd = resourceTimingConnectEnd timing
        requestStart = resourceTimingRequestStart timing
        responseStart = resourceTimingResponseStart timing
        responseEnd = resourceTimingResponseEnd timing
      -- toBeCloseTo(Date.now(), -4) allows a difference below 5000ms.
      assertBool "startTime is close to now" (abs (startMs - nowMs) < 5000)
      assertEqual "domainLookupStart" 0 domainLookupStart
      assertBool "domainLookupEnd >= domainLookupStart" (domainLookupEnd >= domainLookupStart)
      assertEqual "connectStart" domainLookupEnd connectStart
      assertEqual "secureConnectionStart" (-1) secureConnectionStart
      assertBool "connectEnd >= connectStart" (connectEnd >= connectStart)
      assertEqual "requestStart" connectEnd requestStart
      assertBool "responseStart >= requestStart" (responseStart >= requestStart)
      assertBool "responseEnd >= responseStart" (responseEnd >= responseStart)
      assertBool "responseEnd < 60000" (responseEnd < 60000)

timingApp :: Application
timingApp _request respond =
  respond (responseLBS status200 [] "Hello")

trim :: Text -> Text
trim =
  strip

dropOrigin :: Text -> Text
dropOrigin text =
  case breakOnEnd "/" text of
    (_, name) ->
      "/" <> name

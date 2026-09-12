module Playwright.BrowserContextFetchSpec
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
import Control.Exception
  ( try
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
  )
import Network.HTTP.Types
  ( hContentType
  , hLocation
  , status200
  , status302
  , status404
  )
import Network.Wai
  ( Application
  , pathInfo
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
  , Cookie (cookieName, cookieValue)
  , CookieName (cookieNameToText)
  , CookieValue (cookieValueToText)
  , FetchOptions (fetchFailOnStatusCode, fetchPostData, fetchTimeout)
  , FetchPostData (FetchPostText)
  , HeaderName (headerNameToText)
  , HeaderValue (headerValueToText)
  , HttpHeader (httpHeaderName, httpHeaderValue)
  , HttpStatus (httpStatusToInt)
  , Page
  , Timeout
  , Url
  , apiResponseBody
  , apiResponseDispose
  , apiResponseHeaders
  , apiResponseJson
  , apiResponseOk
  , apiResponseStatus
  , apiResponseStatusText
  , apiResponseText
  , apiResponseUrl
  , contextRequest
  , cookies
  , defaultFetchOptions
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluateText
  , fetchDeleteWith
  , fetchFromUrl
  , fetchGet
  , fetchGetWith
  , fetchHeadWith
  , fetchPatchWith
  , fetchPostWith
  , fetchPutWith
  , goto
  , milliseconds
  , urlFromText
  , urlToText
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure, ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( assertProtocolContains
  , testAssetsDir
  , withAssetsContextWith
  , withChromiumContext
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

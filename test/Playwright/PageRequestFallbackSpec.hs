module Playwright.PageRequestFallbackSpec
  ( tests
  ) where

import UnliftIO.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , takeMVar
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import UnliftIO.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.Text
  ( Text
  , pack
  , toLower
  )
import Data.Text.Encoding
  ( decodeUtf8
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , responseLBS
  )
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( ContinueOptions (continueHeaders, continueMethod, continuePostData, continueUrl)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillStatus)
  , HeaderName (HeaderName, headerNameToText)
  , HeaderValue (HeaderValue, headerValueToText)
  , HttpHeader (HttpHeader, httpHeaderName, httpHeaderValue)
  , HttpMethod (HttpMethod)
  , HttpStatus (HttpStatus)
  , Page
  , RouteOptions (routeTimes)
  , RouteTimes (RouteTimes)
  , UrlGlob (UrlGlob)
  , abortRoute
  , continueRoute
  , defaultContinueOptions
  , defaultFulfillOptions
  , defaultGotoOptions
  , defaultRouteOptions
  , evaluate
  , evaluateNumber
  , fallbackRoute
  , fallbackRouteWith
  , fulfillRoute
  , goto
  , requestHeaders
  , requestMethod
  , requestPostData
  , requestUrl
  , responseRequest
  , responseText
  , responseUrl
  , route
  , routeRequest
  , routeWith
  , urlFromText
  , urlToText
  , Playwright
  )
import Playwright.Harness
  ( assertProtocolContains
  , withAssets
  , withChromiumPage
  , assertEqual
  , assertFailure
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
    "page-request-fallback"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          route page (UrlGlob "**/*") fallbackRoute
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          pure ()
    , testCase "should fall back" $
        withAssets $ \prefix page -> do
          intercepted <- newIORef ([] :: [Int])
          route page (UrlGlob "**/empty.html") $ \handled -> do
            modifyIORef' intercepted (1 :)
            fallbackRoute handled
          route page (UrlGlob "**/empty.html") $ \handled -> do
            modifyIORef' intercepted (2 :)
            fallbackRoute handled
          route page (UrlGlob "**/empty.html") $ \handled -> do
            modifyIORef' intercepted (3 :)
            fallbackRoute handled
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "order" [3, 2, 1] . reverse =<< readIORef intercepted
    , testCase "should not chain fulfill" $
        withAssets $ \prefix page -> do
          failed <- newIORef False
          route page (UrlGlob "**/empty.html") $ \_ ->
            writeTrue failed
          route page (UrlGlob "**/empty.html") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus 200)
                , fulfillBody = Just (FulfillText "fulfilled")
                }
          route page (UrlGlob "**/empty.html") fallbackRoute
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "body" "fulfilled" =<< responseText value
          assertEqual "chained" False =<< readIORef failed
    , testCase "should not chain abort" $
        withAssets $ \prefix page -> do
          failed <- newIORef False
          route page (UrlGlob "**/empty.html") $ \_ ->
            writeTrue failed
          route page (UrlGlob "**/empty.html") abortRoute
          route page (UrlGlob "**/empty.html") fallbackRoute
          assertProtocolContains
            "net::ERR_FAILED"
            (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
          assertEqual "chained" False =<< readIORef failed
    , testCase "should chain once" $
        withAssets $ \prefix page -> do
          routeWith
            page
            (UrlGlob "**/empty.html")
            defaultRouteOptions {routeTimes = Just (RouteTimes 1)}
            $ \handled ->
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillStatus = Just (HttpStatus 200)
                  , fulfillBody = Just (FulfillText "fulfilled one")
                  }
          routeWith
            page
            (UrlGlob "**/empty.html")
            defaultRouteOptions {routeTimes = Just (RouteTimes 1)}
            fallbackRoute
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "body" "fulfilled one" =<< responseText value
    , testCase "should amend HTTP headers" $
        withCaptureServer $ \prefix captured page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          seen <- newIORef (Nothing :: Maybe Text)
          route page (UrlGlob "**/sleep.zzz") $ \handled -> do
            headers <- requestHeaders =<< routeRequest handled
            writeIORef seen (headerValue "foo" headers)
            continueRoute handled
          route page (UrlGlob "**/*") $ \handled -> do
            headers <- requestHeaders =<< routeRequest handled
            fallbackRouteWith
              handled
              defaultContinueOptions
                { continueHeaders =
                    Just (headers <> [HttpHeader (HeaderName "foo") (HeaderValue "bar")])
                }
          _ <- evaluate page "() => fetch('/sleep.zzz')"
          fromRoute <- readIORef seen
          fromServer <- capturedFoo <$> takeMVar captured
          assertEqual "route" (Just "bar") fromRoute
          assertEqual "server" (Just "bar") fromServer
    , testCase "should amend method" $
        withCaptureServer $ \prefix captured page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          seen <- newIORef (HttpMethod "")
          route page (UrlGlob "**/*") $ \handled -> do
            writeIORef seen =<< requestMethod =<< routeRequest handled
            continueRoute handled
          route page (UrlGlob "**/*") $ \handled ->
            fallbackRouteWith
              handled
              defaultContinueOptions {continueMethod = Just (HttpMethod "POST")}
          _ <- evaluate page "() => fetch('/sleep.zzz')"
          fromRoute <- readIORef seen
          fromServer <- capturedMethod <$> takeMVar captured
          assertEqual "route" (HttpMethod "POST") fromRoute
          assertEqual "server" "POST" fromServer
    , testCase "should amend post data" $
        withCaptureServer $ \prefix captured page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          seen <- newIORef (Nothing :: Maybe ByteString)
          route page (UrlGlob "**/*") $ \handled -> do
            writeIORef seen =<< requestPostData =<< routeRequest handled
            continueRoute handled
          route page (UrlGlob "**/*") $ \handled ->
            fallbackRouteWith
              handled
              defaultContinueOptions {continuePostData = Just (Char8.pack "doggo")}
          _ <- evaluate page "() => fetch('/sleep.zzz', { method: 'POST', body: 'birdy' })"
          fromRoute <- readIORef seen
          fromServer <- capturedBody <$> takeMVar captured
          assertEqual "route" (Just (Char8.pack "doggo")) fromRoute
          assertEqual "server" (Char8.pack "doggo") fromServer
    , testCase "should override request url" $
        withAssets $ \prefix page -> do
          seen <- newIORef (Nothing :: Maybe Text)
          route page (UrlGlob "**/global-var.html") $ \handled -> do
            writeIORef seen . Just . urlToText =<< requestUrl =<< routeRequest handled
            continueRoute handled
          route page (UrlGlob "**/foo") $ \handled ->
            fallbackRouteWith
              handled
              defaultContinueOptions {continueUrl = Just (urlFromText (prefix <> "/global-var.html"))}
          response <- goto page (urlFromText (prefix <> "/foo")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              request <- responseRequest value
              assertEqual "response" (prefix <> "/global-var.html") . urlToText =<< responseUrl value
              assertEqual "request" (prefix <> "/global-var.html") . urlToText =<< requestUrl request
          assertEqual "handler" (Just (prefix <> "/global-var.html")) =<< readIORef seen
          assertEqual "globalVar" (123 :: Double) =<< evaluateNumber page "() => window['globalVar']"
    ]

writeTrue :: IORef Bool -> Playwright ()
writeTrue ref =
  writeIORef ref True

data Captured = Captured
  { capturedBody :: ByteString
  , capturedFoo :: Maybe Text
  , capturedMethod :: Text
  }

withCaptureServer :: (Text -> MVar Captured -> Page -> Playwright a) -> IO a
withCaptureServer action = do
  captured <- newEmptyMVar
  withApplication (pure (captureSleep captured)) $ \port ->
    withChromiumPage $ \page ->
      action ("http://127.0.0.1:" <> pack (show port)) captured page

captureSleep :: MVar Captured -> Application
captureSleep captured req respond = do
  let
    path = Wai.rawPathInfo req
  if path == "/sleep.zzz"
    then do
      body <- fmap LBS.toStrict (Wai.strictRequestBody req)
      putMVar
        captured
        Captured
          { capturedBody = body
          , capturedFoo = fmap decodeUtf8 (lookup "foo" (Wai.requestHeaders req))
          , capturedMethod = decodeUtf8 (Wai.requestMethod req)
          }
    else
      pure ()
  respond
    ( responseLBS
        status200
        [(hContentType, Char8.pack "text/html")]
        "<html><body></body></html>"
    )

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

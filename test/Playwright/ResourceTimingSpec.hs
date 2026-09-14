module Playwright.ResourceTimingSpec
  ( tests
  ) where

import Control.Monad
  ( filterM
  )
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text
  ( isInfixOf
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
  )
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , staticApp
  )
import Playwright
  ( Playwright
  , Request
  , ResourceTiming (resourceTimingConnectEnd, resourceTimingConnectStart, resourceTimingDomainLookupEnd, resourceTimingDomainLookupStart, resourceTimingRequestStart, resourceTimingResponseEnd, resourceTimingResponseStart, resourceTimingSecureConnectionStart)
  , Response
  , UrlGlob (UrlGlob)
  , defaultGotoOptions
  , goto
  , onRequestFinished
  , onResponse
  , reload
  , requestTiming
  , requestUrl
  , responseFinished
  , responseRequest
  , responseUrl
  , urlFromText
  , urlToText
  , waitForRequestFinished
  , waitForResponse
  )
import Playwright.Harness
  ( testAssetsDir
  , withAssets
  , withLocalApp
  , withChromiumPage
  , assertBool
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
import UnliftIO.Async
  ( wait
  , withAsync
  )
import UnliftIO.IORef
  ( modifyIORef'
  , newIORef
  , readIORef
  )

tests :: TestTree
tests =
  testGroup
    "resource-timing"
    [ testCase "should work" $
        withAssets $ \prefix page ->
          withAsync (waitForRequestFinished page) $ \pending -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            request <- wait pending
            timing <- requestTiming request
            verifyConnectionTimingConsistency timing
            assertBool "requestStart" (resourceTimingRequestStart timing >= resourceTimingConnectEnd timing)
            assertBool "responseStart" (resourceTimingResponseStart timing >= resourceTimingRequestStart timing)
            assertBool "responseEnd" (resourceTimingResponseEnd timing >= resourceTimingResponseStart timing)
            assertBool "fast" (resourceTimingResponseEnd timing < 10000)
    , testCase "should work for subresource" $
        withAssets $ \prefix page -> do
          seen <- newIORef ([] :: [Request])
          onRequestFinished page $ \request -> modifyIORef' seen (<> [request])
          _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
          requests <- readIORef seen
          stylesheets <- filterM isStylesheet requests
          case stylesheets of
            stylesheet : _ -> do
              timing <- requestTiming stylesheet
              verifyConnectionTimingConsistency timing
              assertBool "requestStart" (resourceTimingRequestStart timing >= 0)
              assertBool "responseStart" (resourceTimingResponseStart timing > resourceTimingRequestStart timing)
              assertBool "responseEnd" (resourceTimingResponseEnd timing >= resourceTimingResponseStart timing)
              assertBool "fast" (resourceTimingResponseEnd timing < 10000)
            [] ->
              assertFailure ("expected stylesheet, got " <> show (length requests))
    , testCase "should work for redirect" $
        withChromiumPage $ \page -> do
          assets <- testAssetsDir
          withLocalApp (redirectApp assets) $ \prefix -> do
            seen <- newIORef ([] :: [Response])
            onResponse page $ \response -> modifyIORef' seen (<> [response])
            _ <- goto page (urlFromText (prefix <> "/foo.html")) defaultGotoOptions
            responses <- filterM isRedirectNav =<< readIORef seen
            mapM_ responseFinished responses
            case responses of
              [firstResponse, secondResponse] -> do
                assertEqual "from" (prefix <> "/foo.html") . urlToText =<< responseUrl firstResponse
                assertEqual "to" (prefix <> "/empty.html") . urlToText =<< responseUrl secondResponse
                timing1 <- requestTiming =<< responseRequest firstResponse
                verifyConnectionTimingConsistency timing1
                assertBool "requestStart" (resourceTimingRequestStart timing1 >= resourceTimingConnectEnd timing1)
                assertBool "responseStart" (resourceTimingResponseStart timing1 > resourceTimingRequestStart timing1)
                assertBool "responseEnd" (resourceTimingResponseEnd timing1 >= resourceTimingResponseStart timing1)
                assertBool "fast" (resourceTimingResponseEnd timing1 < 10000)
                timing2 <- requestTiming =<< responseRequest secondResponse
                verifyConnectionTimingConsistency timing2
                assertBool "requestStart2" (resourceTimingRequestStart timing2 >= 0)
                assertBool "responseStart2" (resourceTimingResponseStart timing2 > resourceTimingRequestStart timing2)
                assertBool "responseEnd2" (resourceTimingResponseEnd timing2 >= resourceTimingResponseStart timing2)
                assertBool "fast2" (resourceTimingResponseEnd timing2 < 10000)
              _ ->
                assertFailure ("expected 2 responses, got " <> show (length responses))
    , testCase "should work when serving from memory cache" $
        withChromiumPage $ \page -> do
          assets <- testAssetsDir
          withLocalApp (cachedCssApp assets) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
            withAsync (waitForResponse page (UrlGlob "**/one-style.css")) $ \pending -> do
              _ <- reload page
              response <- wait pending
              responseFinished response
              timing <- requestTiming =<< responseRequest response
              verifyConnectionTimingConsistency timing
              assertEqual "cached" (resourceTimingResponseStart timing) (resourceTimingResponseEnd timing)
              assertBool "fast" (resourceTimingResponseEnd timing < 1000)
    ]

verifyConnectionTimingConsistency :: ResourceTiming -> Playwright ()
verifyConnectionTimingConsistency timing = do
  verifyTimingValue (resourceTimingDomainLookupStart timing) (-1)
  verifyTimingValue (resourceTimingDomainLookupEnd timing) (resourceTimingDomainLookupStart timing)
  verifyTimingValue (resourceTimingConnectStart timing) (resourceTimingDomainLookupEnd timing)
  verifyTimingValue (resourceTimingSecureConnectionStart timing) (resourceTimingConnectStart timing)
  verifyTimingValue (resourceTimingConnectEnd timing) (resourceTimingSecureConnectionStart timing)

verifyTimingValue :: (Ord a, Num a) => a -> a -> Playwright ()
verifyTimingValue value previous =
  assertBool
    "timing order"
    (value == -1 || (value > 0 && value >= previous))

isStylesheet :: Request -> Playwright Bool
isStylesheet request = do
  url <- requestUrl request
  pure ("one-style.css" `isInfixOf` urlToText url)

isRedirectNav :: Response -> Playwright Bool
isRedirectNav response = do
  url <- responseUrl response
  let
    text = urlToText url
  pure ("foo.html" `isInfixOf` text || "empty.html" `isInfixOf` text)

redirectApp :: FilePath -> Application
redirectApp assets request respond =
  case pathInfo request of
    ["foo.html"] ->
      respond
        ( responseLBS
            status302
            [(hLocation, "/empty.html")]
            LBS8.empty
        )
    _ ->
      staticApp (defaultFileServerSettings assets) request respond

cachedCssApp :: FilePath -> Application
cachedCssApp assets request respond =
  case pathInfo request of
    ["one-style.css"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/css"), ("Cache-Control", "public, max-age=10031518")]
            (LBS8.pack "body { background: red }")
        )
    _ ->
      staticApp (defaultFileServerSettings assets) request respond

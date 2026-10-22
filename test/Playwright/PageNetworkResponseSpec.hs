module Playwright.PageNetworkResponseSpec
  ( tests
  ) where

import Codec.Compression.GZip
  ( compress
  )
import Control.Monad
  ( forM_
  )
import Data.Aeson
  ( object
  , (.=)
  )
import Data.ByteString.Builder
  ( lazyByteString
  )
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe
  ( listToMaybe
  )
import Data.Text
  ( Text
  , isInfixOf
  , toLower
  )
import Network.HTTP.Types
  ( hContentEncoding
  , hContentType
  , hLocation
  , mkStatus
  , status200
  , status302
  , status404
  )
import Network.Wai
  ( Application
  , Response
  , pathInfo
  , responseLBS
  , responseStream
  )
import Playwright
  ( HeaderName (headerNameToText)
  , HeaderValue (headerValueToText)
  , HttpHeader (httpHeaderName, httpHeaderValue)
  , HttpStatus (HttpStatus)
  , Playwright
  , UrlMatch (UrlMatchGlob)
  , armResponse
  , continueRoute
  , defaultGotoOptions
  , evaluate
  , evaluateOnFrame
  , frameByUrl
  , goto
  , onRequestFinished
  , requestRedirectedFrom
  , requestResponse
  , requestUrl
  , responseAllHeaders
  , responseBody
  , responseHeaders
  , responseJSON
  , responseRequest
  , responseStatus
  , responseStatusText
  , responseText
  , route
  , urlFromText
  , urlToText
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( withAssets
  , withChromiumPage
  , withLocalApp
  , assertBool
  , assertEqual
  , assertFailure
  , readTestAsset
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
import UnliftIO.Exception
  ( try
  )
import UnliftIO.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import UnliftIO.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , takeMVar
  )

tests :: TestTree
tests =
  testGroup
    "page-network-response"
    [ testCase "should return text" $
        withAssets $ \prefix page -> do
          response <- goto page (urlFromText (prefix <> "/simple.json")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "body" "{\"foo\": \"bar\"}\n" =<< responseText value
    , testCase "should work @smoke" $
        withChromiumPage $ \page ->
          withLocalApp headerApp $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                headers <- responseHeaders value
                assertEqual "foo" (Just "bar") (headerValue "foo" headers)
                assertEqual "baz" (Just "bAz") (headerValue "baz" headers)
    , testCase "should return uncompressed text" $
        withChromiumPage $ \page ->
          withLocalApp gzipApp $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/simple.json")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value ->
                -- responseHeaders does not list content-encoding (see TODO), so check the decoded body.
                assertEqual "body" "{\"foo\": \"bar\"}\n" =<< responseText value
    , testCase "allHeaders should include content-encoding" $
        withChromiumPage $ \page ->
          withLocalApp gzipApp $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/simple.json")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                headers <- responseAllHeaders value
                assertEqual "content-encoding" (Just "gzip") (headerValue "content-encoding" headers)
    , testCase "allHeaders should lowercase header names" $
        withChromiumPage $ \page ->
          withLocalApp headerApp $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                headers <- responseAllHeaders value
                assertEqual "baz" (Just "bAz") (headerValue "baz" headers)
                assertEqual
                  "uppercase names"
                  []
                  [ name
                  | header <- headers
                  , let name = headerNameToText (httpHeaderName header)
                  , name /= toLower name
                  ]
    , testCase "should return status text" $
        withChromiumPage $ \page ->
          withLocalApp coolApp $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/cool")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value ->
                assertEqual "status text" "cool!" =<< responseStatusText value
    , testCase "should return json" $
        withAssets $ \prefix page -> do
          response <- goto page (urlFromText (prefix <> "/simple.json")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "json" (object ["foo" .= ("bar" :: Text)]) =<< responseJSON value
    , testCase "should return body" $
        withAssets $ \prefix page -> do
          response <- goto page (urlFromText (prefix <> "/pptr.png")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              imageBuffer <- readTestAsset "pptr.png"
              assertEqual "body" imageBuffer =<< responseBody value
    , testCase "should return body with compression" $ do
        imageBuffer <- readTestAsset "pptr.png"
        withChromiumPage $ \page ->
          withLocalApp (gzipPngApp (LBS.fromStrict imageBuffer)) $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/pptr.png")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value ->
                assertEqual "body" imageBuffer =<< responseBody value
    , testCase "should throw when requesting body of redirected response" $
        withChromiumPage $ \page ->
          withLocalApp redirectApp $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/foo.html")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                request <- responseRequest value
                previous <- requestRedirectedFrom request
                redirected <-
                  case previous of
                    Nothing ->
                      assertFailure "missing redirected request"
                    Just previousRequest ->
                      requestResponse previousRequest
                case redirected of
                  Nothing ->
                    assertFailure "missing redirect response"
                  Just redirect -> do
                    assertEqual "status" (HttpStatus 302) =<< responseStatus redirect
                    result <- try (responseText redirect) :: Playwright (Either PlaywrightError Text)
                    case result of
                      Left (ProtocolRpc failure) ->
                        assertBool
                          "redirect body error"
                          ("Response body is unavailable for redirect responses" `isInfixOf` protocolFailureMessage failure)
                      Left err ->
                        assertFailure ("expected ProtocolRpc, got: " <> show err)
                      Right _ ->
                        assertFailure "expected the redirect body request to fail"
    , testCase "should wait until response completes" $
        withChromiumPage $ \page -> do
          hit <- newEmptyMVar
          release <- newEmptyMVar
          requestFinished <- newIORef False
          withLocalApp (streamApp hit release) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            onRequestFinished page $ \request -> do
              url <- requestUrl request
              if "/get" `isInfixOf` urlToText url
                then writeIORef requestFinished True
                else pure ()
            takeResponse <- armResponse page (UrlMatchGlob (prefix <> "/get"))
            withAsync (evaluate page "() => fetch('./get', { method: 'GET' })") $ \fetching -> do
              takeMVar hit
              response <- takeResponse
              assertEqual "status" (HttpStatus 200) =<< responseStatus response
              assertEqual "requestfinished before the body completes" False =<< readIORef requestFinished
              withAsync (responseText response) $ \text -> do
                putMVar release ()
                assertEqual "body" "hello world!" =<< wait text
              _ <- wait fetching
              pure ()
    , testCase "should return text for identity encoding" $
        withChromiumPage $ \page ->
          withLocalApp identityApp $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/identity.html")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value ->
                assertEqual "body" "<div>hello</div>" =<< responseText value
    , testCase "should bypass disk cache when page interception is enabled" $
        withChromiumPage $ \page -> do
          apiHits <- newIORef (0 :: Int)
          frameHits <- newIORef (0 :: Int)
          withLocalApp (cacheApp apiHits frameHits) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/frames/one-frame.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/api*") continueRoute
            forM_ [1 .. 3 :: Int] $ \expected -> do
              takeResponse <- armResponse page (UrlMatchGlob "**/api")
              _ <- evaluate page "async () => (await fetch('/api')).status"
              response <- takeResponse
              assertEqual "main frame status" (HttpStatus 200) =<< responseStatus response
              assertEqual "main frame requests" expected =<< readIORef apiHits
            found <- frameByUrl page (UrlMatchGlob "**/frame.html")
            case found of
              Nothing ->
                assertFailure "missing frame"
              Just frame ->
                forM_ [1 .. 3 :: Int] $ \expected -> do
                  takeResponse <- armResponse page (UrlMatchGlob "**/frame/api")
                  _ <- evaluateOnFrame frame "async () => (await fetch('/frame/api')).status"
                  response <- takeResponse
                  assertEqual "subframe status" (HttpStatus 200) =<< responseStatus response
                  assertEqual "subframe requests" expected =<< readIORef frameHits
    ]

headerValue :: Text -> [HttpHeader] -> Maybe Text
headerValue name headers =
  listToMaybe
    [ headerValueToText (httpHeaderValue header)
    | header <- headers
    , toLower (headerNameToText (httpHeaderName header)) == toLower name
    ]

headerApp :: Application
headerApp _ respond =
  respond (responseLBS status200 [("foo", "bar"), ("BaZ", "bAz")] LBS8.empty)

gzipApp :: Application
gzipApp request respond =
  case pathInfo request of
    ["simple.json"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "application/json"), (hContentEncoding, "gzip")]
            (compress (LBS8.pack "{\"foo\": \"bar\"}\n"))
        )
    _ ->
      respond (responseLBS status404 [] LBS8.empty)

redirectApp :: Application
redirectApp request respond =
  case pathInfo request of
    ["foo.html"] ->
      respond (responseLBS status302 [(hLocation, "/empty.html")] LBS8.empty)
    ["empty.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] LBS8.empty)
    _ ->
      respond (responseLBS status404 [] LBS8.empty)

-- | Streams "hello ", then waits for the test before sending the rest.
streamApp :: MVar () -> MVar () -> Application
streamApp hit release request respond =
  case pathInfo request of
    ["get"] ->
      respond
        ( responseStream
            status200
            [(hContentType, "text/plain; charset=utf-8")]
            ( \write flush -> do
                write (lazyByteString (LBS8.pack "hello "))
                flush
                putMVar hit ()
                takeMVar release
                write (lazyByteString (LBS8.pack "wor"))
                flush
                write (lazyByteString (LBS8.pack "ld!"))
                flush
            )
        )
    ["empty.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] LBS8.empty)
    _ ->
      respond (responseLBS status404 [] LBS8.empty)

-- | Serves the two counted API routes with a cacheable response, plus the frame page.
cacheApp :: IORef Int -> IORef Int -> Application
cacheApp apiHits frameHits request respond =
  case pathInfo request of
    ["api"] -> do
      modifyIORef' apiHits (+ 1)
      respond cachedHello
    ["frame", "api"] -> do
      modifyIORef' frameHits (+ 1)
      respond cachedHello
    ["frames", "one-frame.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] (LBS8.pack "<iframe src='./frame.html'></iframe>"))
    ["frames", "frame.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] LBS8.empty)
    _ ->
      respond (responseLBS status404 [] LBS8.empty)
  where
    cachedHello :: Response
    cachedHello =
      responseLBS
        status200
        [(hContentType, "text/plain"), ("cache-control", "public, max-age=31536000")]
        (LBS8.pack "Hello")

coolApp :: Application
coolApp _ respond =
  respond (responseLBS (mkStatus 200 "cool!") [] LBS8.empty)

gzipPngApp :: LBS.ByteString -> Application
gzipPngApp image request respond =
  case pathInfo request of
    ["pptr.png"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "image/png"), (hContentEncoding, "gzip")]
            (compress image)
        )
    _ ->
      respond (responseLBS status404 [] LBS8.empty)

identityApp :: Application
identityApp request respond =
  case pathInfo request of
    ["identity.html"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/html; charset=utf-8"), ("Content-Encoding", "Identity")]
            (LBS8.pack "<div>hello</div>")
        )
    _ ->
      respond (responseLBS status404 [] LBS8.empty)

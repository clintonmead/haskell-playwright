module Playwright.PageNetworkSizesSpec
  ( tests
  ) where

import Codec.Compression.GZip
  ( compress
  )
import Control.Monad
  ( void
  , when
  )
import UnliftIO.Async
  ( race
  , wait
  , withAsync
  )
import UnliftIO.Concurrent
  ( threadDelay
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , takeMVar
  , tryPutMVar
  )
import UnliftIO.Exception
  ( try
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text
  ( Text
  , isInfixOf
  )
import Numeric
  ( showHex
  )
import Network.HTTP.Types
  ( Status
  , hContentEncoding
  , hContentLength
  , hContentType
  , hLocation
  , methodGet
  , methodPost
  , status200
  , status302
  , status401
  , status404
  , status500
  )
import Network.Wai
  ( Application
  , Response
  , pathInfo
  , requestMethod
  , responseLBS
  , responseRaw
  )
import Playwright
  ( BodySize (bodySizeToInt)
  , HeadersSize (headersSizeToInt)
  , HttpStatus (HttpStatus)
  , NetworkSizes (networkRequestBodySize, networkRequestHeadersSize, networkResponseBodySize, networkResponseHeadersSize)
  , Page
  , Request
  , defaultGotoOptions
  , evaluate
  , goto
  , onRequest
  , requestRedirectedTo
  , requestResponse
  , requestSizes
  , requestUrl
  , responseRequest
  , responseStatus
  , urlFromText
  , urlToText
  , waitForRequestFailed
  , Playwright
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  )
import Playwright.Harness
  ( readTestAsset
  , withAssets
  , withChromiumPage
  , withLocalApp
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

tests :: TestTree
tests =
  testGroup
    "page-network-sizes"
    [ testCase "should set bodySize and headersSize" $
        withServerPage emptyGetApp $ \prefix page -> do
          awaitRequest <- requestTo page (prefix <> "/get")
          withAsync awaitRequest $ \pending -> do
            _ <-
              evaluate
                page
                "() => fetch('./get', { method: 'POST', body: '12345' }).then(r => r.text())"
            request <- wait pending
            sizes <- requestSizes request
            assertEqual "body" 5 (bodySizeToInt (networkRequestBodySize sizes))
            assertBool "headers" (headersSizeToInt (networkRequestHeadersSize sizes) >= 250)
    , testCase "should set bodySize to 0 if there was no body" $
        withServerPage emptyGetApp $ \prefix page -> do
          awaitRequest <- requestTo page (prefix <> "/get")
          withAsync awaitRequest $ \pending -> do
            _ <- evaluate page "() => fetch('./get').then(r => r.text())"
            request <- wait pending
            sizes <- requestSizes request
            assertEqual "body" 0 (bodySizeToInt (networkRequestBodySize sizes))
            assertBool "headers" (headersSizeToInt (networkRequestHeadersSize sizes) >= 190)
    , testCase "should set bodySize, headersSize, and transferSize" $
        withServerPage emptyGetApp $ \prefix page -> do
          awaitRequest <- requestTo page (prefix <> "/get")
          withAsync awaitRequest $ \pending -> do
            _ <- evaluate page "() => fetch('./get').then(r => r.text())"
            request <- wait pending
            sizes <- requestSizes request
            assertEqual "response body" 6 (bodySizeToInt (networkResponseBodySize sizes))
            assertBool "response headers" (headersSizeToInt (networkResponseHeadersSize sizes) >= 100)
    , testCase "should set bodySize to 0 when there was no response body" $
        withAssets $ \prefix page -> do
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              request <- responseRequest value
              sizes <- requestSizes request
              assertEqual "response body" 0 (bodySizeToInt (networkResponseBodySize sizes))
              assertBool "response headers" (headersSizeToInt (networkResponseHeadersSize sizes) >= 150)
    , testCase "should have the correct responseBodySize" $
        withAssets $ \prefix page -> do
          asset <- readTestAsset "simplezip.json"
          response <- goto page (urlFromText (prefix <> "/simplezip.json")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              request <- responseRequest value
              sizes <- requestSizes request
              assertEqual "file size" (ByteString.length asset) (bodySizeToInt (networkResponseBodySize sizes))
    , testCase "should have the correct responseBodySize for chunked request" $
        withChromiumPage $ \page -> do
          asset <- readTestAsset "simplezip.json"
          withLocalApp (chunkedApp asset) $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/chunked-simplezip.json")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                request <- responseRequest value
                sizes <- requestSizes request
                assertEqual "chunked" 5175 (bodySizeToInt (networkResponseBodySize sizes))
    , testCase "should have the correct responseBodySize with gzip compression" $
        withChromiumPage $ \page -> do
          asset <- readTestAsset "simplezip.json"
          let compressed = compress (LBS.fromStrict asset)
          withLocalApp (gzipApp compressed) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            awaitRequest <- requestTo page (prefix <> "/simplezip.json")
            withAsync awaitRequest $ \pending -> do
              _ <- evaluate page "() => fetch('./simplezip.json').then(r => r.text())"
              request <- wait pending
              sizes <- requestSizes request
              assertEqual
                "gzip"
                (fromIntegral (LBS.length compressed) :: Int)
                (bodySizeToInt (networkResponseBodySize sizes))
    , testCase "should handle redirects" $
        withServerPage redirectApp $ \prefix page -> do
          awaitRequest <- requestTo page (prefix <> "/foo")
          withAsync awaitRequest $ \pending -> do
            _ <-
              evaluate
                page
                "() => fetch('/foo', { method: 'POST', body: '12345' }).then(r => r.text())"
            request <- wait pending
            sizes <- requestSizes request
            assertEqual "redirect body" 5 (bodySizeToInt (networkRequestBodySize sizes))
            redirected <- requestRedirectedTo request
            case redirected of
              Nothing ->
                assertFailure "missing redirectedTo"
              Just nextRequest -> do
                nextSizes <- requestSizes nextRequest
                assertEqual "bar" 3 (bodySizeToInt (networkResponseBodySize nextSizes))
    , testCase "should throw for failed requests" $
        withChromiumPage $ \page -> do
          html <- readTestAsset "one-style.html"
          withLocalApp (failedCssApp html) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            withAsync (waitForRequestFailed page) $ \pending -> do
              _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
              request <- wait pending
              result <- try (requestSizes request)
              case result of
                Left (DecodeFailure message) ->
                  assertBool
                    "failed sizes"
                    ("Unable to fetch sizes for failed request" `isInfixOf` message)
                Left err ->
                  assertFailure ("expected DecodeFailure, got: " <> show err)
                Right _ ->
                  assertFailure "expected sizes() to fail"
    , statusCodeTest 200 status200
    , statusCodeTest 401 status401
    , statusCodeTest 404 status404
    , statusCodeTest 500 status500
    ]

statusCodeTest :: Int -> Status -> TestTree
statusCodeTest code status =
  testCase ("should work with " <> show code <> " status code") $
    withServerPage (statusApp status) $ \prefix page -> do
      awaitRequest <- requestTo page (prefix <> "/foo")
      withAsync awaitRequest $ \pending -> do
        _ <-
          evaluate
            page
            "() => fetch('/foo', { method: 'POST', body: '12345' }).then(r => r.text())"
        request <- wait pending
        maybeResponse <- requestResponse request
        case maybeResponse of
          Nothing ->
            assertFailure "missing response"
          Just response ->
            assertEqual "status" (HttpStatus code) =<< responseStatus response
        sizes <- requestSizes request
        assertEqual "request body" 5 (bodySizeToInt (networkRequestBodySize sizes))
        assertEqual "response body" 3 (bodySizeToInt (networkResponseBodySize sizes))

-- | Collects the first request to an exact URL. Call it before the action
-- that makes the request: 'waitForRequest' subscribes on a forked thread, so a
-- request that fires first is missed. The returned action waits at most 30 s.
requestTo :: Page -> Text -> Playwright (Playwright Request)
requestTo page url = do
  found <- newEmptyMVar
  onRequest page $ \request -> do
    current <- urlToText <$> requestUrl request
    when (current == url) (void (tryPutMVar found request))
  pure $ do
    outcome <- race (threadDelay 30000000) (takeMVar found)
    case outcome of
      Left () ->
        assertFailure ("timed out waiting for request to " <> show url)
      Right request ->
        pure request

withServerPage :: Application -> (Text -> Page -> Playwright a) -> IO a
withServerPage app action =
  withChromiumPage $ \page ->
    withLocalApp app $ \prefix -> do
      _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
      action prefix page

emptyHtmlResponse :: Response
emptyHtmlResponse =
  responseLBS status200 [(hContentType, "text/html")] ""

emptyGetApp :: Application
emptyGetApp request respond =
  case pathInfo request of
    ["empty.html"] ->
      respond emptyHtmlResponse
    ["get"]
      | requestMethod request == methodPost ->
          respond (plainResponse (LBS8.pack "ok"))
      | requestMethod request == methodGet ->
          respond (plainResponse (LBS8.pack "abc134"))
    _ ->
      respond (responseLBS status404 [] "")

statusApp :: Status -> Application
statusApp status request respond =
  case pathInfo request of
    ["empty.html"] ->
      respond emptyHtmlResponse
    ["foo"]
      | requestMethod request == methodPost ->
          respond
            ( responseLBS
                status
                [ (hContentType, "text/plain; charset=utf-8")
                , (hContentLength, "3")
                ]
                (LBS8.pack "bar")
            )
    _ ->
      respond (responseLBS status404 [] "")

redirectApp :: Application
redirectApp request respond =
  case pathInfo request of
    ["empty.html"] ->
      respond emptyHtmlResponse
    ["foo"] ->
      respond (responseLBS status302 [(hLocation, "/bar")] "")
    ["bar"] ->
      respond (plainResponse (LBS8.pack "bar"))
    _ ->
      respond (responseLBS status404 [] "")

gzipApp :: LBS.ByteString -> Application
gzipApp compressed request respond =
  case pathInfo request of
    ["empty.html"] ->
      respond emptyHtmlResponse
    ["simplezip.json"] ->
      respond
        ( responseLBS
            status200
            [ (hContentType, "application/json")
            , (hContentEncoding, "gzip")
            , (hContentLength, ByteString.Char8.pack (show (LBS.length compressed)))
            ]
            compressed
        )
    _ ->
      respond (responseLBS status404 [] "")

failedCssApp :: ByteString -> Application
failedCssApp html request respond =
  case pathInfo request of
    ["empty.html"] ->
      respond emptyHtmlResponse
    ["one-style.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] (LBS.fromStrict html))
    ["one-style.css"] ->
      respond (responseRaw (\_ _ -> pure ()) (responseLBS status500 [] ""))
    _ ->
      respond (responseLBS status404 [] "")

chunkedApp :: ByteString -> Application
chunkedApp content request respond =
  case pathInfo request of
    ["chunked-simplezip.json"] ->
      respond
        ( responseRaw
            ( \_ sink -> do
                sink
                  "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\nTransfer-Encoding: chunked\r\n\r\n"
                writeChunks sink content ((ByteString.length content + 9) `div` 10)
                sink "0\r\n\r\n"
            )
            (responseLBS status500 [] "")
        )
    _ ->
      respond (responseLBS status404 [] "")

writeChunks :: (ByteString -> IO ()) -> ByteString -> Int -> IO ()
writeChunks sink content chunkSize
  | ByteString.null content || chunkSize <= 0 =
      pure ()
  | otherwise = do
      let
        (chunk, rest) = ByteString.splitAt chunkSize content
      sink (chunkFrame chunk)
      writeChunks sink rest chunkSize

chunkFrame :: ByteString -> ByteString
chunkFrame chunk =
  ByteString.Char8.pack (showHex (ByteString.length chunk) "")
    <> "\r\n"
    <> chunk
    <> "\r\n"

plainResponse :: LBS8.ByteString -> Response
plainResponse body =
  responseLBS
    status200
    [ (hContentType, "text/plain; charset=utf-8")
    , (hContentLength, ByteString.Char8.pack (show (LBS8.length body)))
    ]
    body

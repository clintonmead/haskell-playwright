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
import Data.List
  ( sort
  )
import Data.Maybe
  ( isJust
  , listToMaybe
  )
import Data.Text
  ( Text
  , isInfixOf
  , isSuffixOf
  , pack
  , toLower
  )
import Network.HTTP.Types
  ( Header
  , Status
  , hContentEncoding
  , hContentLength
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
  ( ContentType (ContentType)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType, fulfillHeaders, fulfillStatus)
  , HeaderName (HeaderName, headerNameToText)
  , HeaderValue (HeaderValue, headerValueToText)
  , HttpHeader (HttpHeader, httpHeaderName, httpHeaderValue)
  , HttpStatus (HttpStatus)
  , Playwright
  , UrlMatch (UrlMatchGlob)
  , armResponse
  , armResponseWhere
  , continueRoute
  , defaultFulfillOptions
  , defaultGotoOptions
  , evaluate
  , evaluateOnFrame
  , evaluateText
  , frameByUrl
  , fulfillRoute
  , goto
  , onRequestFinished
  , requestFrame
  , requestRedirectedFrom
  , requestResponse
  , requestUrl
  , responseAllHeaders
  , responseBody
  , responseFromServiceWorker
  , responseHeaderValue
  , responseHeaderValues
  , responseHeaders
  , responseHeadersArray
  , responseJSON
  , responseOk
  , responseRequest
  , responseStatus
  , responseStatusText
  , responseText
  , responseUrl
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
  , testAssetsDir
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
  ( SomeException
  , try
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
    , testCase "should return headers after route.fulfill" $
        withChromiumPage $ \page -> do
          route page (UrlMatchGlob "**/*") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus 200)
                , fulfillHeaders =
                    [ HttpHeader (HeaderName "foo") (HeaderValue "bar")
                    , HttpHeader (HeaderName "content-language") (HeaderValue "en")
                    ]
                , fulfillContentType = Just (ContentType "text/plain")
                , fulfillBody = Just (FulfillText "done")
                }
          response <- goto page (urlFromText "http://playwright.test/empty.html") defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              headers <- responseAllHeaders value
              assertEqual
                "all headers"
                [ ("content-language", "en")
                , ("content-length", "4")
                , ("content-type", "text/plain")
                , ("foo", "bar")
                ]
                (sortedHeaders headers)
    , testCase "should report if request was fromServiceWorker" $
        withAssets $ \prefix page -> do
          navigation <- goto page (urlFromText (prefix <> "/serviceworkers/fetch/sw.html")) defaultGotoOptions
          case navigation of
            Nothing ->
              assertFailure "missing navigation response"
            Just value ->
              assertEqual "navigation from worker" False =<< responseFromServiceWorker value
          _ <- evaluate page "async () => { await window.activationPromise; }"
          -- The worker makes its own fetch of the same URL, and that request has no
          -- frame. Only the page's request, which the worker answers, has one.
          takeResponse <- armResponseWhere page $ \response -> do
            url <- responseUrl response
            request <- responseRequest response
            frame <- requestFrame request
            pure (isSuffixOf "/example.txt" (urlToText url) && isJust frame)
          _ <- evaluate page "async () => (await fetch('/example.txt')).status"
          response <- takeResponse
          assertEqual "fetch from worker" True =<< responseFromServiceWorker response
    , testCase "should return body for prefetch script" $
        withChromiumPage $ \page ->
          withLocalApp prefetchApp $ \prefix -> do
            takeResponse <- armResponse page (UrlMatchGlob "**/prefetch.js")
            _ <- goto page (urlFromText (prefix <> "/prefetch.html")) defaultGotoOptions
            response <- takeResponse
            assertEqual "body" "// Scripts will be pre-fetched" =<< responseText response
    , testCase "should return body for image with evicted body" $
        withChromiumPage $ \page ->
          withLocalApp evictedImageApp $ \prefix -> do
            takeResponse <- armResponse page (UrlMatchGlob "**/pixel.gif")
            _ <- goto page (urlFromText (prefix <> "/page.html")) defaultGotoOptions
            response <- takeResponse
            assertEqual "body" (LBS.toStrict truncatedGif) =<< responseBody response
    , testCase "should report all headers" $
        withChromiumPage $ \page ->
          withLocalApp
            ( headersPathApp
                [ ("header-a", "value-a")
                , ("header-b", "value-b")
                , ("header-a", "value-a-1")
                , ("header-a", "value-a-2")
                ]
            )
            $ \prefix -> do
              takeResponse <- armResponse page (UrlMatchGlob "**/headers")
              _ <- goto page (urlFromText (prefix <> "/headers")) defaultGotoOptions
              response <- takeResponse
              headers <- responseHeadersArray response
              -- Warp also sends Date and Content-Length, so keep only the test headers.
              let testHeaders =
                    [ header
                    | header <- headers
                    , toLower (headerNameToText (httpHeaderName header)) `elem` ["header-a", "header-b"]
                    ]
              assertEqual
                "header lines"
                (sort [("header-a", "value-a"), ("header-a", "value-a-1"), ("header-a", "value-a-2"), ("header-b", "value-b")])
                (sortedHeaders testHeaders)
    , testCase "should report multiple set-cookie headers" $
        withChromiumPage $ \page ->
          withLocalApp (headersPathApp [("Set-Cookie", "a=b"), ("Set-Cookie", "c=d")]) $ \prefix -> do
            takeResponse <- armResponse page (UrlMatchGlob "**/headers")
            _ <- goto page (urlFromText (prefix <> "/headers")) defaultGotoOptions
            response <- takeResponse
            assertEqual "set-cookie lines" ["a=b", "c=d"] =<< responseHeaderValues response (HeaderName "set-cookie")
            assertEqual "merged set-cookie" (Just "a=b\nc=d") =<< responseHeaderValue response (HeaderName "set-cookie")
            assertEqual "missing header" Nothing =<< responseHeaderValue response (HeaderName "not-there")
    , testCase "should behave the same way for headers and allHeaders" $
        withChromiumPage $ \page ->
          withLocalApp
            ( headersPathApp
                [ ("header-a", "a=b")
                , ("header-a", "c=d")
                , ("Name-A", "v1")
                , ("name-b", "v4")
                , ("Name-a", "v2")
                , ("name-A", "v3")
                ]
            )
            $ \prefix -> do
              takeResponse <- armResponse page (UrlMatchGlob "**/headers")
              _ <- goto page (urlFromText (prefix <> "/headers")) defaultGotoOptions
              response <- takeResponse
              -- Upstream also checks that headers() matches allHeaders(). That check is not
              -- ported: responseHeaders returns the initializer's unmerged list. See the TODO.
              allHeaders <- responseAllHeaders response
              assertEqual "header-a" (Just "a=b, c=d") (headerValue "header-a" allHeaders)
              assertEqual "name-a" (Just "v1, v2, v3") (headerValue "name-a" allHeaders)
              assertEqual "name-b" (Just "v4") (headerValue "name-b" allHeaders)
    , testCase "should provide a Response with a file URL" $
        withChromiumPage $ \page -> do
          assets <- testAssetsDir
          response <-
            goto page (urlFromText ("file://" <> pack (assets <> "/frames/two-frames.html"))) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              assertEqual "status" (HttpStatus 200) =<< responseStatus value
              assertEqual "ok" True =<< responseOk value
    , testCase "should return multiple header value" $
        withChromiumPage $ \page ->
          withLocalApp
            ( headersPathApp
                [ ("Name-A", "v1")
                , ("Name-a", "v2")
                , ("name-A", "v3")
                ]
            )
            $ \prefix -> do
              takeResponse <- armResponse page (UrlMatchGlob "**/headers")
              _ <- goto page (urlFromText (prefix <> "/headers")) defaultGotoOptions
              response <- takeResponse
              assertEqual "status" (HttpStatus 200) =<< responseStatus response
              -- Upstream reads headers(); responseAllHeaders is the merged form here.
              headers <- responseAllHeaders response
              assertEqual "name-a" (Just "v1, v2, v3") (headerValue "name-a" headers)
    , testCase "should return uncompressed text for brotli encoding" $
        withChromiumPage $ \page ->
          withLocalApp brotliApp $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/brotli.json")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                headers <- responseAllHeaders value
                assertEqual "content-encoding" (Just "br") (headerValue "content-encoding" headers)
                assertEqual "body" "{\"foo\": \"bar\"}\n" =<< responseText value
    , testCase "should return non-utf8 body even when content-type says utf8" $
        withChromiumPage $ \page ->
          withLocalApp binaryApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            takeResponse <- armResponse page (UrlMatchGlob "**/binary-as-text")
            bytesReceived <-
              evaluateText page "() => fetch('/binary-as-text').then(r => r.bytes()).then(b => Array.from(b).join(','))"
            response <- takeResponse
            assertEqual "body" (LBS.toStrict binaryBytes) =<< responseBody response
            assertEqual "page bytes" "128,129,130,255,254,0,1,2" bytesReceived
    , testCase "should return set-cookie header after route.fulfill" $
        withChromiumPage $ \page -> do
          -- Upstream marks this it.fail on chromium and webkit (microsoft/playwright#11035), so
          -- the case passes while the set-cookie header is still missing from allHeaders().
          route page (UrlMatchGlob "**/*") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus 200)
                , fulfillHeaders = [HttpHeader (HeaderName "set-cookie") (HeaderValue "a=b")]
                , fulfillContentType = Just (ContentType "text/plain")
                , fulfillBody = Just (FulfillText "")
                }
          outcome <-
            try
              ( do
                  response <- goto page (urlFromText "http://playwright.test/empty.html") defaultGotoOptions
                  case response of
                    Nothing ->
                      assertFailure "missing response"
                    Just value -> do
                      headers <- responseAllHeaders value
                      assertEqual "set-cookie" (Just "a=b") (headerValue "set-cookie" headers)
              )
              :: Playwright (Either SomeException ())
          case outcome of
            Left _ ->
              pure ()
            Right () ->
              assertFailure "set-cookie reached allHeaders(); the upstream it.fail (issue 11035) has been fixed, so port this as a passing case"
    , testCase "Response.formData() should parse multipart/form-data in page context" $
        withChromiumPage $ \page ->
          withLocalApp headerApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            result <-
              evaluateText
                page
                "async () => { const boundary = '----WebKitFormBoundary1234'; const body = [`--${boundary}`, 'Content-Disposition: form-data; name=\"field1\"', '', 'value1', `--${boundary}`, 'Content-Disposition: form-data; name=\"file1\"; filename=\"test.txt\"', 'Content-Type: text/plain', '', 'hello', `--${boundary}--`].join('\\r\\n'); const response = new Response(body, { headers: { 'Content-Type': `multipart/form-data; boundary=${boundary}` } }); const fd = await response.formData(); const file = fd.get('file1'); return JSON.stringify({ field1: fd.get('field1'), filename: file instanceof File ? file.name : null, fileContent: file instanceof File ? await file.text() : null }); }"
            assertEqual "form data" "{\"field1\":\"value1\",\"filename\":\"test.txt\",\"fileContent\":\"hello\"}" result
    , testCase "should give a readable error when response.body() races with navigation" $
        withAssets $ \prefix page -> do
          takeResponse <- armResponse page (UrlMatchGlob (prefix <> "/title.html"))
          _ <- goto page (urlFromText (prefix <> "/title.html")) defaultGotoOptions
          response <- takeResponse
          -- Navigate away so the browser frees the first page's network resource.
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          result <- try (responseBody response)
          case result of
            Left (ProtocolRpc failure) ->
              assertBool
                "navigated away error"
                ("navigated away" `isInfixOf` protocolFailureMessage failure)
            Left err ->
              assertFailure ("expected ProtocolRpc, got: " <> show err)
            Right _ ->
              assertFailure "expected responseBody to fail after navigating away"
    ]

sortedHeaders :: [HttpHeader] -> [(Text, Text)]
sortedHeaders headers =
  sort
    [ (headerNameToText (httpHeaderName header), headerValueToText (httpHeaderValue header))
    | header <- headers
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

-- | Serves /headers with the given headers, and 404 for anything else.
headersPathApp :: [Header] -> Application
headersPathApp headers request respond =
  case pathInfo request of
    ["headers"] ->
      respond (responseLBS status200 headers LBS8.empty)
    _ ->
      respond (responseLBS status404 [] LBS8.empty)

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

-- | A response with the Content-Length header that Node's servers send. The driver
-- refetches a body that the browser has evicted only when that header is present.
sizedResponse :: Status -> [Header] -> LBS.ByteString -> Response
sizedResponse status headers body =
  responseLBS status ((hContentLength, LBS.toStrict (LBS8.pack (show (LBS.length body)))) : headers) body

-- | Serves a page that prefetches a script, so the browser requests it on its own.
prefetchApp :: Application
prefetchApp request respond =
  case pathInfo request of
    ["prefetch.html"] ->
      respond
        ( sizedResponse
            status200
            [(hContentType, "text/html")]
            (LBS8.pack "<link rel='prefetch' href='./prefetch.js'>")
        )
    ["prefetch.js"] ->
      respond (sizedResponse status200 [(hContentType, "application/javascript")] (LBS8.pack "// Scripts will be pre-fetched"))
    _ ->
      respond (responseLBS status404 [] LBS8.empty)

-- | A 1x1 gif cut short after its header, which Chromium may evict from its cache.
truncatedGif :: LBS.ByteString
truncatedGif = LBS.pack [0x47, 0x49, 0x46, 0x38, 0x39, 0x61, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x2c]

evictedImageApp :: Application
evictedImageApp request respond =
  case pathInfo request of
    ["page.html"] ->
      respond
        ( sizedResponse
            status200
            [(hContentType, "text/html")]
            (LBS8.pack "<html><body><img src=\"/pixel.gif\"></body></html>")
        )
    ["pixel.gif"] ->
      respond (sizedResponse status200 [(hContentType, "image/gif")] truncatedGif)
    _ ->
      respond (responseLBS status404 [] LBS8.empty)

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

-- | Serves JSON as brotli. The stream is one uncompressed meta-block (RFC 7932 section 9.2),
-- which needs no encoder: a 16-bit window, ISLAST=0, MNIBBLES=4, MLEN-1=14, ISUNCOMPRESSED=1,
-- the 15 raw bytes, then an empty last meta-block.
brotliApp :: Application
brotliApp request respond =
  case pathInfo request of
    ["brotli.json"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "application/json"), (hContentEncoding, "br")]
            (LBS.pack [0xE0, 0x00, 0x10] <> LBS8.pack "{\"foo\": \"bar\"}\n" <> LBS.pack [0x03])
        )
    _ ->
      respond (responseLBS status404 [] LBS8.empty)

-- | Bytes that are invalid UTF-8, served as text/plain with a UTF-8 charset.
binaryBytes :: LBS.ByteString
binaryBytes = LBS.pack [0x80, 0x81, 0x82, 0xFF, 0xFE, 0x00, 0x01, 0x02]

binaryApp :: Application
binaryApp request respond =
  case pathInfo request of
    ["binary-as-text"] ->
      respond (sizedResponse status200 [(hContentType, "text/plain;charset=UTF-8")] binaryBytes)
    ["empty.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] LBS8.empty)
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

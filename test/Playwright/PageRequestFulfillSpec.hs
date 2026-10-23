module Playwright.PageRequestFulfillSpec
  ( tests
  ) where

import Codec.Compression.GZip qualified as GZip
import Control.Concurrent
  ( threadDelay
  )
import Control.Monad
  ( forM_
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Aeson
  ( Value
  , eitherDecodeStrict
  , object
  , (.=)
  )
import Data.Aeson.Types
  ( Parser
  , parseEither
  , withObject
  , (.:)
  , (.:?)
  )
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.IORef
  ( IORef
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.List
  ( find
  , sort
  )
import Data.Maybe
  ( fromMaybe
  )
import Data.Text
  ( Text
  , intercalate
  , isInfixOf
  , isSuffixOf
  , pack
  , splitOn
  , strip
  , toLower
  , unpack
  )
import UnliftIO.Async
  ( race
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , putMVar
  , takeMVar
  )
import Network.HTTP.Types
  ( status200
  )
import Network.Wai
  ( Application
  , rawPathInfo
  , responseLBS
  )
import Playwright
  ( apiResponseHeaders
  , ContentType (ContentType)
  , ContinueOptions (continueHeaders)
  , FulfillBody (FulfillBytes, FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType, fulfillHeaders, fulfillResponse, fulfillStatus)
  , HeaderName (HeaderName, headerNameToText)
  , HeaderValue (HeaderValue, headerValueToText)
  , HttpHeader (HttpHeader, httpHeaderName, httpHeaderValue)
  , HttpStatus (HttpStatus, httpStatusToInt)
  , UrlMatch (UrlMatchGlob)
  , RequestFailure (requestFailureText)
  , armRequestFailed
  , armResponse
  , continueRoute
  , continueRouteWith
  , defaultContinueOptions
  , defaultFulfillOptions
  , defaultGotoOptions
  , evaluate
  , evaluateBool
  , evaluateText
  , fetchGet
  , fetchRequest
  , fulfillRoute
  , newRequestContext
  , pageRequest
  , requestFailure
  , requestHeaders
  , goto
  , requestUrl
  , responseAllHeaders
  , responseHeaders
  , responseJSON
  , responseStatus
  , responseStatusText
  , responseText
  , route
  , routeFetch
  , routeRequest
  , title
  , unrouteAll
  , urlFromText
  , urlToText
  , waitForFunction
  )
import Playwright.Harness
  ( Recorded (recordedHeaders)
  , readTestAsset
  , recordRequest
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
    "page-request-fulfill"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          route page (UrlMatchGlob "**/*") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus 201)
                , fulfillHeaders = [HttpHeader (HeaderName "foo") (HeaderValue "bar")]
                , fulfillContentType = Just (ContentType "text/html")
                , fulfillBody = Just (FulfillText "Yo, page!")
                }
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              status <- responseStatus value
              assertEqual "status" 201 (httpStatusToInt status)
              headers <- responseHeaders value
              assertEqual "foo" (Just "bar") (headerValue "foo" headers)
          body <- evaluateText page "() => document.body.textContent"
          assertEqual "body" "Yo, page!" body
    , testCase "should work with status code 422" $
        withAssets $ \prefix page -> do
          route page (UrlMatchGlob "**/*") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus 422)
                , fulfillBody = Just (FulfillText "Yo, page!")
                }
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              status <- responseStatus value
              assertEqual "status" 422 (httpStatusToInt status)
              text <- responseStatusText value
              assertEqual "statusText" "Unprocessable Entity" text
          body <- evaluateText page "() => document.body.textContent"
          assertEqual "body" "Yo, page!" body
    , testCase "should support cors with GET" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          route page (UrlMatchGlob "**/cars*") $ \handled -> do
            request <- routeRequest handled
            url <- requestUrl request
            let
              origin :: Text
              origin =
                if "allow" `isSuffixOf` urlToText url
                  then "*"
                  else "none"
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillContentType = Just (ContentType "application/json")
                , fulfillHeaders =
                    [HttpHeader (HeaderName "access-control-allow-origin") (HeaderValue origin)]
                , fulfillStatus = Just (HttpStatus 200)
                , fulfillBody = Just (FulfillText "[\"electric\",\"gas\"]")
                }
          allowed <-
            evaluateText
              page
              "async () => { const response = await fetch('https://example.com/cars?allow', { mode: 'cors' }); return JSON.stringify(await response.json()); }"
          assertEqual "allowed" "[\"electric\",\"gas\"]" allowed
    , testCase "should allow mocking binary responses" $
        withChromiumPage $ \page ->
          withLocalApp emptyApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            image <- readTestAsset "pptr.png"
            route page (UrlMatchGlob "**/*") $ \handled ->
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillContentType = Just (ContentType "image/png")
                  , fulfillBody = Just (FulfillBytes image)
                  }
            _ <-
              evaluate
                page
                ( "() => new Promise(done => { const img = document.createElement('img'); img.src = '"
                    <> prefix
                    <> "/does-not-exist.png'; document.body.appendChild(img); img.onload = () => done(true); })"
                )
            assertEqual "loaded" True =<< evaluateBool page "() => document.querySelector('img').naturalWidth > 0"
    , testCase "should work with buffer as body" $
        withChromiumPage $ \page ->
          withLocalApp emptyApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/data.json") $ \handled ->
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillContentType = Just (ContentType "text/plain")
                  , fulfillBody = Just (FulfillBytes (BS8.pack "Yo, page!"))
                  }
            body <- evaluateText page "() => fetch('./data.json').then(r => r.text())"
            assertEqual "body" "Yo, page!" body
    , testCase "should fulfill json" $
        withChromiumPage $ \page ->
          withLocalApp emptyApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/data.json") $ \handled ->
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillStatus = Just (HttpStatus 201)
                  , fulfillContentType = Just (ContentType "application/json")
                  , fulfillHeaders = [HttpHeader (HeaderName "foo") (HeaderValue "bar")]
                  , fulfillBody = Just (FulfillText "{\"bar\":\"baz\"}")
                  }
            takeResponse <- armResponse page (UrlMatchGlob "**/*")
            body <- evaluateText page "() => fetch('./data.json').then(r => r.text())"
            response <- takeResponse
            assertEqual "status" (HttpStatus 201) =<< responseStatus response
            assertEqual "body" "{\"bar\":\"baz\"}" body
    , testCase "should not throw if request was cancelled by the page" $
        withAssets $ \prefix page -> do
          intercepted <- newEmptyMVar
          route page (UrlMatchGlob "**/data.json") $ \handled -> putMVar intercepted handled
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <-
            evaluate
              page
              "() => { globalThis.controller = new AbortController(); fetch('/data.json', { signal: globalThis.controller.signal }).catch(() => {}); }"
          -- Bounded: a route that never fires must fail the test, not hang the run.
          outcome <- race (liftIO (threadDelay 30000000)) (takeMVar intercepted)
          case outcome of
            Left () ->
              assertFailure "route never intercepted data.json"
            Right handled -> do
              takeFailure <- armRequestFailed page
              _ <- evaluate page "() => globalThis.controller.abort()"
              failed <- takeFailure
              failure <- requestFailure failed
              case failure of
                Nothing ->
                  assertFailure "missing failure"
                Just value ->
                  assertBool
                    "cancelled or aborted"
                    (any (`isInfixOf` toLower (requestFailureText value)) ["cancelled", "aborted"])
              -- Fulfilling a route whose request was cancelled must not throw.
              fulfillRoute handled defaultFulfillOptions {fulfillStatus = Just (HttpStatus 200)}
    , testCase "should fulfill with fetch result" $
        withAssets $ \prefix page -> do
          ctx <- pageRequest page
          route page (UrlMatchGlob "**/*") $ \handled -> do
            response <- fetchGet ctx (urlFromText (prefix <> "/simple.json"))
            fulfillRoute handled defaultFulfillOptions {fulfillResponse = Just response}
          navigated <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case navigated of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              assertEqual "status" (HttpStatus 200) =<< responseStatus value
              assertEqual "json" (object ["foo" .= ("bar" :: Text)]) =<< responseJSON value
    , testCase "should fulfill with fetch result and overrides" $
        withAssets $ \prefix page -> do
          ctx <- pageRequest page
          route page (UrlMatchGlob "**/*") $ \handled -> do
            response <- fetchGet ctx (urlFromText (prefix <> "/simple.json"))
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillResponse = Just response
                , fulfillStatus = Just (HttpStatus 201)
                , -- Case matters for the tested behavior.
                  fulfillHeaders =
                    [ HttpHeader (HeaderName "Content-Type") (HeaderValue "application/json")
                    , HttpHeader (HeaderName "foo") (HeaderValue "bar")
                    ]
                }
          navigated <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case navigated of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              assertEqual "status" (HttpStatus 201) =<< responseStatus value
              headers <- responseAllHeaders value
              assertEqual "foo" (Just "bar") (headerValue "foo" headers)
              assertEqual "json" (object ["foo" .= ("bar" :: Text)]) =<< responseJSON value
    , testCase "should fulfill with global fetch result" $
        withAssets $ \prefix page -> do
          route page (UrlMatchGlob "**/*") $ \handled -> do
            ctx <- newRequestContext
            response <- fetchGet ctx (urlFromText (prefix <> "/simple.json"))
            fulfillRoute handled defaultFulfillOptions {fulfillResponse = Just response}
          navigated <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case navigated of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              assertEqual "status" (HttpStatus 200) =<< responseStatus value
              assertEqual "json" (object ["foo" .= ("bar" :: Text)]) =<< responseJSON value
    , testCase "should fetch original request and fulfill" $
        withAssets $ \prefix page -> do
          ctx <- pageRequest page
          route page (UrlMatchGlob "**/*") $ \handled -> do
            response <- fetchRequest ctx =<< routeRequest handled
            fulfillRoute handled defaultFulfillOptions {fulfillResponse = Just response}
          navigated <- goto page (urlFromText (prefix <> "/title.html")) defaultGotoOptions
          case navigated of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "status" (HttpStatus 200) =<< responseStatus value
          assertEqual "title" "Woof-Woof" =<< title page
    , testCase "should fulfill with unassigned status codes" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          route page (UrlMatchGlob "**/data.json") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus 430)
                , fulfillBody = Just (FulfillText "Yo, page!")
                }
          result <-
            evaluateText
              page
              "async () => { const response = await fetch('/data.json'); return response.status + ' ' + response.statusText; }"
          assertEqual "status and statusText" "430 Unknown" result
    , testCase "should stringify intercepted request response headers" $
        withAssets $ \prefix page -> do
          route page (UrlMatchGlob "**/*") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus 200)
                , fulfillHeaders = [HttpHeader (HeaderName "foo") (HeaderValue "true")]
                , fulfillBody = Just (FulfillText "Yo, page!")
                }
          navigated <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case navigated of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              assertEqual "status" (HttpStatus 200) =<< responseStatus value
              headers <- responseHeaders value
              assertEqual "foo" (Just "true") (headerValue "foo" headers)
          body <- evaluateText page "() => document.body.textContent"
          assertEqual "body" "Yo, page!" body
    , testCase "should not modify the headers sent to the server" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          requests <- liftIO (newIORef [])
          withLocalApp (corsRecordingApp requests) $ \cross -> do
            -- this is just to enable request interception, which disables caching in chromium
            route page (UrlMatchGlob (prefix <> "/unused")) continueRoute
            text <- evaluateText page (fetchTextScript (cross <> "/something"))
            assertEqual "text" "done" text
            route page (UrlMatchGlob (cross <> "/something")) $ \handled -> do
              request <- routeRequest handled
              headers <- requestHeaders request
              continueRouteWith handled defaultContinueOptions {continueHeaders = Just headers}
            textAfterRoute <- evaluateText page (fetchTextScript (cross <> "/something"))
            assertEqual "textAfterRoute" "done" textAfterRoute
            seen <- liftIO (readIORef requests)
            case reverse seen of
              [first, second] ->
                -- the browser does not send the headers in a stable order, so compare them sorted
                assertEqual "headers" (sort (recordedHeaders first)) (sort (recordedHeaders second))
              other ->
                assertFailure ("expected two intercepted requests, got " <> show (length other))
    , testCase "should include the origin header" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          seen <- liftIO (newIORef Nothing)
          withLocalApp emptyApp $ \cross -> do
            route page (UrlMatchGlob (cross <> "/something")) $ \handled -> do
              request <- routeRequest handled
              headers <- requestHeaders request
              liftIO (writeIORef seen (Just headers))
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillHeaders = [HttpHeader (HeaderName "Access-Control-Allow-Origin") (HeaderValue "*")]
                  , fulfillContentType = Just (ContentType "text/plain")
                  , fulfillBody = Just (FulfillText "done")
                  }
            text <- evaluateText page (fetchTextScript (cross <> "/something"))
            assertEqual "text" "done" text
            captured <- liftIO (readIORef seen)
            case captured of
              Nothing ->
                assertFailure "route was not hit"
              Just headers ->
                assertEqual "origin" (Just prefix) (headerValue "origin" headers)
    , testCase "should fulfill with multiple set-cookie" $
        withAssets $ \prefix page -> do
          let
            cookies :: [Text]
            cookies = ["a=b", "c=d"]
          route page (UrlMatchGlob "**/multiple-set-cookie.html") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillHeaders =
                    [ HttpHeader (HeaderName "X-Header-1") (HeaderValue "v1")
                    , HttpHeader (HeaderName "Set-Cookie") (HeaderValue (intercalate "\n" cookies))
                    , HttpHeader (HeaderName "X-Header-2") (HeaderValue "v2")
                    ]
                , fulfillBody = Just (FulfillText "")
                }
          navigated <- goto page (urlFromText (prefix <> "/multiple-set-cookie.html")) defaultGotoOptions
          cookie <- evaluateText page "() => document.cookie"
          assertEqual "cookies" (sort cookies) (sort (fmap strip (splitOn ";" cookie)))
          case navigated of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              headers <- responseHeaders value
              assertEqual "x-header-1" (Just "v1") (headerValue "x-header-1" headers)
              assertEqual "x-header-2" (Just "v2") (headerValue "x-header-2" headers)
    , testCase "should fulfill with fetch response that has multiple set-cookie" $
        withAssets $ \_ page ->
          withLocalApp cookieApp $ \cross -> do
            route page (UrlMatchGlob "**/empty.html") $ \handled -> do
              ctx <- newRequestContext
              request <- routeRequest handled
              response <- fetchRequest ctx request
              fulfillRoute handled defaultFulfillOptions {fulfillResponse = Just response}
            _ <- goto page (urlFromText (cross <> "/empty.html")) defaultGotoOptions
            cookie <- evaluateText page "() => document.cookie"
            assertEqual "cookies" ["a=b", "c=d"] (sort (fmap strip (splitOn ";" cookie)))
    , testCase "should fulfill with har response" $
        withAssets $ \_ page -> do
          bytes <- readTestAsset "har-fulfill.har"
          entries <- either assertFailure pure (parseHar bytes)
          route page (UrlMatchGlob "**/*") $ \handled -> do
            request <- routeRequest handled
            url <- urlToText <$> requestUrl request
            entry <-
              maybe
                (assertFailure ("missing HAR entry for " <> unpack url))
                pure
                (findHarResponse entries url)
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus (harEntryStatus entry))
                , fulfillHeaders =
                    [ HttpHeader (HeaderName name) (HeaderValue value)
                    | (name, value) <- harEntryHeaders entry
                    ]
                , fulfillBody = Just (FulfillText (harEntryText entry))
                }
          _ <- goto page (urlFromText "http://no.playwright/") defaultGotoOptions
          -- The HAR has a redirect for the script.
          value <- evaluateText page "() => window.value"
          assertEqual "window.value" "foo" value
          -- The HAR has a POST for the css file, and the route matches it ignoring the method.
          background <- evaluateText page "() => getComputedStyle(document.body).backgroundColor"
          assertEqual "background" "rgb(0, 255, 255)" background
    , testCase "should work with svg image and charset in content type" $
        withChromiumPage $ \page ->
          withLocalApp emptyApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "**/*") $ \handled ->
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillContentType = Just (ContentType "image/svg+xml ; charset=utf-8")
                  , fulfillBody =
                      Just
                        ( FulfillText
                            "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"50\" height=\"50\"><rect x=\"10\" y=\"10\" width=\"30\" height=\"30\" stroke=\"black\" stroke-width=\"5\" fill=\"none\"/></svg>"
                        )
                  }
            -- Upstream compares a screenshot of the image; there is no snapshot helper here,
            -- so check that the image loads and decodes at the size the SVG declares.
            loaded <-
              evaluateBool
                page
                ( "async () => { const img = document.createElement('img'); img.src = '"
                    <> prefix
                    <> "/does-not-exist.svg'; document.body.appendChild(img); return new Promise(done => { img.onload = () => done(true); img.onerror = () => done(false); }); }"
                )
            assertBool "loaded" loaded
            assertEqual "size" True
              =<< evaluateBool page "() => { const img = document.querySelector('img'); return img.naturalWidth === 50 && img.naturalHeight === 50; }"
    , testCase "should work with preload link" $
        withAssets $ \prefix page -> do
          route page (UrlMatchGlob "**/one-style.css") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillContentType = Just (ContentType "text/css; charset=utf-8")
                , fulfillHeaders =
                    [ HttpHeader (HeaderName "custom") (HeaderValue "value")
                    , HttpHeader (HeaderName "cache-control") (HeaderValue "no-cache, no-store")
                    ]
                , fulfillBody = Just (FulfillText "body { background-color: green; }")
                }
          takeResponse <- armResponse page (UrlMatchGlob "**/one-style.css")
          _ <- goto page (urlFromText (prefix <> "/preload.html")) defaultGotoOptions
          response <- takeResponse
          headers <- responseHeaders response
          assertEqual "custom header" (Just "value") (headerValue "custom" headers)
          _ <- waitForFunction page "() => window.preloadedStyles"
          background <- evaluateText page "() => getComputedStyle(document.body).backgroundColor"
          assertEqual "background" "rgb(0, 128, 0)" background
    , testCase "should work with gzip content encoding through route fetch" $
        withChromiumPage $ \page ->
          withLocalApp gzipApp $ \prefix -> do
            route page (UrlMatchGlob "**/one-style.html") $ \handled -> do
              apiResponse <- routeFetch handled
              assertEqual
                "content-encoding"
                (Just "gzip")
                (headerValue "content-encoding" (apiResponseHeaders apiResponse))
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillResponse = Just apiResponse
                  }
            _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
            text <- evaluateText page "() => document.querySelector('div').textContent"
            assertEqual "text" "hello, world!" text
            background <- evaluateText page "() => getComputedStyle(document.body).backgroundColor"
            assertEqual "background" "rgb(255, 192, 203)" background
    , testCase "should not hit the network when a body is fulfilled" $ do
        served <- newIORef False
        withChromiumPage $ \page ->
          withLocalApp (styleApp served) $ \prefix -> do
            route page (UrlMatchGlob "**/one-style.css") $ \handled ->
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillStatus = Just (HttpStatus 404)
                  , fulfillContentType = Just (ContentType "text/plain")
                  , fulfillBody = Just (FulfillText "Not Found! (mocked)")
                  }
            takeResponse <- armResponse page (UrlMatchGlob "**/one-style.css")
            _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
            response <- takeResponse
            text <- responseText response
            assertEqual "text" "Not Found! (mocked)" text
        requested <- readIORef served
        assertBool "stylesheet was requested from the server" (not requested)
    , testCase "should return the fulfilled body for each status" $ do
        served <- newIORef False
        withChromiumPage $ \page ->
          withLocalApp (styleApp served) $ \prefix ->
            forM_ [100, 200, 404, 500 :: Int] $ \status -> do
              let body = "Custom body " <> pack (show status)
              route page (UrlMatchGlob "**/one-style.css") $ \handled ->
                fulfillRoute
                  handled
                  defaultFulfillOptions
                    { fulfillStatus = Just (HttpStatus status)
                    , fulfillContentType = Just (ContentType "text/plain")
                    , fulfillBody = Just (FulfillText body)
                    }
              takeResponse <- armResponse page (UrlMatchGlob "**/one-style.css")
              _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
              response <- takeResponse
              text <- responseText response
              assertEqual ("body for status " ++ show status) body text
              unrouteAll page
    ]

-- | Answers every path with an empty 200, so the page can load.
emptyApp :: Application
emptyApp _ respond =
  respond (responseLBS status200 [] LBS8.empty)

-- | Serves the style page, and records whether the stylesheet reached the server.
styleApp :: IORef Bool -> Application
styleApp served request respond =
  if rawPathInfo request == "/one-style.css"
    then do
      writeIORef served True
      css <- readTestAsset "one-style.css"
      respond (responseLBS status200 [("Content-Type", "text/css")] (LBS.fromStrict css))
    else
      respond
        ( responseLBS
            status200
            [("Content-Type", "text/html")]
            "<!DOCTYPE html><link rel='stylesheet' href='./one-style.css'><div>hello, world!</div>"
        )

-- | Serves the style page gzipped, and the stylesheet as it is.
gzipApp :: Application
gzipApp request respond =
  if rawPathInfo request == "/one-style.css"
    then do
      css <- readTestAsset "one-style.css"
      respond (responseLBS status200 [("Content-Type", "text/css")] (LBS.fromStrict css))
    else do
      html <- readTestAsset "one-style.html"
      respond
        ( responseLBS
            status200
            [ ("Content-Type", "text/html")
            , ("Content-Encoding", "gzip")
            ]
            (GZip.compress (LBS.fromStrict html))
        )

-- | Records every request, and answers with "done" under a CORS header.
corsRecordingApp :: IORef [Recorded] -> Application
corsRecordingApp requests request respond = do
  recordRequest requests request
  respond (responseLBS status200 [("Access-Control-Allow-Origin", "*")] (LBS8.pack "done"))

-- | Sets two cookies on every response, and answers with an empty HTML page.
cookieApp :: Application
cookieApp _ respond =
  respond
    ( responseLBS
        status200
        [ ("Set-Cookie", "a=b")
        , ("Set-Cookie", "c=d")
        , ("Content-Type", "text/html")
        ]
        LBS8.empty
    )

-- | One HAR entry, reduced to what the fulfill route needs.
data HarEntry = HarEntry
  { harEntryUrl :: Text
  , harEntryRedirect :: Text
  , harEntryStatus :: Int
  , harEntryHeaders :: [(Text, Text)]
  , harEntryText :: Text
  }

parseHar :: BS8.ByteString -> Either String [HarEntry]
parseHar bytes =
  eitherDecodeStrict bytes >>= parseEither harEntries

harEntries :: Value -> Parser [HarEntry]
harEntries =
  withObject "HAR" $ \root -> do
    harLog <- root .: "log"
    entries <- withObject "log" (.: "entries") harLog
    traverse harEntry entries

harEntry :: Value -> Parser HarEntry
harEntry =
  withObject "entry" $ \entry -> do
    request <- entry .: "request"
    url <- withObject "request" (.: "url") request
    response <- entry .: "response"
    withObject
      "response"
      ( \fields -> do
          status <- fields .: "status"
          redirect <- fields .: "redirectURL"
          headers <- fields .: "headers" >>= traverse harHeader
          content <- fields .: "content"
          text <- withObject "content" (.:? "text") content
          pure
            HarEntry
              { harEntryUrl = url
              , harEntryRedirect = redirect
              , harEntryStatus = status
              , harEntryHeaders = headers
              , harEntryText = fromMaybe "" text
              }
      )
      response

harHeader :: Value -> Parser (Text, Text)
harHeader =
  withObject "header" $ \header ->
    (,) <$> header .: "name" <*> header .: "value"

-- | Follows the redirect chain from a URL to the entry that answers it.
findHarResponse :: [HarEntry] -> Text -> Maybe HarEntry
findHarResponse entries url =
  case find ((== url) . harEntryUrl) entries of
    Nothing ->
      Nothing
    Just entry
      | strip (harEntryRedirect entry) == "" ->
          Just entry
      | otherwise ->
          findHarResponse entries (harEntryRedirect entry)

-- | Fetches the URL from inside the page, and returns the response text.
fetchTextScript :: Text -> Text
fetchTextScript url =
  "async () => (await fetch('" <> url <> "')).text()"

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

module Playwright.PageRequestFulfillSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text
  ( Text
  , isInfixOf
  , isSuffixOf
  , toLower
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
  , responseLBS
  )
import Playwright
  ( ContentType (ContentType)
  , FulfillBody (FulfillBytes, FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType, fulfillHeaders, fulfillStatus)
  , HeaderName (HeaderName, headerNameToText)
  , HeaderValue (HeaderValue, headerValueToText)
  , HttpHeader (HttpHeader, httpHeaderName, httpHeaderValue)
  , HttpStatus (HttpStatus, httpStatusToInt)
  , UrlMatch (UrlMatchGlob)
  , RequestFailure (requestFailureText)
  , armRequestFailed
  , armResponse
  , defaultFulfillOptions
  , defaultGotoOptions
  , evaluate
  , evaluateBool
  , evaluateText
  , fulfillRoute
  , requestFailure
  , goto
  , requestUrl
  , responseHeaders
  , responseStatus
  , responseStatusText
  , route
  , routeRequest
  , urlFromText
  , urlToText
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
    ]

-- | Answers every path with an empty 200, so the page can load.
emptyApp :: Application
emptyApp _ respond =
  respond (responseLBS status200 [] LBS8.empty)

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

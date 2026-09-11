module Playwright.PageNetworkRequestSpec
  ( tests
  ) where

import Control.Concurrent.Async
  ( wait
  , withAsync
  )
import Data.Text
  ( isInfixOf
  , toLower
  )
import Playwright
  ( HeaderName (headerNameToText)
  , HttpHeader (httpHeaderName, httpHeaderValue)
  , HeaderValue (headerValueToText)
  , UrlGlob (UrlGlob)
  , defaultGotoOptions
  , evaluate
  , framesEqual
  , goto
  , mainFrame
  , requestFrame
  , requestHeaders
  , requestUrl
  , responseRequest
  , urlFromText
  , urlToText
  , waitForRequest
  )
import Playwright.Harness
  ( withAssets
  )
import Test.Tasty
  ( TestTree
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
  testGroup
    "page-network-request"
    [ testCase "should work for main frame navigation request" $
        withAssets $ \prefix page -> do
          withAsync (waitForRequest page (UrlGlob (prefix <> "/empty.html"))) $ \handle -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            request <- wait handle
            frame <- requestFrame request
            case frame of
              Nothing ->
                assertFailure "expected frame"
              Just actual ->
                assertEqual "main frame" True (framesEqual actual (mainFrame page))
    , testCase "should work for fetch requests" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync (waitForRequest page (UrlGlob (prefix <> "/digits/1.png"))) $ \handle -> do
            _ <- evaluate page "() => fetch('/digits/1.png')"
            request <- wait handle
            frame <- requestFrame request
            case frame of
              Nothing ->
                assertFailure "expected frame"
              Just actual ->
                assertEqual "main frame" True (framesEqual actual (mainFrame page))
    , testCase "should return headers" $
        withAssets $ \prefix page -> do
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              request <- responseRequest value
              headers <- requestHeaders request
              let userAgent =
                    fmap (headerValueToText . httpHeaderValue)
                      ( filter
                          ( \header ->
                              toLower (headerNameToText (httpHeaderName header)) == "user-agent"
                          )
                          headers
                      )
              case userAgent of
                [agent] ->
                  assertBool "Chrome" ("Chrome" `isInfixOf` agent)
                _ ->
                  assertFailure "missing user-agent"
    , testCase "should expose request url after navigation" $
        withAssets $ \prefix page -> do
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              request <- responseRequest value
              url <- requestUrl request
              assertEqual "url" (prefix <> "/empty.html") (urlToText url)
    ]

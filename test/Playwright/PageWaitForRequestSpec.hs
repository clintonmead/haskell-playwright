module Playwright.PageWaitForRequestSpec
  ( tests
  ) where

import Control.Concurrent.Async
  ( wait
  , withAsync
  )
import Playwright
  ( UrlGlob (UrlGlob)
  , defaultGotoOptions
  , evaluate
  , goto
  , milliseconds
  , requestUrl
  , urlFromText
  , urlToText
  , waitForRequest
  , waitForRequestWith
  )
import Playwright.Harness
  ( assertWaitTimeout
  , withAssets
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , assertFailure
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-wait-for-request"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let wanted = prefix <> "/digits/2.png"
          withAsync (waitForRequest page (UrlGlob wanted)) $ \handle -> do
            _ <-
              evaluate
                page
                "() => { void fetch('/digits/1.png'); void fetch('/digits/2.png'); void fetch('/digits/3.png'); }"
            request <- wait handle
            url <- requestUrl request
            assertEqual "url" wanted (urlToText url)
    , testCase "should respect timeout" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case milliseconds 1 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              assertWaitTimeout (waitForRequestWith page (UrlGlob (prefix <> "/does-not-exist.png")) timeout)
    ]

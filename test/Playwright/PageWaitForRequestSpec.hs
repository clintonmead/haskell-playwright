module Playwright.PageWaitForRequestSpec
  ( tests
  ) where

import UnliftIO.Async
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
  , setDefaultTimeout
  )
import Playwright.Harness
  ( assertWaitTimeout
  , withAssets
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
    , testCase "should respect default timeout" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case milliseconds 1 of
            Nothing ->
              assertFailure "timeout"
            Just timeout -> do
              setDefaultTimeout page timeout
              assertWaitTimeout (waitForRequest page (UrlGlob (prefix <> "/does-not-exist.png")))
    ]

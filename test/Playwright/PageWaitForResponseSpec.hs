module Playwright.PageWaitForResponseSpec
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
  , urlFromText
  , urlToText
  , waitForResponse
  , responseUrl
  )
import Playwright.Harness
  ( withAssets
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-wait-for-response"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let wanted = prefix <> "/digits/2.png"
          withAsync (waitForResponse page (UrlGlob wanted)) $ \handle -> do
            _ <-
              evaluate
                page
                "() => { void fetch('/digits/1.png'); void fetch('/digits/2.png'); void fetch('/digits/3.png'); }"
            response <- wait handle
            url <- responseUrl response
            assertEqual "url" wanted (urlToText url)
    ]

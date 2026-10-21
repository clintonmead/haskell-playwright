module Playwright.PageRequestAbortSpec
  ( tests
  ) where

import Playwright
  ( UrlMatch (UrlMatchGlob)
  , abortRoute
  , defaultGotoOptions
  , goto
  , route
  , urlFromText
  )
import Playwright.Harness
  ( assertProtocolContains
  , withAssets
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
    "page-route-abort"
    [ testCase "should fail navigation when aborting main resource" $
        withAssets $ \prefix page -> do
          route page (UrlMatchGlob "**/*") abortRoute
          assertProtocolContains
            "net::ERR_FAILED"
            (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
    ]

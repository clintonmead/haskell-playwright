module Playwright.PageWaitForLoadStateSpec
  ( tests
  ) where

import Playwright
  ( LoadState (LoadStateDomContentLoaded, LoadStateLoad)
  , waitForLoadState
  )
import Playwright.Harness
  ( withAssetPage
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
    "page-wait-for-load-state"
    [ testCase "should resolve immediately if loaded" $
        withAssetPage "one-style.html" $ \page ->
          waitForLoadState page LoadStateLoad
    , testCase "should resolve immediately if load state matches" $
        withAssetPage "empty.html" $ \page ->
          waitForLoadState page LoadStateDomContentLoaded
    ]

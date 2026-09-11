module Playwright.PageClickSpec
  ( tests
  ) where

import Playwright
  ( click
  , evaluateText
  )
import Playwright.Harness
  ( locate
  , withAssetPage
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
    "page-click"
    [ testCase "should click the button" $
        withAssetPage "input/button.html" $ \page -> do
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    ]

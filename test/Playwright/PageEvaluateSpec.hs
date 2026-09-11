module Playwright.PageEvaluateSpec
  ( tests
  ) where

import Playwright
  ( evaluateNumber
  )
import Playwright.Harness
  ( withChromiumPage
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
    "page-evaluate"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          result <- evaluateNumber page "() => 7 * 3"
          assertEqual "21" (21 :: Double) result
    ]

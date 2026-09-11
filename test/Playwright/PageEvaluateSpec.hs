module Playwright.PageEvaluateSpec
  ( tests
  ) where

import Playwright
  ( SpecialJsValue (SpecialNaN)
  , evaluateNumber
  , evaluateSpecial
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
    , testCase "should transfer NaN" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecial page "() => NaN"
          assertEqual "NaN" SpecialNaN special
    ]

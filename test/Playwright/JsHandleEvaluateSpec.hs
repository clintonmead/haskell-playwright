module Playwright.JsHandleEvaluateSpec
  ( tests
  ) where

import Playwright
  ( evaluateHandle
  , evaluateOnHandleExpressionNumbers
  , evaluateOnHandleNumbers
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
    "jshandle-evaluate"
    [ testCase "should work with function" $
        withChromiumPage $ \page -> do
          handle <-
            evaluateHandle
              page
              "() => { window.foo = [1, 2]; return window; }"
          assertEqual "foo" [1, 2 :: Double]
            =<< evaluateOnHandleNumbers handle "w => w.foo"
    , testCase "should work with expression" $
        withChromiumPage $ \page -> do
          handle <-
            evaluateHandle
              page
              "() => { window.foo = [1, 2]; return window; }"
          assertEqual "foo" [1, 2 :: Double]
            =<< evaluateOnHandleExpressionNumbers handle "window.foo"
    ]

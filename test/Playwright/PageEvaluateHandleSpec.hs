module Playwright.PageEvaluateHandleSpec
  ( tests
  ) where

import Data.Text
  ( isInfixOf
  )
import Playwright
  ( evaluateBoolWith
  , evaluateHandle
  , evaluateTextWith
  , jsHandleArgument
  )
import Playwright.Harness
  ( withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertBool
  , assertEqual
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-evaluate-handle"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandle page "() => window"
          assertEqual "window" True =<< evaluateBoolWith page "e => e === window" (jsHandleArgument handle)
    , testCase "should accept object handle as an argument" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandle page "() => navigator"
          userAgent <- evaluateTextWith page "e => e.userAgent" (jsHandleArgument handle)
          assertBool "Mozilla" ("Mozilla" `isInfixOf` userAgent)
    , testCase "should accept object handle to primitive types" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandle page "() => 5"
          assertEqual "is five" True =<< evaluateBoolWith page "e => Object.is(e, 5)" (jsHandleArgument handle)
    ]

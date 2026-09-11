module Playwright.PageEvaluateSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Playwright
  ( SpecialJsValue (SpecialInfinity, SpecialNaN, SpecialNegInfinity, SpecialNegZero, SpecialNull, SpecialUndefined)
  , boolArgument
  , evaluateBoolWith
  , evaluateNumber
  , evaluateNumberWith
  , evaluateNumbersWith
  , evaluateSpecial
  , evaluateSpecialWith
  , evaluateTextWith
  , numberArgument
  , numbersArgument
  , specialArgument
  , textArgument
  )
import Playwright.Harness
  ( withAssetPage
  , withChromiumPage
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
          special <- evaluateSpecialWith page "a => a" (specialArgument SpecialNaN)
          assertEqual "NaN" SpecialNaN special
    , testCase "should transfer -0" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecialWith page "a => a" (specialArgument SpecialNegZero)
          assertEqual "-0" SpecialNegZero special
    , testCase "should transfer Infinity" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecialWith page "a => a" (specialArgument SpecialInfinity)
          assertEqual "Infinity" SpecialInfinity special
    , testCase "should transfer -Infinity" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecialWith page "a => a" (specialArgument SpecialNegInfinity)
          assertEqual "-Infinity" SpecialNegInfinity special
    , testCase "should transfer null" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecialWith page "a => a" (specialArgument SpecialNull)
          assertEqual "null" SpecialNull special
    , testCase "should transfer undefined" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecialWith page "a => a" (specialArgument SpecialUndefined)
          assertEqual "undefined" SpecialUndefined special
    , testCase "should transfer 0" $
        withChromiumPage $ \page -> do
          result <- evaluateNumberWith page "a => a" (numberArgument 0)
          assertEqual "0" (0 :: Double) result
    , testCase "should transfer 100" $
        withChromiumPage $ \page -> do
          result <- evaluateNumberWith page "a => a" (numberArgument 100)
          assertEqual "100" (100 :: Double) result
    , testCase "should transfer -1" $
        withChromiumPage $ \page -> do
          result <- evaluateNumberWith page "a => a" (numberArgument (-1))
          assertEqual "-1" ((-1) :: Double) result
    , testCase "should transfer true" $
        withChromiumPage $ \page -> do
          result <- evaluateBoolWith page "a => a" (boolArgument True)
          assertEqual "true" True result
    , testCase "should transfer false" $
        withChromiumPage $ \page -> do
          result <- evaluateBoolWith page "a => a" (boolArgument False)
          assertEqual "false" False result
    , testCase "should transfer string" $
        withChromiumPage $ \page -> do
          result <- evaluateTextWith page "a => a" (textArgument "hello")
          assertEqual "string" ("hello" :: Text) result
    , testCase "should transfer unicode string" $
        withChromiumPage $ \page -> do
          result <- evaluateTextWith page "a => a" (textArgument "こんにちは")
          assertEqual "unicode" ("こんにちは" :: Text) result
    , testCase "should return undefined for empty function" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecial page "() => {}"
          assertEqual "undefined" SpecialUndefined special
    , testCase "should transfer arrays" $
        withChromiumPage $ \page -> do
          result <- evaluateNumbersWith page "a => a" (numbersArgument [1, 2, 3])
          assertEqual "array" [1, 2, 3 :: Double] result
    , testCase "should transfer arrays as arrays, not objects" $
        withChromiumPage $ \page -> do
          result <- evaluateBoolWith page "a => Array.isArray(a)" (numbersArgument [1, 2, 3])
          assertEqual "isArray" True result
    , testCase "should modify global environment" $
        withChromiumPage $ \page -> do
          _ <- evaluateNumber page "() => { window.globalVar = 123; return 123; }"
          result <- evaluateNumber page "() => window.globalVar"
          assertEqual "global" (123 :: Double) result
    , testCase "should evaluate in the page context" $
        withAssetPage "global-var.html" $ \page -> do
          result <- evaluateNumber page "() => window.globalVar"
          assertEqual "global" (123 :: Double) result
    ]

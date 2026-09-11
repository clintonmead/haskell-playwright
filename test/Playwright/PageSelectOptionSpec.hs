module Playwright.PageSelectOptionSpec
  ( tests
  ) where

import Playwright
  ( SelectOption (SelectLabel, SelectValue, SelectValueOrLabel)
  , evaluateTexts
  , selectOption
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
    "page-select-option"
    [ testCase "should select single option" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- selectOption (locate page "select") [SelectValueOrLabel "blue"]
          assertEqual "input" ["blue"] =<< evaluateTexts page "() => window.result.onInput"
          assertEqual "change" ["blue"] =<< evaluateTexts page "() => window.result.onChange"
    , testCase "should select single option by value" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- selectOption (locate page "select") [SelectValue "blue"]
          assertEqual "input" ["blue"] =<< evaluateTexts page "() => window.result.onInput"
    , testCase "should fall back to selecting by label" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- selectOption (locate page "select") [SelectValueOrLabel "Blue"]
          assertEqual "input" ["blue"] =<< evaluateTexts page "() => window.result.onInput"
    , testCase "should select single option by label" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- selectOption (locate page "select") [SelectLabel "Indigo"]
          assertEqual "input" ["indigo"] =<< evaluateTexts page "() => window.result.onInput"
    ]

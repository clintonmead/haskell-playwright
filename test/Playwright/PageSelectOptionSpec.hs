module Playwright.PageSelectOptionSpec
  ( tests
  ) where

import Playwright
  ( SelectOption (SelectIndex, SelectLabel, SelectValue, SelectValueOrLabel)
  , evaluate
  , evaluateTexts
  , selectOption
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
  , withAssetPage
  , assertEqual
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
    , testCase "should select single option by index" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- selectOption (locate page "select") [SelectIndex 2]
          assertEqual "input" ["brown"] =<< evaluateTexts page "() => window.result.onInput"
    , testCase "should select only first option" $
        withAssetPage "input/select.html" $ \page -> do
          _ <-
            selectOption
              (locate page "select")
              [SelectValueOrLabel "blue", SelectValueOrLabel "green", SelectValueOrLabel "red"]
          assertEqual "input" ["blue"] =<< evaluateTexts page "() => window.result.onInput"
    , testCase "should select multiple options" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- evaluate page "() => window.makeMultiple()"
          _ <-
            selectOption
              (locate page "select")
              [SelectValueOrLabel "blue", SelectValueOrLabel "green", SelectValueOrLabel "red"]
          assertEqual "input" ["blue", "green", "red"] =<< evaluateTexts page "() => window.result.onInput"
    , testCase "should throw when element is not a select" $
        withAssetPage "input/select.html" $ \page ->
          assertProtocolContains
            "Element is not a <select> element"
            (selectOption (locate page "body") [SelectValueOrLabel ""])
    , testCase "should return empty list on no matched values" $
        withAssetPage "input/select.html" $ \page -> do
          selected <- selectOption (locate page "select") []
          assertEqual "selected" [] selected
    ]

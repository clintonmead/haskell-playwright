module Playwright.LocatorMiscSpec
  ( tests
  ) where

import Playwright
  ( check
  , clear
  , evaluate
  , evaluateBool
  , evaluateText
  , fill
  , hover
  , setChecked
  , setContent
  , uncheck
  )
import Playwright.Harness
  ( locate
  , withAssetPage
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
    "locator-misc"
    [ testCase "should hover" $
        withAssetPage "input/scrollable.html" $ \page -> do
          hover (locate page "#button-6")
          assertEqual "hovered" "button-6" =<< evaluateText page "() => document.querySelector('button:hover').id"
    , testCase "should hover when Node is removed" $
        withAssetPage "input/scrollable.html" $ \page -> do
          _ <- evaluate page "() => { delete window.Node; }"
          hover (locate page "#button-6")
          assertEqual "hovered" "button-6" =<< evaluateText page "() => document.querySelector('button:hover').id"
    , testCase "should fill input" $
        withAssetPage "input/textarea.html" $ \page -> do
          fill (locate page "input") "some value"
          assertEqual "filled" "some value" =<< evaluateText page "() => window.result"
    , testCase "should fill input when Node is removed" $
        withAssetPage "input/textarea.html" $ \page -> do
          _ <- evaluate page "() => { delete window.Node; }"
          fill (locate page "input") "some value"
          assertEqual "filled" "some value" =<< evaluateText page "() => window.result"
    , testCase "should clear input" $
        withAssetPage "input/textarea.html" $ \page -> do
          fill (locate page "input") "some value"
          assertEqual "filled" "some value" =<< evaluateText page "() => window.result"
          clear (locate page "input")
          assertEqual "cleared" "" =<< evaluateText page "() => window.result"
    , testCase "should check the box" $
        withChromiumPage $ \page -> do
          setContent page "<input id='checkbox' type='checkbox'></input>"
          check (locate page "input")
          assertEqual "checked" True =<< evaluateBool page "() => checkbox.checked"
    , testCase "should check the box using setChecked" $
        withChromiumPage $ \page -> do
          setContent page "<input id='checkbox' type='checkbox'></input>"
          setChecked (locate page "input") True
          assertEqual "checked" True =<< evaluateBool page "() => checkbox.checked"
          setChecked (locate page "input") False
          assertEqual "unchecked" False =<< evaluateBool page "() => checkbox.checked"
    , testCase "should uncheck the box" $
        withChromiumPage $ \page -> do
          setContent page "<input id='checkbox' type='checkbox' checked></input>"
          uncheck (locate page "input")
          assertEqual "unchecked" False =<< evaluateBool page "() => checkbox.checked"
    ]

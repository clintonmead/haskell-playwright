module Playwright.ElementHandleMiscSpec
  ( tests
  ) where

import Playwright
  ( SelectOption (SelectValueOrLabel)
  , checkHandle
  , disposeHandle
  , elementArgument
  , evaluate
  , evaluateBool
  , evaluateBoolWith
  , evaluateText
  , evaluateTexts
  , fillHandle
  , focusHandle
  , hoverHandle
  , querySelector
  , selectorFromText
  , selectOptionHandle
  , setCheckedHandle
  , setContent
  , uncheckHandle
  )
import Playwright.Harness
  ( requireHandle
  , withAssetPage
  , withChromiumPage
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
    "elementhandle-misc"
    [ testCase "should hover" $
        withAssetPage "input/scrollable.html" $ \page -> do
          button <- requireHandle =<< querySelector page (selectorFromText "#button-6")
          hoverHandle button
          assertEqual "hovered" "button-6" =<< evaluateText page "() => document.querySelector('button:hover').id"
    , testCase "should hover when Node is removed" $
        withAssetPage "input/scrollable.html" $ \page -> do
          _ <- evaluate page "() => { delete window.Node; }"
          button <- requireHandle =<< querySelector page (selectorFromText "#button-6")
          hoverHandle button
          assertEqual "hovered" "button-6" =<< evaluateText page "() => document.querySelector('button:hover').id"
    , testCase "should fill input when Node is removed" $
        withAssetPage "input/textarea.html" $ \page -> do
          _ <- evaluate page "() => { delete window.Node; }"
          handle <- requireHandle =<< querySelector page (selectorFromText "input")
          fillHandle handle "some value"
          assertEqual "filled" "some value" =<< evaluateText page "() => window.result"
    , testCase "should check the box" $
        withChromiumPage $ \page -> do
          setContent page "<input id='checkbox' type='checkbox'></input>"
          input <- requireHandle =<< querySelector page (selectorFromText "input")
          checkHandle input
          assertEqual "checked" True =<< evaluateBool page "() => checkbox.checked"
    , testCase "should check the box using setChecked" $
        withChromiumPage $ \page -> do
          setContent page "<input id='checkbox' type='checkbox'></input>"
          input <- requireHandle =<< querySelector page (selectorFromText "input")
          setCheckedHandle input True
          assertEqual "checked" True =<< evaluateBool page "() => checkbox.checked"
          setCheckedHandle input False
          assertEqual "unchecked" False =<< evaluateBool page "() => checkbox.checked"
    , testCase "should uncheck the box" $
        withChromiumPage $ \page -> do
          setContent page "<input id='checkbox' type='checkbox' checked></input>"
          input <- requireHandle =<< querySelector page (selectorFromText "input")
          uncheckHandle input
          assertEqual "unchecked" False =<< evaluateBool page "() => checkbox.checked"
    , testCase "should select single option" $
        withAssetPage "input/select.html" $ \page -> do
          select <- requireHandle =<< querySelector page (selectorFromText "select")
          _ <- selectOptionHandle select [SelectValueOrLabel "blue"]
          assertEqual "input" ["blue"] =<< evaluateTexts page "() => window.result.onInput"
          assertEqual "change" ["blue"] =<< evaluateTexts page "() => window.result.onChange"
    , testCase "should focus a button" $
        withAssetPage "input/button.html" $ \page -> do
          button <- requireHandle =<< querySelector page (selectorFromText "button")
          assertEqual "not focused" False
            =<< evaluateBoolWith page "e => document.activeElement === e" (elementArgument button)
          focusHandle button
          assertEqual "focused" True
            =<< evaluateBoolWith page "e => document.activeElement === e" (elementArgument button)
    , testCase "should allow disposing twice" $
        withChromiumPage $ \page -> do
          setContent page "<section>39</section>"
          element <- requireHandle =<< querySelector page (selectorFromText "section")
          disposeHandle element
          disposeHandle element
    ]

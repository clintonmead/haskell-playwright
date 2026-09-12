module Playwright.LocatorMiscSpec
  ( tests
  ) where

import Data.IORef
  ( newIORef
  , readIORef
  , writeIORef
  )
import Playwright
  ( BindingName (BindingName)
  , DomEventType (DomEventType)
  , SelectOption (SelectValueOrLabel)
  , blur
  , check
  , clear
  , dispatchEvent
  , evaluate
  , evaluateBool
  , evaluateOnLocator
  , evaluateOnLocatorBool
  , evaluateText
  , evaluateTexts
  , exposeFunction
  , fill
  , focus
  , hover
  , selectOption
  , setChecked
  , setContent
  , uncheck
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
  , withAssetPage
  , withChromiumPage
  )
import Playwright.SerializedArgument
  ( undefinedArgument
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
    , testCase "should select single option" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- selectOption (locate page "select") [SelectValueOrLabel "blue"]
          assertEqual "input" ["blue"] =<< evaluateTexts page "() => window.result.onInput"
          assertEqual "change" ["blue"] =<< evaluateTexts page "() => window.result.onChange"
    , testCase "should focus and blur a button" $
        withAssetPage "input/button.html" $ \page -> do
          focused <- newIORef False
          blurred <- newIORef False
          _ <-
            exposeFunction page (BindingName "focusEvent") $ \_ -> do
              writeIORef focused True
              pure undefinedArgument
          _ <-
            exposeFunction page (BindingName "blurEvent") $ \_ -> do
              writeIORef blurred True
              pure undefinedArgument
          let
            button = locate page "button"
          assertEqual "not focused" False =<< evaluateOnLocatorBool button "button => document.activeElement === button"
          _ <-
            evaluateOnLocator
              button
              "button => { button.addEventListener('focus', window.focusEvent); button.addEventListener('blur', window.blurEvent); }"
          focus button
          assertEqual "focused flag" True =<< readIORef focused
          assertEqual "blurred flag" False =<< readIORef blurred
          assertEqual "focused" True =<< evaluateOnLocatorBool button "button => document.activeElement === button"
          blur button
          assertEqual "still focused flag" True =<< readIORef focused
          assertEqual "blurred" True =<< readIORef blurred
          assertEqual "not focused after blur" False =<< evaluateOnLocatorBool button "button => document.activeElement === button"
    , testCase "focus should respect strictness" $
        withChromiumPage $ \page -> do
          setContent page "<div>A</div><div>B</div>"
          assertProtocolContains "strict mode violation" (focus (locate page "div"))
    , testCase "should dispatch click event via ElementHandles" $
        withAssetPage "input/button.html" $ \page -> do
          dispatchEvent (locate page "button") (DomEventType "click")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    ]

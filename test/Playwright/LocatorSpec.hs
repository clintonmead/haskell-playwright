module Playwright.LocatorSpec
  ( tests
  ) where

import Playwright
  ( childLocator
  , click
  , evaluateText
  , fill
  , getAttribute
  , innerHtml
  , innerText
  , inputValue
  , isHidden
  , isVisible
  , locator
  , selectorFromText
  , setContent
  , textContent
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
    "locator"
    [ testCase "click button from official fixtures" $
        withAssetPage "input/button.html" $ \page -> do
          click (locator page (selectorFromText "button"))
          result <- evaluateText page "() => window.result"
          assertEqual "clicked" "Clicked" result
    , testCase "fill textarea from official fixtures" $
        withAssetPage "input/textarea.html" $ \page -> do
          fill (locator page (selectorFromText "textarea")) "some value"
          result <- evaluateText page "() => window.result"
          assertEqual "filled" "some value" result
    , testCase "getAttribute innerText textContent innerHTML" $
        withAssetPage "dom.html" $ \page -> do
          let
            outer = locator page (selectorFromText "#outer")
            inner = childLocator outer (selectorFromText "#inner")
          name <- getAttribute outer "name"
          missing <- getAttribute outer "foo"
          assertEqual "name" (Just "value") name
          assertEqual "missing" Nothing missing
          innerTextValue <- innerText inner
          assertEqual "innerText" "Text, more text" innerTextValue
          text <- textContent inner
          assertEqual "textContent" (Just "Text,\nmore text") text
          html <- innerHtml outer
          assertEqual "innerHTML" "<div id=\"inner\">Text,\nmore text</div>" html
    , testCase "fill inputValue" $
        withAssetPage "dom.html" $ \page -> do
          let
            input = locator page (selectorFromText "#input")
          fill input "input value"
          value <- inputValue input
          assertEqual "inputValue" "input value" value
    , testCase "isVisible and isHidden" $
        withChromiumPage $ \page -> do
          setContent page "<div>Hi</div><span></span>"
          let
            divLoc = locator page (selectorFromText "div")
            spanLoc = locator page (selectorFromText "span")
            missing = locator page (selectorFromText "no-such-element")
          assertEqual "div visible" True =<< isVisible divLoc
          assertEqual "div hidden" False =<< isHidden divLoc
          assertEqual "span visible" False =<< isVisible spanLoc
          assertEqual "span hidden" True =<< isHidden spanLoc
          assertEqual "missing visible" False =<< isVisible missing
          assertEqual "missing hidden" True =<< isHidden missing
    ]

module Playwright.LocatorSpec
  ( tests
  ) where

import Playwright
  ( childLocator
  , click
  , evaluateOnLocator
  , evaluateText
  , fill
  , getAttribute
  , innerHtml
  , innerText
  , inputValue
  , isChecked
  , isDisabled
  , isEditable
  , isEnabled
  , isHidden
  , isVisible
  , locator
  , selectorFromText
  , setContent
  , textContent
  , uncheck
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
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
            outer = locate page "#outer"
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
            input = locate page "#input"
          fill input "input value"
          value <- inputValue input
          assertEqual "inputValue" "input value" value
    , testCase "inputValue should throw on non-input" $
        withAssetPage "dom.html" $ \page ->
          assertProtocolContains
            "Node is not an <input>, <textarea> or <select> element"
            (inputValue (locate page "#inner"))
    , testCase "innerText should throw" $
        withChromiumPage $ \page -> do
          setContent page "<svg>text</svg>"
          assertProtocolContains "Node is not an HTMLElement" (innerText (locate page "svg"))
    , testCase "isVisible and isHidden" $
        withChromiumPage $ \page -> do
          setContent page "<div>Hi</div><span></span>"
          let
            divLoc = locate page "div"
            spanLoc = locate page "span"
            missing = locate page "no-such-element"
          assertEqual "div visible" True =<< isVisible divLoc
          assertEqual "div hidden" False =<< isHidden divLoc
          assertEqual "span visible" False =<< isVisible spanLoc
          assertEqual "span hidden" True =<< isHidden spanLoc
          assertEqual "missing visible" False =<< isVisible missing
          assertEqual "missing hidden" True =<< isHidden missing
    , testCase "isEnabled and isDisabled should work" $
        withChromiumPage $ \page -> do
          setContent page "<button disabled>button1</button><button>button2</button><div>div</div>"
          let
            divLoc = locate page "div"
            button1 = locate page ":text(\"button1\")"
            button2 = locate page ":text(\"button2\")"
          assertEqual "div enabled" True =<< isEnabled divLoc
          assertEqual "div disabled" False =<< isDisabled divLoc
          assertEqual "button1 enabled" False =<< isEnabled button1
          assertEqual "button1 disabled" True =<< isDisabled button1
          assertEqual "button2 enabled" True =<< isEnabled button2
          assertEqual "button2 disabled" False =<< isDisabled button2
    , testCase "isEditable should work" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<input id=input1 disabled><textarea></textarea><input id=input2><div contenteditable=\"true\"></div><span id=span1 role=textbox aria-readonly=true></span><span id=span2 role=textbox></span><button>button</button>"
          _ <- evaluateOnLocator (locate page "textarea") "t => { t.readOnly = true; }"
          assertEqual "input1" False =<< isEditable (locate page "#input1")
          assertEqual "input2" True =<< isEditable (locate page "#input2")
          assertEqual "textarea" False =<< isEditable (locate page "textarea")
          assertEqual "div" True =<< isEditable (locate page "div")
          assertEqual "span1" False =<< isEditable (locate page "#span1")
          assertEqual "span2" True =<< isEditable (locate page "#span2")
          assertProtocolContains
            "Element is not an <input>, <textarea>, <select> or [contenteditable] and does not have a role allowing [aria-readonly]"
            (isEditable (locate page "button"))
    , testCase "isChecked should work" $
        withChromiumPage $ \page -> do
          setContent page "<input type='checkbox' checked><div>Not a checkbox</div>"
          let
            element = locate page "input"
          assertEqual "checked" True =<< isChecked element
          _ <- evaluateOnLocator element "input => { input.checked = false; }"
          assertEqual "unchecked" False =<< isChecked element
          assertProtocolContains "Not a checkbox or radio button" (isChecked (locate page "div"))
    , testCase "isChecked should work for indeterminate input" $
        withChromiumPage $ \page -> do
          setContent page "<input type=\"checkbox\" checked>"
          _ <- evaluateOnLocator (locate page "input") "e => { e.indeterminate = true; }"
          assertEqual "indeterminate still checked" True =<< isChecked (locate page "input")
          uncheck (locate page "input")
          assertEqual "after uncheck" False =<< isChecked (locate page "input")
    ]

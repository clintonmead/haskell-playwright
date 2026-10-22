module Playwright.ElementHandleConvenienceSpec
  ( tests
  ) where

import Playwright
  ( SelectOption (SelectValue)
  , evaluate
  , fill
  , isChecked
  , isEditable
  , isEditableHandle
  , isDisabled
  , isDisabledHandle
  , isEnabled
  , isEnabledHandle
  , isHidden
  , isHiddenHandle
  , isVisible
  , isVisibleHandle
  , getAttribute
  , getAttributeHandle
  , innerHtml
  , innerHtmlHandle
  , innerText
  , innerTextHandle
  , inputValue
  , inputValueHandle
  , querySelector
  , selectOption
  , selectorFromText
  , setContent
  , textContent
  , textContentHandle
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
  , requireHandle
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
    "elementhandle-convenience"
    [ testCase "getAttribute should work" $
        withAssetPage "dom.html" $ \page -> do
          handle <- requireHandle =<< querySelector page (selectorFromText "#outer")
          assertEqual "handle name" (Just "value") =<< getAttributeHandle handle "name"
          assertEqual "handle missing" Nothing =<< getAttributeHandle handle "foo"
          assertEqual "page name" (Just "value") =<< getAttribute (locate page "#outer") "name"
          assertEqual "page missing" Nothing =<< getAttribute (locate page "#outer") "foo"
    , testCase "inputValue should work" $
        withAssetPage "dom.html" $ \page -> do
          _ <- selectOption (locate page "#select") [SelectValue "foo"]
          assertEqual "select" "foo" =<< inputValue (locate page "#select")
          fill (locate page "#textarea") "text value"
          assertEqual "textarea" "text value" =<< inputValue (locate page "#textarea")
          fill (locate page "#input") "input value"
          assertEqual "input" "input value" =<< inputValue (locate page "#input")
          handle <- requireHandle =<< querySelector page (selectorFromText "#input")
          assertEqual "handle" "input value" =<< inputValueHandle handle
          assertProtocolContains
            "Node is not an <input>, <textarea> or <select> element"
            (inputValue (locate page "#inner"))
    , testCase "innerHTML should work" $
        withAssetPage "dom.html" $ \page -> do
          handle <- requireHandle =<< querySelector page (selectorFromText "#outer")
          assertEqual
            "handle"
            "<div id=\"inner\">Text,\nmore text</div>"
            =<< innerHtmlHandle handle
          assertEqual
            "page"
            "<div id=\"inner\">Text,\nmore text</div>"
            =<< innerHtml (locate page "#outer")
    , testCase "innerText should work" $
        withAssetPage "dom.html" $ \page -> do
          handle <- requireHandle =<< querySelector page (selectorFromText "#inner")
          assertEqual "handle" "Text, more text" =<< innerTextHandle handle
          assertEqual "page" "Text, more text" =<< innerText (locate page "#inner")
    , testCase "isEditable should work" $
        withChromiumPage $ \page -> do
          setContent page "<input id=input1 disabled><textarea></textarea><input id=input2>"
          _ <- evaluate page "() => { document.querySelector('textarea').readOnly = true; }"
          input1 <- requireHandle =<< querySelector page (selectorFromText "#input1")
          assertEqual "input1 handle" False =<< isEditableHandle input1
          assertEqual "input1 page" False =<< isEditable (locate page "#input1")
          input2 <- requireHandle =<< querySelector page (selectorFromText "#input2")
          assertEqual "input2 handle" True =<< isEditableHandle input2
          assertEqual "input2 page" True =<< isEditable (locate page "#input2")
          textarea <- requireHandle =<< querySelector page (selectorFromText "textarea")
          assertEqual "textarea handle" False =<< isEditableHandle textarea
          assertEqual "textarea page" False =<< isEditable (locate page "textarea")
    , testCase "isChecked should work" $
        withChromiumPage $ \page -> do
          setContent page "<input type='checkbox' checked><div>Not a checkbox</div>"
          assertEqual "checked" True =<< isChecked (locate page "input")
          _ <- evaluate page "() => { document.querySelector('input').checked = false; }"
          assertEqual "unchecked" False =<< isChecked (locate page "input")
          assertProtocolContains "Not a checkbox or radio button" (isChecked (locate page "div"))
    , testCase "isVisible and isHidden should work" $
        withChromiumPage $ \page -> do
          setContent page "<div>Hi</div><span></span>"
          divHandle <- requireHandle =<< querySelector page (selectorFromText "div")
          assertEqual "div visible" True =<< isVisibleHandle divHandle
          assertEqual "div hidden" False =<< isHiddenHandle divHandle
          assertEqual "div locator visible" True =<< isVisible (locate page "div")
          spanHandle <- requireHandle =<< querySelector page (selectorFromText "span")
          assertEqual "span visible" False =<< isVisibleHandle spanHandle
          assertEqual "span hidden" True =<< isHiddenHandle spanHandle
          assertEqual "span locator hidden" True =<< isHidden (locate page "span")
    , testCase "isEnabled and isDisabled should work" $
        withChromiumPage $ \page -> do
          setContent page "<button disabled>button1</button><button>button2</button><div>div</div>"
          divHandle <- requireHandle =<< querySelector page (selectorFromText "div")
          assertEqual "div enabled" True =<< isEnabledHandle divHandle
          assertEqual "div disabled" False =<< isDisabledHandle divHandle
          assertEqual "div locator enabled" True =<< isEnabled (locate page "div")
          assertEqual "div locator disabled" False =<< isDisabled (locate page "div")
          button1 <- requireHandle =<< querySelector page (selectorFromText ":text(\"button1\")")
          assertEqual "button1 enabled" False =<< isEnabledHandle button1
          assertEqual "button1 disabled" True =<< isDisabledHandle button1
          assertEqual "button1 locator enabled" False =<< isEnabled (locate page ":text(\"button1\")")
          button2 <- requireHandle =<< querySelector page (selectorFromText ":text(\"button2\")")
          assertEqual "button2 enabled" True =<< isEnabledHandle button2
          assertEqual "button2 disabled" False =<< isDisabledHandle button2
    , testCase "textContent should work" $
        withAssetPage "dom.html" $ \page -> do
          handle <- requireHandle =<< querySelector page (selectorFromText "#inner")
          assertEqual "handle" (Just "Text,\nmore text") =<< textContentHandle handle
          assertEqual "locator" (Just "Text,\nmore text") =<< textContent (locate page "#inner")
    ]

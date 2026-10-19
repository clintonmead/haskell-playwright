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
    ]

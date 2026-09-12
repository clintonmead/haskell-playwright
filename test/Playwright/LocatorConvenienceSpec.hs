module Playwright.LocatorConvenienceSpec
  ( tests
  ) where

import Playwright
  ( AriaRole (AriaRole)
  , GetByRoleOptions (getByRoleName)
  , SelectOption (SelectValue)
  , allInnerTexts
  , allTextContents
  , childLocator
  , defaultGetByRoleOptions
  , describeLocator
  , evaluateHandleOnLocator
  , evaluateOnLocator
  , fill
  , frames
  , getAttribute
  , getByRole
  , innerHtml
  , innerText
  , innerTextWith
  , inputValue
  , isChecked
  , isDisabled
  , isEditable
  , isEnabled
  , jsHandleToString
  , locatorDescription
  , locatorOnFrame
  , locatorPage
  , milliseconds
  , pageGetAttribute
  , pageInnerHtml
  , pageInnerText
  , pageInputValue
  , pageIsChecked
  , pageIsDisabled
  , pageIsEditable
  , pageIsEnabled
  , pageTextContent
  , pagesEqual
  , selectorFromText
  , selectOption
  , setContent
  , textContent
  , uncheck
  , waitUntilJsHandlePreview
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
  , withAssetPage
  , withChromiumPage
  , assertEqual
  , assertFailure
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
    "locator-convenience"
    [ testCase "should have a nice preview" $
        withAssetPage "dom.html" $ \page -> do
          let
            inner = childLocator (locate page "#outer") (selectorFromText "#inner")
          textHandle <- evaluateHandleOnLocator inner "e => e.firstChild"
          preview <- waitUntilJsHandlePreview textHandle (== "JSHandle@#text=Text,\x21B5more text")
          assertEqual "text node" "JSHandle@#text=Text,\x21B5more text" preview
          assertEqual "toString" "JSHandle@#text=Text,\x21B5more text" =<< jsHandleToString textHandle
    , testCase "getAttribute should work" $
        withAssetPage "dom.html" $ \page -> do
          let
            loc = locate page "#outer"
          assertEqual "name" (Just "value") =<< getAttribute loc "name"
          assertEqual "missing" Nothing =<< getAttribute loc "foo"
          assertEqual "page name" (Just "value") =<< pageGetAttribute page (selectorFromText "#outer") "name"
          assertEqual "page missing" Nothing =<< pageGetAttribute page (selectorFromText "#outer") "foo"
    , testCase "inputValue should work" $
        withAssetPage "dom.html" $ \page -> do
          _ <- selectOption (locate page "#select") [SelectValue "foo"]
          assertEqual "select" "foo" =<< pageInputValue page (selectorFromText "#select")
          fill (locate page "#textarea") "text value"
          assertEqual "textarea" "text value" =<< pageInputValue page (selectorFromText "#textarea")
          fill (locate page "#input") "input value"
          assertEqual "input" "input value" =<< pageInputValue page (selectorFromText "#input")
          assertEqual "locator" "input value" =<< inputValue (locate page "#input")
          assertProtocolContains
            "Node is not an <input>, <textarea> or <select> element"
            (pageInputValue page (selectorFromText "#inner"))
          assertProtocolContains
            "Node is not an <input>, <textarea> or <select> element"
            (inputValue (locate page "#inner"))
    , testCase "innerHTML should work" $
        withAssetPage "dom.html" $ \page -> do
          assertEqual "locator" "<div id=\"inner\">Text,\nmore text</div>" =<< innerHtml (locate page "#outer")
          assertEqual "page" "<div id=\"inner\">Text,\nmore text</div>" =<< pageInnerHtml page (selectorFromText "#outer")
    , testCase "innerText should work" $
        withAssetPage "dom.html" $ \page -> do
          assertEqual "locator" "Text, more text" =<< innerText (locate page "#inner")
          assertEqual "page" "Text, more text" =<< pageInnerText page (selectorFromText "#inner")
    , testCase "innerText should throw" $
        withChromiumPage $ \page -> do
          setContent page "<svg>text</svg>"
          assertProtocolContains "Node is not an HTMLElement" (pageInnerText page (selectorFromText "svg"))
          assertProtocolContains "Node is not an HTMLElement" (innerText (locate page "svg"))
    , testCase "innerText should produce log" $
        withChromiumPage $ \page -> do
          setContent page "<div>Hello</div>"
          case milliseconds 1000 of
            Nothing ->
              assertFailure "milliseconds 1000"
            Just timeout ->
              assertProtocolContains
                "Timeout 1000ms exceeded"
                (innerTextWith (locate page "span") timeout)
    , testCase "textContent should work" $
        withAssetPage "dom.html" $ \page -> do
          assertEqual "locator" (Just "Text,\nmore text") =<< textContent (locate page "#inner")
          assertEqual "page" (Just "Text,\nmore text") =<< pageTextContent page (selectorFromText "#inner")
    , testCase "isEnabled and isDisabled should work" $
        withChromiumPage $ \page -> do
          setContent page "<button disabled>button1</button><button>button2</button><div>div</div>"
          assertEqual "div enabled" True =<< isEnabled (locate page "div")
          assertEqual "div disabled" False =<< isDisabled (locate page "div")
          assertEqual "page div enabled" True =<< pageIsEnabled page (selectorFromText "div")
          assertEqual "page div disabled" False =<< pageIsDisabled page (selectorFromText "div")
          assertEqual "button1 enabled" False =<< isEnabled (locate page ":text(\"button1\")")
          assertEqual "button1 disabled" True =<< isDisabled (locate page ":text(\"button1\")")
          assertEqual "page button1 enabled" False =<< pageIsEnabled page (selectorFromText ":text(\"button1\")")
          assertEqual "page button1 disabled" True =<< pageIsDisabled page (selectorFromText ":text(\"button1\")")
          assertEqual "button2 enabled" True =<< isEnabled (locate page ":text(\"button2\")")
          assertEqual "button2 disabled" False =<< isDisabled (locate page ":text(\"button2\")")
          assertEqual "page button2 enabled" True =<< pageIsEnabled page (selectorFromText ":text(\"button2\")")
          assertEqual "page button2 disabled" False =<< pageIsDisabled page (selectorFromText ":text(\"button2\")")
    , testCase "isEditable should work" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<input id=input1 disabled><textarea></textarea><input id=input2><div contenteditable=\"true\"></div><span id=span1 role=textbox aria-readonly=true></span><span id=span2 role=textbox></span><button>button</button>"
          _ <- evaluateOnLocator (locate page "textarea") "t => { t.readOnly = true; }"
          assertEqual "input1" False =<< isEditable (locate page "#input1")
          assertEqual "page input1" False =<< pageIsEditable page (selectorFromText "#input1")
          assertEqual "input2" True =<< isEditable (locate page "#input2")
          assertEqual "page input2" True =<< pageIsEditable page (selectorFromText "#input2")
          assertEqual "textarea" False =<< isEditable (locate page "textarea")
          assertEqual "page textarea" False =<< pageIsEditable page (selectorFromText "textarea")
          assertEqual "div" True =<< isEditable (locate page "div")
          assertEqual "span1" False =<< isEditable (locate page "#span1")
          assertEqual "span2" True =<< isEditable (locate page "#span2")
          assertProtocolContains
            "Element is not an <input>, <textarea>, <select> or [contenteditable] and does not have a role allowing [aria-readonly]"
            (isEditable (locate page "button"))
    , testCase "isChecked should work" $
        withChromiumPage $ \page -> do
          setContent page "<input type='checkbox' checked><div>Not a checkbox</div>"
          assertEqual "checked" True =<< isChecked (locate page "input")
          assertEqual "page checked" True =<< pageIsChecked page (selectorFromText "input")
          _ <- evaluateOnLocator (locate page "input") "input => { input.checked = false; }"
          assertEqual "unchecked" False =<< isChecked (locate page "input")
          assertEqual "page unchecked" False =<< pageIsChecked page (selectorFromText "input")
          assertProtocolContains "Not a checkbox or radio button" (pageIsChecked page (selectorFromText "div"))
    , testCase "isChecked should work for indeterminate input" $
        withChromiumPage $ \page -> do
          setContent page "<input type=\"checkbox\" checked>"
          _ <- evaluateOnLocator (locate page "input") "e => { e.indeterminate = true; }"
          assertEqual "indeterminate" True =<< isChecked (locate page "input")
          uncheck (locate page "input")
          assertEqual "after uncheck" False =<< isChecked (locate page "input")
    , testCase "allTextContents should work" $
        withChromiumPage $ \page -> do
          setContent page "<div>A</div><div>B</div><div>C</div>"
          assertEqual "texts" ["A", "B", "C"] =<< allTextContents (locate page "div")
    , testCase "allInnerTexts should work" $
        withChromiumPage $ \page -> do
          setContent page "<div>A</div><div>B</div><div>C</div>"
          assertEqual "texts" ["A", "B", "C"] =<< allInnerTexts (locate page "div")
    , testCase "should return page" $
        withAssetPage "frames/two-frames.html" $ \page -> do
          assertEqual "outer" True (pagesEqual page (locatorPage (locate page "#outer")))
          assertEqual "inner" True (pagesEqual page (locatorPage (childLocator (locate page "#outer") (selectorFromText "#inner"))))
          listed <- frames page
          case listed of
            (_ : frame : _) ->
              assertEqual
                "in frame"
                True
                (pagesEqual page (locatorPage (locatorOnFrame page frame (selectorFromText "div"))))
            _ ->
              assertFailure "expected a subframe"
    , testCase "description should return null for locator without description" $
        withChromiumPage $ \page ->
          assertEqual "none" Nothing (locatorDescription (locate page "button"))
    , testCase "description should return description for locator with simple description" $
        withChromiumPage $ \page ->
          assertEqual "simple" (Just "Submit button") (locatorDescription (describeLocator (locate page "button") "Submit button"))
    , testCase "description should return description with special characters" $
        withChromiumPage $ \page ->
          assertEqual
            "special"
            (Just "Button with \"quotes\" and 'apostrophes'")
            (locatorDescription (describeLocator (locate page "div") "Button with \"quotes\" and 'apostrophes'"))
    , testCase "description should return description for chained locators" $
        withChromiumPage $ \page ->
          assertEqual
            "chained"
            (Just "Form input field")
            (locatorDescription (describeLocator (childLocator (locate page "form") (selectorFromText "input")) "Form input field"))
    , testCase "description should return description for locator with multiple describe calls" $
        withChromiumPage $ \page -> do
          let
            locator1 = describeLocator (locate page "foo") "First description"
            locator2 = describeLocator (childLocator locator1 (selectorFromText "button")) "Second description"
            locator3 = childLocator locator2 (selectorFromText "button")
          assertEqual "first" (Just "First description") (locatorDescription locator1)
          assertEqual "second" (Just "Second description") (locatorDescription locator2)
          assertEqual "cleared" Nothing (locatorDescription locator3)
    , testCase "toString prefers description" $
        withChromiumPage $ \page -> do
          let
            loc =
              describeLocator
                (getByRole page (AriaRole "button") (defaultGetByRoleOptions {getByRoleName = Just "Submit"}))
                "Submit button"
          assertEqual "description" (Just "Submit button") (locatorDescription loc)
    ]

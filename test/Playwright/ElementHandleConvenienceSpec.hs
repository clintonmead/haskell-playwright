module Playwright.ElementHandleConvenienceSpec
  ( tests
  ) where

import Playwright
  ( SelectOption (SelectValue)
  , fill
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
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
  , requireHandle
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
    ]

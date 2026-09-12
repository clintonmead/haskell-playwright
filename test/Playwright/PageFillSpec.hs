module Playwright.PageFillSpec
  ( tests
  ) where

import Data.Text
  ( Text
  , unpack
  )
import Playwright
  ( click
  , evaluateOnLocator
  , evaluateOnLocatorText
  , evaluateOnLocatorWith
  , evaluateText
  , fill
  , innerText
  , setContent
  , textArgument
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
    "page-fill"
    ( [ testCase "should fill textarea" $
          withAssetPage "input/textarea.html" $ \page -> do
            fill (locate page "textarea") "some value"
            assertEqual "result" "some value" =<< evaluateText page "() => window.result"
      , testCase "should fill input" $
          withAssetPage "input/textarea.html" $ \page -> do
            fill (locate page "input") "some value"
            assertEqual "result" "some value" =<< evaluateText page "() => window.result"
      , testCase "should fill range input" $
          withChromiumPage $ \page -> do
            setContent page "<input type=range min=0 max=100 value=50>"
            fill (locate page "input") "42"
            assertEqual "value" "42" =<< evaluateOnLocatorText (locate page "input") "input => input.value"
      , testCase "should throw on incorrect range value" $
          withChromiumPage $ \page -> do
            setContent page "<input type=range min=0 max=100 value=50>"
            assertProtocolContains "Malformed value" (fill (locate page "input") "foo")
            assertProtocolContains "Malformed value" (fill (locate page "input") "200")
            assertProtocolContains "Malformed value" (fill (locate page "input") "15.43")
      , testCase "should fill color input" $
          withChromiumPage $ \page -> do
            setContent page "<input type=color value=\"#e66465\">"
            fill (locate page "input") "#aaaaaa"
            assertEqual "value" "#aaaaaa" =<< evaluateOnLocatorText (locate page "input") "input => input.value"
      , testCase "should throw on incorrect color value" $
          withChromiumPage $ \page -> do
            setContent page "<input type=color value=\"#e66465\">"
            assertProtocolContains "Malformed value" (fill (locate page "input") "badvalue")
      , testCase "should fill date input after clicking" $
          withChromiumPage $ \page -> do
            setContent page "<input type=date>"
            click (locate page "input")
            fill (locate page "input") "2020-03-02"
            assertEqual "value" "2020-03-02" =<< evaluateOnLocatorText (locate page "input") "input => input.value"
      , testCase "should throw on incorrect date" $
          withChromiumPage $ \page -> do
            setContent page "<input type=date>"
            assertProtocolContains "Malformed value" (fill (locate page "input") "2020-13-05")
      , testCase "should fill time input" $
          withChromiumPage $ \page -> do
            setContent page "<input type=time>"
            fill (locate page "input") "13:15"
            assertEqual "value" "13:15" =<< evaluateOnLocatorText (locate page "input") "input => input.value"
      , testCase "should fill month input" $
          withChromiumPage $ \page -> do
            setContent page "<input type=month>"
            fill (locate page "input") "2020-07"
            assertEqual "value" "2020-07" =<< evaluateOnLocatorText (locate page "input") "input => input.value"
      , testCase "should throw on incorrect month" $
          withChromiumPage $ \page -> do
            setContent page "<input type=month>"
            assertProtocolContains "Malformed value" (fill (locate page "input") "2020-13")
      , testCase "should fill week input" $
          withChromiumPage $ \page -> do
            setContent page "<input type=week>"
            fill (locate page "input") "2020-W50"
            assertEqual "value" "2020-W50" =<< evaluateOnLocatorText (locate page "input") "input => input.value"
      , testCase "should throw on incorrect week" $
          withChromiumPage $ \page -> do
            setContent page "<input type=week>"
            assertProtocolContains "Malformed value" (fill (locate page "input") "2020-123")
      , testCase "should throw on incorrect time" $
          withChromiumPage $ \page -> do
            setContent page "<input type=time>"
            assertProtocolContains "Malformed value" (fill (locate page "input") "25:05")
      , testCase "should fill datetime-local input" $
          withChromiumPage $ \page -> do
            setContent page "<input type=datetime-local>"
            fill (locate page "input") "2020-03-02T05:15"
            assertEqual "value" "2020-03-02T05:15" =<< evaluateOnLocatorText (locate page "input") "input => input.value"
      , testCase "should throw on incorrect datetime-local" $
          withChromiumPage $ \page -> do
            setContent page "<input type=datetime-local>"
            assertProtocolContains "Malformed value" (fill (locate page "input") "abc")
      , testCase "should fill contenteditable" $
          withAssetPage "input/textarea.html" $ \page -> do
            fill (locate page "div[contenteditable]") "some value"
            assertEqual "text" "some value"
              =<< evaluateOnLocatorText (locate page "div[contenteditable]") "div => div.textContent"
      , testCase "should fill contenteditable with new lines" $
          withChromiumPage $ \page -> do
            setContent page "<div contenteditable=\"true\"></div>"
            fill (locate page "div[contenteditable]") "John\nDoe"
            assertEqual "innerText" "John\nDoe" =<< innerText (locate page "div[contenteditable]")
      , testCase "should fill elements with existing value and selection" $
          withAssetPage "input/textarea.html" $ \page -> do
            _ <- evaluateOnLocator (locate page "input") "input => { input.value = 'value one'; }"
            fill (locate page "input") "another value"
            assertEqual "first" "another value" =<< evaluateText page "() => window.result"
            _ <-
              evaluateOnLocator
                (locate page "input")
                "input => { input.selectionStart = 1; input.selectionEnd = 2; }"
            fill (locate page "input") "maybe this one"
            assertEqual "second" "maybe this one" =<< evaluateText page "() => window.result"
            _ <-
              evaluateOnLocator
                (locate page "div[contenteditable]")
                "div => { div.innerHTML = 'some text <span>some more text<span> and even more text'; const range = document.createRange(); range.selectNodeContents(div.querySelector('span')); const selection = window.getSelection(); selection.removeAllRanges(); selection.addRange(range); }"
            fill (locate page "div[contenteditable]") "replace with this"
            assertEqual "contenteditable" "replace with this"
              =<< evaluateOnLocatorText (locate page "div[contenteditable]") "div => div.textContent"
      , testCase "should throw nice error when element is not an input" $
          withChromiumPage $ \page -> do
            setContent page "<select><option>value1</option></select>"
            assertProtocolContains
              "Element is not an <input>, <textarea> or [contenteditable] element"
              (fill (locate page "select") "")
      ]
        <> fmap unsupportedInput unsupportedInputTypes
        <> fmap fillInputType fillableInputTypes
    )

unsupportedInputTypes :: [Text]
unsupportedInputTypes =
  ["button", "checkbox", "file", "image", "radio", "reset", "submit"]

fillableInputTypes :: [Text]
fillableInputTypes =
  ["password", "search", "tel", "text", "url", "invalid-type"]

unsupportedInput :: Text -> TestTree
unsupportedInput inputType =
  testCase ("should throw on unsupported input " <> unpack inputType) $
    withAssetPage "input/textarea.html" $ \page -> do
      _ <-
        evaluateOnLocatorWith
          (locate page "input")
          "(input, type) => { input.setAttribute('type', type); }"
          (textArgument inputType)
      assertProtocolContains
        ("Input of type \"" <> inputType <> "\" cannot be filled")
        (fill (locate page "input") "")

fillInputType :: Text -> TestTree
fillInputType inputType =
  testCase ("should fill input type " <> unpack inputType) $
    withAssetPage "input/textarea.html" $ \page -> do
      _ <-
        evaluateOnLocatorWith
          (locate page "input")
          "(input, type) => { input.setAttribute('type', type); }"
          (textArgument inputType)
      fill (locate page "input") ("text " <> inputType)
      assertEqual "result" ("text " <> inputType) =<< evaluateText page "() => window.result"

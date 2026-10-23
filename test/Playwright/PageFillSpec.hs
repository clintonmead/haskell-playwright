module Playwright.PageFillSpec
  ( tests
  ) where

import Data.Text
  ( Text
  , isInfixOf
  , unpack
  )
import Playwright
  ( armFrameNavigated
  , click
  , defaultGotoOptions
  , evaluateOnLocator
  , evaluateOnLocatorText
  , evaluateOnLocatorWith
  , evaluateText
  , evaluateTexts
  , fill
  , focus
  , goto
  , innerText
  , setContent
  , textArgument
  , url
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( assertProtocolContains
  , assertStillPending
  , locate
  , rafraf
  , withAssetPage
  , withAssets
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
import UnliftIO.Async
  ( wait
  , withAsync
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
      , testCase "should throw nice error without injected script stack when element is not an <input>" $
          withChromiumPage $ \page -> do
            setContent page "<select><option>value1</option></select>"
            assertProtocolContains
              "Element is not an <input>, <textarea> or [contenteditable] element\nCall log:"
              (fill (locate page "select") "")
      , testCase "should fill color input case insensitive" $
          withChromiumPage $ \page -> do
            setContent page "<input type=color value=\"#e66465\">"
            fill (locate page "input") "#AbCd00"
            assertEqual "value" "#abcd00" =<< evaluateOnLocatorText (locate page "input") "input => input.value"
      , testCase "should not double-fill in contenteditable with beforeinput handler" $
          withChromiumPage $ \page -> do
            setContent
              page
              "\n    <div id=\"editor\" contenteditable=\"true\"></div>\n    <script>\n      const editor = document.getElementById('editor');\n      editor.addEventListener('beforeinput', (event) => {\n        event.preventDefault();\n        editor.textContent = event.data;\n      });\n    </script>\n  "
            fill (locate page "#editor") "Playwright"
            assertEqual "text" "Playwright" =<< evaluateOnLocatorText (locate page "#editor") "editor => editor.textContent"
      , testCase "should retry on disabled element" $
          withAssetPage "input/textarea.html" $ \page -> do
            _ <- evaluateOnLocator (locate page "input") "input => { input.disabled = true; }"
            withAsync (fill (locate page "input") "some value") $ \pending -> do
              rafraf page 5
              assertStillPending pending
              assertEqual "before" "" =<< evaluateText page "() => window.result"
              _ <- evaluateOnLocator (locate page "input") "input => { input.disabled = false; }"
              wait pending
              assertEqual "after" "some value" =<< evaluateText page "() => window.result"
      , testCase "should retry on readonly element" $
          withAssetPage "input/textarea.html" $ \page -> do
            _ <- evaluateOnLocator (locate page "textarea") "textarea => { textarea.readOnly = true; }"
            withAsync (fill (locate page "textarea") "some value") $ \pending -> do
              rafraf page 5
              assertStillPending pending
              assertEqual "before" "" =<< evaluateText page "() => window.result"
              _ <- evaluateOnLocator (locate page "textarea") "textarea => { textarea.readOnly = false; }"
              wait pending
              assertEqual "after" "some value" =<< evaluateText page "() => window.result"
      , testCase "should retry on invisible element" $
          withAssetPage "input/textarea.html" $ \page -> do
            _ <- evaluateOnLocator (locate page "input") "input => { input.style.display = 'none'; }"
            withAsync (fill (locate page "input") "some value") $ \pending -> do
              rafraf page 5
              assertStillPending pending
              assertEqual "before" "" =<< evaluateText page "() => window.result"
              _ <- evaluateOnLocator (locate page "input") "input => { input.style.display = 'inline'; }"
              wait pending
              assertEqual "after" "some value" =<< evaluateText page "() => window.result"
      , testCase "should be able to fill the body" $
          withChromiumPage $ \page -> do
            setContent page "<body contentEditable=\"true\"></body>"
            fill (locate page "body") "some value"
            assertEqual "text" "some value" =<< evaluateText page "() => document.body.textContent"
      , testCase "should fill fixed position input" $
          withChromiumPage $ \page -> do
            setContent page "<input style='position: fixed;' />"
            fill (locate page "input") "some value"
            assertEqual "value" "some value" =<< evaluateText page "() => document.querySelector('input').value"
      , testCase "should be able to fill when focus is in the wrong frame" $
          withChromiumPage $ \page -> do
            setContent
              page
              "\n    <div contentEditable=\"true\"></div>\n    <iframe></iframe>\n  "
            focus (locate page "iframe")
            fill (locate page "div") "some value"
            assertEqual "text" "some value" =<< evaluateOnLocatorText (locate page "div") "div => div.textContent"
      , testCase "should be able to fill the input[type=number]" $
          withChromiumPage $ \page -> do
            setContent page "<input id=\"input\" type=\"number\"></input>"
            fill (locate page "input") "42"
            assertEqual "value" "42" =<< evaluateText page "() => window.input.value"
      , testCase "should be able to fill exponent into the input[type=number]" $
          withChromiumPage $ \page -> do
            setContent page "<input id=\"input\" type=\"number\"></input>"
            fill (locate page "input") "-10e5"
            assertEqual "value" "-10e5" =<< evaluateText page "() => window.input.value"
      , testCase "should be able to fill input[type=number] with empty string" $
          withChromiumPage $ \page -> do
            setContent page "<input id=\"input\" type=\"number\" value=\"123\"></input>"
            fill (locate page "input") ""
            assertEqual "value" "" =<< evaluateText page "() => window.input.value"
      , testCase "should not be able to fill text into the input[type=number]" $
          withChromiumPage $ \page -> do
            setContent page "<input id=\"input\" type=\"number\"></input>"
            assertProtocolContains
              "Cannot type text into input[type=number]"
              (fill (locate page "input") "abc")
      , testCase "should be able to clear using fill()" $
          withAssetPage "input/textarea.html" $ \page -> do
            fill (locate page "input") "some value"
            assertEqual "filled" "some value" =<< evaluateText page "() => window.result"
            fill (locate page "input") ""
            assertEqual "cleared" "" =<< evaluateText page "() => window.result"
      , testCase "should not throw when fill causes navigation" $
          withAssets $ \prefix page -> do
            _ <- goto page (urlFromText (prefix <> "/input/textarea.html")) defaultGotoOptions
            setContent page "<input type=date>"
            _ <-
              evaluateOnLocator
                (locate page "input")
                "input => input.addEventListener('input', () => { window.location.href = '/empty.html'; })"
            navigated <- armFrameNavigated page
            fill (locate page "input") "2020-03-02"
            _ <- navigated
            current <- url page
            assertEqual "url" True (isInfixOf "empty.html" (urlToText current))
      , testCase "fill back to back" $
          withChromiumPage $ \page -> do
            setContent page "<input id=\"one\"></input><input id=\"two\"></input>"
            fill (locate page "id=one") "first value"
            fill (locate page "id=two") "second value"
            assertEqual "one" "first value" =<< evaluateOnLocatorText (locate page "id=one") "input => input.value"
            assertEqual "two" "second value" =<< evaluateOnLocatorText (locate page "id=two") "input => input.value"
      , testCase "should fill contenteditable with focus handler that collapses selection" $
          withChromiumPage $ \page -> do
            setContent
              page
              "\n    <div contenteditable=\"true\">initial text</div>\n    <script>\n      const editor = document.querySelector('[contenteditable]');\n      editor.addEventListener('focus', () => {\n        const selection = window.getSelection();\n        if (selection.rangeCount > 0)\n          selection.collapseToEnd();\n      });\n    </script>\n  "
            fill (locate page "div[contenteditable]") "some value"
            assertEqual "text" "some value"
              =<< evaluateOnLocatorText (locate page "div[contenteditable]") "div => div.textContent"
      ]
        <> fmap unsupportedInput unsupportedInputTypes
        <> fmap fillInputType fillableInputTypes
        <> fmap composedInputEvents composedInputTypes
    )

-- | Input types with the value that fills them, for the shadow DOM event check.
composedInputTypes :: [(Text, Text)]
composedInputTypes =
  [ ("color", "#aaaaaa")
  , ("date", "2020-03-02")
  , ("time", "13:15")
  , ("datetime-local", "2020-03-02T13:15:30")
  , ("month", "2020-03")
  , ("range", "42")
  , ("week", "2020-W50")
  ]

composedInputEvents :: (Text, Text) -> TestTree
composedInputEvents (inputType, value) =
  testCase ("input event.composed should be true and cross shadow dom boundary - " <> unpack inputType) $
    withAssets $ \prefix page -> do
      _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
      setContent
        page
        ( "<body><script>\n    const div = document.createElement('div');\n    const shadowRoot = div.attachShadow({mode: 'open'});\n    shadowRoot.innerHTML = '<input type="
            <> inputType
            <> "></input>';\n    document.body.appendChild(div);\n  </script></body>"
        )
      _ <-
        evaluateOnLocator
          (locate page "body")
          "body => { window.firedBodyEvents = []; for (const event of ['input', 'change']) { body.addEventListener(event, e => { window.firedBodyEvents.push(e.type + ':' + e.composed); }, false); } }"
      _ <-
        evaluateOnLocator
          (locate page "input")
          "input => { window.firedEvents = []; for (const event of ['input', 'change']) { input.addEventListener(event, e => { window.firedEvents.push(e.type + ':' + e.composed); }, false); } }"
      fill (locate page "input") value
      assertEqual "input events" ["input:true", "change:false"] =<< evaluateTexts page "() => window.firedEvents"
      assertEqual "body events" ["input:true"] =<< evaluateTexts page "() => window.firedBodyEvents"

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

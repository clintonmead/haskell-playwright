module Playwright.PageSelectOptionSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.List
  ( sort
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Playwright
  ( SelectAttributes (SelectAttributes)
  , SelectOption (SelectIndex, SelectLabel, SelectMatching, SelectValue, SelectValueOrLabel)
  , Page
  , Playwright
  , SelectOptionOptions (selectOptionOptionsTimeout)
  , armFrameNavigated
  , defaultGotoOptions
  , defaultSelectOptionOptions
  , evaluate
  , evaluateBool
  , evaluateNumber
  , evaluateOnLocator
  , evaluateOnLocatorText
  , evaluateText
  , evaluateTexts
  , goto
  , selectOption
  , selectOptionWith
  , setContent
  , url
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( assertEqual
  , assertProtocolContains
  , assertStillPending
  , locate
  , rafraf
  , requireTimeout
  , withAssetPage
  , withAssets
  , withChromiumPage
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
    "page-select-option"
    [ testCase "should select single option" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- selectOption (locate page "select") [SelectValueOrLabel "blue"]
          assertEqual "input" ["blue"] =<< evaluateTexts page "() => window.result.onInput"
          assertEqual "change" ["blue"] =<< evaluateTexts page "() => window.result.onChange"
    , testCase "should select single option by value" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- selectOption (locate page "select") [SelectValue "blue"]
          assertEqual "input" ["blue"] =<< evaluateTexts page "() => window.result.onInput"
    , testCase "should fall back to selecting by label" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- selectOption (locate page "select") [SelectValueOrLabel "Blue"]
          assertEqual "input" ["blue"] =<< evaluateTexts page "() => window.result.onInput"
    , testCase "should select single option by label" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- selectOption (locate page "select") [SelectLabel "Indigo"]
          assertEqual "input" ["indigo"] =<< evaluateTexts page "() => window.result.onInput"
    , testCase "should select single option by label with html whitespace" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- selectOption (locate page "select") [SelectLabel "HTML"]
          assertEqual "input" ["html"] =<< evaluateTexts page "() => window.result.onInput"
          assertEqual "change" ["html"] =<< evaluateTexts page "() => window.result.onChange"
    , testCase "should select single option by index" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- selectOption (locate page "select") [SelectIndex 2]
          assertEqual "input" ["brown"] =<< evaluateTexts page "() => window.result.onInput"
    , testCase "should select single option by multiple attributes" $
        withAssetPage "input/select.html" $ \page -> do
          _ <-
            selectOption
              (locate page "select")
              [SelectMatching (SelectAttributes (Just "green") (Just "Green") Nothing)]
          assertEqual "input" ["green"] =<< evaluateTexts page "() => window.result.onInput"
          assertEqual "change" ["green"] =<< evaluateTexts page "() => window.result.onChange"
    , testCase "should not select single option when some attributes do not match" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- evaluate page "() => { document.querySelector('select').value = undefined; }"
          timeout <- requireTimeout 1000
          assertProtocolContains
            "Timeout"
            ( selectOptionWith
                (locate page "select")
                [SelectMatching (SelectAttributes (Just "green") (Just "Brown") Nothing)]
                defaultSelectOptionOptions {selectOptionOptionsTimeout = Just timeout}
            )
          assertEqual "value" "" =<< evaluateText page "() => document.querySelector('select').value"
    , testCase "should select only first option" $
        withAssetPage "input/select.html" $ \page -> do
          _ <-
            selectOption
              (locate page "select")
              [SelectValueOrLabel "blue", SelectValueOrLabel "green", SelectValueOrLabel "red"]
          assertEqual "input" ["blue"] =<< evaluateTexts page "() => window.result.onInput"
    , testCase "should not throw when select causes navigation" $
        withAssetPage "input/select.html" $ \page -> do
          _ <-
            evaluate
              page
              "() => { document.querySelector('select').addEventListener('input', () => { window.location.href = '/empty.html'; }); }"
          navigated <- armFrameNavigated page
          _ <- selectOption (locate page "select") [SelectValueOrLabel "blue"]
          _ <- navigated
          current <- url page
          assertEqual "url" True (Text.isInfixOf "empty.html" (urlToText current))
    , testCase "should select multiple options" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- evaluate page "() => window.makeMultiple()"
          _ <-
            selectOption
              (locate page "select")
              [SelectValueOrLabel "blue", SelectValueOrLabel "green", SelectValueOrLabel "red"]
          assertEqual "input" ["blue", "green", "red"] =<< evaluateTexts page "() => window.result.onInput"
    , testCase "should select multiple options with attributes" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- evaluate page "() => window.makeMultiple()"
          _ <-
            selectOption
              (locate page "select")
              [SelectValue "blue", SelectLabel "Green", SelectIndex 4]
          assertEqual "input" ["blue", "gray", "green"] =<< evaluateTexts page "() => window.result.onInput"
          assertEqual "change" ["blue", "gray", "green"] =<< evaluateTexts page "() => window.result.onChange"
    , testCase "should respect event bubbling" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- selectOption (locate page "select") [SelectValueOrLabel "blue"]
          assertEqual "bubbling input" ["blue"] =<< evaluateTexts page "() => window.result.onBubblingInput"
          assertEqual "bubbling change" ["blue"] =<< evaluateTexts page "() => window.result.onBubblingChange"
    , testCase "should throw when element is not a select" $
        withAssetPage "input/select.html" $ \page ->
          assertProtocolContains
            "Element is not a <select> element"
            (selectOption (locate page "body") [SelectValueOrLabel ""])
    , testCase "should return empty list on no matched values" $
        withAssetPage "input/select.html" $ \page -> do
          selected <- selectOption (locate page "select") []
          assertEqual "selected" [] selected
    , testCase "should return an array of matched values" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- evaluate page "() => window.makeMultiple()"
          selected <-
            selectOption
              (locate page "select")
              [SelectValueOrLabel "blue", SelectValueOrLabel "black", SelectValueOrLabel "magenta"]
          assertEqual "selected" ["black", "blue", "magenta"] (sort selected)
    , testCase "should return an array of one element when multiple is not set" $
        withAssetPage "input/select.html" $ \page -> do
          selected <-
            selectOption
              (locate page "select")
              [SelectValueOrLabel "42", SelectValueOrLabel "blue", SelectValueOrLabel "black", SelectValueOrLabel "magenta"]
          assertEqual "length" (1 :: Int) (length selected)
    , testCase "should return empty list on no values" $
        withAssetPage "input/select.html" $ \page -> do
          selected <- selectOption (locate page "select") []
          assertEqual "selected" [] selected
    , testCase "should unselect all options when passed no values" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- evaluate page "() => window.makeMultiple()"
          _ <-
            selectOption
              (locate page "select")
              [SelectValueOrLabel "blue", SelectValueOrLabel "black", SelectValueOrLabel "magenta"]
          _ <- selectOption (locate page "select") []
          assertEqual "none selected" True
            =<< evaluateBool page "() => Array.from(document.querySelector('select').options).every(option => !option.selected)"
    , testCase "should deselect all options when passed no values for a multiple select" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- evaluate page "() => window.makeMultiple()"
          _ <-
            selectOption
              (locate page "select")
              [SelectValueOrLabel "blue", SelectValueOrLabel "black", SelectValueOrLabel "magenta"]
          _ <- selectOption (locate page "select") []
          assertEqual "none selected" True
            =<< evaluateBool page "() => Array.from(document.querySelector('select').options).every(option => !option.selected)"
    , testCase "should deselect all options when passed no values for a select without multiple" $
        withAssetPage "input/select.html" $ \page -> do
          _ <-
            selectOption
              (locate page "select")
              [SelectValueOrLabel "blue", SelectValueOrLabel "black", SelectValueOrLabel "magenta"]
          _ <- selectOption (locate page "select") []
          assertEqual "none selected" True
            =<< evaluateBool page "() => Array.from(document.querySelector('select').options).every(option => !option.selected)"
    , testCase "should work when re-defining top-level Event class" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- evaluate page "() => { window.Event = null; }"
          _ <- selectOption (locate page "select") [SelectValueOrLabel "blue"]
          assertEqual "input" ["blue"] =<< evaluateTexts page "() => window.result.onInput"
          assertEqual "change" ["blue"] =<< evaluateTexts page "() => window.result.onChange"
    , testCase "should wait for option to be present" $
        withAssetPage "input/select.html" $ \page ->
          withAsync (selectOption (locate page "select") [SelectValueOrLabel "scarlet"]) $ \pending -> do
            rafraf page 5
            assertStillPending pending
            appendOption page "scarlet" "Scarlet"
            assertEqual "selected" ["scarlet"] =<< wait pending
    , testCase "should wait for option index to be present" $
        withAssetPage "input/select.html" $ \page -> do
          len <- evaluateNumber page "() => document.querySelector('select').options.length"
          withAsync (selectOption (locate page "select") [SelectIndex (round len)]) $ \pending -> do
            rafraf page 5
            assertStillPending pending
            appendOption page "scarlet" "Scarlet"
            assertEqual "selected" ["scarlet"] =<< wait pending
    , testCase "should wait for multiple options to be present" $
        withAssetPage "input/select.html" $ \page -> do
          _ <- evaluate page "() => window.makeMultiple()"
          withAsync
            (selectOption (locate page "select") [SelectValueOrLabel "green", SelectValueOrLabel "scarlet"])
            $ \pending -> do
              rafraf page 5
              assertStillPending pending
              appendOption page "scarlet" "Scarlet"
              assertEqual "selected" ["green", "scarlet"] =<< wait pending
    , testCase "input event.composed should be true and cross shadow dom boundary" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent
            page
            "<body><script>\n  const div = document.createElement('div');\n  const shadowRoot = div.attachShadow({mode: 'open'});\n  shadowRoot.innerHTML = `<select>\n    <option value=\"black\">Black</option>\n    <option value=\"blue\">Blue</option>\n  </select>`;\n  document.body.appendChild(div);\n</script></body>"
          _ <-
            evaluateOnLocator
              (locate page "body")
              "body => { window.firedBodyEvents = []; for (const event of ['input', 'change']) { body.addEventListener(event, e => { window.firedBodyEvents.push(e.type + ':' + e.composed); }, false); } }"
          _ <-
            evaluateOnLocator
              (locate page "select")
              "select => { window.firedEvents = []; for (const event of ['input', 'change']) { select.addEventListener(event, e => { window.firedEvents.push(e.type + ':' + e.composed); }, false); } }"
          _ <- selectOption (locate page "select") [SelectValueOrLabel "blue"]
          assertEqual "select events" ["input:true", "change:false"] =<< evaluateTexts page "() => window.firedEvents"
          assertEqual "body events" ["input:true"] =<< evaluateTexts page "() => window.firedBodyEvents"
    , testCase "should wait for select to be enabled" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<select disabled>\n      <option>one</option>\n      <option>two</option>\n    </select>\n\n    <script>\n    function hydrate() {\n      const select = document.querySelector('select');\n      select.removeAttribute('disabled');\n      select.addEventListener('change', () => {\n        window['result'] = select.value;\n      });\n    }\n    </script>"
          withAsync (selectOption (locate page "select") [SelectValueOrLabel "two"]) $ \pending -> do
            liftIO (threadDelay 1000000)
            _ <- evaluate page "() => hydrate()"
            _ <- wait pending
            assertEqual "result" "two" =<< evaluateText page "() => window.result"
            assertEqual "value" "two" =<< evaluateOnLocatorText (locate page "select") "select => select.value"
    , testCase "should wait for option to be enabled" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<select>\n      <option>one</option>\n      <option disabled id=myoption>two</option>\n    </select>\n\n    <script>\n    function hydrate() {\n      const option = document.querySelector('#myoption');\n      option.removeAttribute('disabled');\n      const select = document.querySelector('select');\n      select.addEventListener('change', () => {\n        window['result'] = select.value;\n      });\n    }\n    </script>"
          timeout <- requireTimeout 1000
          assertProtocolContains
            "option being selected is not enabled"
            ( selectOptionWith
                (locate page "select")
                [SelectValueOrLabel "two"]
                defaultSelectOptionOptions {selectOptionOptionsTimeout = Just timeout}
            )
          withAsync (selectOption (locate page "select") [SelectValueOrLabel "two"]) $ \pending -> do
            liftIO (threadDelay 1000000)
            _ <- evaluate page "() => hydrate()"
            _ <- wait pending
            assertEqual "result" "two" =<< evaluateText page "() => window.result"
            assertEqual "value" "two" =<< evaluateOnLocatorText (locate page "select") "select => select.value"
    , testCase "should wait for optgroup to be enabled" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<select>\n      <option>one</option>\n      <optgroup label=\"Group\" disabled id=mygroup>\n        <option>two</option>\n      </optgroup>\n    </select>\n\n    <script>\n    function hydrate() {\n      const group = document.querySelector('#mygroup');\n      group.removeAttribute('disabled');\n      const select = document.querySelector('select');\n      select.addEventListener('change', () => {\n        window['result'] = select.value;\n      });\n    }\n    </script>"
          timeout <- requireTimeout 1000
          assertProtocolContains
            "option being selected is not enabled"
            ( selectOptionWith
                (locate page "select")
                [SelectValueOrLabel "two"]
                defaultSelectOptionOptions {selectOptionOptionsTimeout = Just timeout}
            )
          withAsync (selectOption (locate page "select") [SelectValueOrLabel "two"]) $ \pending -> do
            liftIO (threadDelay 1000000)
            _ <- evaluate page "() => hydrate()"
            _ <- wait pending
            assertEqual "result" "two" =<< evaluateText page "() => window.result"
            assertEqual "value" "two" =<< evaluateOnLocatorText (locate page "select") "select => select.value"
    , testCase "should wait for select to be swapped" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<select disabled>\n      <option>one</option>\n      <option>two</option>\n    </select>\n\n    <script>\n    function hydrate() {\n      const select = document.querySelector('select');\n      select.remove();\n\n      const newSelect = document.createElement('select');\n      const option1 = document.createElement('option');\n      option1.textContent = 'one';\n      newSelect.appendChild(option1);\n      const option2 = document.createElement('option');\n      option2.textContent = 'two';\n      newSelect.appendChild(option2);\n\n      document.body.appendChild(newSelect);\n\n      newSelect.addEventListener('change', () => {\n        window['result'] = newSelect.value;\n      });\n    }\n    </script>"
          withAsync (selectOption (locate page "select") [SelectValueOrLabel "two"]) $ \pending -> do
            liftIO (threadDelay 1000000)
            _ <- evaluate page "() => hydrate()"
            _ <- wait pending
            assertEqual "value" "two" =<< evaluateOnLocatorText (locate page "select") "select => select.value"
            assertEqual "result" "two" =<< evaluateText page "() => window.result"
    , testCase "should return [] on no matched values" $
        withAssetPage "input/select.html" $ \page -> do
          result <- selectOption (locate page "select") []
          assertEqual "result" [] result
    , testCase "should return [] on no values" $
        withAssetPage "input/select.html" $ \page -> do
          result <- selectOption (locate page "select") []
          assertEqual "result" [] result
    ]

-- | Appends an option to the page's select, so a waiting selectOption can
-- match it.
appendOption :: Page -> Text -> Text -> Playwright ()
appendOption page value label = do
  _ <-
    evaluate
      page
      ( "() => { const option = document.createElement('option'); option.value = '"
          <> value
          <> "'; option.textContent = '"
          <> label
          <> "'; document.querySelector('select').appendChild(option); }"
      )
  pure ()

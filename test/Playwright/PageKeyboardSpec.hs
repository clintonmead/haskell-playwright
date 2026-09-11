module Playwright.PageKeyboardSpec
  ( tests
  ) where

import Control.Monad
  ( replicateM_
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Playwright
  ( Key (Key)
  , evaluate
  , evaluateBoolWith
  , evaluateHandle
  , evaluateOnLocatorText
  , evaluateText
  , focus
  , jsHandleArgument
  , jsonValueTexts
  , keyboardDown
  , keyboardInsertText
  , keyboardPress
  , keyboardType
  , keyboardUp
  , press
  , setContent
  , typeText
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
  , withAssetPage
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
    "page-keyboard"
    [ testCase "should type into a textarea" $
        withChromiumPage $ \page -> do
          _ <-
            evaluate
              page
              "() => { const textarea = document.createElement('textarea'); document.body.appendChild(textarea); textarea.focus(); }"
          let
            typed :: Text
            typed = "Hello world. I am the text that was typed!"
          keyboardType page typed
          value <- evaluateOnLocatorText (locate page "textarea") "el => el.value"
          assertEqual "typed" typed value
    , testCase "should move with the arrow keys" $
        withAssetPage "input/textarea.html" $ \page -> do
          typeText (locate page "textarea") "Hello World!"
          assertEqual "initial" "Hello World!"
            =<< evaluateOnLocatorText (locate page "textarea") "el => el.value"
          replicateM_ (Text.length "World!") (keyboardPress page (Key "ArrowLeft"))
          keyboardType page "inserted "
          assertEqual "inserted" "Hello inserted World!"
            =<< evaluateOnLocatorText (locate page "textarea") "el => el.value"
          keyboardDown page (Key "Shift")
          replicateM_ (Text.length "inserted ") (keyboardPress page (Key "ArrowLeft"))
          keyboardUp page (Key "Shift")
          keyboardPress page (Key "Backspace")
          assertEqual "restored" "Hello World!"
            =<< evaluateOnLocatorText (locate page "textarea") "el => el.value"
    , testCase "should send a character with locator press" $
        withAssetPage "input/textarea.html" $ \page -> do
          press (locate page "textarea") (Key "a")
          assertEqual "a" "a"
            =<< evaluateOnLocatorText (locate page "textarea") "el => el.value"
          _ <-
            evaluate
              page
              "() => { window.addEventListener('keydown', e => e.preventDefault(), true); }"
          press (locate page "textarea") (Key "b")
          assertEqual "still a" "a"
            =<< evaluateOnLocatorText (locate page "textarea") "el => el.value"
    , testCase "should send a character with insertText" $
        withAssetPage "input/textarea.html" $ \page -> do
          focus (locate page "textarea")
          keyboardInsertText page "嗨"
          assertEqual "cjk" "嗨"
            =<< evaluateOnLocatorText (locate page "textarea") "el => el.value"
          _ <-
            evaluate
              page
              "() => { window.addEventListener('keydown', e => e.preventDefault(), true); }"
          keyboardInsertText page "a"
          assertEqual "cjk+a" "嗨a"
            =<< evaluateOnLocatorText (locate page "textarea") "el => el.value"
    , testCase "should report only input event for insertText" $
        withAssetPage "input/textarea.html" $ \page -> do
          focus (locate page "textarea")
          events <-
            evaluateHandle
              page
              "() => { const events = []; document.addEventListener('keydown', e => events.push(e.type)); document.addEventListener('keyup', e => events.push(e.type)); document.addEventListener('keypress', e => events.push(e.type)); document.addEventListener('input', e => events.push(e.type)); return events; }"
          keyboardInsertText page "hello world"
          assertEqual "input only" ["input" :: Text] =<< jsonValueTexts events
    , testCase "should emit keydown, keypress, textInput and input when typing a character" $
        withChromiumPage $ \page -> do
          setContent page "<input>"
          events <-
            evaluateHandle
              page
              "() => { const events = []; for (const type of ['keydown', 'keypress', 'textInput', 'input', 'keyup']) document.querySelector('input').addEventListener(type, () => events.push(type)); return events; }"
          focus (locate page "input")
          keyboardPress page (Key "f")
          assertEqual
            "events"
            ["keydown", "keypress", "textInput", "input", "keyup" :: Text]
            =<< jsonValueTexts events
    , testCase "should not type canceled events" $
        withAssetPage "input/textarea.html" $ \page -> do
          focus (locate page "textarea")
          _ <-
            evaluate
              page
              "() => { window.addEventListener('keydown', event => { event.stopPropagation(); event.stopImmediatePropagation(); if (event.key === 'l') event.preventDefault(); if (event.key === 'o') event.preventDefault(); }, false); }"
          keyboardType page "Hello World!"
          assertEqual "canceled" "He Wrd!"
            =<< evaluateOnLocatorText (locate page "textarea") "el => el.value"
    , testCase "should type all kinds of characters" $
        withAssetPage "input/textarea.html" $ \page -> do
          focus (locate page "textarea")
          let
            typed :: Text
            typed = "This text goes onto two lines.\nThis character is 嗨."
          keyboardType page typed
          assertEqual "typed" typed
            =<< evaluateOnLocatorText (locate page "textarea") "el => el.value"
    , testCase "should throw on unknown keys" $
        withChromiumPage $ \page -> do
          assertProtocolContains "Unknown key: \"NotARealKey\"" (keyboardPress page (Key "NotARealKey"))
          assertProtocolContains "Unknown key: \"ё\"" (keyboardPress page (Key "ё"))
          assertProtocolContains "Unknown key: \"😊\"" (keyboardPress page (Key "😊"))
    , testCase "should type emoji" $
        withAssetPage "input/textarea.html" $ \page -> do
          typeText (locate page "textarea") "👹 Tokyo street Japan 🇯🇵"
          assertEqual "emoji" "👹 Tokyo street Japan 🇯🇵"
            =<< evaluateOnLocatorText (locate page "textarea") "el => el.value"
    , testCase "should handle selectAll" $
        withAssetPage "input/textarea.html" $ \page -> do
          typeText (locate page "textarea") "some text"
          keyboardDown page (Key "ControlOrMeta")
          keyboardPress page (Key "a")
          keyboardUp page (Key "ControlOrMeta")
          keyboardPress page (Key "Backspace")
          assertEqual "cleared" ""
            =<< evaluateOnLocatorText (locate page "textarea") "el => el.value"
    , testCase "pressing Meta should not result in any text insertion" $
        withChromiumPage $ \page -> do
          setContent page "<input type=\"text\" value=\"hello world\">"
          focus (locate page "input")
          keyboardPress page (Key "Meta")
          assertEqual "unchanged" "hello world"
            =<< evaluateOnLocatorText (locate page "input") "el => el.value"
    , testCase "should send proper codes while typing" $
        withAssetPage "input/keyboard.html" $ \page -> do
          keyboardType page "!"
          bang <- evaluateText page "() => getResult()"
          assertEqual
            "bang"
            "Keydown: ! Digit1 STANDARD []\nKeypress: ! Digit1 STANDARD 33 []\nKeyup: ! Digit1 STANDARD []"
            bang
          keyboardType page "^"
          caret <- evaluateText page "() => getResult()"
          assertEqual
            "caret"
            "Keydown: ^ Digit6 STANDARD []\nKeypress: ^ Digit6 STANDARD 94 []\nKeyup: ^ Digit6 STANDARD []"
            caret
    , testCase "should have correct Keydown/Keyup order when pressing Escape" $
        withAssetPage "input/keyboard.html" $ \page -> do
          keyboardPress page (Key "Escape")
          result <- evaluateText page "() => getResult()"
          assertEqual
            "escape"
            "Keydown: Escape Escape STANDARD []\nKeyup: Escape Escape STANDARD []"
            result
    , testCase "should dispatch a click event on a button when Space gets pressed" $
        withChromiumPage $ \page -> do
          setContent page "<button type=\"button\">a11y</button>"
          handle <-
            evaluateHandle
              page
              "() => { const actual = { clicked: false }; document.querySelector('button').addEventListener('click', () => (actual.clicked = true)); return actual; }"
          focus (locate page "button")
          keyboardPress page (Key "Space")
          assertEqual "clicked" True
            =<< evaluateBoolWith page "e => e.clicked" (jsHandleArgument handle)
    ]


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
  ( BoundingBox (boundingBoxHeight, boundingBoxWidth, boundingBoxX, boundingBoxY)
  , CssPixel (cssPixelToScientific)
  , Frame
  , JsHandle
  , Key (Key)
  , MouseButton (MouseButtonRight)
  , Page
  , Playwright
  , Point (Point)
  , boundingBox
  , cssPixelFromInt
  , evaluateBool
  , evaluateTexts
  , mouseClickWith
  , mouseDownWith
  , mouseMove
  , pressHandle
  , querySelector
  , asElement
  , click
  , contentFrameHandle
  , defaultGotoOptions
  , evaluate
  , evaluateBoolWith
  , evaluateHandle
  , evaluateNumberWith
  , evaluateOnFrameText
  , evaluateOnHandleText
  , evaluateOnLocatorText
  , evaluateText
  , first
  , focus
  , goto
  , inputValue
  , jsHandleArgument
  , jsonValueTexts
  , keyboardDown
  , keyboardInsertText
  , keyboardPress
  , keyboardType
  , keyboardUp
  , locatorOnFrame
  , press
  , selectorFromText
  , setContent
  , typeText
  , urlFromText
  , waitForFunction
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
  , requireHandle
  , withAssetPage
  , withAssets
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
    , testCase "insertText should only emit input event" $
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
    , testCase "pressing Meta should not result in any text insertion on any platform" $
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
    , testCase "should send proper codes while typing with shift" $
        withAssetPage "input/keyboard.html" $ \page -> do
          keyboardDown page (Key "Shift")
          keyboardType page "~"
          result <- evaluateText page "() => getResult()"
          assertEqual
            "shift tilde"
            "Keydown: Shift ShiftLeft LEFT [Shift]\nKeydown: ~ Backquote STANDARD [Shift]\nKeypress: ~ Backquote STANDARD 126 [Shift]\nKeyup: ~ Backquote STANDARD [Shift]"
            result
          keyboardUp page (Key "Shift")
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
    , testCase "should dispatch a click event on a button when Enter gets pressed" $
        withChromiumPage $ \page -> do
          setContent page "<button type=\"button\">a11y</button>"
          handle <-
            evaluateHandle
              page
              "() => { const actual = { clicked: false }; document.querySelector('button').addEventListener('click', () => (actual.clicked = true)); return actual; }"
          focus (locate page "button")
          keyboardPress page (Key "Enter")
          assertEqual "clicked" True
            =<< evaluateBoolWith page "e => e.clicked" (jsHandleArgument handle)
    , testCase "should press plus" $
        withAssetPage "input/keyboard.html" $ \page -> do
          keyboardPress page (Key "+")
          result <- evaluateText page "() => getResult()"
          assertEqual
            "plus"
            "Keydown: + Equal STANDARD []\nKeypress: + Equal STANDARD 43 []\nKeyup: + Equal STANDARD []"
            result
    , testCase "should press shift plus" $
        withAssetPage "input/keyboard.html" $ \page -> do
          keyboardPress page (Key "Shift++")
          result <- evaluateText page "() => getResult()"
          assertEqual
            "shift plus"
            "Keydown: Shift ShiftLeft LEFT [Shift]\nKeydown: + Equal STANDARD [Shift]\nKeypress: + Equal STANDARD 43 [Shift]\nKeyup: + Equal STANDARD [Shift]\nKeyup: Shift ShiftLeft LEFT []"
            result
    , testCase "should support plus-separated modifiers" $
        withAssetPage "input/keyboard.html" $ \page -> do
          keyboardPress page (Key "Shift+~")
          result <- evaluateText page "() => getResult()"
          assertEqual
            "shift tilde"
            "Keydown: Shift ShiftLeft LEFT [Shift]\nKeydown: ~ Backquote STANDARD [Shift]\nKeypress: ~ Backquote STANDARD 126 [Shift]\nKeyup: ~ Backquote STANDARD [Shift]\nKeyup: Shift ShiftLeft LEFT []"
            result
    , testCase "should support multiple plus-separated modifiers" $
        withAssetPage "input/keyboard.html" $ \page -> do
          keyboardPress page (Key "Control+Shift+~")
          result <- evaluateText page "() => getResult()"
          assertEqual
            "control shift tilde"
            "Keydown: Control ControlLeft LEFT [Control]\nKeydown: Shift ShiftLeft LEFT [Control Shift]\nKeydown: ~ Backquote STANDARD [Control Shift]\nKeyup: ~ Backquote STANDARD [Control Shift]\nKeyup: Shift ShiftLeft LEFT [Control]\nKeyup: Control ControlLeft LEFT []"
            result
    , testCase "should shift raw codes" $
        withAssetPage "input/keyboard.html" $ \page -> do
          keyboardPress page (Key "Shift+Digit3")
          result <- evaluateText page "() => getResult()"
          assertEqual
            "hash"
            "Keydown: Shift ShiftLeft LEFT [Shift]\nKeydown: # Digit3 STANDARD [Shift]\nKeypress: # Digit3 STANDARD 35 [Shift]\nKeyup: # Digit3 STANDARD [Shift]\nKeyup: Shift ShiftLeft LEFT []"
            result
    , testCase "should specify repeat property" $
        withAssetPage "input/textarea.html" $ \page -> do
          focus (locate page "textarea")
          lastEvent <- captureLastKeydown page
          keyboardDown page (Key "a")
          assertEqual "first a" False =<< evaluateBoolWith page "e => e.repeat" (jsHandleArgument lastEvent)
          keyboardPress page (Key "a")
          assertEqual "repeat a" True =<< evaluateBoolWith page "e => e.repeat" (jsHandleArgument lastEvent)
          keyboardDown page (Key "b")
          assertEqual "first b" False =<< evaluateBoolWith page "e => e.repeat" (jsHandleArgument lastEvent)
          keyboardDown page (Key "b")
          assertEqual "repeat b" True =<< evaluateBoolWith page "e => e.repeat" (jsHandleArgument lastEvent)
          keyboardUp page (Key "a")
          keyboardDown page (Key "a")
          assertEqual "a after up" False =<< evaluateBoolWith page "e => e.repeat" (jsHandleArgument lastEvent)
    , testCase "should specify location" $
        withAssetPage "input/textarea.html" $ \page -> do
          lastEvent <- captureLastKeydown page
          press (locate page "textarea") (Key "Digit5")
          assertEqual "location 0" (0 :: Double) =<< evaluateNumberWith page "e => e.location" (jsHandleArgument lastEvent)
          assertEqual "key 5" "5" =<< evaluateOnHandleText lastEvent "e => e.key"
          assertEqual "code Digit5" "Digit5" =<< evaluateOnHandleText lastEvent "e => e.code"
          press (locate page "textarea") (Key "ControlLeft")
          assertEqual "location 1" (1 :: Double) =<< evaluateNumberWith page "e => e.location" (jsHandleArgument lastEvent)
          assertEqual "key Control" "Control" =<< evaluateOnHandleText lastEvent "e => e.key"
          assertEqual "code ControlLeft" "ControlLeft" =<< evaluateOnHandleText lastEvent "e => e.code"
          press (locate page "textarea") (Key "ControlRight")
          assertEqual "location 2" (2 :: Double) =<< evaluateNumberWith page "e => e.location" (jsHandleArgument lastEvent)
          assertEqual "key Control right" "Control" =<< evaluateOnHandleText lastEvent "e => e.key"
          assertEqual "code ControlRight" "ControlRight" =<< evaluateOnHandleText lastEvent "e => e.code"
          press (locate page "textarea") (Key "NumpadSubtract")
          assertEqual "location 3" (3 :: Double) =<< evaluateNumberWith page "e => e.location" (jsHandleArgument lastEvent)
          assertEqual "key minus" "-" =<< evaluateOnHandleText lastEvent "e => e.key"
          assertEqual "code NumpadSubtract" "NumpadSubtract" =<< evaluateOnHandleText lastEvent "e => e.code"
    , testCase "should press Enter" $
        withChromiumPage $ \page -> do
          setContent page "<textarea></textarea>"
          focus (locate page "textarea")
          lastEvent <- captureLastKeydown page
          testEnterKey page lastEvent "Enter" "Enter" "Enter"
          testEnterKey page lastEvent "NumpadEnter" "Enter" "NumpadEnter"
          testEnterKey page lastEvent "\n" "Enter" "Enter"
          testEnterKey page lastEvent "\r" "Enter" "Enter"
    , testCase "should press audio and media control keys" $
        withChromiumPage $ \page -> do
          setContent page "<input autofocus>"
          focus (locate page "input")
          lastEvent <- captureLastKeydown page
          mapM_
            ( \key -> do
                keyboardPress page (Key key)
                assertEqual ("key " <> show key) key =<< evaluateOnHandleText lastEvent "e => e.key"
                assertEqual ("code " <> show key) key =<< evaluateOnHandleText lastEvent "e => e.code"
                assertEqual ("location " <> show key) (0 :: Double)
                  =<< evaluateNumberWith page "e => e.location" (jsHandleArgument lastEvent)
            )
            ["AudioVolumeMute", "AudioVolumeDown", "AudioVolumeUp", "MediaTrackNext", "MediaTrackPrevious", "MediaPlayPause"]
    , testCase "should type emoji into an iframe" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          frame <- attachFrame page prefix "emoji-test"
          typeText (locatorOnFrame page frame (selectorFromText "textarea")) "👹 Tokyo street Japan 🇯🇵"
          assertEqual "emoji"
            "👹 Tokyo street Japan 🇯🇵"
            =<< evaluateOnFrameText frame "() => document.querySelector('textarea').value"
    , testCase "should be able to prevent selectAll" $
        withAssetPage "input/textarea.html" $ \page -> do
          typeText (locate page "textarea") "some text"
          _ <-
            evaluate
              page
              "() => { document.querySelector('textarea').addEventListener('keydown', event => { if (event.key === 'a' && (event.metaKey || event.ctrlKey)) event.preventDefault(); }, false); }"
          keyboardDown page (Key "ControlOrMeta")
          keyboardPress page (Key "a")
          keyboardUp page (Key "ControlOrMeta")
          keyboardPress page (Key "Backspace")
          assertEqual "one deleted" "some tex"
            =<< evaluateOnLocatorText (locate page "textarea") "el => el.value"
    , testCase "should press the meta key" $
        withChromiumPage $ \page -> do
          lastEvent <- captureLastKeydown page
          keyboardPress page (Key "Meta")
          assertEqual "key" "Meta" =<< evaluateOnHandleText lastEvent "e => e.key"
          assertEqual "code" "MetaLeft" =<< evaluateOnHandleText lastEvent "e => e.code"
          assertEqual "meta" True =<< evaluateBoolWith page "e => e.metaKey" (jsHandleArgument lastEvent)
    , testCase "should work with keyboard events with empty.html" $
        withAssetPage "empty.html" $ \page -> do
          lastEvent <- captureLastKeydown page
          keyboardPress page (Key "a")
          assertEqual "a" "a" =<< evaluateOnHandleText lastEvent "e => e.key"
    , testCase "should scroll with PageDown" $
        withAssetPage "input/scrollable.html" $ \page -> do
          click (locate page "body")
          keyboardPress page (Key "PageDown")
          _ <- waitForFunction page "() => scrollY > 0"
          pure ()
    , testCase "should move around the selection in a contenteditable" $
        withChromiumPage $ \page -> do
          setContent page "<div contenteditable></div>"
          focus (locate page "div")
          keyboardType page "Hello World"
          keyboardDown page (Key "Control")
          keyboardDown page (Key "Shift")
          keyboardPress page (Key "ArrowLeft")
          keyboardUp page (Key "Shift")
          keyboardUp page (Key "Control")
          assertEqual "World" "World" =<< evaluateText page "() => window.getSelection().toString()"
    , testCase "should support simple copy-pasting" $
        withChromiumPage $ \page -> do
          setContent page "<div contenteditable>123</div>"
          focus (locate page "div")
          keyboardPress page (Key "ControlOrMeta+KeyA")
          keyboardPress page (Key "ControlOrMeta+KeyC")
          keyboardPress page (Key "ControlOrMeta+KeyV")
          keyboardPress page (Key "ControlOrMeta+KeyV")
          assertEqual "copied" "123123" =<< evaluateText page "() => document.querySelector('div').textContent"
    , testCase "should support simple cut-pasting" $
        withChromiumPage $ \page -> do
          setContent page "<div contenteditable>123</div>"
          focus (locate page "div")
          keyboardPress page (Key "ControlOrMeta+KeyA")
          keyboardPress page (Key "ControlOrMeta+KeyX")
          keyboardPress page (Key "ControlOrMeta+KeyV")
          keyboardPress page (Key "ControlOrMeta+KeyV")
          assertEqual "cut" "123123" =<< evaluateText page "() => document.querySelector('div').textContent"
    , testCase "should support undo-redo" $
        withChromiumPage $ \page -> do
          setContent page "<div contenteditable></div>"
          typeText (locate page "div") "123"
          assertEqual "typed" "123" =<< evaluateOnLocatorText (locate page "div") "el => el.textContent"
          keyboardPress page (Key "ControlOrMeta+KeyZ")
          assertEqual "undone" "" =<< evaluateOnLocatorText (locate page "div") "el => el.textContent"
          keyboardPress page (Key "Shift+ControlOrMeta+KeyZ")
          assertEqual "redone" "123" =<< evaluateOnLocatorText (locate page "div") "el => el.textContent"
    , testCase "should type repeatedly in contenteditable in shadow dom" $
        withChromiumPage $ \page -> do
          setContent page shadowEditorsHtml
          typeText (first (locate page "shadow-element > .editor")) "This is the first box."
          typeText (locate page "section .editor") "This is the second box."
          assertEqual "first" "This is the first box."
            =<< evaluateOnLocatorText (first (locate page "shadow-element > .editor")) "el => el.textContent"
          assertEqual "second" "This is the second box."
            =<< evaluateOnLocatorText (locate page "section .editor") "el => el.textContent"
    , testCase "should type repeatedly in contenteditable in shadow dom with nested elements" $
        withChromiumPage $ \page -> do
          setContent page shadowNestedEditorsHtml
          typeText (first (locate page "shadow-element > .editor")) "This is the first box: "
          typeText (locate page "section .editor") "This is the second box: "
          assertEqual "first" "This is the first box: hello"
            =<< evaluateOnLocatorText (first (locate page "shadow-element > .editor")) "el => el.textContent"
          assertEqual "second" "This is the second box: world"
            =<< evaluateOnLocatorText (locate page "section .editor") "el => el.textContent"
    , testCase "should type repeatedly in input in shadow dom" $
        withChromiumPage $ \page -> do
          setContent page shadowInputsHtml
          typeText (first (locate page "shadow-element > .editor")) "This is the first box."
          typeText (locate page "section .editor") "This is the second box."
          assertEqual "first" "This is the first box."
            =<< inputValue (first (locate page "shadow-element > .editor"))
          assertEqual "second" "This is the second box."
            =<< inputValue (locate page "section .editor")
    , testCase "type to non-focusable element should maintain old focus" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div id=\"focusable\" tabindex=\"0\">focusable div</div><div id=\"non-focusable-and-non-editable\">non-editable, non-focusable</div>"
          focus (locate page "#focusable")
          assertEqual "focused" "focusable" =<< evaluateText page "() => document.activeElement.id"
          typeText (locate page "#non-focusable-and-non-editable") "foo"
          assertEqual "still focused" "focusable" =<< evaluateText page "() => document.activeElement.id"
    , testCase "should send a character with ElementHandle.press" $
        withAssetPage "input/textarea.html" $ \page -> do
          textarea <- requireHandle =<< querySelector page (selectorFromText "textarea")
          pressHandle textarea (Key "a")
          assertEqual "first" "a" =<< evaluateOnLocatorText (locate page "textarea") "t => t.value"
          _ <- evaluate page "() => window.addEventListener('keydown', e => e.preventDefault(), true)"
          pressHandle textarea (Key "b")
          assertEqual "prevented" "a" =<< evaluateOnLocatorText (locate page "textarea") "t => t.value"
    , testCase "should dispatch key events in separate tasks" $
        withChromiumPage $ \page -> do
          setContent page "<input>"
          log' <-
            evaluateHandle
              page
              "() => { const log = []; const input = document.querySelector('input'); for (const type of ['keydown', 'keypress']) { input.addEventListener(type, () => { log.push(type); queueMicrotask(() => log.push('microtask-' + type)); }); } input.focus(); return log; }"
          keyboardPress page (Key "a")
          assertEqual "order" ["keydown", "microtask-keydown", "keypress", "microtask-keypress"]
            =<< jsonValueTexts log'
    , testCase "should report shiftKey" $
        withAssetPage "input/keyboard.html" $ \page ->
          mapM_ (checkModifierReports page) ["Shift", "Alt", "Control"]
    , testCase "should report multiple modifiers" $
        withAssetPage "input/keyboard.html" $ \page -> do
          keyboardDown page (Key "Control")
          expectResult page "control down" "Keydown: Control ControlLeft LEFT [Control]"
          keyboardDown page (Key "Alt")
          expectResult page "alt down" "Keydown: Alt AltLeft LEFT [Alt Control]"
          keyboardDown page (Key ";")
          expectResult page "semicolon down" "Keydown: ; Semicolon STANDARD [Alt Control]"
          keyboardUp page (Key ";")
          expectResult page "semicolon up" "Keyup: ; Semicolon STANDARD [Alt Control]"
          keyboardUp page (Key "Control")
          expectResult page "control up" "Keyup: Control ControlLeft LEFT [Alt]"
          keyboardUp page (Key "Alt")
          expectResult page "alt up" "Keyup: Alt AltLeft LEFT []"
    , testCase "should dispatch insertText after context menu was opened" $
        withAssetPage "input/textarea.html" $ \page -> do
          _ <- openContextMenuWatch page
          textarea <- requireHandle =<< querySelector page (selectorFromText "textarea")
          box <- requireBox =<< boundingBox textarea
          _ <- mouseClickWith page (centerOf box) (Just MouseButtonRight) Nothing
          _ <- evaluate page "() => window.contextMenuPromise"
          keyboardInsertText page "嗨"
          _ <- waitForFunction page "() => document.querySelector('textarea').value === '嗨'"
          assertEqual "value" "嗨" =<< evaluateOnLocatorText (locate page "textarea") "t => t.value"
    , testCase "should type after context menu was opened" $
        withChromiumPage $ \page -> do
          _ <-
            evaluate
              page
              "() => { window.keys = []; window.addEventListener('keydown', e => window.keys.push(e.key)); window.contextMenuPromise = new Promise(x => window.addEventListener('contextmenu', () => x('menu'), false)); }"
          mouseMove page (pointAt 100 100)
          mouseDownWith page (Just MouseButtonRight)
          _ <- evaluate page "() => window.contextMenuPromise"
          keyboardDown page (Key "ArrowDown")
          _ <- waitForFunction page "() => window.keys.length === 1"
          assertEqual "keys" ["ArrowDown"] =<< evaluateTexts page "() => window.keys"
    , testCase "should have correct Keydown/Keyup order when pressing Escape key" $
        withAssetPage "input/keyboard.html" $ \page -> do
          keyboardPress page (Key "Escape")
          expectResult page "escape" "Keydown: Escape Escape STANDARD []\nKeyup: Escape Escape STANDARD []"
    , testCase "should close dialog on Escape key press in contenteditable" $
        withChromiumPage $ \page -> do
          setContent page "\n    <dialog>\n      <div contenteditable>Edit Me</div>\n    </dialog>\n  "
          _ <- evaluate page "() => document.querySelector('dialog').showModal()"
          assertEqual "open" True =<< evaluateBool page "() => document.querySelector('dialog').open"
          press (locate page "dialog [contenteditable]") (Key "Escape")
          assertEqual "closed" False =<< evaluateBool page "() => document.querySelector('dialog').open"
    , testCase "should work after a cross origin navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let otherOrigin = Text.replace "127.0.0.1" "localhost" prefix
          _ <- goto page (urlFromText (otherOrigin <> "/empty.html")) defaultGotoOptions
          lastEvent <- captureLastKeydown page
          keyboardPress page (Key "a")
          assertEqual "key" "a" =<< evaluateOnHandleText lastEvent "e => e.key"
    -- Upstream cases not ported here (platform- or browser-specific):
    --   should support MacOS shortcuts: upstream is it.skip(platform !== 'darwin'). The
    --     Shift+Control+Alt+KeyB word-select shortcut is only meaningful on macOS, and this
    --     suite runs headless Chromium on Linux, so the case is skipped as platform-specific.
    --   should expose keyIdentifier in webkit: upstream is it.skip(browserName !== 'webkit').
    --     event.keyIdentifier exists only in WebKit, and this suite runs Chromium only.
    --   should move to the start of the document: upstream is it.skip(!isMac). Shift+Meta+ArrowUp
    --     moves the selection to the document start only on macOS, so the case is skipped as
    --     platform-specific.
    ]

-- | Checks the Keydown / Keypress / Keyup report for a modifier held while
-- typing "!".
checkModifierReports :: Page -> Text -> Playwright ()
checkModifierReports page modifier = do
  keyboardDown page (Key modifier)
  expectResult page "modifier down" ("Keydown: " <> modifier <> " " <> modifier <> "Left LEFT [" <> modifier <> "]")
  keyboardDown page (Key "!")
  let
    bangDown =
      if modifier == "Shift"
        then "Keydown: ! Digit1 STANDARD [Shift]\nKeypress: ! Digit1 STANDARD 33 [Shift]"
        else "Keydown: ! Digit1 STANDARD [" <> modifier <> "]"
  expectResult page "bang down" bangDown
  keyboardUp page (Key "!")
  expectResult page "bang up" ("Keyup: ! Digit1 STANDARD [" <> modifier <> "]")
  keyboardUp page (Key modifier)
  expectResult page "modifier up" ("Keyup: " <> modifier <> " " <> modifier <> "Left LEFT []")

-- | Asserts the text the keyboard.html page recorded so far.
expectResult :: Page -> String -> Text -> Playwright ()
expectResult page label expected =
  assertEqual label expected =<< evaluateText page "() => getResult()"

-- | Records that a context menu opened, so the test can wait for it.
openContextMenuWatch :: Page -> Playwright ()
openContextMenuWatch page = do
  _ <-
    evaluate
      page
      "() => { window.contextMenuPromise = new Promise(x => window.addEventListener('contextmenu', () => x('menu'), false)); }"
  pure ()

-- | The centre of an element, rounded to whole pixels.
centerOf :: BoundingBox -> Point
centerOf box =
  Point
    (cssPixelFromInt (midpoint (boundingBoxX box) (boundingBoxWidth box)))
    (cssPixelFromInt (midpoint (boundingBoxY box) (boundingBoxHeight box)))
  where
    midpoint :: CssPixel -> CssPixel -> Int
    midpoint start size =
      round (cssPixelToScientific start + cssPixelToScientific size / 2)

pointAt :: Int -> Int -> Point
pointAt x y =
  Point (cssPixelFromInt x) (cssPixelFromInt y)

requireBox :: Maybe BoundingBox -> Playwright BoundingBox
requireBox found =
  case found of
    Nothing ->
      assertFailure "expected a bounding box"
    Just box ->
      pure box

captureLastKeydown :: Page -> Playwright JsHandle
captureLastKeydown page =
  evaluateHandle
    page
    "() => { const lastEvent = { repeat: false, location: -1, code: '', key: '', metaKey: false, keyIdentifier: 'unsupported' }; document.addEventListener('keydown', e => { lastEvent.repeat = e.repeat; lastEvent.location = e.location; lastEvent.key = e.key; lastEvent.code = e.code; lastEvent.metaKey = e.metaKey; lastEvent.keyIdentifier = 'keyIdentifier' in e && typeof e['keyIdentifier'] === 'string' && e['keyIdentifier']; }, true); return lastEvent; }"

testEnterKey :: Page -> JsHandle -> Text -> Text -> Text -> Playwright ()
testEnterKey page lastEvent key expectedKey expectedCode = do
  keyboardPress page (Key key)
  assertEqual "key" expectedKey =<< evaluateOnHandleText lastEvent "e => e.key"
  assertEqual "code" expectedCode =<< evaluateOnHandleText lastEvent "e => e.code"
  assertEqual "newline" "\n" =<< evaluateOnLocatorText (locate page "textarea") "el => el.value"
  _ <- evaluate page "() => { document.querySelector('textarea').value = ''; }"
  pure ()

attachFrame :: Page -> Text -> Text -> Playwright Frame
attachFrame page prefix frameId = do
  handle <-
    evaluateHandle
      page
      ( "async () => { const frame = document.createElement('iframe'); frame.src = '"
          <> prefix
          <> "/input/textarea.html'; frame.id = '"
          <> frameId
          <> "'; document.body.appendChild(frame); await new Promise(x => frame.onload = x); return frame; }"
      )
  element <- requireHandle =<< asElement handle
  found <- contentFrameHandle element
  case found of
    Nothing ->
      assertFailure "expected frame"
    Just frame ->
      pure frame

shadowEditorsHtml :: Text
shadowEditorsHtml =
  "<html><body><shadow-element></shadow-element><script>customElements.define('shadow-element', class extends HTMLElement { constructor() { super(); this.attachShadow({ mode: 'open' }); } connectedCallback() { this.shadowRoot.innerHTML = `<style>.editor { padding: 1rem; margin: 1rem; border: 1px solid #ccc; }</style><div class=editor contenteditable id=foo></div><hr><section><div class=editor contenteditable id=bar></div></section>`; } });</script></body></html>"

shadowNestedEditorsHtml :: Text
shadowNestedEditorsHtml =
  "<html><body><shadow-element></shadow-element><script>customElements.define('shadow-element', class extends HTMLElement { constructor() { super(); this.attachShadow({ mode: 'open' }); } connectedCallback() { this.shadowRoot.innerHTML = `<style>.editor { padding: 1rem; margin: 1rem; border: 1px solid #ccc; }</style><div class=editor contenteditable id=foo><p>hello</p></div><hr><section><div class=editor contenteditable id=bar><p>world</p></div></section>`; } });</script></body></html>"

shadowInputsHtml :: Text
shadowInputsHtml =
  "<html><body><shadow-element></shadow-element><script>customElements.define('shadow-element', class extends HTMLElement { constructor() { super(); this.attachShadow({ mode: 'open' }); } connectedCallback() { this.shadowRoot.innerHTML = `<style>.editor { padding: 1rem; margin: 1rem; border: 1px solid #ccc; }</style><input class=editor id=foo><hr><section><input class=editor id=bar></section>`; } });</script></body></html>"


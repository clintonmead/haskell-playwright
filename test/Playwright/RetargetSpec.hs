module Playwright.RetargetSpec
  ( tests
  ) where

import Control.Concurrent.Async
  ( wait
  , withAsync
  )
import Control.Monad
  ( forM_
  )
import Data.IORef
  ( newIORef
  , readIORef
  , writeIORef
  )
import Data.Text
  ( Text
  )
import Playwright
  ( ElementState (ElementDisabled, ElementEditable, ElementEnabled, ElementHidden, ElementVisible)
  , InputFile (InputFile)
  , InputFileName (InputFileName)
  , Page
  , SelectOption (SelectValue)
  , check
  , click
  , evalOnSelector
  , evalOnSelectorBool
  , evalOnSelectorText
  , evaluate
  , evaluateNumber
  , evaluateOnElement
  , evaluateOnElementNumber
  , evaluateOnElementText
  , evaluateText
  , fill
  , getAttribute
  , innerText
  , inputValue
  , inputValueHandle
  , isChecked
  , isDisabled
  , isEditable
  , isEditableHandle
  , isEnabled
  , isHidden
  , isVisible
  , locatorSelectText
  , pageIsDisabled
  , pageIsEnabled
  , pageIsHidden
  , pageIsVisible
  , querySelector
  , selectOption
  , selectorFromText
  , setChecked
  , setContent
  , setInputFiles
  , textContent
  , uncheck
  , waitForElementState
  )
import Playwright.Harness
  ( locate
  , readTestAsset
  , requireHandle
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
    "retarget"
    [ testCase "element state checks should work as expected for label with zero-sized input" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<label>\n    Click me\n    <input disabled style=\"width:0;height:0;padding:0;margin:0;border:0;\">\n  </label>"
          assertEqual "visible" True =<< pageIsVisible page (selectorFromText "text=Click me")
          assertEqual "hidden" False =<< pageIsHidden page (selectorFromText "text=Click me")
          assertEqual "enabled" False =<< pageIsEnabled page (selectorFromText "text=Click me")
          assertEqual "disabled" True =<< pageIsDisabled page (selectorFromText "text=Click me")
    , testCase "should wait for enclosing disabled button" $
        withChromiumPage $ \page -> do
          setContent page "<button><span>Target</span></button>"
          handle <- requireHandle =<< querySelector page (selectorFromText "text=Target")
          done <- newIORef False
          withAsync
            (waitForElementState handle ElementDisabled >> writeIORef done True)
            $ \pending -> do
              rafraf page 5
              assertEqual "still waiting" False =<< readIORef done
              _ <-
                evaluateOnElement
                  handle
                  "span => { span.parentElement.disabled = true; return true; }"
              wait pending
    , testCase "should wait for enclosing button with a disabled fieldset" $
        withChromiumPage $ \page -> do
          setContent page "<fieldset disabled=true><button><span>Target</span></button></div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "text=Target")
          done <- newIORef False
          withAsync
            (waitForElementState handle ElementEnabled >> writeIORef done True)
            $ \pending -> do
              rafraf page 5
              assertEqual "still waiting" False =<< readIORef done
              _ <-
                evaluateOnElement
                  handle
                  "span => { span.parentElement.parentElement.disabled = false; return true; }"
              wait pending
    , testCase "should wait for enclosing enabled button" $
        withChromiumPage $ \page -> do
          setContent page "<button disabled><span>Target</span></button>"
          handle <- requireHandle =<< querySelector page (selectorFromText "text=Target")
          done <- newIORef False
          withAsync
            (waitForElementState handle ElementEnabled >> writeIORef done True)
            $ \pending -> do
              rafraf page 5
              assertEqual "still waiting" False =<< readIORef done
              _ <-
                evaluateOnElement
                  handle
                  "span => { span.parentElement.disabled = false; return true; }"
              wait pending
    , testCase "should check the box outside shadow dom label" $
        withChromiumPage $ \page -> do
          setContent page "<div></div>"
          _ <-
            evalOnSelector
              page
              (selectorFromText "div")
              "div => {\n\
              \  const root = div.attachShadow({ mode: 'open' });\n\
              \  const label = document.createElement('label');\n\
              \  label.setAttribute('for', 'target');\n\
              \  label.textContent = 'Click me';\n\
              \  root.appendChild(label);\n\
              \  const input = document.createElement('input');\n\
              \  input.setAttribute('type', 'checkbox');\n\
              \  input.setAttribute('id', 'target');\n\
              \  root.appendChild(input);\n\
              \}"
          check (locate page "label")
          assertEqual "checked" True
            =<< evalOnSelectorBool page (selectorFromText "input") "input => input.checked"
    , testCase "setInputFiles should work with label" $
        withChromiumPage $ \page -> do
          setContent page "<label for=target>Choose a file</label><input id=target type=file>"
          bytes <- readTestAsset "file-to-upload.txt"
          setInputFiles
            (locate page "text=Choose a file")
            [InputFile (InputFileName "file-to-upload.txt") bytes]
          handle <- requireHandle =<< querySelector page (selectorFromText "input")
          assertEqual "count" (1 :: Double) =<< evaluateOnElementNumber handle "e => e.files.length"
          assertEqual "name" "file-to-upload.txt"
            =<< evaluateOnElementText handle "e => e.files[0].name"
    , testCase "enabled/disabled retargeting" $
        withChromiumPage $ \page ->
          forM_ enabledCases $ \(dom, enabled, sel) -> do
            setContent page dom
            let
              target = locate page sel
            handle <- requireHandle =<< querySelector page (selectorFromText sel)
            assertEqual "enabled" enabled =<< isEnabled target
            assertEqual "disabled" (not enabled) =<< isDisabled target
            waitForElementState
              handle
              (if enabled then ElementEnabled else ElementDisabled)
    , testCase "visible/hidden retargeting" $
        withChromiumPage $ \page ->
          forM_ visibleCases $ \(dom, visible, sel) -> do
            setContent page dom
            let
              target = locate page sel
            handle <- requireHandle =<< querySelector page (selectorFromText sel)
            assertEqual "visible" visible =<< isVisible target
            assertEqual "hidden" (not visible) =<< isHidden target
            waitForElementState
              handle
              (if visible then ElementVisible else ElementHidden)
    , testCase "editable retargeting" $
        withChromiumPage $ \page ->
          forM_ editableCases $ \(dom, editable, sel) -> do
            setContent page dom
            let
              target = locate page sel
            handle <- requireHandle =<< querySelector page (selectorFromText sel)
            assertEqual "editable" editable =<< isEditable target
            if editable
              then waitForElementState handle ElementEditable
              else pure ()
    , testCase "input value retargeting" $
        withChromiumPage $ \page ->
          forM_ inputValueCases $ \(dom, sel) -> do
            setContent page dom
            let
              target = locate page sel
            handle <- requireHandle =<< querySelector page (selectorFromText sel)
            assertEqual "empty locator" "" =<< inputValue target
            assertEqual "empty handle" "" =<< inputValueHandle handle
            fill target "foo"
            assertEqual "foo locator" "foo" =<< inputValue target
            assertEqual "foo handle" "foo" =<< inputValueHandle handle
            _ <-
              evalOnSelectorText
                page
                (selectorFromText "#target")
                "input => { input.value = 'bar'; return input.value; }"
            assertEqual "bar locator" "bar" =<< inputValue target
            assertEqual "bar handle" "bar" =<< inputValueHandle handle
            locatorSelectText target
            assertEqual "selection" "bar" =<< evaluateText page "() => window.getSelection().toString()"
    , testCase "selection retargeting" $
        withChromiumPage $ \page ->
          forM_ selectionCases $ \(dom, sel) -> do
            setContent page dom
            let
              target = locate page sel
            handle <- requireHandle =<< querySelector page (selectorFromText sel)
            assertEqual "locator editable" True =<< isEditable target
            assertEqual "handle editable" True =<< isEditableHandle handle
            fill target "foo"
            locatorSelectText target
            assertEqual "selection" "foo" =<< evaluateText page "() => window.getSelection().toString()"
    , testCase "select options retargeting" $
        withChromiumPage $ \page ->
          forM_ selectCases $ \(dom, sel) -> do
            setContent page dom
            let
              target = locate page sel
            handle <- requireHandle =<< querySelector page (selectorFromText sel)
            assertEqual "dog locator" "dog" =<< inputValue target
            assertEqual "dog handle" "dog" =<< inputValueHandle handle
            _ <- selectOption target [SelectValue "cat"]
            assertEqual "cat locator" "cat" =<< inputValue target
            assertEqual "cat handle" "cat" =<< inputValueHandle handle
    , testCase "direct actions retargeting" $
        withChromiumPage $ \page ->
          forM_ directCases $ \(dom, sel) -> do
            setContent page dom
            let
              target = locate page sel
            assertEqual "innerText" "content" =<< innerText target
            assertEqual "textContent" (Just "content") =<< textContent target
            _ <-
              evalOnSelectorText
                page
                (selectorFromText "div")
                "div => { div.id = 'myid'; return div.id; }"
            assertEqual "id" (Just "myid") =<< getAttribute target "id"
    , testCase "check retargeting" $
        withChromiumPage $ \page ->
          forM_ checkCases $ \(dom, sel) -> do
            setContent page dom
            let
              target = locate page sel
            assertEqual "unchecked" False =<< isChecked target
            _ <-
              evalOnSelectorBool
                page
                (selectorFromText "input")
                "input => { input.checked = true; return input.checked; }"
            assertEqual "checked" True =<< isChecked target
            uncheck target
            assertEqual "unchecked again" False
              =<< evalOnSelectorBool page (selectorFromText "input") "input => input.checked"
            check target
            assertEqual "checked again" True
              =<< evalOnSelectorBool page (selectorFromText "input") "input => input.checked"
            setChecked target False
            assertEqual "setChecked false" False
              =<< evalOnSelectorBool page (selectorFromText "input") "input => input.checked"
    , testCase "should not retarget anchor into parent label" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<label disabled>Text<a href='#' onclick='window.__clicked=1'>Target</a></label>"
          click (locate page "a")
          assertEqual "clicked 1" (1 :: Double) =<< evaluateNumber page "() => window.__clicked"
          setContent
            page
            "<input type=\"radio\" id=\"input-id\" checked disabled />\n    <label for=\"input-id\">Text<a href='#' onclick='window.__clicked=2'>Target</a></label>"
          click (locate page "a")
          assertEqual "clicked 2" (2 :: Double) =<< evaluateNumber page "() => window.__clicked"
    ]

rafraf :: Page -> Int -> IO ()
rafraf page count =
  forM_ [1 .. count] $ \(_ :: Int) -> do
    _ <-
      evaluate
        page
        "async () => { await new Promise(f => window.requestAnimationFrame(() => window.requestAnimationFrame(f))); }"
    pure ()

enabledCases :: [(Text, Bool, Text)]
enabledCases =
  [ (domInLabel "<input id=target>" False False False, True, "label")
  , (domLabelFor "<input id=target>" False False False, True, "label")
  , (domStandalone "<input id=target>", True, "input")
  , (domInButton "<input id=target>" False False False, True, "input")
  , (domInButton "<input id=target>" True False False, True, "input")
  , (domInLabel "<li role=menuitem id=target aria-disabled=false></li>" False False False, True, "li")
  , (domInLabel "<input id=target disabled>" False False False, False, "label")
  , (domLabelFor "<input id=target disabled>" False False False, False, "label")
  , (domStandalone "<input id=target disabled>", False, "input")
  , (domInButton "<input id=target disabled>" False False False, False, "input")
  , (domInButton "<input id=target disabled>" True False False, False, "input")
  , (domInLabel "<li role=menuitem id=target aria-disabled=true></li>" False False False, False, "li")
  ]

visibleCases :: [(Text, Bool, Text)]
visibleCases =
  [ (domInLabel "<span id=target>content</span>" False False False, True, "label")
  , (domInLabel "<span id=target hidden>content</span>" False False False, True, "label")
  , (domLabelFor "<span id=target>content</span>" False False False, True, "label")
  , (domLabelFor "<span id=target hidden>content</span>" False False False, True, "label")
  , (domStandalone "<span id=target>content</span>", True, "span")
  , (domInButton "<span id=target>content</span>" False False False, True, "span")
  , (domInLabel "<span id=target>content</span>" False True False, False, "label")
  , (domLabelFor "<span id=target>content</span>" False True False, False, "label")
  , (domStandalone "<span id=target hidden>content</span>", False, "span")
  , (domInButton "<span id=target hidden>content</span>" False False False, False, "span")
  , (domInButton "<span id=target>content</span>" False True False, False, "span")
  ]

editableCases :: [(Text, Bool, Text)]
editableCases =
  [ (domInLabel "<input id=target>" False False False, True, "label")
  , (domLabelFor "<input id=target>" False False False, True, "label")
  , (domStandalone "<input id=target>", True, "input")
  , (domInButton "<input id=target>" False False False, True, "input")
  , (domInButton "<input id=target>" False False True, True, "input")
  , (domInLabel "<input id=target readonly>" False False False, False, "label")
  , (domLabelFor "<input id=target readonly>" False False False, False, "label")
  , (domStandalone "<input id=target readonly>", False, "input")
  , (domInButton "<input id=target readonly>" False False False, False, "input")
  , (domInButton "<input id=target readonly>" False False True, False, "input")
  ]

inputValueCases :: [(Text, Text)]
inputValueCases =
  [ (domInLabel "<input id=target>" False False False, "label")
  , (domLabelFor "<input id=target>" False False False, "label")
  , (domStandalone "<input id=target>", "input")
  , (domInButton "<input id=target>" False False False, "input")
  ]

selectionCases :: [(Text, Text)]
selectionCases =
  [ (domStandalone "<div contenteditable id=target>content</div>", "div")
  , (domInButton "<div contenteditable id=target>content</div>" False False False, "div")
  ]

selectCases :: [(Text, Text)]
selectCases =
  [ (domInLabel selectHtml False False False, "label")
  , (domLabelFor selectHtml False False False, "label")
  , (domStandalone selectHtml, "select")
  , (domInButton selectHtml False False False, "select")
  ]

directCases :: [(Text, Text)]
directCases =
  [ (domInLabel "<div>content</div><input id=target value=oh>" False False False, "div")
  , (domLabelFor "<div>content</div><input id=target value=oh>" False False False, "div")
  , (domStandalone "<div>content</div>", "div")
  , (domInButton "<div>content</div>" False False False, "div")
  ]

checkCases :: [(Text, Text)]
checkCases =
  [ (domInLabel "<input type=checkbox id=target>" False False False, "label")
  , (domLabelFor "<input type=checkbox id=target>" False False False, "label")
  , (domStandalone "<input type=checkbox id=target>", "input")
  , (domInButton "<input type=checkbox id=target>" False False False, "input")
  ]

selectHtml :: Text
selectHtml =
  "<select id=target multiple><option value=dog selected>Dog</option><option value=cat>Cat</option></select>"

domInLabel :: Text -> Bool -> Bool -> Bool -> Text
domInLabel inner disabled hidden readonly =
  "<label" <> optionsToAttributes disabled hidden readonly <> ">Text " <> inner <> "</label>"

domLabelFor :: Text -> Bool -> Bool -> Bool -> Text
domLabelFor inner disabled hidden readonly =
  "<label"
    <> optionsToAttributes disabled hidden readonly
    <> " for=\"target\"><h1>Text</h1></label>"
    <> inner

domStandalone :: Text -> Text
domStandalone inner =
  inner

domInButton :: Text -> Bool -> Bool -> Bool -> Text
domInButton inner disabled hidden readonly =
  "<button" <> optionsToAttributes disabled hidden readonly <> ">Button " <> inner <> "</button>"

optionsToAttributes :: Bool -> Bool -> Bool -> Text
optionsToAttributes disabled hidden readonly =
  (if disabled then " disabled" else "")
    <> (if hidden then " hidden" else "")
    <> (if readonly then " readonly" else "")

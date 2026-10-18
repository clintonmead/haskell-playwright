module Playwright.PageFocusSpec
  ( tests
  ) where

import Playwright
  ( Key (Key)
  , click
  , evaluateText
  , evaluateTexts
  , evalOnSelectorText
  , focus
  , keyboardPress
  , keyboardType
  , selectorFromText
  , setContent
  )
import Playwright.Harness
  ( locate
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
    "page-focus"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          setContent page "<div id=d1 tabIndex=0></div>"
          assertEqual "body" "BODY" =<< evaluateText page "() => document.activeElement.nodeName"
          focus (locate page "#d1")
          assertEqual "id" "d1" =<< evaluateText page "() => document.activeElement.id"
    , testCase "should emit focus event" $
        withChromiumPage $ \page -> do
          setContent page "<div id=d1 tabIndex=0></div>"
          _ <-
            evaluateText
              page
              "() => { window.focusEvents = []; document.getElementById('d1').addEventListener('focus', () => window.focusEvents.push('focus')); return 'ok'; }"
          focus (locate page "#d1")
          assertEqual "focused" ["focus"] =<< evaluateTexts page "() => window.focusEvents"
    , testCase "should emit blur event" $
        withChromiumPage $ \page -> do
          setContent page "<div id=d1 tabIndex=0>DIV1</div><div id=d2 tabIndex=0>DIV2</div>"
          focus (locate page "#d1")
          _ <-
            evaluateText
              page
              "() => { window.focusEvents = []; document.getElementById('d1').addEventListener('blur', () => window.focusEvents.push('blur')); document.getElementById('d2').addEventListener('focus', () => window.focusEvents.push('focus')); return 'ok'; }"
          focus (locate page "#d2")
          assertEqual "events" ["blur", "focus"] =<< evaluateTexts page "() => window.focusEvents"
    , testCase "should traverse focus" $
        withChromiumPage $ \page -> do
          setContent page "<input id=\"i1\"><input id=\"i2\">"
          _ <-
            evaluateText
              page
              "() => { window.focusEvents = []; document.getElementById('i2').addEventListener('focus', () => window.focusEvents.push('focus')); return 'ok'; }"
          focus (locate page "#i1")
          keyboardType page "First"
          keyboardPress page (Key "Tab")
          keyboardType page "Last"
          assertEqual "focused" ["focus"] =<< evaluateTexts page "() => window.focusEvents"
          assertEqual "i1" "First" =<< evalOnSelectorText page (selectorFromText "#i1") "e => e.value"
          assertEqual "i2" "Last" =<< evalOnSelectorText page (selectorFromText "#i2") "e => e.value"
    , testCase "should traverse focus in all directions" $
        withChromiumPage $ \page -> do
          setContent page "<input value=\"1\"><input value=\"2\"><input value=\"3\">"
          let
            activeValue = evaluateText page "() => document.activeElement.value"
          keyboardPress page (Key "Tab")
          assertEqual "first" "1" =<< activeValue
          keyboardPress page (Key "Tab")
          assertEqual "second" "2" =<< activeValue
          keyboardPress page (Key "Tab")
          assertEqual "third" "3" =<< activeValue
          keyboardPress page (Key "Shift+Tab")
          assertEqual "back to second" "2" =<< activeValue
          keyboardPress page (Key "Shift+Tab")
          assertEqual "back to first" "1" =<< activeValue
    , testCase "clicking checkbox should activate it" $
        withChromiumPage $ \page -> do
          setContent page "<input type=checkbox></input>"
          click (locate page "input")
          assertEqual "nodeName" "INPUT" =<< evaluateText page "() => document.activeElement.nodeName"
    , testCase "tab should cycle between single input and browser" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<label for=\"input1\">input1</label>\n    <input id=\"input1\">\n    <script>\n    {\n      window.focusEvents = [];\n      const input = document.getElementById('input1');\n      input.addEventListener('blur', () => focusEvents.push('blur'));\n      input.addEventListener('focus', () => focusEvents.push('focus'));\n    }\n    </script>"
          let
            activeTag = evaluateText page "() => document.activeElement.tagName"
            activeId = evaluateText page "() => document.activeElement.id"
          assertEqual "start" "BODY" =<< activeTag
          keyboardPress page (Key "Tab")
          assertEqual "input focused" "input1" =<< activeId
          assertEqual "focus events 1" ["focus"] =<< evaluateTexts page "() => window.focusEvents"
          keyboardPress page (Key "Tab")
          assertEqual "browser focused" "BODY" =<< activeTag
          assertEqual "focus events 2" ["focus", "blur"] =<< evaluateTexts page "() => window.focusEvents"
          keyboardPress page (Key "Tab")
          assertEqual "input focused again" "input1" =<< activeId
          assertEqual "focus events 3" ["focus", "blur", "focus"] =<< evaluateTexts page "() => window.focusEvents"
    , testCase "tab should cycle between document elements and browser" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<input id=\"input1\">\n    <input id=\"input2\">\n    <script>\n      window.focusEvents = [];\n      {\n        const input = document.getElementById('input1');\n        input.addEventListener('blur', () => focusEvents.push('blur1'));\n        input.addEventListener('focus', () => focusEvents.push('focus1'));\n      }\n      {\n        const input = document.getElementById('input2');\n        input.addEventListener('blur', () => focusEvents.push('blur2'));\n        input.addEventListener('focus', () => focusEvents.push('focus2'));\n      }\n    </script>"
          let
            activeTag = evaluateText page "() => document.activeElement.tagName"
            activeId = evaluateText page "() => document.activeElement.id"
          assertEqual "start" "BODY" =<< activeTag
          keyboardPress page (Key "Tab")
          assertEqual "input1 focused" "input1" =<< activeId
          assertEqual "events 1" ["focus1"] =<< evaluateTexts page "() => window.focusEvents"
          keyboardPress page (Key "Tab")
          assertEqual "input2 focused" "input2" =<< activeId
          assertEqual "events 2" ["focus1", "blur1", "focus2"] =<< evaluateTexts page "() => window.focusEvents"
          keyboardPress page (Key "Tab")
          assertEqual "browser focused" "BODY" =<< activeTag
          assertEqual "events 3" ["focus1", "blur1", "focus2", "blur2"] =<< evaluateTexts page "() => window.focusEvents"
          keyboardPress page (Key "Tab")
          assertEqual "input1 focused again" "input1" =<< activeId
          assertEqual "events 4" ["focus1", "blur1", "focus2", "blur2", "focus1"] =<< evaluateTexts page "() => window.focusEvents"
    , testCase "keeps focus on element when attempting to focus a non-focusable element" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div id=\"focusable\" tabindex=\"0\">focusable</div>\n      <div id=\"non-focusable\">not focusable</div>\n      <script>\n        window.eventLog = [];\n\n        const focusable = document.getElementById(\"focusable\");\n\n        focusable.addEventListener('blur', () => window.eventLog.push('blur focusable'));\n        focusable.addEventListener('focus', () => window.eventLog.push('focus focusable'));\n\n        const nonFocusable = document.getElementById(\"non-focusable\");\n        nonFocusable.addEventListener('blur', () => window.eventLog.push('blur non-focusable'));\n        nonFocusable.addEventListener('focus', () => window.eventLog.push('focus non-focusable'));\n      </script>"
          click (locate page "#focusable")
          assertEqual "focusable" "focusable" =<< evaluateText page "() => document.activeElement?.id"
          focus (locate page "#non-focusable")
          assertEqual "still focusable" "focusable" =<< evaluateText page "() => document.activeElement?.id"
          assertEqual "event log" ["focus focusable"] =<< evaluateTexts page "() => window.eventLog"
    ]

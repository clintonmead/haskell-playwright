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
  , evaluateOnLocatorText
  , focus
  , keyboardDown
  , keyboardInsertText
  , keyboardPress
  , keyboardType
  , keyboardUp
  , press
  , typeText
  )
import Playwright.Harness
  ( locate
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
    ]

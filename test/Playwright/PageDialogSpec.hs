module Playwright.PageDialogSpec
  ( tests
  ) where

import UnliftIO.Async
  ( withAsync
  )
import Playwright
  ( Dialog (dialogDefaultValue, dialogMessage, dialogType)
  , DialogType (DialogAlert, DialogPrompt)
  , SpecialJsValue (SpecialNull)
  , acceptDialog
  , acceptDialogWith
  , armDialog
  , dismissDialog
  , evaluateBool
  , evaluateSpecial
  , evaluateText
  , onDialog
  , setContent
  , click
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
    "page-dialog"
    [ testCase "should fire" $
        withChromiumPage $ \page -> do
          takeDialog <- armDialog page
          withAsync
            ( do
                dialog <- takeDialog
                assertEqual "type" DialogAlert (dialogType dialog)
                assertEqual "default" "" (dialogDefaultValue dialog)
                assertEqual "message" "yo" (dialogMessage dialog)
                acceptDialog dialog
            )
            $ \_ -> do
              assertEqual "returned" True =<< evaluateBool page "() => { alert('yo'); return true; }"
    , testCase "should allow accepting prompts" $
        withChromiumPage $ \page -> do
          takeDialog <- armDialog page
          withAsync
            ( do
                dialog <- takeDialog
                assertEqual "type" DialogPrompt (dialogType dialog)
                assertEqual "default" "yes." (dialogDefaultValue dialog)
                assertEqual "message" "question?" (dialogMessage dialog)
                acceptDialogWith dialog (Just "answer!")
            )
            $ \_ -> do
              assertEqual "answer" "answer!" =<< evaluateText page "() => prompt('question?', 'yes.')"
    , testCase "should dismiss the prompt" $
        withChromiumPage $ \page -> do
          takeDialog <- armDialog page
          withAsync (takeDialog >>= dismissDialog) $ \_ -> do
            special <- evaluateSpecial page "() => prompt('question?')"
            assertEqual "null" SpecialNull special
    , testCase "should accept the confirm prompt" $
        withChromiumPage $ \page -> do
          takeDialog <- armDialog page
          withAsync (takeDialog >>= acceptDialog) $ \_ -> do
            assertEqual "true" True =<< evaluateBool page "() => confirm('boolean?')"
    , testCase "should dismiss the confirm prompt" $
        withChromiumPage $ \page -> do
          takeDialog <- armDialog page
          withAsync (takeDialog >>= dismissDialog) $ \_ -> do
            assertEqual "false" False =<< evaluateBool page "() => confirm('boolean?')"
    , testCase "should handle multiple alerts" $
        withChromiumPage $ \page -> do
          onDialog page acceptDialog
          setContent
            page
            "\n    <p>Hello World</p>\n    <script>\n      alert('Please dismiss this dialog');\n      alert('Please dismiss this dialog');\n      alert('Please dismiss this dialog');\n    </script>\n  "
          assertEqual "text" "Hello World" =<< evaluateText page "() => document.querySelector('p').textContent"
    , testCase "should handle multiple confirms" $
        withChromiumPage $ \page -> do
          onDialog page acceptDialog
          setContent
            page
            "\n    <p>Hello World</p>\n    <script>\n      confirm('Please confirm me?');\n      confirm('Please confirm me?');\n      confirm('Please confirm me?');\n    </script>\n  "
          assertEqual "text" "Hello World" =<< evaluateText page "() => document.querySelector('p').textContent"
    , testCase "should auto-dismiss the prompt without listeners" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecial page "() => prompt('question?')"
          assertEqual "result" SpecialNull special
    , testCase "should auto-dismiss the alert without listeners" $
        withChromiumPage $ \page -> do
          setContent page "<div onclick=\"window.alert(123); window._clicked=true\">Click me</div>"
          click (locate page "div")
          assertEqual "clicked" True =<< evaluateBool page "() => window._clicked"

    ]

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
  )
import Playwright.Harness
  ( withChromiumPage
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
    ]

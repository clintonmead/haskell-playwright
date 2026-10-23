module Playwright.PageDialogSpec
  ( tests
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Maybe
  ( fromMaybe
  )
import System.Timeout
  ( timeout
  )
import UnliftIO.Async
  ( withAsync
  )
import UnliftIO.Concurrent
  ( threadDelay
  )
import UnliftIO.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  )
import Playwright
  ( Dialog (dialogDefaultValue, dialogMessage, dialogType)
  , DialogType (DialogAlert, DialogConfirm, DialogPrompt)
  , SpecialJsValue (SpecialNull)
  , acceptDialog
  , acceptDialogWith
  , armDialog
  , dialogsEqual
  , dismissDialog
  , evaluateBool
  , evaluateSpecial
  , evaluateText
  , onDialog
  , onPageDialogClosed
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

-- Waits up to 30 seconds for the first entry to land in the ref.
awaitClosed :: IORef [a] -> IO [a]
awaitClosed ref =
  fromMaybe [] <$> timeout 30000000 (poll ref)
  where
    poll :: IORef [a] -> IO [a]
    poll r = do
      items <- readIORef r
      if null items
        then threadDelay 50000 >> poll r
        else pure items

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
    , testCase "should fire dialogclosed when dialog is accepted" $
        withChromiumPage $ \page -> do
          opened <- newIORef []
          closed <- newIORef []
          onDialog page $ \dialog -> do
            liftIO (modifyIORef' opened (dialog :))
            acceptDialog dialog
          onPageDialogClosed page (\dialog -> liftIO (modifyIORef' closed (dialog :)))
          _ <- evaluateBool page "() => { alert('yo'); return true; }"
          closedDialogs <- liftIO (awaitClosed closed)
          assertEqual "closed count" 1 (length closedDialogs)
          openedDialogs <- liftIO (readIORef opened)
          assertEqual "same dialog" True (and (zipWith dialogsEqual closedDialogs openedDialogs))
          assertEqual "roundtrip" True =<< evaluateBool page "() => true"
          assertEqual "roundtrip" True =<< evaluateBool page "() => true"
          closedAfter <- liftIO (readIORef closed)
          assertEqual "closed count after roundtrips" 1 (length closedAfter)
    , testCase "should fire dialogclosed when dialog is dismissed" $
        withChromiumPage $ \page -> do
          closed <- newIORef []
          onDialog page dismissDialog
          onPageDialogClosed page (\dialog -> liftIO (modifyIORef' closed (dialog :)))
          _ <- evaluateBool page "() => confirm('boolean?')"
          closedDialogs <- liftIO (awaitClosed closed)
          assertEqual "types" [DialogConfirm] (map dialogType closedDialogs)
          assertEqual "messages" ["boolean?"] (map dialogMessage closedDialogs)
    , testCase "should fire dialogclosed for auto-dismissed dialogs" $
        withChromiumPage $ \page -> do
          closed <- newIORef []
          onPageDialogClosed page (\dialog -> liftIO (modifyIORef' closed (dialog :)))
          _ <- evaluateBool page "() => { alert('yo'); return true; }"
          closedDialogs <- liftIO (awaitClosed closed)
          assertEqual "types" [DialogAlert] (map dialogType closedDialogs)
          assertEqual "messages" ["yo"] (map dialogMessage closedDialogs)
    , testCase "should be able to close context with open alert" $
        withChromiumPage $ \page -> do
          takeDialog <- armDialog page
          _ <- evaluateBool page "() => { window.setTimeout(() => alert('hello'), 0); return true; }"
          dialog <- takeDialog
          assertEqual "message" "hello" (dialogMessage dialog)

    ]

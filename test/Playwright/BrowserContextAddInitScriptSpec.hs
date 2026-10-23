module Playwright.BrowserContextAddInitScriptSpec
  ( tests
  ) where

import Data.Text
  ( Text
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
  ( Playwright
  , addContextInitScript
  , addContextInitScriptExposing
  , addInitScript
  , armPopup
  , contextExposeFunction
  , defaultGotoOptions
  , defaultNewContextOptions
  , dispose
  , evaluateBool
  , evaluateNumber
  , goto
  , newPage
  , urlFromText
  )
import Playwright.BindingName
  ( BindingName (BindingName)
  )
import Playwright.Harness
  ( withAssetsContextWith
  , assertEqual
  , assertFailure
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  , numberArgument
  , undefinedArgument
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadNumber, PayloadString)
  , SerializedValue (SerializedNode)
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
    "browsercontext-add-init-script"
    [ testCase "should work with browser context scripts" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          _ <- addContextInitScript ctx "window[\"temp\"] = 123"
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "temp" (123 :: Double) =<< evaluateNumber page "() => window.temp"
    , testCase "should work with browser context scripts for already created pages" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          _ <- addContextInitScript ctx "window[\"temp\"] = 123"
          addInitScript page "window[\"injected\"] = window[\"temp\"]"
          _ <- goto page (urlFromText (prefix <> "/tamperable.html")) defaultGotoOptions
          assertEqual "result" (123 :: Double) =<< evaluateNumber page "() => window.result"
    , testCase "should remove context init script after dispose" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          script <- addContextInitScript ctx "window[\"temp\"] = 123"
          _ <- goto page (urlFromText (prefix <> "/tamperable.html")) defaultGotoOptions
          assertEqual "before" (123 :: Double) =<< evaluateNumber page "() => window.temp"
          dispose script
          _ <- goto page (urlFromText (prefix <> "/tamperable.html")) defaultGotoOptions
          assertEqual "after" True =<< evaluateBool page "() => window.temp === undefined"
    , testCase "should expose functions passed as arguments" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          received <- newIORef []
          _ <-
            addContextInitScriptExposing
              ctx
              "async ({ cb }) => { await cb(location.href); }"
              [("cb", recordText received)]
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          pollUntil (elem (prefix <> "/empty.html") <$> readIORef received)
    , testCase "should work without navigation, after all bindings" $
        withAssetsContextWith defaultNewContextOptions $ \_ ctx _ -> do
          received <- newIORef []
          _ <- contextExposeFunction ctx (BindingName "woof") (recordText received)
          _ <- addContextInitScript ctx "window['woof']('hey'); window['temp'] = 123;"
          page <- newPage ctx
          assertEqual "temp" (123 :: Double) =<< evaluateNumber page "() => window.temp"
          pollUntil (elem "hey" <$> readIORef received)
    , testCase "should work without navigation in popup" $
        withAssetsContextWith defaultNewContextOptions $ \_ ctx _ -> do
          _ <- addContextInitScript ctx "window['temp'] = 123"
          page <- newPage ctx
          awaitPopup <- armPopup page
          _ <- evaluateBool page "() => !!(window.win = window.open())"
          popup <- awaitPopup
          assertEqual "temp" (123 :: Double) =<< evaluateNumber popup "() => window.temp"
    -- Skipped: "should work with browser context scripts with a path". addContextInitScript takes script
    -- source only, so there is no path option to port.
    -- "should work with browser context scripts with a path"
    , testCase "should remove context init script and keep working in new pages" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx _ -> do
          script <- addContextInitScript ctx "window['temp'] = 123"
          dispose script
          page <- newPage ctx
          _ <- goto page (urlFromText (prefix <> "/tamperable.html")) defaultGotoOptions
          assertEqual "after" True =<< evaluateBool page "() => window.temp === undefined"
    , testCase "init script should run only once in popup" $
        withAssetsContextWith defaultNewContextOptions $ \_ ctx _ -> do
          _ <- addContextInitScript ctx "window['callCount'] = (window['callCount'] || 0) + 1;"
          page <- newPage ctx
          awaitPopup <- armPopup page
          _ <- evaluateBool page "() => !!window.open('about:blank')"
          popup <- awaitPopup
          assertEqual "count" (1 :: Double) =<< evaluateNumber popup "() => window.callCount"
    , testCase "should expose functions that survive navigation" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          received <- newIORef []
          _ <-
            addContextInitScriptExposing
              ctx
              "({ cb }) => { window.cb = cb; }"
              [("cb", doubleAndRecord received)]
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "first" (2 :: Double) =<< evaluateNumber page "() => window.cb(1)"
          _ <- goto page (urlFromText (prefix <> "/tamperable.html")) defaultGotoOptions
          assertEqual "second" (4 :: Double) =<< evaluateNumber page "() => window.cb(2)"
          assertEqual "received" [1, 2] =<< readIORef received
    , testCase "should expose functions in popups" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          _ <-
            addContextInitScriptExposing
              ctx
              "({ mul }) => { window.mul = mul; }"
              [("mul", multiply)]
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          awaitPopup <- armPopup page
          _ <- evaluateBool page "() => !!window.open('about:blank')"
          popup <- awaitPopup
          assertEqual "product" (42 :: Double) =<< evaluateNumber popup "() => window.mul(6, 7)"
    , testCase "should remove exposed functions after dispose" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          script <-
            addContextInitScriptExposing
              ctx
              "({ cb }) => { window.cb = cb; }"
              [("cb", doubleNumber)]
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "before" (42 :: Double) =<< evaluateNumber page "() => window.cb(21)"
          dispose script
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "after" True =<< evaluateBool page "() => typeof window.cb === 'undefined'"
    ]

-- | Records the string argument it is called with, and returns nothing.
recordText :: IORef [Text] -> [SerializedValue] -> Playwright SerializedArgument
recordText received args = do
  case args of
    [SerializedNode (PayloadString href) _] ->
      modifyIORef' received (<> [href])
    _ ->
      pure ()
  pure undefinedArgument

-- | Doubles a number and records the argument it was called with.
doubleAndRecord :: IORef [Double] -> [SerializedValue] -> Playwright SerializedArgument
doubleAndRecord received args =
  case args of
    [SerializedNode (PayloadNumber n) _] -> do
      modifyIORef' received (<> [n])
      pure (numberArgument (n * 2))
    _ ->
      pure undefinedArgument

doubleNumber :: [SerializedValue] -> Playwright SerializedArgument
doubleNumber args =
  case args of
    [SerializedNode (PayloadNumber n) _] ->
      pure (numberArgument (n * 2))
    _ ->
      pure undefinedArgument

multiply :: [SerializedValue] -> Playwright SerializedArgument
multiply args =
  case args of
    [SerializedNode (PayloadNumber left) _, SerializedNode (PayloadNumber right) _] ->
      pure (numberArgument (left * right))
    _ ->
      pure undefinedArgument

-- | Polls the check until it passes, with a bounded number of attempts.
pollUntil :: Playwright Bool -> Playwright ()
pollUntil check = go (50 :: Int)
  where
    go :: Int -> Playwright ()
    go 0 = assertFailure "condition not met"
    go attempts = do
      ok <- check
      if ok
        then pure ()
        else threadDelay 100000 >> go (attempts - 1)

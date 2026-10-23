module Playwright.PageAddInitScriptCallbackSpec
  ( tests
  ) where

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
  , addPageInitScriptExposing
  , defaultGotoOptions
  , dispose
  , evaluateBool
  , evaluateNumber
  , goto
  , urlFromText
  )
import Playwright.Harness
  ( assertEqual
  , assertFailure
  , withAssets
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  , numberArgument
  , undefinedArgument
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadNumber)
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
    "page-add-init-script-callback"
    [ testCase "should call a function passed as an argument" $
        withAssets $ \prefix page -> do
          received <- newIORef []
          _ <-
            addPageInitScriptExposing
              page
              "async ({ cb }) => { await cb(1); await cb(2); }"
              [("cb", recordNumber received)]
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          pollUntil ((== [1, 2]) <$> readIORef received)
    , testCase "should return the callback result to the page" $
        withAssets $ \prefix page -> do
          _ <-
            addPageInitScriptExposing
              page
              "async ({ double }) => { window.result = await double(21); }"
              [("double", doubleNumber)]
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          pollUntil (evaluateBool page "() => window.result === 42")
          assertEqual "result" (42 :: Double) =<< evaluateNumber page "() => window.result"
    , testCase "should support multiple callbacks" $
        withAssets $ \prefix page -> do
          _ <-
            addPageInitScriptExposing
              page
              "async ({ add, mul }) => { window.result = (await add(2, 3)) + (await mul(2, 3)); }"
              [("add", addNumbers), ("mul", multiply)]
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          pollUntil (evaluateBool page "() => window.result === 11")
          assertEqual "result" (11 :: Double) =<< evaluateNumber page "() => window.result"
    , testCase "should work in a child frame" $
        withAssets $ \prefix page -> do
          received <- newIORef []
          _ <-
            addPageInitScriptExposing
              page
              "async ({ cb }) => { await cb(42); }"
              [("cb", recordNumber received)]
          _ <- goto page (urlFromText (prefix <> "/frames/one-frame.html")) defaultGotoOptions
          -- Once for the main frame document, once for the child frame document.
          pollUntil ((== [42, 42]) <$> readIORef received)
    , testCase "should survive a navigation and keep working" $
        withAssets $ \prefix page -> do
          received <- newIORef []
          _ <-
            addPageInitScriptExposing
              page
              "({ cb }) => { window.cb = cb; }"
              [("cb", doubleAndRecord received)]
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "first" (2 :: Double) =<< evaluateNumber page "() => window.cb(1)"
          _ <- goto page (urlFromText (prefix <> "/tamperable.html")) defaultGotoOptions
          assertEqual "second" (4 :: Double) =<< evaluateNumber page "() => window.cb(2)"
          assertEqual "received" [1, 2] =<< readIORef received
    , testCase "should remove exposed functions after dispose" $
        withAssets $ \prefix page -> do
          script <-
            addPageInitScriptExposing
              page
              "({ cb }) => { window.cb = cb; }"
              [("cb", doubleNumber)]
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "before" (42 :: Double) =<< evaluateNumber page "() => window.cb(21)"
          dispose script
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "after" True =<< evaluateBool page "() => typeof window.cb === 'undefined'"
    ]

-- | Records the number the page passed in, and returns nothing.
recordNumber :: IORef [Double] -> [SerializedValue] -> Playwright SerializedArgument
recordNumber received args =
  case args of
    [SerializedNode (PayloadNumber n) _] -> do
      modifyIORef' received (<> [n])
      pure undefinedArgument
    _ ->
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

addNumbers :: [SerializedValue] -> Playwright SerializedArgument
addNumbers args =
  case args of
    [SerializedNode (PayloadNumber left) _, SerializedNode (PayloadNumber right) _] ->
      pure (numberArgument (left + right))
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

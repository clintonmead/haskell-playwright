module Playwright.BfcacheSpec
  ( tests
  ) where

import UnliftIO.Exception
  ( throwIO
  )
import Playwright
  ( BindingName (BindingName)
  , LaunchOptions (launchOptionsIgnoreDefaultArgs)
  , LoadState (LoadStateCommit)
  , Playwright
  , defaultGotoOptions
  , defaultLaunchOptions
  , defaultNewContextOptions
  , evaluate
  , evaluateNumber
  , exposeFunction
  , goBackWith
  , goto
  , numberArgument
  , pageClickSelector
  , selectorFromText
  , setContent
  , urlFromText
  )
import Playwright.Error
  ( PlaywrightError (EvaluateUnexpected)
  )
import Playwright.Harness
  ( withAssetsLaunchWith
  , assertEqual
  )
import Playwright.SerializedArgument
  ( SerializedArgument
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
    "bfcache"
    [ testCase "bindings should work after restoring from bfcache" $
        withAssetsLaunchWith bfcacheLaunch defaultNewContextOptions $ \prefix _ctx page -> do
          _ <- exposeFunction page (BindingName "add") addNumbers
          _ <- goto page (urlFromText (prefix <> "/cached/bfcached.html")) defaultGotoOptions
          assertEqual "before" (3 :: Double) =<< evaluateNumber page "async () => window.add(1, 2)"
          setContent page "<a href='about:blank'>click me</a>"
          pageClickSelector page (selectorFromText "a")
          _ <- goBackWith page (Just LoadStateCommit)
          _ <- evaluate page "async () => window.didShow"
          assertEqual "after" (5 :: Double) =<< evaluateNumber page "async () => window.add(2, 3)"
    ]

bfcacheLaunch :: LaunchOptions
bfcacheLaunch =
  defaultLaunchOptions {launchOptionsIgnoreDefaultArgs = Just ["--disable-back-forward-cache"]}

addNumbers :: [SerializedValue] -> Playwright SerializedArgument
addNumbers args =
  case args of
    [SerializedNode (PayloadNumber left) _, SerializedNode (PayloadNumber right) _] ->
      pure (numberArgument (left + right))
    _ ->
      throwIO (EvaluateUnexpected (SerializedNode (PayloadNumber 0) Nothing))

module Playwright.BrowserContextOfflineSpec
  ( tests
  ) where

import Playwright
  ( defaultGotoOptions
  , evaluateBool
  , goto
  , pageContext
  , setOffline
  , urlFromText
  )
import Playwright.Harness
  ( withAssets
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
    "browsercontext-offline"
    [ testCase "should emulate navigator.onLine" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let
            ctx = pageContext page
          assertEqual "online" True =<< evaluateBool page "() => window.navigator.onLine"
          setOffline ctx True
          assertEqual "offline" False =<< evaluateBool page "() => window.navigator.onLine"
          setOffline ctx False
          assertEqual "online again" True =<< evaluateBool page "() => window.navigator.onLine"
    ]

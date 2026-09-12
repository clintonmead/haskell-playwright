module Playwright.BrowserContextCspSpec
  ( tests
  ) where

import UnliftIO.Exception
  ( try
  )
import Control.Monad
  ( void
  )
import Playwright
  ( NewContextOptions (newContextBypassCSP)
  , Playwright
  , ScriptTag (scriptTagContent)
  , addScriptTag
  , defaultGotoOptions
  , defaultNewContextOptions
  , emptyScriptTag
  , evaluateBool
  , evaluateNumber
  , goto
  , urlFromText
  )
import Playwright.Error
  ( PlaywrightError
  )
import Playwright.Harness
  ( withAssetsWith
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
    "browsercontext-csp"
    [ testCase "should honor CSP meta tag" $
        withAssetsWith defaultNewContextOptions $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/csp.html")) defaultGotoOptions
          assertEqual "inline blocked" False
            =<< evaluateBool page "() => window.__inlineScriptValue === 42"
          void
            ( try @Playwright @PlaywrightError
                ( addScriptTag
                    page
                    emptyScriptTag {scriptTagContent = Just "window[\"__injected\"] = 42;"}
                )
            )
          assertEqual "injected blocked" False
            =<< evaluateBool page "() => window.__injected === 42"
    , testCase "should bypass CSP meta tag" $
        withAssetsWith defaultNewContextOptions {newContextBypassCSP = Just True} $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/csp.html")) defaultGotoOptions
          assertEqual "inline" True
            =<< evaluateBool page "() => window.__inlineScriptValue === 42"
          _ <-
            addScriptTag
              page
              emptyScriptTag {scriptTagContent = Just "window[\"__injected\"] = 42;"}
          assertEqual "injected" (42 :: Double)
            =<< evaluateNumber page "() => window.__injected"
    ]

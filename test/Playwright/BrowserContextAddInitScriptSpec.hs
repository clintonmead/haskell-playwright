module Playwright.BrowserContextAddInitScriptSpec
  ( tests
  ) where

import Playwright
  ( addContextInitScript
  , addInitScript
  , defaultGotoOptions
  , defaultNewContextOptions
  , dispose
  , evaluateBool
  , evaluateNumber
  , goto
  , urlFromText
  )
import Playwright.Harness
  ( withAssetsContextWith
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
    ]

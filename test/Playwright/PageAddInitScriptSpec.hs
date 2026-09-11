module Playwright.PageAddInitScriptSpec
  ( tests
  ) where

import Playwright
  ( addInitScript
  , defaultGotoOptions
  , evaluateNumber
  , goto
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
    "page-add-init-script"
    [ testCase "should work with content" $
        withAssets $ \prefix page -> do
          addInitScript page "window[\"injected\"] = 123"
          _ <- goto page (urlFromText (prefix <> "/tamperable.html")) defaultGotoOptions
          assertEqual "injected" (123 :: Double) =<< evaluateNumber page "() => window.result"
    , testCase "should support multiple scripts" $
        withAssets $ \prefix page -> do
          addInitScript page "window['script1'] = 1;"
          addInitScript page "window['script2'] = 2;"
          _ <- goto page (urlFromText (prefix <> "/tamperable.html")) defaultGotoOptions
          assertEqual "script1" (1 :: Double) =<< evaluateNumber page "() => window.script1"
          assertEqual "script2" (2 :: Double) =<< evaluateNumber page "() => window.script2"
    , testCase "should work with trailing comments" $
        withAssets $ \prefix page -> do
          addInitScript page "// comment"
          addInitScript page "window.secret = 42;"
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "secret" (42 :: Double) =<< evaluateNumber page "() => window.secret"
    , testCase "should evaluate before anything else on the page" $
        withAssets $ \prefix page -> do
          addInitScript page "window['injected'] = 123;"
          _ <- goto page (urlFromText (prefix <> "/tamperable.html")) defaultGotoOptions
          assertEqual "result" (123 :: Double) =<< evaluateNumber page "() => window.result"
    ]

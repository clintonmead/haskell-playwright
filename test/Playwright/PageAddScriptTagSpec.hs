module Playwright.PageAddScriptTagSpec
  ( tests
  ) where

import Playwright
  ( ScriptTag (scriptTagContent, scriptTagUrl)
  , addScriptTag
  , defaultGotoOptions
  , emptyScriptTag
  , evaluateNumber
  , goto
  , urlFromText
  )
import Playwright.Harness
  ( withAssetPage
  , withAssets
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
    "page-add-script-tag"
    [ testCase "should work with a url" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <-
            addScriptTag
              page
              emptyScriptTag {scriptTagUrl = Just (urlFromText (prefix <> "/injectedfile.js"))}
          assertEqual "injected" (42 :: Double) =<< evaluateNumber page "() => window.__injected"
    , testCase "should work with content" $
        withAssetPage "empty.html" $ \page -> do
          _ <-
            addScriptTag
              page
              emptyScriptTag {scriptTagContent = Just "window[\"__injected\"] = 35;"}
          assertEqual "injected" (35 :: Double) =<< evaluateNumber page "() => window.__injected"
    ]

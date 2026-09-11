module Playwright.BrowserContextJavaScriptSpec
  ( tests
  ) where

import Playwright
  ( NewContextOptions (newContextJavaScriptEnabled)
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluateText
  , goto
  , urlFromText
  )
import Playwright.Harness
  ( assertProtocolContains
  , withAssetsWith
  , withChromiumContextWith
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
    "browsercontext-javascript"
    [ testCase "should disable javascript" $
        withChromiumContextWith
          defaultNewContextOptions {newContextJavaScriptEnabled = Just False}
          $ \_ page -> do
            _ <-
              goto
                page
                (urlFromText "data:text/html, <script>var something = \"forbidden\"</script>")
                defaultGotoOptions
            assertProtocolContains
              "something is not defined"
              (evaluateText page "() => something")
    , testCase "should run scripts when javascript is enabled" $
        withChromiumContextWith defaultNewContextOptions $ \_ page -> do
          _ <-
            goto
              page
              (urlFromText "data:text/html, <script>var something = \"forbidden\"</script>")
              defaultGotoOptions
          assertEqual "value" "forbidden" =<< evaluateText page "() => something"
    , testCase "should be able to navigate after disabling javascript" $
        withAssetsWith defaultNewContextOptions {newContextJavaScriptEnabled = Just False} $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          pure ()
    ]

module Playwright.PageAddScriptTagSpec
  ( tests
  ) where

import Control.Monad
  ( void
  )
import Data.Text
  ( isInfixOf
  )
import Playwright
  ( Playwright
  , ScriptTag (scriptTagContent, scriptTagPath, scriptTagType, scriptTagUrl)
  , addScriptTag
  , defaultGotoOptions
  , emptyScriptTag
  , evaluateNumber
  , evaluateText
  , goto
  , urlFromText
  , waitForFunction
  )
import Playwright.Error
  ( PlaywrightError
  )
import Playwright.Harness
  ( assertEqual
  , assertFailure
  , assertProtocolContains
  , testAssetsDir
  , withAssetPage
  , withAssets
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Exception
  ( try
  )

tests :: TestTree
tests =
  testGroup
    "page-add-script-tag"
    [ testCase "should throw an error if no options are provided" $
        withChromiumPage $ \page ->
          assertProtocolContains
            "Provide an object with a `url`, `path` or `content` property"
            (addScriptTag page emptyScriptTag)
    , testCase "should work with a url" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <-
            addScriptTag
              page
              emptyScriptTag {scriptTagUrl = Just (urlFromText (prefix <> "/injectedfile.js"))}
          assertEqual "injected" (42 :: Double) =<< evaluateNumber page "() => window.__injected"
    , testCase "should work with a url and type=module" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <-
            addScriptTag
              page
              emptyScriptTag
                { scriptTagUrl = Just (urlFromText (prefix <> "/es6/es6import.js"))
                , scriptTagType = Just "module"
                }
          _ <- waitForFunction page "() => window.__es6injected"
          assertEqual "injected" (42 :: Double) =<< evaluateNumber page "() => window.__es6injected"
    , testCase "should work with a path and type=module" $ do
        assets <- testAssetsDir
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <-
            addScriptTag
              page
              emptyScriptTag
                { scriptTagPath = Just (assets <> "/es6/es6pathimport.js")
                , scriptTagType = Just "module"
                }
          _ <- waitForFunction page "() => window.__es6injected"
          assertEqual "injected" (42 :: Double) =<< evaluateNumber page "() => window.__es6injected"
    , testCase "should work with a content and type=module" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <-
            addScriptTag
              page
              emptyScriptTag
                { scriptTagContent = Just "import num from '/es6/es6module.js';window.__es6injected = num;"
                , scriptTagType = Just "module"
                }
          _ <- waitForFunction page "() => window.__es6injected"
          assertEqual "injected" (42 :: Double) =<< evaluateNumber page "() => window.__es6injected"
    , testCase "should throw an error if loading from url fail" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertFails
            (addScriptTag page emptyScriptTag {scriptTagUrl = Just (urlFromText (prefix <> "/nonexistfile.js"))})
    , testCase "should work with a path" $ do
        assets <- testAssetsDir
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- addScriptTag page emptyScriptTag {scriptTagPath = Just (assets <> "/injectedfile.js")}
          assertEqual "injected" (42 :: Double) =<< evaluateNumber page "() => window.__injected"
    , testCase "should include sourceURL when path is provided" $ do
        assets <- testAssetsDir
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- addScriptTag page emptyScriptTag {scriptTagPath = Just (assets <> "/injectedfile.js")}
          stack <- evaluateText page "() => window.__injectedError.stack"
          assertEqual "stack names the file" True ("injectedfile.js" `isInfixOf` stack)
    , testCase "should work with content" $
        withAssetPage "empty.html" $ \page -> do
          _ <-
            addScriptTag
              page
              emptyScriptTag {scriptTagContent = Just "window[\"__injected\"] = 35;"}
          assertEqual "injected" (35 :: Double) =<< evaluateNumber page "() => window.__injected"
    , testCase "should throw when added with content to the CSP page" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/csp.html")) defaultGotoOptions
          assertFails
            (addScriptTag page emptyScriptTag {scriptTagContent = Just "window[\"__injected\"] = 35;"})
    , testCase "should throw a nice error when the request fails" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let missing = prefix <> "/this_does_not_exist.js"
          assertFails (addScriptTag page emptyScriptTag {scriptTagUrl = Just (urlFromText missing)})
    ]

-- | Fails the test unless the action throws a Playwright error.
assertFails :: Playwright a -> Playwright ()
assertFails action = do
  outcome <- try (void action) :: Playwright (Either PlaywrightError ())
  case outcome of
    Left _ ->
      pure ()
    Right () ->
      assertFailure "expected the action to throw"

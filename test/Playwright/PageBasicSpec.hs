module Playwright.PageBasicSpec
  ( tests
  ) where

import Data.Text
  ( Text
  , isInfixOf
  , isPrefixOf
  )
import Playwright
  ( Key (Key)
  , defaultGotoOptions
  , evaluateBool
  , evaluateText
  , goto
  , press
  , urlFromText
  )
import Playwright.Harness
  ( locate
  , withAssetPage
  , withChromiumPage
  , assertBool
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
    "page-basic"
    [ testCase "page.press should work" $
        withAssetPage "input/textarea.html" $ \page -> do
          press (locate page "textarea") (Key "a")
          assertEqual "value" "a" =<< evaluateText page "() => document.querySelector('textarea').value"
    , testCase "has navigator.webdriver set to true" $
        withChromiumPage $ \page -> do
          assertEqual "webdriver" True =<< evaluateBool page "() => navigator.webdriver"
    , testCase "should have sane user agent" $
        withChromiumPage $ \page -> do
          userAgent <- evaluateText page "() => navigator.userAgent"
          assertBool "Mozilla" ("Mozilla/5.0" `isPrefixOf` userAgent)
          assertBool "AppleWebKit" ("AppleWebKit/" `isInfixOf` userAgent)
          assertBool "Chrome" ("Chrome/" `isInfixOf` userAgent)
          assertBool "Safari" ("Safari/" `isInfixOf` userAgent)
    , testCase "should fire load when expected" $
        withChromiumPage $ \page -> do
          _ <- goto page (urlFromText "about:blank") defaultGotoOptions
          title <- evaluateText page "() => document.readyState"
          assertEqual "complete" ("complete" :: Text) title
    ]

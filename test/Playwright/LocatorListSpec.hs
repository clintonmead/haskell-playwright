module Playwright.LocatorListSpec
  ( tests
  ) where

import Playwright
  ( allLocators
  , setContent
  , textContent
  )
import Playwright.Harness
  ( locate
  , withChromiumPage
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
    "locator-list"
    [ testCase "locator.all should work" $
        withChromiumPage $ \page -> do
          setContent page "<div><p>A</p><p>B</p><p>C</p></div>"
          locators <- allLocators (locate page "div >> p")
          texts <- traverse textContent locators
          assertEqual "texts" [Just "A", Just "B", Just "C"] texts
    ]

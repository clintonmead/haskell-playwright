module Playwright.ElementHandlePressSpec
  ( tests
  ) where

import Playwright
  ( Key (Key)
  , evaluateOnLocatorText
  , press
  , setContent
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
    "elementhandle-press"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          setContent page "<input type='text' />"
          press (locate page "input") (Key "h")
          assertEqual "value" "h" =<< evaluateOnLocatorText (locate page "input") "input => input.value"
    , testCase "should not select existing value" $
        withChromiumPage $ \page -> do
          setContent page "<input type='text' value='hello' />"
          press (locate page "input") (Key "w")
          assertEqual "value" "whello" =<< evaluateOnLocatorText (locate page "input") "input => input.value"
    ]

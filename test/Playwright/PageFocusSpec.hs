module Playwright.PageFocusSpec
  ( tests
  ) where

import Playwright
  ( evaluateText
  , focus
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
    "page-focus"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          setContent page "<div id=d1 tabIndex=0></div>"
          assertEqual "body" "BODY" =<< evaluateText page "() => document.activeElement.nodeName"
          focus (locate page "#d1")
          assertEqual "id" "d1" =<< evaluateText page "() => document.activeElement.id"
    ]

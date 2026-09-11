module Playwright.ElementHandleClickSpec
  ( tests
  ) where

import Playwright
  ( clickHandle
  , evaluate
  , evaluateText
  , querySelector
  , selectorFromText
  )
import Playwright.Harness
  ( requireHandle
  , withAssetPage
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
    "elementhandle-click"
    [ testCase "should work" $
        withAssetPage "input/button.html" $ \page -> do
          handle <- requireHandle =<< querySelector page (selectorFromText "button")
          clickHandle handle
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should work with Node removed" $
        withAssetPage "input/button.html" $ \page -> do
          _ <- evaluate page "() => { delete window.Node; }"
          handle <- requireHandle =<< querySelector page (selectorFromText "button")
          clickHandle handle
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    ]

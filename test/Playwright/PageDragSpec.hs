module Playwright.PageDragSpec
  ( tests
  ) where

import Playwright
  ( dragAndDrop
  , evaluateOnLocatorBool
  , hover
  , mouseDown
  , mouseUp
  )
import Playwright.Harness
  ( locate
  , withAssetPage
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
    "page-drag"
    [ testCase "should work" $
        withAssetPage "drag-n-drop.html" $ \page -> do
          hover (locate page "#source")
          mouseDown page
          hover (locate page "#target")
          mouseUp page
          assertEqual "dropped" True
            =<< evaluateOnLocatorBool
              (locate page "#target")
              "target => target.contains(document.querySelector('#source'))"
    , testCase "should work with dragAndDrop" $
        withAssetPage "drag-n-drop.html" $ \page -> do
          dragAndDrop (locate page "#source") (locate page "#target")
          assertEqual "dropped" True
            =<< evaluateOnLocatorBool
              (locate page "#target")
              "target => target.contains(document.querySelector('#source'))"
    ]

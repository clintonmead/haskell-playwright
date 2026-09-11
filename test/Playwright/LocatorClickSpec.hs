module Playwright.LocatorClickSpec
  ( tests
  ) where

import Playwright
  ( click
  , dblclick
  , evaluate
  , evaluateBool
  , evaluateOnLocator
  , evaluateText
  , setContent
  )
import Playwright.Harness
  ( locate
  , withAssetPage
  , withChromiumPage
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
    "locator-click"
    [ testCase "should work" $
        withAssetPage "input/button.html" $ \page -> do
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should work with Node removed" $
        withAssetPage "input/button.html" $ \page -> do
          _ <- evaluate page "() => { delete window.Node; }"
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should double click the button" $
        withAssetPage "input/button.html" $ \page -> do
          _ <-
            evaluate
              page
              "() => { window.double = false; const button = document.querySelector('button'); button.addEventListener('dblclick', () => { window.double = true; }); }"
          dblclick (locate page "button")
          assertEqual "double" True =<< evaluateBool page "() => window.double"
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should click if the target element is removed in pointerup event" $
        withChromiumPage $ \page -> do
          setContent page "<button id=clickme>Clickable</button>"
          _ <-
            evaluateOnLocator
              (locate page "#clickme")
              "element => { element.addEventListener('pointerup', () => element.remove(), false); }"
          click (locate page "#clickme")
    , testCase "should click if the target element is removed in pointerdown event" $
        withChromiumPage $ \page -> do
          setContent page "<button id=clickme>Clickable</button>"
          _ <-
            evaluateOnLocator
              (locate page "#clickme")
              "element => { element.addEventListener('pointerdown', () => element.remove(), false); }"
          click (locate page "#clickme")
    ]

module Playwright.PageClickSpec
  ( tests
  ) where

import Playwright
  ( click
  , evaluate
  , evaluateBool
  , evaluateNumber
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
    "page-click"
    [ testCase "should click the button" $
        withAssetPage "input/button.html" $ \page -> do
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should click svg" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<svg height=\"100\" width=\"100\"><circle onclick=\"window.__CLICKED=42\" cx=\"50\" cy=\"50\" r=\"40\" stroke=\"black\" stroke-width=\"3\" fill=\"red\" /></svg>"
          click (locate page "circle")
          assertEqual "clicked" (42 :: Double) =<< evaluateNumber page "() => window.__CLICKED"
    , testCase "should click the button if window.Node is removed" $
        withAssetPage "input/button.html" $ \page -> do
          _ <- evaluate page "() => { delete window.Node; }"
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should click on a span with an inline element inside" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<style>span::before { content: 'q'; }</style><span onclick='window.CLICKED=42'></span>"
          click (locate page "span")
          assertEqual "clicked" (42 :: Double) =<< evaluateNumber page "() => window.CLICKED"
    , testCase "should click the aligned 1x1 div" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"width: 1px; height: 1px;\" onclick=\"window.__clicked = true\"></div>"
          click (locate page "div")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should click the half-aligned 1x1 div" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"margin-left: 20.5px; margin-top: 11.5px; width: 1px; height: 1px;\" onclick=\"window.__clicked = true\"></div>"
          click (locate page "div")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should click the unaligned 1x1 div v1" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"margin-left: 20.23px; margin-top: 11.65px; width: 1px; height: 1px;\" onclick=\"window.__clicked = true\"></div>"
          click (locate page "div")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    ]

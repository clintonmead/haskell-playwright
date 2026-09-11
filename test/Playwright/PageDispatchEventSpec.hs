module Playwright.PageDispatchEventSpec
  ( tests
  ) where

import Playwright
  ( DomEventType (DomEventType)
  , dispatchEvent
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
    "page-dispatchevent"
    [ testCase "should dispatch click event" $
        withAssetPage "input/button.html" $ \page -> do
          dispatchEvent (locate page "button") (DomEventType "click")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should dispatch click svg" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<svg height=\"100\" width=\"100\"><circle onclick=\"window.__CLICKED=42\" cx=\"50\" cy=\"50\" r=\"40\" stroke=\"black\" stroke-width=\"3\" fill=\"red\" /></svg>"
          dispatchEvent (locate page "circle") (DomEventType "click")
          assertEqual "clicked" (42 :: Double) =<< evaluateNumber page "() => window.__CLICKED"
    , testCase "should dispatch click on a span with an inline element inside" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<style>span::before { content: 'q'; }</style><span onclick='window.CLICKED=42'></span>"
          dispatchEvent (locate page "span") (DomEventType "click")
          assertEqual "clicked" (42 :: Double) =<< evaluateNumber page "() => window.CLICKED"
    , testCase "should dispatch click after navigation" $
        withAssetPage "input/button.html" $ \page -> do
          dispatchEvent (locate page "button") (DomEventType "click")
          dispatchEvent (locate page "button") (DomEventType "click")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    ]

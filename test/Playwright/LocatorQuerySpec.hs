module Playwright.LocatorQuerySpec
  ( tests
  ) where

import Playwright
  ( childLocator
  , first
  , lastLocator
  , locatorCount
  , nth
  , selectorFromText
  , setContent
  )
import Playwright.Harness
  ( locate
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
    "locator-query"
    [ testCase "should respect first and last" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<section><div><p>A</p></div><div><p>A</p><p>A</p></div><div><p>A</p><p>A</p><p>A</p></div></section>"
          let
            pInDiv = childLocator (locate page "div") (selectorFromText "p")
          assertEqual "div >> p" 6 =<< locatorCount (locate page "div >> p")
          assertEqual "nested" 6 =<< locatorCount pInDiv
          assertEqual "first" 1 =<< locatorCount (childLocator (first (locate page "div")) (selectorFromText "p"))
          assertEqual "last" 3 =<< locatorCount (childLocator (lastLocator (locate page "div")) (selectorFromText "p"))
    , testCase "should respect nth" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<section><div><p>A</p></div><div><p>A</p><p>A</p></div><div><p>A</p><p>A</p><p>A</p></div></section>"
          assertEqual "nth 0" 1 =<< locatorCount (nth (locate page "div >> p") 0)
          assertEqual "nth 1" 2
            =<< locatorCount (childLocator (nth (locate page "div") 1) (selectorFromText "p"))
          assertEqual "nth 2" 3
            =<< locatorCount (childLocator (nth (locate page "div") 2) (selectorFromText "p"))
    ]

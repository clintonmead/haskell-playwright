module Playwright.LocatorElementHandleSpec
  ( tests
  ) where

import Playwright
  ( childLocator
  , elementHandle
  , elementHandles
  , elementArgument
  , evaluateTextWith
  , selectorFromText
  , setContent
  )
import Playwright.Harness
  ( locate
  , requireHandle
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
    "locator-element-handle"
    [ testCase "should query existing element" $
        withChromiumPage $ \page -> do
          setContent page "<html><body><div class=\"second\"><div class=\"inner\">A</div></div></body></html>"
          inner <-
            requireHandle
              =<< elementHandle
                ( childLocator
                    (childLocator (locate page "html") (selectorFromText ".second"))
                    (selectorFromText ".inner")
                )
          assertEqual "text" "A" =<< evaluateTextWith page "e => e.textContent" (elementArgument inner)
    , testCase "should query existing elements" $
        withChromiumPage $ \page -> do
          setContent page "<html><body><div>A</div><br/><div>B</div></body></html>"
          handles <- elementHandles (childLocator (locate page "html") (selectorFromText "div"))
          texts <- traverse (\handle -> evaluateTextWith page "e => e.textContent" (elementArgument handle)) handles
          assertEqual "texts" ["A", "B"] texts
    , testCase "should return empty array for non-existing elements" $
        withChromiumPage $ \page -> do
          setContent page "<html><body><span>A</span><br/><span>B</span></body></html>"
          handles <- elementHandles (childLocator (locate page "html") (selectorFromText "div"))
          assertEqual "empty" 0 (length handles)
    ]

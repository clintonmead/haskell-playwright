module Playwright.BrowserContextViewportSpec
  ( tests
  ) where

import Playwright
  ( NewContextOptions (newContextUserAgent, newContextViewport)
  , UserAgent (UserAgent)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , defaultNewContextOptions
  , evaluateNumber
  , evaluateText
  )
import Playwright.Harness
  ( withChromiumContextWith
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
    "browsercontext-viewport"
    [ testCase "should propagate default viewport to the page" $
        withChromiumContextWith
          defaultNewContextOptions
            { newContextViewport =
                Just (ViewportSize (ViewportWidth 456) (ViewportHeight 789))
            }
          $ \_ page -> do
            assertEqual "width" (456 :: Double)
              =<< evaluateNumber page "() => window.innerWidth"
            assertEqual "height" (789 :: Double)
              =<< evaluateNumber page "() => window.innerHeight"
    , testCase "should emulate user agent" $
        withChromiumContextWith
          defaultNewContextOptions {newContextUserAgent = Just (UserAgent "foobar")}
          $ \_ page ->
            assertEqual "ua" "foobar" =<< evaluateText page "() => navigator.userAgent"
    ]

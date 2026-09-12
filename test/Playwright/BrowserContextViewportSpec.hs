module Playwright.BrowserContextViewportSpec
  ( tests
  ) where

import Playwright
  ( NewContextOptions (newContextDeviceScaleFactor, newContextHasTouch, newContextIsMobile, newContextNoDefaultViewport, newContextUserAgent, newContextViewport)
  , Point (Point)
  , UserAgent (UserAgent)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , cssPixelFromInt
  , defaultNewContextOptions
  , deviceScaleFactorFromInt
  , evaluateBool
  , evaluateNumber
  , evaluateText
  , mouseDown
  , mouseMove
  , mouseUp
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
    , testCase "should respect deviceScaleFactor" $
        withChromiumContextWith
          defaultNewContextOptions {newContextDeviceScaleFactor = Just (deviceScaleFactorFromInt 3)}
          $ \_ page ->
            assertEqual "ratio" (3 :: Double)
              =<< evaluateNumber page "() => window.devicePixelRatio"
    , testCase "should not hang with touch-enabled viewports" $
        withChromiumContextWith
          defaultNewContextOptions
            { newContextViewport = Just (ViewportSize (ViewportWidth 375) (ViewportHeight 667))
            , newContextHasTouch = Just True
            , newContextIsMobile = Just True
            }
          $ \_ page -> do
            mouseDown page
            mouseMove page (Point (cssPixelFromInt 100) (cssPixelFromInt 10))
            mouseUp page
            assertEqual "touch points" True
              =<< evaluateBool page "() => navigator.maxTouchPoints > 0"
            assertEqual "coarse" True
              =<< evaluateBool page "() => matchMedia('(pointer: coarse)').matches"
    , testCase "should allow no default viewport" $
        withChromiumContextWith
          defaultNewContextOptions {newContextNoDefaultViewport = Just True}
          $ \_ page ->
            assertEqual "positive" True
              =<< evaluateBool page "() => window.innerWidth > 0 && window.innerHeight > 0"
    ]

module Playwright.BrowserContextViewportSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Playwright
  ( NewContextOptions (newContextDeviceScaleFactor, newContextHasTouch, newContextIsMobile, newContextNoDefaultViewport, newContextUserAgent, newContextViewport)
  , Point (Point)
  , UserAgent (UserAgent)
  , Page
  , Playwright
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , cssPixelFromInt
  , defaultGotoOptions
  , defaultNewContextOptions
  , deviceScaleFactorFromInt
  , evaluateBool
  , goto
  , urlFromText
  , evaluateNumber
  , evaluateText
  , mouseDown
  , mouseMove
  , mouseUp
  , setViewportSize
  )
import Playwright.Harness
  ( assertBool
  , withAssets
  , withChromiumContext
  , withChromiumContextWith
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
    , testCase "should get the proper default viewport size" $
        withChromiumContext $ \_ page -> do
          assertEqual "width" (1280 :: Double) =<< evaluateNumber page "() => window.innerWidth"
          assertEqual "height" (720 :: Double) =<< evaluateNumber page "() => window.innerHeight"
    , testCase "should set the proper viewport size" $
        withChromiumContext $ \_ page -> do
          setViewportSize page (ViewportSize (ViewportWidth 345) (ViewportHeight 456))
          assertEqual "width" (345 :: Double) =<< evaluateNumber page "() => window.innerWidth"
          assertEqual "height" (456 :: Double) =<< evaluateNumber page "() => window.innerHeight"
    , testCase "should return correct outerWidth and outerHeight" $
        withChromiumContext $ \_ page -> do
          setViewportSize page (ViewportSize (ViewportWidth 410) (ViewportHeight 420))
          assertEqual "inner width" (410 :: Double) =<< evaluateNumber page "() => window.innerWidth"
          assertEqual "inner height" (420 :: Double) =<< evaluateNumber page "() => window.innerHeight"
          assertBool "outer width" . (>= 410) =<< evaluateNumber page "() => window.outerWidth"
          assertBool "outer height" . (>= 420) =<< evaluateNumber page "() => window.outerHeight"
    , testCase "should emulate device width" $
        withChromiumContext $ \_ page -> do
          setViewportSize page (ViewportSize (ViewportWidth 300) (ViewportHeight 300))
          assertEqual "screen" (300 :: Double) =<< evaluateNumber page "() => window.screen.width"
          mediaMatches page "(min-device-width: 200px)" >>= assertEqual "min 200" True
          mediaMatches page "(min-device-width: 400px)" >>= assertEqual "min 400" False
          mediaMatches page "(max-device-width: 200px)" >>= assertEqual "max 200" False
          mediaMatches page "(max-device-width: 400px)" >>= assertEqual "max 400" True
          mediaMatches page "(device-width: 600px)" >>= assertEqual "device 600" False
          mediaMatches page "(device-width: 300px)" >>= assertEqual "device 300" True
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          assertEqual "screen after" (500 :: Double) =<< evaluateNumber page "() => window.screen.width"
          mediaMatches page "(min-device-width: 400px)" >>= assertEqual "min 400 after" True
          mediaMatches page "(min-device-width: 600px)" >>= assertEqual "min 600 after" False
          mediaMatches page "(device-width: 200px)" >>= assertEqual "device 200 after" False
          mediaMatches page "(device-width: 500px)" >>= assertEqual "device 500 after" True
    , testCase "should emulate device height" $
        withChromiumContext $ \_ page -> do
          setViewportSize page (ViewportSize (ViewportWidth 300) (ViewportHeight 300))
          assertEqual "screen" (300 :: Double) =<< evaluateNumber page "() => window.screen.height"
          mediaMatches page "(min-device-height: 200px)" >>= assertEqual "min 200" True
          mediaMatches page "(min-device-height: 400px)" >>= assertEqual "min 400" False
          mediaMatches page "(max-device-height: 200px)" >>= assertEqual "max 200" False
          mediaMatches page "(max-device-height: 400px)" >>= assertEqual "max 400" True
          mediaMatches page "(device-height: 600px)" >>= assertEqual "device 600" False
          mediaMatches page "(device-height: 300px)" >>= assertEqual "device 300" True
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          assertEqual "screen after" (500 :: Double) =<< evaluateNumber page "() => window.screen.height"
    , testCase "should emulate availWidth and availHeight" $
        withChromiumContext $ \_ page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 600))
          assertEqual "availWidth" (500 :: Double) =<< evaluateNumber page "() => window.screen.availWidth"
          assertEqual "availHeight" (600 :: Double) =<< evaluateNumber page "() => window.screen.availHeight"
    , testCase "should not have touch by default" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/mobile.html")) defaultGotoOptions
          assertEqual "touch" False =<< evaluateBool page "() => 'ontouchstart' in window"
    ]

-- | Whether a media query matches the page.
mediaMatches :: Page -> Text -> Playwright Bool
mediaMatches page query =
  evaluateBool page ("() => matchMedia('" <> query <> "').matches")

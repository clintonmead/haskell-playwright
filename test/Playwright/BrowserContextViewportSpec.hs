module Playwright.BrowserContextViewportSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Playwright
  ( NewContextOptions (newContextDeviceScaleFactor, newContextHasTouch, newContextIsMobile, newContextNoDefaultViewport, newContextScreen, newContextUserAgent, newContextViewport)
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
  , setContent
  , setViewportSize
  , tap
  )
import Playwright.Harness
  ( assertBool
  , assertProtocolContains
  , locate
  , withAssets
  , withAssetsContextWith
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
    , testCase "should set both screen and viewport options" $
        withChromiumContextWith
          defaultNewContextOptions
            { newContextScreen = Just (ViewportSize (ViewportWidth 1280) (ViewportHeight 720))
            , newContextViewport = Just (ViewportSize (ViewportWidth 1000) (ViewportHeight 600))
            }
          $ \_ page -> do
            assertEqual "screen width" (1280 :: Double) =<< evaluateNumber page "() => screen.width"
            assertEqual "screen height" (720 :: Double) =<< evaluateNumber page "() => screen.height"
            assertEqual "inner width" (1000 :: Double) =<< evaluateNumber page "() => window.innerWidth"
            assertEqual "inner height" (600 :: Double) =<< evaluateNumber page "() => window.innerHeight"
    , testCase "should throw on tap if hasTouch is not enabled" $
        withChromiumContext $ \_ page -> do
          -- Partial: the upstream page.tap(selector) form has no Haskell API, so only the locator form is ported.
          setContent page "<div>a</div>"
          assertProtocolContains "The page does not support tap" (tap (locate page "div"))
    , testCase "should set window.screen.orientation.type for mobile devices" $
        withAssetsContextWith iPhone14 $ \prefix _ page -> do
          _ <- goto page (urlFromText (prefix <> "/index.html")) defaultGotoOptions
          assertEqual "orientation" "portrait-primary" =<< evaluateText page "() => window.screen.orientation.type"
    , testCase "should not have touch by default" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/mobile.html")) defaultGotoOptions
          assertEqual "touch" False =<< evaluateBool page "() => 'ontouchstart' in window"
    ]

-- | The options Playwright applies for the @iPhone 14@ device descriptor.
-- The Haskell API has no device registry, so the values are copied here.
iPhone14 :: NewContextOptions
iPhone14 =
  defaultNewContextOptions
    { newContextUserAgent =
        Just (UserAgent "Mozilla/5.0 (iPhone; CPU iPhone OS 16_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/26.6 Mobile/15E148 Safari/604.1")
    , newContextViewport = Just (ViewportSize (ViewportWidth 390) (ViewportHeight 664))
    , newContextScreen = Just (ViewportSize (ViewportWidth 390) (ViewportHeight 844))
    , newContextDeviceScaleFactor = Just (deviceScaleFactorFromInt 3)
    , newContextIsMobile = Just True
    , newContextHasTouch = Just True
    }

-- | Whether a media query matches the page.
mediaMatches :: Page -> Text -> Playwright Bool
mediaMatches page query =
  evaluateBool page ("() => matchMedia('" <> query <> "').matches")

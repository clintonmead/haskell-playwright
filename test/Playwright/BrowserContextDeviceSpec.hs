module Playwright.BrowserContextDeviceSpec
  ( tests
  ) where

import Data.Text
  ( Text
  , isInfixOf
  )
import Data.Text qualified as Text
import Playwright
  ( NewContextOptions (newContextDeviceScaleFactor, newContextHasTouch, newContextIsMobile, newContextScreen, newContextUserAgent, newContextViewport)
  , UserAgent (UserAgent)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , clickHandle
  , defaultGotoOptions
  , defaultNewContextOptions
  , deviceScaleFactorFromInt
  , evaluateNumber
  , evaluateOnElement
  , evaluateText
  , goto
  , querySelector
  , selectorFromText
  , setContent
  , textContentHandle
  , urlFromText
  )
import Playwright.Harness
  ( requireHandle
  , withAssetsWith
  , withChromiumContextWith
  , assertBool
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
    "browsercontext-device"
    [ testCase "should work" $
        withAssetsWith iPhone6 $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/mobile.html")) defaultGotoOptions
          assertEqual "width" (375 :: Double) =<< evaluateNumber page "() => window.innerWidth"
          ua <- evaluateText page "() => navigator.userAgent"
          assertBool "iPhone" ("iPhone" `isInfixOf` ua)
    , testCase "should support clicking" $
        withAssetsWith iPhone6 $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/input/button.html")) defaultGotoOptions
          button <- requireHandle =<< querySelector page (selectorFromText "button")
          _ <- evaluateOnElement button "button => { button.style.marginTop = '200px'; }"
          clickHandle button
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => result"
    , testCase "should scroll to click" $
        withAssetsWith
          defaultNewContextOptions
            { newContextViewport = Just (ViewportSize (ViewportWidth 400) (ViewportHeight 400))
            , newContextDeviceScaleFactor = Just (deviceScaleFactorFromInt 1)
            , newContextIsMobile = Just True
            }
          $ \prefix page -> do
            _ <- goto page (urlFromText (prefix <> "/input/scrollable.html")) defaultGotoOptions
            element <- requireHandle =<< querySelector page (selectorFromText "#button-91")
            clickHandle element
            clicked <- textContentHandle element
            assertEqual "clicked" (Just ("clicked" :: Text)) clicked
    , testCase "should scroll twice when emulated" $
        withChromiumContextWith iPhone6 $ \_ page -> do
          setContent page iPhoneScrollHtml
          _ <- evaluateNumber page "() => { window.scroll(0, 100); return window.scrollY; }"
          assertEqual "100" (100 :: Double) =<< evaluateNumber page "() => window.scrollY"
          _ <- evaluateNumber page "() => { window.scroll(0, 200); return window.scrollY; }"
          assertEqual "200" (200 :: Double) =<< evaluateNumber page "() => window.scrollY"
    , testCase "should reset scroll top after a navigation" $
        withAssetsWith iPhone6 $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/input/scrollable.html")) defaultGotoOptions
          _ <- evaluateNumber page "() => { window.scroll(0, 100); return window.scrollY; }"
          _ <- goto page (urlFromText (prefix <> "/input/scrollable2.html")) defaultGotoOptions
          assertEqual "reset" (0 :: Double) =<< evaluateNumber page "() => window.scrollY"
    , testCase "should scroll to a precise position with mobile scale" $
        withAssetsWith iPhone6 $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/input/scrollable.html")) defaultGotoOptions
          height <- evaluateNumber page "() => document.body.scrollHeight"
          assertBool "tall" (height > 1000)
          _ <- evaluateNumber page "() => { window.scroll(0, 100); return window.scrollY; }"
          assertEqual "100" (100 :: Double) =<< evaluateNumber page "() => window.scrollY"
    , testCase "should emulate viewport and screen size" $
        withChromiumContextWith iPhone12 $ \_ page -> do
          setContent page "<meta name=\"viewport\" content=\"width=device-width, user-scalable=no\" />"
          assertEqual "screen width" (390 :: Double) =<< evaluateNumber page "() => window.screen.width"
          assertEqual "screen height" (844 :: Double) =<< evaluateNumber page "() => window.screen.height"
          assertEqual "inner width" (390 :: Double) =<< evaluateNumber page "() => window.innerWidth"
          assertEqual "inner height" (664 :: Double) =<< evaluateNumber page "() => window.innerHeight"
    , testCase "should emulate viewport without screen size" $
        withChromiumContextWith iPhone6 $ \_ page -> do
          setContent page "<meta name=\"viewport\" content=\"width=device-width, user-scalable=no\" />"
          assertEqual "screen width" (375 :: Double) =<< evaluateNumber page "() => window.screen.width"
          assertEqual "screen height" (667 :: Double) =<< evaluateNumber page "() => window.screen.height"
          assertEqual "inner width" (375 :: Double) =<< evaluateNumber page "() => window.innerWidth"
          assertEqual "inner height" (667 :: Double) =<< evaluateNumber page "() => window.innerHeight"
    ]

iPhone6 :: NewContextOptions
iPhone6 =
  defaultNewContextOptions
    { newContextUserAgent =
        Just (UserAgent "Mozilla/5.0 (iPhone; CPU iPhone OS 11_0 like Mac OS X) AppleWebKit/604.1.38 (KHTML, like Gecko) Version/26.5 Mobile/15A372 Safari/604.1")
    , newContextViewport = Just (ViewportSize (ViewportWidth 375) (ViewportHeight 667))
    , newContextDeviceScaleFactor = Just (deviceScaleFactorFromInt 2)
    , newContextIsMobile = Just True
    , newContextHasTouch = Just True
    }

iPhone12 :: NewContextOptions
iPhone12 =
  defaultNewContextOptions
    { newContextUserAgent =
        Just (UserAgent "Mozilla/5.0 (iPhone; CPU iPhone OS 14_4 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/26.5 Mobile/15E148 Safari/604.1")
    , newContextViewport = Just (ViewportSize (ViewportWidth 390) (ViewportHeight 664))
    , newContextScreen = Just (ViewportSize (ViewportWidth 390) (ViewportHeight 844))
    , newContextDeviceScaleFactor = Just (deviceScaleFactorFromInt 3)
    , newContextIsMobile = Just True
    , newContextHasTouch = Just True
    }

iPhoneScrollHtml :: Text
iPhoneScrollHtml =
  "<meta name=\"viewport\" content=\"width=device-width, user-scalable=no\" />"
    <> Text.replicate 40 "<p>Lorem ipsum dolor sit amet consectetur adipiscing elit proin, integer curabitur imperdiet rhoncus cursus tincidunt bibendum.</p>"
    <> "<button>hi</button>"

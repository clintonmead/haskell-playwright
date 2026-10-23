module Playwright.BrowserContextViewportMobileSpec
  ( tests
  ) where

import Data.Text
  ( Text
  , replace
  )
import Playwright
  ( AriaRole (AriaRole)
  , BoundingBox (boundingBoxHeight, boundingBoxWidth, boundingBoxX, boundingBoxY)
  , ClickOptions (clickOptionsForce)
  , ConsoleMessage (consoleMessageText)
  , ContentType (ContentType)
  , CssPixel (cssPixelToScientific)
  , Exactness (Inexact)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType)
  , NewContextOptions (newContextDeviceScaleFactor, newContextHasTouch, newContextIsMobile, newContextUserAgent, newContextViewport)
  , Point (Point)
  , UrlMatch (UrlMatchGlob)
  , UserAgent (UserAgent)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , armConsole
  , click
  , clickWith
  , contextBrowser
  , cssPixelFromInt
  , defaultClickOptions
  , defaultFulfillOptions
  , defaultGetByRoleOptions
  , defaultGotoOptions
  , defaultNewContextOptions
  , deviceScaleFactorFromInt
  , evaluateBool
  , evaluateNumber
  , evaluateText
  , fulfillRoute
  , getByRole
  , getByText
  , goto
  , locator
  , selectorFromText
  , locatorBoundingBox
  , mouseClick
  , mouseMove
  , mouseWheel
  , newContextWith
  , newPage
  , route
  , setViewportSize
  , urlFromText
  , waitForFunction
  , wheelDeltaFromInt
  )
import Playwright.Harness
  ( assertBool
  , withAssetsContextWith
  , withAssetsWith
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
    "browsercontext-viewport-mobile"
    [ testCase "should support mobile emulation" $
        withAssetsWith iPhone6 $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/mobile.html")) defaultGotoOptions
          assertEqual "375" (375 :: Double) =<< evaluateNumber page "() => window.innerWidth"
          setViewportSize page (ViewportSize (ViewportWidth 400) (ViewportHeight 300))
          assertEqual "400" (400 :: Double) =<< evaluateNumber page "() => window.innerWidth"
    , testCase "should support touch emulation" $
        withAssetsWith iPhone6 $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/mobile.html")) defaultGotoOptions
          assertEqual "ontouchstart" True =<< evaluateBool page "() => 'ontouchstart' in window"
          assertEqual "dispatch" "Received touch"
            =<< evaluateText
              page
              "() => { let fulfill; const promise = new Promise(x => fulfill = x); window.ontouchstart = function() { fulfill('Received touch'); }; window.dispatchEvent(new Event('touchstart')); fulfill('Did not receive touch'); return promise; }"
    , testCase "should be detectable" $
        withChromiumContextWith iPhone6 $ \_ page ->
          assertEqual "touch" True
            =<< evaluateBool page "() => 'ontouchstart' in window || !!window.TouchEvent"
    , testCase "should detect touch when applying viewport with touches" $
        withAssetsWith
          defaultNewContextOptions
            { newContextViewport = Just (ViewportSize (ViewportWidth 800) (ViewportHeight 600))
            , newContextHasTouch = Just True
            }
          $ \prefix page -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            assertEqual "touch" True
              =<< evaluateBool page "() => 'ontouchstart' in window || !!window.TouchEvent"
    , testCase "should support landscape emulation" $
        withAssetsContextWith iPhone6 $ \prefix ctx page -> do
          _ <- goto page (urlFromText (prefix <> "/mobile.html")) defaultGotoOptions
          assertEqual "portrait" False
            =<< evaluateBool page "() => matchMedia('(orientation: landscape)').matches"
          browser <- contextBrowser ctx
          landscapeCtx <- newContextWith browser iPhone6Landscape
          landscape <- newPage landscapeCtx
          assertEqual "landscape" True
            =<< evaluateBool landscape "() => matchMedia('(orientation: landscape)').matches"
    , testCase "should support window.orientation emulation" $
        withAssetsWith
          defaultNewContextOptions
            { newContextViewport = Just (ViewportSize (ViewportWidth 300) (ViewportHeight 400))
            , newContextIsMobile = Just True
            }
          $ \prefix page -> do
            _ <- goto page (urlFromText (prefix <> "/mobile.html")) defaultGotoOptions
            assertEqual "0" (0 :: Double) =<< evaluateNumber page "() => window.orientation"
            setViewportSize page (ViewportSize (ViewportWidth 400) (ViewportHeight 300))
            assertEqual "90" (90 :: Double) =<< evaluateNumber page "() => window.orientation"
    , testCase "should preserve window.orientation override after navigation" $
        withAssetsWith
          defaultNewContextOptions
            { newContextViewport = Just (ViewportSize (ViewportWidth 400) (ViewportHeight 300))
            , newContextIsMobile = Just True
            }
          $ \prefix page -> do
            _ <- goto page (urlFromText (prefix <> "/mobile.html")) defaultGotoOptions
            assertEqual "same origin" (90 :: Double) =<< evaluateNumber page "() => window.orientation"
            _ <- goto page (urlFromText (replace "127.0.0.1" "localhost" prefix <> "/mobile.html")) defaultGotoOptions
            assertEqual "cross origin" (90 :: Double) =<< evaluateNumber page "() => window.orientation"
    , testCase "should preserve screen.orientation.type override after navigation" $
        withAssetsWith
          defaultNewContextOptions
            { newContextViewport = Just (ViewportSize (ViewportWidth 300) (ViewportHeight 400))
            , newContextIsMobile = Just True
            }
          $ \prefix page -> do
            _ <- goto page (urlFromText (prefix <> "/mobile.html")) defaultGotoOptions
            assertEqual "same origin" "portrait-primary"
              =<< evaluateText page "() => window.screen.orientation.type"
            _ <- goto page (urlFromText (replace "127.0.0.1" "localhost" prefix <> "/mobile.html")) defaultGotoOptions
            assertEqual "cross origin" "portrait-primary"
              =<< evaluateText page "() => window.screen.orientation.type"
    , testCase "should fire orientationchange event" $
        withAssetsWith
          defaultNewContextOptions
            { newContextViewport = Just (ViewportSize (ViewportWidth 300) (ViewportHeight 400))
            , newContextIsMobile = Just True
            }
          $ \prefix page -> do
            _ <- goto page (urlFromText (prefix <> "/mobile.html")) defaultGotoOptions
            _ <-
              evaluateBool
                page
                "() => { let counter = 0; window.addEventListener('orientationchange', () => console.log(++counter)); return true; }"
            takeFirst <- armConsole page
            setViewportSize page (ViewportSize (ViewportWidth 400) (ViewportHeight 300))
            firstMessage <- takeFirst
            assertEqual "1" "1" (consoleMessageText firstMessage)
            takeSecond <- armConsole page
            setViewportSize page (ViewportSize (ViewportWidth 300) (ViewportHeight 400))
            secondMessage <- takeSecond
            assertEqual "2" "2" (consoleMessageText secondMessage)
    , testCase "default mobile viewports to 980 width" $
        withAssetsWith
          defaultNewContextOptions
            { newContextViewport = Just (ViewportSize (ViewportWidth 320) (ViewportHeight 480))
            , newContextIsMobile = Just True
            }
          $ \prefix page -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            assertEqual "980" (980 :: Double) =<< evaluateNumber page "() => window.innerWidth"
    , testCase "respect meta viewport tag" $
        withAssetsWith
          defaultNewContextOptions
            { newContextViewport = Just (ViewportSize (ViewportWidth 320) (ViewportHeight 480))
            , newContextIsMobile = Just True
            }
          $ \prefix page -> do
            _ <- goto page (urlFromText (prefix <> "/mobile.html")) defaultGotoOptions
            assertEqual "320" (320 :: Double) =<< evaluateNumber page "() => window.innerWidth"
    , testCase "should emulate the hover media feature" $
        withChromiumContextWith iPhone6 $ \ctx mobile -> do
          assertEqual "hover" False =<< evaluateBool mobile "() => matchMedia('(hover: hover)').matches"
          assertEqual "none" True =<< evaluateBool mobile "() => matchMedia('(hover: none)').matches"
          assertEqual "any hover" False =<< evaluateBool mobile "() => matchMedia('(any-hover: hover)').matches"
          assertEqual "any none" True =<< evaluateBool mobile "() => matchMedia('(any-hover: none)').matches"
          assertEqual "coarse" True =<< evaluateBool mobile "() => matchMedia('(pointer: coarse)').matches"
          assertEqual "fine" False =<< evaluateBool mobile "() => matchMedia('(pointer: fine)').matches"
          browser <- contextBrowser ctx
          desktopCtx <- newContextWith browser defaultNewContextOptions
          desktop <- newPage desktopCtx
          assertEqual "desktop none" False =<< evaluateBool desktop "() => matchMedia('(hover: none)').matches"
          assertEqual "desktop hover" True =<< evaluateBool desktop "() => matchMedia('(hover: hover)').matches"
          assertEqual "desktop fine" True =<< evaluateBool desktop "() => matchMedia('(pointer: fine)').matches"
    , testCase "should scroll when emulating a mobile viewport" $
        withAssetsWith
          defaultNewContextOptions
            { newContextViewport = Just (ViewportSize (ViewportWidth 1000) (ViewportHeight 600))
            , newContextIsMobile = Just True
            }
          $ \prefix page -> do
            _ <- goto page (urlFromText (prefix <> "/input/scrollable.html")) defaultGotoOptions
            mouseMove page (Point (cssPixelFromInt 50) (cssPixelFromInt 60))
            mouseWheel page (wheelDeltaFromInt 0) (wheelDeltaFromInt 100)
            _ <- waitForFunction page "() => window.scrollY === 100"
            pure ()
    , testCase "mouse should work with mobile viewports and cross process navigations" $
        withAssetsContextWith
          defaultNewContextOptions
            { newContextViewport = Just (ViewportSize (ViewportWidth 360) (ViewportHeight 640))
            , newContextIsMobile = Just True
            }
          $ \prefix _ page -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            _ <- goto page (urlFromText (replace "127.0.0.1" "localhost" prefix <> "/mobile.html")) defaultGotoOptions
            _ <-
              evaluateBool
                page
                "() => { document.addEventListener('click', event => { window.result = { x: event.clientX, y: event.clientY }; }); return true; }"
            mouseClick page (Point (cssPixelFromInt 30) (cssPixelFromInt 40))
            assertEqual "x" (30 :: Double) =<< evaluateNumber page "() => window.result.x"
            assertEqual "y" (40 :: Double) =<< evaluateNumber page "() => window.result.y"
    , testCase "should scroll mobile page with background-attachment: fixed" $
        withAssetsWith iPhone12 $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/input/background-fixed.html")) defaultGotoOptions
          click (getByRole page (AriaRole "button") defaultGetByRoleOptions)
          assertBool "scrolled past 1000" . (> 1000) =<< evaluateNumber page "() => window.scrollY"
    , testCase "view scale should reset after navigation" $
        withChromiumContextWith
          defaultNewContextOptions
            { newContextViewport = Just (ViewportSize (ViewportWidth 390) (ViewportHeight 664))
            , newContextIsMobile = Just True
            }
          $ \_ page -> do
            _ <-
              goto
                page
                (urlFromText "data:text/html,<meta name='viewport' content='device-width, initial-scale=1'><button>Mobile Viewport</button>")
                defaultGotoOptions
            route page (UrlMatchGlob "**/button.html") $ \r ->
              fulfillRoute
                r
                defaultFulfillOptions
                  { fulfillBody = Just (FulfillText buttonPageHtml)
                  , fulfillContentType = Just (ContentType "text/html")
                  }
            _ <- goto page (urlFromText "http://localhost/button.html") defaultGotoOptions
            clickWith (getByText page "Click me" Inexact) defaultClickOptions {clickOptionsForce = Just True}
            box <- locatorBoundingBox (locator page (selectorFromText "button"))
            assertEqual "clicks" (1 :: Double) =<< evaluateNumber page "() => window.clicks.length"
            x <- evaluateNumber page "() => window.clicks[0].x"
            y <- evaluateNumber page "() => window.clicks[0].y"
            case box of
              Nothing -> assertBool "button has a box" False
              Just b ->
                assertBool "click inside button" $
                  toD (boundingBoxX b) <= x
                    && x <= toD (boundingBoxX b) + toD (boundingBoxWidth b)
                    && toD (boundingBoxY b) <= y
                    && y <= toD (boundingBoxY b) + toD (boundingBoxHeight b)
    ]

-- | Converts a CSS pixel measurement to a plain Double for range checks.
toD :: CssPixel -> Double
toD = realToFrac . cssPixelToScientific

buttonPageHtml :: Text
buttonPageHtml =
  "<body><button>Click me</button><script>window.clicks = []; document.addEventListener('click', e => { const dot = document.createElement('div'); dot.style.position = 'absolute'; dot.style.width = '10px'; dot.style.height = '10px'; dot.style.borderRadius = '5px'; dot.style.backgroundColor = 'red'; dot.style.left = e.pageX + 'px'; dot.style.top = e.pageY + 'px'; dot.textContent = 'x: ' + e.pageX + ' y: ' + e.pageY; document.body.appendChild(dot); window.clicks.push({ x: e.pageX, y: e.pageY }); });</script></body>"

iPhone12 :: NewContextOptions
iPhone12 =
  defaultNewContextOptions
    { newContextUserAgent =
        Just (UserAgent "Mozilla/5.0 (iPhone; CPU iPhone OS 14_4 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/26.6 Mobile/15E148 Safari/604.1")
    , newContextViewport = Just (ViewportSize (ViewportWidth 390) (ViewportHeight 664))
    , newContextDeviceScaleFactor = Just (deviceScaleFactorFromInt 3)
    , newContextIsMobile = Just True
    , newContextHasTouch = Just True
    }

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

iPhone6Landscape :: NewContextOptions
iPhone6Landscape =
  iPhone6 {newContextViewport = Just (ViewportSize (ViewportWidth 667) (ViewportHeight 375))}

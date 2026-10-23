module Playwright.BrowserContextPagesSpec
  ( tests
  ) where

import Playwright
  ( BoundingBox (boundingBoxHeight, boundingBoxWidth, boundingBoxX, boundingBoxY)
  , ClickOptions (clickOptionsPosition)
  , CssPixel (cssPixelToScientific)
  , NewContextOptions (newContextDeviceScaleFactor, newContextHasTouch, newContextIsMobile, newContextJavaScriptEnabled, newContextViewport)
  , Point (Point)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , armNavigationOptions
  , click
  , clickWith
  , closePage
  , cssPixelFromInt
  , defaultClickOptions
  , defaultGotoOptions
  , defaultNewContextOptions
  , defaultWaitForNavigationOptions
  , deviceScaleFactorFromInt
  , evaluateNumber
  , evaluateOnFrameText
  , evaluateText
  , focus
  , goto
  , locatorBoundingBox
  , locatorOnFrame
  , mouseDown
  , mouseMove
  , mouseUp
  , newPage
  , selectorFromText
  , setContent
  , url
  , urlFromText
  , urlToText
  , contextPages
  , contextsEqual
  , pageContext
  , pagesEqual
  )
import Playwright.Harness
  ( attachFrame
  , assertBool
  , locate
  , withAssetsContextWith
  , withAssetsWith
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

-- Scales a CSS pixel value by 100 and rounds, so fractional layout values
-- compare exactly. Same approach as LocatorHighlightSpec.
scaledPixels :: CssPixel -> Integer
scaledPixels px = round (cssPixelToScientific px * 100)

tests :: TestTree
tests =
  testGroup
    "browsercontext-pages"
    [ testCase "should not be visible in context.pages" $
        withChromiumContext $ \ctx page -> do
          listed <- contextPages ctx
          assertEqual "contains" True (any (pagesEqual page) listed)
          closePage page
          after <- contextPages ctx
          assertEqual "gone" False (any (pagesEqual page) after)
    , testCase "page.context should return the correct instance" $
        withChromiumContext $ \ctx page ->
          assertEqual "context" True (contextsEqual (pageContext page) ctx)
    , testCase "frame.focus should work multiple times" $
        withChromiumContext $ \ctx page1 -> do
          page2 <- newPage ctx
          mapM_
            ( \page -> do
                setContent page "<button id=\"foo\" onfocus=\"window.gotFocus=true\"></button>"
                focus (locate page "#foo")
                assertEqual "focused" "true" =<< evaluateText page "() => String(!!window.gotFocus)"
            )
            [page1, page2]
    , testCase "should click with disabled javascript" $
        withAssetsWith defaultNewContextOptions {newContextJavaScriptEnabled = Just False} $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/wrappedlink.html")) defaultGotoOptions
          takeNavigation <- armNavigationOptions page defaultWaitForNavigationOptions
          click (locate page "a")
          _ <- takeNavigation
          assertEqual "url" (prefix <> "/wrappedlink.html#clicked") . urlToText =<< url page
    , testCase "should keep selection in multiple pages" $
        withChromiumContext $ \ctx page1 -> do
          page2 <- newPage ctx
          mapM_
            ( \page -> do
                setContent page "<div id=\"container\">lorem ipsum dolor sit amet</div>"
                _ <-
                  evaluateText
                    page
                    "() => { const element = document.getElementById('container'); const textNode = element.firstChild; const range = document.createRange(); range.setStart(textNode, 6); range.setEnd(textNode, 11); const selection = document.getSelection(); selection.removeAllRanges(); selection.addRange(range); return 'selected'; }"
                pure ()
            )
            [page1, page2]
          mapM_
            ( \page -> do
                selection <-
                  evaluateText
                    page
                    "() => { const selection = document.getSelection(); const range = selection.getRangeAt(0); return [selection.rangeCount, range.startOffset, range.endOffset].join(','); }"
                assertEqual "selection" "1,6,11" selection
            )
            [page1, page2]
    , testCase "should not hang with touch-enabled viewports" $
        withChromiumContextWith
          defaultNewContextOptions
            { newContextViewport = Just (ViewportSize (ViewportWidth 375) (ViewportHeight 667))
            , newContextHasTouch = Just True
            }
          $ \_ page -> do
            mouseDown page
            mouseMove page (Point (cssPixelFromInt 100) (cssPixelFromInt 10))
            mouseUp page
    , testCase "should click the button with deviceScaleFactor set" $
        withAssetsContextWith
          defaultNewContextOptions
            { newContextViewport = Just (ViewportSize (ViewportWidth 400) (ViewportHeight 400))
            , newContextDeviceScaleFactor = Just (deviceScaleFactorFromInt 5)
            }
          $ \prefix _ page -> do
            assertEqual "devicePixelRatio" 5 =<< evaluateNumber page "() => window.devicePixelRatio"
            setContent page "<div style=\"width:100px;height:100px\">spacer</div>"
            frame <- attachFrame page "button-test" (prefix <> "/input/button.html")
            click (locatorOnFrame page frame (selectorFromText "button"))
            assertEqual "result" "Clicked" =<< evaluateOnFrameText frame "() => window.result"
    , testCase "should click the button with offset with page scale" $
        withAssetsContextWith
          defaultNewContextOptions
            { newContextViewport = Just (ViewportSize (ViewportWidth 400) (ViewportHeight 400))
            , newContextIsMobile = Just True
            }
          $ \prefix _ page -> do
            _ <- goto page (urlFromText (prefix <> "/input/button.html")) defaultGotoOptions
            _ <-
              evaluateText
                page
                "() => { const button = document.querySelector('button'); button.style.borderWidth = '8px'; document.body.style.margin = '0'; return 'ok'; }"
            clickWith
              (locate page "button")
              defaultClickOptions
                { clickOptionsPosition = Just (Point (cssPixelFromInt 20) (cssPixelFromInt 10))
                }
            assertEqual "result" "Clicked" =<< evaluateText page "() => window.result"
            -- Expect 20;10 + 8px of border in each direction. Allow a delta of 2 as
            -- browsers round differently during css -> dip -> css conversion.
            pageX <- evaluateNumber page "() => window.pageX"
            pageY <- evaluateNumber page "() => window.pageY"
            assertBool ("pageX " <> show pageX) (abs (28 - pageX) <= 2)
            assertBool ("pageY " <> show pageY) (abs (18 - pageY) <= 2)
    , testCase "should return bounding box with page scale" $
        withAssetsContextWith
          defaultNewContextOptions
            { newContextViewport = Just (ViewportSize (ViewportWidth 400) (ViewportHeight 400))
            , newContextIsMobile = Just True
            }
          $ \prefix _ page -> do
            _ <- goto page (urlFromText (prefix <> "/input/button.html")) defaultGotoOptions
            _ <-
              evaluateText
                page
                "() => { document.body.style.margin = '0'; const button = document.querySelector('button'); button.style.borderWidth = '0'; button.style.width = '200px'; button.style.height = '20px'; button.style.marginLeft = '17px'; button.style.marginTop = '23px'; return 'ok'; }"
            box <- locatorBoundingBox (locate page "button")
            assertEqual
              "box"
              (Just (1700, 2300, 20000, 2000))
              (fmap (\b -> (scaledPixels (boundingBoxX b), scaledPixels (boundingBoxY b), scaledPixels (boundingBoxWidth b), scaledPixels (boundingBoxHeight b))) box)
    -- Skipped: "should not leak listeners during navigation of 20 pages". Upstream observes
    -- Node's process 'warning' event, which has no equivalent in the Haskell client.
    -- "should not leak listeners during navigation of 20 pages"
    -- Skipped: "should close page while a reload is committing". Upstream marks it fixme on
    -- Chromium (issues 42068 and 536385539), so there is no passing Chromium behaviour to port.
    -- "should close page while a reload is committing"
    ]

module Playwright.EmulationFocusSpec
  ( tests
  ) where

import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as BS
import Data.Text
  ( Text
  )
import Playwright
  ( Frame
  , Page
  , Playwright
  , Point (Point)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , click
  , cssPixelFromInt
  , defaultGotoOptions
  , defaultLaunchOptions
  , defaultNewContextOptions
  , defaultScreenshotOptions
  , evaluate
  , evaluateBool
  , evaluateHandleOnFrame
  , evaluateNumber
  , evaluateOnFrame
  , evaluateOnFrameBool
  , evaluateText
  , evaluateWith
  , focus
  , hover
  , jsonValueTexts
  , keyboardType
  , locatorOnFrame
  , mouseClick
  , newBrowserPage
  , newContextWith
  , newPage
  , screenshot
  , selectorFromText
  , setContent
  , setViewportSize
  , textArgument
  , urlFromText
  , waitForPopup
  , withBrowser
  , chromium
  , goto
  )
import Playwright.Harness
  ( attachFrame
  , locate
  , withAssets
  , withAssetsContextWith
  , withChromiumBrowser
  , withChromiumContext
  , withChromiumPage
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
import UnliftIO.Async
  ( wait
  , withAsync
  )

tests :: TestTree
tests =
  testGroup
    "emulation-focus"
    [ testCase "should think that it is focused by default" $
        withChromiumPage $ \page ->
          assertEqual "focused" True =<< evaluateBool page "() => document.hasFocus()"
    , testCase "should think that all pages are focused" $
        withChromiumContext $ \ctx page -> do
          page2 <- newPage ctx
          assertEqual "page1" True =<< evaluateBool page "() => document.hasFocus()"
          assertEqual "page2" True =<< evaluateBool page2 "() => document.hasFocus()"
    , testCase "should focus popups by default" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync (waitForPopup page) $ \pending -> do
            _ <- evaluateWith page "url => { window.open(url); }" (textArgument (prefix <> "/empty.html"))
            popup <- wait pending
            assertEqual "popup" True =<< evaluateBool popup "() => document.hasFocus()"
            assertEqual "page" True =<< evaluateBool page "() => document.hasFocus()"
    , testCase "should provide target for keyboard events" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          page2 <- newPage ctx
          _ <- goto page (urlFromText (prefix <> "/input/textarea.html")) defaultGotoOptions
          _ <- goto page2 (urlFromText (prefix <> "/input/textarea.html")) defaultGotoOptions
          focus (locate page "input")
          focus (locate page2 "input")
          keyboardType page "first"
          keyboardType page2 "second"
          assertEqual "first" "first" =<< evaluateText page "() => result"
          assertEqual "second" "second" =<< evaluateText page2 "() => result"
    , testCase "should not affect mouse event target page" $
        withChromiumContext $ \ctx page -> do
          page2 <- newPage ctx
          _ <- evaluate page "() => { document.onclick = () => window.clickCount = (window.clickCount || 0) + 1; }"
          _ <- evaluate page2 "() => { document.onclick = () => window.clickCount = (window.clickCount || 0) + 1; }"
          focus (locate page "body")
          focus (locate page2 "body")
          mouseClick page (Point (cssPixelFromInt 1) (cssPixelFromInt 1))
          mouseClick page2 (Point (cssPixelFromInt 1) (cssPixelFromInt 1))
          assertEqual "page1" (1 :: Double) =<< evaluateNumber page "() => window.clickCount"
          assertEqual "page2" (1 :: Double) =<< evaluateNumber page2 "() => window.clickCount"
    , testCase "should change document.activeElement" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          page2 <- newPage ctx
          _ <- goto page (urlFromText (prefix <> "/input/textarea.html")) defaultGotoOptions
          _ <- goto page2 (urlFromText (prefix <> "/input/textarea.html")) defaultGotoOptions
          focus (locate page "input")
          focus (locate page2 "textarea")
          assertEqual "input" "INPUT" =<< evaluateText page "() => document.activeElement.tagName"
          assertEqual "textarea" "TEXTAREA" =<< evaluateText page2 "() => document.activeElement.tagName"
    , testCase "should change focused iframe" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          frame1 <- attachFrame page "frame1" (prefix <> "/input/textarea.html")
          frame2 <- attachFrame page "frame2" (prefix <> "/input/textarea.html")
          _ <- evaluateOnFrame frame1 "() => { self._events = []; const element = document.querySelector('input'); element.onfocus = element.onblur = e => self._events.push(e.type); }"
          _ <- evaluateOnFrame frame2 "() => { self._events = []; const element = document.querySelector('input'); element.onfocus = element.onblur = e => self._events.push(e.type); }"
          assertEqual "f1" False =<< evaluateOnFrameBool frame1 "() => document.hasFocus()"
          assertEqual "f2" False =<< evaluateOnFrameBool frame2 "() => document.hasFocus()"
          focusFrameInput page frame1
          assertEqual "focus1" ["focus" :: Text] =<< evaluateOnFrameTextList frame1
          assertEqual "none2" ([] :: [Text]) =<< evaluateOnFrameTextList frame2
          assertEqual "has1" True =<< evaluateOnFrameBool frame1 "() => document.hasFocus()"
          assertEqual "has2" False =<< evaluateOnFrameBool frame2 "() => document.hasFocus()"
          focusFrameInput page frame2
          assertEqual "blur1" ["focus", "blur" :: Text] =<< evaluateOnFrameTextList frame1
          assertEqual "focus2" ["focus" :: Text] =<< evaluateOnFrameTextList frame2
    , testCase "should focus with more than one page/context" $
        withChromiumBrowser $ \browser -> do
          ctx1 <- newContextWith browser defaultNewContextOptions
          ctx2 <- newContextWith browser defaultNewContextOptions
          page1 <- newPage ctx1
          page2 <- newPage ctx2
          setContent page1 "<button id=\"foo\" onfocus=\"window.gotFocus=true\">foo</button>"
          setContent page2 "<button id=\"foo\" onfocus=\"window.gotFocus=true\">foo</button>"
          focus (locate page1 "#foo")
          focus (locate page2 "#foo")
          assertEqual "p1" True =<< evaluateBool page1 "() => !!window.gotFocus"
          assertEqual "p2" True =<< evaluateBool page2 "() => !!window.gotFocus"
    , testCase "should not fire blur events when interacting with more than one page/context" $
        withChromiumBrowser $ \browser -> do
          ctx1 <- newContextWith browser defaultNewContextOptions
          ctx2 <- newContextWith browser defaultNewContextOptions
          page1 <- newPage ctx1
          page2 <- newPage ctx2
          setContent page1 "<button id=\"foo\" onblur=\"window.gotBlur=true\">foo</button>"
          setContent page2 "<button id=\"foo\" onblur=\"window.gotBlur=true\">foo</button>"
          click (locate page1 "#foo")
          click (locate page2 "#foo")
          assertEqual "p1" False =<< evaluateBool page1 "() => !!window.gotBlur"
          assertEqual "p2" False =<< evaluateBool page2 "() => !!window.gotBlur"
    , testCase "should trigger hover state concurrently" $
        withChromiumBrowser $ \browser1 -> do
          ctx1 <- newContextWith browser1 defaultNewContextOptions
          page1 <- newPage ctx1
          page2 <- newPage ctx1
          browserType <- chromium
          withBrowser browserType defaultLaunchOptions $ \browser2 -> do
            page3 <- newBrowserPage browser2
            setHoverContent page1
            setHoverContent page2
            setHoverContent page3
            hover (locate page1 "span")
            hover (locate page2 "span")
            hover (locate page3 "span")
            click (locate page1 "button")
            click (locate page2 "button")
            click (locate page3 "button")
            assertEqual "p1" (1 :: Double) =<< evaluateNumber page1 "() => window.clicked"
            assertEqual "p2" (1 :: Double) =<< evaluateNumber page2 "() => window.clicked"
            assertEqual "p3" (1 :: Double) =<< evaluateNumber page3 "() => window.clicked"
            click (locate page1 "button")
            click (locate page2 "button")
            click (locate page3 "button")
            assertEqual "p1 again" (2 :: Double) =<< evaluateNumber page1 "() => window.clicked"
            assertEqual "p2 again" (2 :: Double) =<< evaluateNumber page2 "() => window.clicked"
            assertEqual "p3 again" (2 :: Double) =<< evaluateNumber page3 "() => window.clicked"
    , testCase "should not affect screenshots" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          page2 <- newPage ctx
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          setViewportSize page2 (ViewportSize (ViewportWidth 50) (ViewportHeight 50))
          _ <- goto page2 (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          focus (locate page "body")
          focus (locate page2 "body")
          sanity <- screenshot page defaultScreenshotOptions
          cell <- screenshot page2 defaultScreenshotOptions
          -- Upstream compares these against snapshots; a PNG signature and IHDR size check is the closest byte-level check here.
          assertBool "sanity screenshot is a png" (pngMagic `BS.isPrefixOf` sanity)
          assertEqual "sanity size" (500, 500) (pngSize sanity)
          assertBool "grid cell screenshot is a png" (pngMagic `BS.isPrefixOf` cell)
          assertEqual "grid cell size" (50, 50) (pngSize cell)
    ]

pngMagic :: ByteString
pngMagic =
  "\137PNG\r\n\26\n"

-- Width and height come from the IHDR chunk: big-endian u32 at bytes 16-19 and 20-23.
pngSize :: ByteString -> (Int, Int)
pngSize image =
  (bigEndian32 (BS.take 4 (BS.drop 16 image)), bigEndian32 (BS.take 4 (BS.drop 20 image)))

bigEndian32 :: ByteString -> Int
bigEndian32 =
  BS.foldl' (\acc byte -> acc * 256 + fromIntegral byte) 0

setHoverContent :: Page -> Playwright ()
setHoverContent page =
  setContent
    page
    "<style>button { display: none; } div:hover button { display: inline; }</style><div><span>hover me</span><button onclick=\"window.clicked=1+(window.clicked || 0)\">click me</button></div>"

focusFrameInput :: Page -> Frame -> Playwright ()
focusFrameInput page frame =
  focus (locatorOnFrame page frame (selectorFromText "input"))

evaluateOnFrameTextList :: Frame -> Playwright [Text]
evaluateOnFrameTextList frame = do
  handle <- evaluateHandleOnFrame frame "() => self._events"
  jsonValueTexts handle

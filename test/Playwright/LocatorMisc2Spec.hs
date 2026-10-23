module Playwright.LocatorMisc2Spec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text
  ( pack
  )
import Playwright
  ( Page
  , BoundingBox (boundingBoxHeight, boundingBoxWidth, boundingBoxX, boundingBoxY)
  , Exactness (Inexact)
  , Key (Key)
  , Locator (locatorSelector)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , WaitForState (WaitForHidden, WaitForVisible)
  , chainLocator
  , childLocator
  , click
  , cssPixelFromInt
  , defaultGotoOptions
  , defaultScreenshotOptions
  , locatorScreenshot
  , evalOnSelector
  , evalOnSelectorText
  , evaluate
  , evaluateOnLocator
  , evaluateOnLocatorNumber
  , evaluateText
  , fill
  , filterVisible
  , frameLocator
  , goto
  , innerText
  , inputValue
  , locatorBoundingBox
  , locatorCount
  , locatorScrollIntoView
  , locatorSelectText
  , nth
  , press
  , pressSequentially
  , selectorFromText
  , setContent
  , setViewportSize
  , typeText
  , urlFromText
  , waitFor
  , Playwright
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
  , withAssetPage
  , withAssets
  , withChromiumPage
  , assertEqual
  , assertFailure
  )
import Playwright.Locator qualified as Locator
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
    "locator-misc-2"
    [ testCase "should press" $
        withChromiumPage $ \page -> do
          setContent page "<input type='text' />"
          press (locate page "input") (Key "h")
          assertEqual "value" "h" =<< evalOnSelectorText page (selectorFromText "input") "input => input.value"
    , testCase "should scroll into view" $
        withAssetPage "offscreenbuttons.html" $ \page ->
          mapM_ (assertScrollsIntoView page) ([0 .. 10] :: [Int])
    , testCase "should select textarea" $
        withAssetPage "input/textarea.html" $ \page -> do
          _ <- evaluateOnLocator (locate page "textarea") "textarea => { textarea.value = 'some value'; }"
          locatorSelectText (locate page "textarea")
          assertEqual "selected" "some value" =<< evaluateText page "() => window.getSelection().toString()"
    , testCase "should type" $
        withChromiumPage $ \page -> do
          setContent page "<input type='text' />"
          typeText (locate page "input") "hello"
          assertEqual "value" "hello" =<< evalOnSelectorText page (selectorFromText "input") "input => input.value"
    , testCase "should pressSequentially" $
        withChromiumPage $ \page -> do
          setContent page "<input type='text' />"
          pressSequentially (locate page "input") "hello"
          assertEqual "value" "hello" =<< evalOnSelectorText page (selectorFromText "input") "input => input.value"
    , testCase "should return bounding box" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          box <- locatorBoundingBox (locate page ".box:nth-of-type(13)")
          case box of
            Nothing ->
              assertFailure "expected bounding box"
            Just found -> do
              assertEqual "x" (cssPixelFromInt 100) (boundingBoxX found)
              assertEqual "y" (cssPixelFromInt 50) (boundingBoxY found)
              assertEqual "width" (cssPixelFromInt 50) (boundingBoxWidth found)
              assertEqual "height" (cssPixelFromInt 50) (boundingBoxHeight found)
    , testCase "should take screenshot" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          _ <- evaluate page "() => window.scrollBy(50, 100)"
          image <- locatorScreenshot (locate page ".box:nth-of-type(3)") defaultScreenshotOptions
          assertEqual "png signature" True (pngMagic `BS.isPrefixOf` image)
          assertEqual "element size" (50, 50) (pngSize image)
    , testCase "should waitFor" $
        withChromiumPage $ \page -> do
          setContent page "<div></div>"
          let
            loc = locate page "span"
          withAsync (waitFor loc WaitForVisible) $ \pending -> do
            _ <- evalOnSelector page (selectorFromText "div") "div => { div.innerHTML = '<span>target</span>'; }"
            wait pending
          assertEqual "text" "target" =<< evalOnSelectorText page (selectorFromText "span") "e => e.textContent"
    , testCase "should waitFor hidden" $
        withChromiumPage $ \page -> do
          setContent page "<div><span>target</span></div>"
          let
            loc = locate page "span"
          withAsync (waitFor loc WaitForHidden) $ \pending -> do
            _ <- evalOnSelector page (selectorFromText "div") "div => { div.innerHTML = ''; }"
            wait pending
    , testCase "should combine visible with other selectors" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div><div class=\"item\" style=\"display: none\">Hidden data0</div><div class=\"item\">visible data1</div><div class=\"item\" style=\"display: none\">Hidden data1</div><div class=\"item\">visible data2</div><div class=\"item\" style=\"display: none\">Hidden data1</div><div class=\"item\">visible data3</div></div>"
          assertEqual "nth visible" "visible data2"
            =<< evalOnSelectorText page (selectorFromText ".item >> visible=true >> nth=1") "e => e.textContent"
          assertEqual "data3" "visible data3"
            =<< evalOnSelectorText page (selectorFromText ".item >> visible=true >> text=data3") "e => e.textContent"
    , testCase "should support filter(visible)" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div><div class=\"item\" style=\"display: none\">Hidden data0</div><div class=\"item\">visible data1</div><div class=\"item\" style=\"display: none\">Hidden data1</div><div class=\"item\">visible data2</div><div class=\"item\" style=\"display: none\">Hidden data2</div><div class=\"item\">visible data3</div></div>"
          assertEqual "nth" "visible data2" =<< innerText (nth (filterVisible (locate page ".item") True) 1)
          assertEqual "data3" "visible data3" =<< innerText (Locator.getByText (filterVisible (locate page ".item") True) "data3" Inexact)
          assertEqual "hidden" "Hidden data1" =<< innerText (Locator.getByText (filterVisible (locate page ".item") False) "data1" Inexact)
    , testCase "should support visible()" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div><div class=\"item\" style=\"display: none\">Hidden data0</div><div class=\"item\">visible data1</div><div class=\"item\" style=\"display: none\">Hidden data1</div><div class=\"item\">visible data2</div><div class=\"item\" style=\"display: none\">Hidden data2</div><div class=\"item\">visible data3</div></div>"
          assertEqual "nth" "visible data2" =<< innerText (nth (filterVisible (locate page ".item") True) 1)
          assertEqual "data3" "visible data3" =<< innerText (Locator.getByText (filterVisible (locate page ".item") True) "data3" Inexact)
          assertEqual "count" 3 =<< locatorCount (filterVisible (locate page ".item") True)
    , testCase "locator.count should work with deleted Map in main world" $
        withChromiumPage $ \page -> do
          _ <- evaluate page "() => { Map = 1; }"
          assertEqual "count" 0 =<< locatorCount (locate page "#searchResultTableDiv .x-grid3-row")
    , testCase "Locator.locator() and FrameLocator.locator() should accept locator" $
        withChromiumPage $ \page -> do
          setContent page "<div><input value=outer></div><iframe srcdoc=\"<div><input value=inner></div>\"></iframe>"
          let
            inputLocator = locate page "input"
            iframe = frameLocator page (selectorFromText "iframe")
            divLocator = locate page "div"
          assertEqual "page input" "outer" =<< inputValue inputLocator
          assertEqual "div chained" "outer" =<< inputValue (chainLocator (locate page "div") inputLocator)
          assertEqual "frame locator" "inner" =<< inputValue (childLocator iframe (locatorSelector inputLocator))
          assertEqual "frame nested" "inner" =<< inputValue (childLocator (childLocator iframe (selectorFromText "div")) (locatorSelector inputLocator))
          assertEqual "div input" "outer" =<< inputValue (childLocator divLocator (selectorFromText "input"))
          assertEqual "frame div" "inner" =<< inputValue (childLocator (childLocator iframe (locatorSelector divLocator)) (selectorFromText "input"))
    , testCase "should fill programmatically enabled textarea" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<button>Enable</button><form><textarea id=\"text\" disabled></textarea></form><script>document.querySelector('button').addEventListener('click', () => { document.querySelector('#text').disabled = false; });</script>"
          click (locate page "button")
          fill (locate page "#text") "Hello"
          assertEqual "filled" "Hello" =<< inputValue (locate page "#text")
    , testCase "should scroll zero-sized element into view" $
        withChromiumPage $ \page -> do
          setViewportSize page (ViewportSize (ViewportWidth 1280) (ViewportHeight 720))
          setContent
            page
            "<style>html,body { margin: 0; padding: 0; } ::-webkit-scrollbar { display: none; } * { scrollbar-width: none; }</style><div style=\"height: 2000px; text-align: center; border: 10px solid blue;\"><h1>SCROLL DOWN</h1></div><div id=lazyload style=\"font-size:75px; background-color: green;\"></div><script>const lazyLoadElement = document.querySelector('#lazyload'); const observer = new IntersectionObserver((entries) => { if (entries.some(entry => entry.isIntersecting)) { lazyLoadElement.textContent = 'LAZY LOADED CONTENT'; lazyLoadElement.style.height = '20px'; observer.disconnect(); } }); observer.observe(lazyLoadElement);</script>"
          let
            lazy = locate page "#lazyload"
          before <- locatorBoundingBox lazy
          case before of
            Nothing ->
              assertFailure "expected bounding box"
            Just found -> do
              assertEqual "x" (cssPixelFromInt 0) (boundingBoxX found)
              assertEqual "y" (cssPixelFromInt 2020) (boundingBoxY found)
              assertEqual "width" (cssPixelFromInt 1280) (boundingBoxWidth found)
              assertEqual "height" (cssPixelFromInt 0) (boundingBoxHeight found)
          locatorScrollIntoView lazy
          waitFor (Locator.getByText lazy "LAZY LOADED CONTENT" Inexact) WaitForVisible
          assertEqual "text" "LAZY LOADED CONTENT" =<< innerText lazy
          after <- locatorBoundingBox lazy
          case after of
            Nothing ->
              assertFailure "expected bounding box after scroll"
            Just found -> do
              assertEqual "x after" (cssPixelFromInt 0) (boundingBoxX found)
              assertEqual "y after" (cssPixelFromInt 720) (boundingBoxY found)
              assertEqual "width after" (cssPixelFromInt 1280) (boundingBoxWidth found)
              assertEqual "height after" (cssPixelFromInt 20) (boundingBoxHeight found)
    , testCase "press should throw on unknown keys" $
        withChromiumPage $ \page -> do
          setContent page "<input type='text' value='hello' />"
          let
            loc = locate page "input"
          assertProtocolContains "Unknown key: \"NotARealKey\"" (press loc (Key "NotARealKey"))
          assertProtocolContains "Unknown key: \"ё\"" (press loc (Key "ё"))
          assertProtocolContains "Unknown key: \"😊\"" (press loc (Key "😊"))
    ]

assertScrollsIntoView :: Page -> Int -> Playwright ()
assertScrollsIntoView page i = do
  let
    button = locate page ("#btn" <> pack (show i))
  before <- evaluateOnLocatorNumber button "button => button.getBoundingClientRect().right - window.innerWidth"
  assertEqual "before" (fromIntegral i * (10 :: Double)) before
  locatorScrollIntoView button
  after <- evaluateOnLocatorNumber button "button => button.getBoundingClientRect().right - window.innerWidth"
  assertEqual "after" True (after <= 0)
  _ <- evaluate page "() => window.scrollTo(0, 0)"
  pure ()

pngMagic :: ByteString
pngMagic =
  "\137PNG\r\n\26\n"

pngSize :: ByteString -> (Int, Int)
pngSize image =
  (bigEndian32 (BS.take 4 (BS.drop 16 image)), bigEndian32 (BS.take 4 (BS.drop 20 image)))

bigEndian32 :: ByteString -> Int
bigEndian32 =
  BS.foldl' (\acc byte -> acc * 256 + fromIntegral byte) 0

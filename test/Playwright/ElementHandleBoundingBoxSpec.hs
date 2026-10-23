module Playwright.ElementHandleBoundingBoxSpec
  ( tests
  ) where

import Data.Text
  ( replace
  )
import Playwright
  ( BoundingBox (BoundingBox, boundingBoxX, boundingBoxY)
  , CssPixel (CssPixel, cssPixelToScientific)
  , Point (Point)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , WaitForState (WaitForAttached)
  , boundingBox
  , boundingBoxHeight
  , boundingBoxWidth
  , childLocator
  , cssPixelFromInt
  , elementArgument
  , elementHandle
  , enterFrameSelector
  , evaluateNumberWith
  , evaluateOnElementNumber
  , evaluateWith
  , defaultGotoOptions
  , evaluateBool
  , frameLocator
  , frames
  , goto
  , mouseClick
  , nestedSelector
  , querySelector
  , selectorFromText
  , setContent
  , setViewportSize
  , urlFromText
  , waitForSelectorHandleOnFrame
  )
import Playwright.Harness
  ( requireHandle
  , withAssets
  , withChromiumPage
  , assertEqual
  , assertFailure
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
    "elementhandle-bounding-box"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          handle <- requireHandle =<< querySelector page (selectorFromText ".box:nth-of-type(13)")
          box <- boundingBox handle
          assertEqual
            "box"
            ( Just
                ( BoundingBox
                    (cssPixelFromInt 100)
                    (cssPixelFromInt 50)
                    (cssPixelFromInt 50)
                    (cssPixelFromInt 50)
                )
            )
            box
    , testCase "should handle nested frames" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 616) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/frames/nested-frames.html")) defaultGotoOptions
          let
            outer = frameLocator page (selectorFromText "[name=\"2frames\"]")
            nested = childLocator outer (nestedSelector (selectorFromText "[name=dos]") enterFrameSelector)
          handle <- requireHandle =<< elementHandle (childLocator nested (selectorFromText "div"))
          box <- boundingBox handle
          assertEqual
            "box"
            ( Just
                ( BoundingBox
                    (cssPixelFromInt 24)
                    (cssPixelFromInt 224)
                    (cssPixelFromInt 268)
                    (cssPixelFromInt 18)
                )
            )
            box
    , testCase "should get frame box" $
        withChromiumPage $ \page -> do
          setViewportSize page (ViewportSize (ViewportWidth 250) (ViewportHeight 250))
          setContent
            page
            "<style>body { display: flex; height: 500px; margin: 0px; } body iframe { flex-shrink: 1; border: 0; background-color: green; }</style><iframe></iframe>"
          element <- requireHandle =<< querySelector page (selectorFromText "iframe")
          box <- boundingBox element
          assertEqual
            "box"
            ( Just
                ( BoundingBox
                    (cssPixelFromInt 0)
                    (cssPixelFromInt 0)
                    (cssPixelFromInt 300)
                    (cssPixelFromInt 500)
                )
            )
            box
    , testCase "should handle scroll offset and click" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<style>* { margin: 0; padding: 0; }</style><div style=\"width:8000px; height:8000px;\"><div id=target style=\"width:20px; height:20px; margin-left:230px; margin-top:340px;\" onclick=\"window.__clicked = true\"></div></div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "#target")
          box1 <- boundingBox handle
          _ <- evaluateBool page "() => { window.scrollBy(200, 300); return true; }"
          box2 <- boundingBox handle
          assertEqual
            "before scroll"
            ( Just
                ( BoundingBox
                    (cssPixelFromInt 230)
                    (cssPixelFromInt 340)
                    (cssPixelFromInt 20)
                    (cssPixelFromInt 20)
                )
            )
            box1
          assertEqual
            "after scroll"
            ( Just
                ( BoundingBox
                    (cssPixelFromInt 30)
                    (cssPixelFromInt 40)
                    (cssPixelFromInt 20)
                    (cssPixelFromInt 20)
                )
            )
            box2
          case box2 of
            Nothing ->
              assertFailure "missing bounding box after scroll"
            Just box ->
              mouseClick
                page
                ( Point
                    (shiftPixel (boundingBoxX box) 10)
                    (shiftPixel (boundingBoxY box) 10)
                )
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should return null for invisible elements" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"display:none\">hi</div>"
          element <- requireHandle =<< querySelector page (selectorFromText "div")
          assertEqual "no box" Nothing =<< boundingBox element
    , testCase "should get bounding box of element inside a cross-origin iframe" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let crossOrigin = replace "127.0.0.1" "localhost" prefix
          setContent
            page
            ( "<div style=\"height:120px\"></div><iframe style=\"border:0;margin-left:30px;width:300px;height:200px\" src=\""
                <> crossOrigin
                <> "/input/button.html\"></iframe>"
            )
          allFrames <- frames page
          case allFrames of
            [_, frame] -> do
              button <- requireHandle =<< waitForSelectorHandleOnFrame frame (selectorFromText "button") WaitForAttached
              iframe <- requireHandle =<< querySelector page (selectorFromText "iframe")
              iframeBox <- boundingBox iframe
              box <- boundingBox button
              innerX <- evaluateOnElementNumber button "e => e.getBoundingClientRect().left"
              innerY <- evaluateOnElementNumber button "e => e.getBoundingClientRect().top"
              case (iframeBox, box) of
                (Just outer, Just found) -> do
                  assertEqual
                    "x"
                    (wholePixels (pixelValue (boundingBoxX outer) + innerX))
                    (wholePixels (pixelValue (boundingBoxX found)))
                  assertEqual
                    "y"
                    (wholePixels (pixelValue (boundingBoxY outer) + innerY))
                    (wholePixels (pixelValue (boundingBoxY found)))
                _ ->
                  assertFailure "missing bounding boxes"
            _ ->
              assertFailure "expected the main frame and one iframe"
    , testCase "should force a layout" $
        withChromiumPage $ \page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          setContent page "<div style=\"width: 100px; height: 100px\">hello</div>"
          element <- requireHandle =<< querySelector page (selectorFromText "div")
          _ <- evaluateWith page "element => element.style.height = '200px'" (elementArgument element)
          assertEqual "box"
            (Just (BoundingBox (cssPixelFromInt 8) (cssPixelFromInt 8) (cssPixelFromInt 100) (cssPixelFromInt 200)))
            =<< boundingBox element
    , testCase "should work with SVG nodes" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"500\" height=\"500\"><rect id=\"theRect\" x=\"30\" y=\"50\" width=\"200\" height=\"300\"></rect></svg>"
          element <- requireHandle =<< querySelector page (selectorFromText "#theRect")
          box <- boundingBox element
          case box of
            Nothing ->
              assertFailure "expected a bounding box"
            Just found -> do
              let
                web property = evaluateNumberWith page ("e => e.getBoundingClientRect()." <> property) (elementArgument element)
              assertEqual "x" (pixelValue (boundingBoxX found)) =<< web "x"
              assertEqual "y" (pixelValue (boundingBoxY found)) =<< web "y"
              assertEqual "width" (pixelValue (boundingBoxWidth found)) =<< web "width"
              assertEqual "height" (pixelValue (boundingBoxHeight found)) =<< web "height"
    , testCase "should work when inline box child is outside of viewport" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<style>i { position: absolute; top: -1000px; } body { margin: 0; font-size: 12px; }</style><span><i>woof</i><b>doggo</b></span>"
          element <- requireHandle =<< querySelector page (selectorFromText "span")
          box <- boundingBox element
          case box of
            Nothing ->
              assertFailure "expected a bounding box"
            Just found -> do
              let
                web property = evaluateNumberWith page ("e => e.getBoundingClientRect()." <> property) (elementArgument element)
                expected =
                  [ pixelValue (boundingBoxX found)
                  , pixelValue (boundingBoxY found)
                  , pixelValue (boundingBoxWidth found)
                  , pixelValue (boundingBoxHeight found)
                  ]
              actual <- mapM web ["x", "y", "width", "height"]
              assertEqual "rect" (map hundredths expected) (map hundredths actual)
    ]

-- | The pixel value as a Double, for comparing with numbers read from the page.
pixelValue :: CssPixel -> Double
pixelValue = realToFrac . cssPixelToScientific

-- | Round to whole pixels, as the upstream cross-origin comparison does.
wholePixels :: Double -> Int
wholePixels = round

-- | Round to hundredths of a pixel, for comparing boxes read two different ways.
hundredths :: Double -> Int
hundredths value = round (value * 100)

shiftPixel :: CssPixel -> Int -> CssPixel
shiftPixel pixel delta =
  CssPixel (cssPixelToScientific pixel + fromIntegral delta)

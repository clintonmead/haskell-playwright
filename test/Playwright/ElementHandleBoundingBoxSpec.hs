module Playwright.ElementHandleBoundingBoxSpec
  ( tests
  ) where

import Playwright
  ( BoundingBox (BoundingBox, boundingBoxX, boundingBoxY)
  , CssPixel (CssPixel, cssPixelToScientific)
  , Point (Point)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , boundingBox
  , cssPixelFromInt
  , defaultGotoOptions
  , evaluateBool
  , goto
  , mouseClick
  , querySelector
  , selectorFromText
  , setContent
  , setViewportSize
  , urlFromText
  )
import Playwright.Harness
  ( requireHandle
  , withAssets
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , assertFailure
  , testCase
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
    ]

shiftPixel :: CssPixel -> Int -> CssPixel
shiftPixel pixel delta =
  CssPixel (cssPixelToScientific pixel + fromIntegral delta)

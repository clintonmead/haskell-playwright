module Playwright.LocatorHighlightSpec
  ( tests
  ) where

import Playwright
  ( BoundingBox (BoundingBox, boundingBoxHeight, boundingBoxWidth, boundingBoxX, boundingBoxY)
  , CssPixel (CssPixel, cssPixelToScientific)
  , WaitForState (WaitForVisible)
  , highlight
  , isVisible
  , locatorBoundingBox
  , setContent
  , textContent
  , waitFor
  )
import Playwright.Harness
  ( locate
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
    "locator-highlight"
    [ testCase "should highlight locator" $
        withChromiumPage $ \page -> do
          setContent page "<input type='text' />"
          highlight (locate page "input")
          waitFor (locate page "x-pw-tooltip") WaitForVisible
          tooltip <- textContent (locate page "x-pw-tooltip")
          assertEqual "tooltip" (Just "locator('input')") tooltip
          visible <- isVisible (locate page "x-pw-highlight")
          assertEqual "highlight" True visible
          box1 <- requireBox =<< locatorBoundingBox (locate page "input")
          box2 <- requireBox =<< locatorBoundingBox (locate page "x-pw-highlight")
          assertEqual "box" (roundBox box1) (roundBox box2)
    ]

requireBox :: Maybe BoundingBox -> IO BoundingBox
requireBox found =
  case found of
    Just box ->
      pure box
    Nothing ->
      assertFailure "missing bounding box"

roundBox :: BoundingBox -> BoundingBox
roundBox box =
  BoundingBox
    { boundingBoxX = roundPixel (boundingBoxX box)
    , boundingBoxY = roundPixel (boundingBoxY box)
    , boundingBoxWidth = roundPixel (boundingBoxWidth box)
    , boundingBoxHeight = roundPixel (boundingBoxHeight box)
    }

roundPixel :: CssPixel -> CssPixel
roundPixel pixel =
  CssPixel (fromInteger (round (cssPixelToScientific pixel)))

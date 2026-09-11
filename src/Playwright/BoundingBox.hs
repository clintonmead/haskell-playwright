module Playwright.BoundingBox
  ( BoundingBox (..)
  ) where

import Playwright.CssPixel
  ( CssPixel
  )

data BoundingBox = BoundingBox
  { boundingBoxX :: CssPixel
  , boundingBoxY :: CssPixel
  , boundingBoxWidth :: CssPixel
  , boundingBoxHeight :: CssPixel
  }
  deriving stock (Eq, Show)

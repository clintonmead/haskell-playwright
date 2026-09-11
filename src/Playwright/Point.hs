module Playwright.Point
  ( Point (..)
  ) where

import Playwright.CssPixel
  ( CssPixel
  )

data Point = Point
  { pointX :: CssPixel
  , pointY :: CssPixel
  }
  deriving stock (Eq, Show)

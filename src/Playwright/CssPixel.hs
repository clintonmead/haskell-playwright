module Playwright.CssPixel
  ( CssPixel (..)
  , cssPixelFromInt
  ) where

import Data.Scientific
  ( Scientific
  )

newtype CssPixel = CssPixel
  { cssPixelToScientific :: Scientific
  }
  deriving stock (Eq, Show)

cssPixelFromInt :: Int -> CssPixel
cssPixelFromInt n =
  CssPixel (fromIntegral n)

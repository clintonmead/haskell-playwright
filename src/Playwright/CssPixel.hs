module Playwright.CssPixel
  ( CssPixel (..)
  , cssPixelFromDouble
  , cssPixelFromInt
  ) where

import Data.Scientific
  ( Scientific
  , fromFloatDigits
  )

newtype CssPixel = CssPixel
  { cssPixelToScientific :: Scientific
  }
  deriving stock (Eq, Show)

cssPixelFromInt :: Int -> CssPixel
cssPixelFromInt n =
  CssPixel (fromIntegral n)

cssPixelFromDouble :: Double -> CssPixel
cssPixelFromDouble n =
  CssPixel (fromFloatDigits n)

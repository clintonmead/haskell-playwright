module Playwright.ForcedColors
  ( ForcedColors (..)
  ) where

data ForcedColors
  = ForcedColorsActive
  | ForcedColorsNone
  | ForcedColorsNoOverride
  deriving stock (Eq, Ord, Show)

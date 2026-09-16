module Playwright.Contrast
  ( Contrast (..)
  ) where

data Contrast
  = ContrastNoPreference
  | ContrastMore
  | ContrastNoOverride
  deriving stock (Eq, Ord, Show)

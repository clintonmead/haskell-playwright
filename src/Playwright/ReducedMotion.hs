module Playwright.ReducedMotion
  ( ReducedMotion (..)
  ) where

data ReducedMotion
  = ReducedMotionReduce
  | ReducedMotionNoPreference
  | ReducedMotionNoOverride
  deriving stock (Eq, Ord, Show)

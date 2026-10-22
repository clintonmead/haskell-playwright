module Playwright.ScrollOption
  ( ScrollOption (..)
  ) where

-- | Whether an action may scroll the element into view first. 'ScrollNone'
-- fails instead of scrolling.
data ScrollOption
  = ScrollAuto
  | ScrollNone
  deriving stock (Eq, Ord, Show)

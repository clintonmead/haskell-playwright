module Playwright.ViewportSize
  ( ViewportHeight (..)
  , ViewportSize (..)
  , ViewportWidth (..)
  ) where

newtype ViewportWidth = ViewportWidth
  { viewportWidthToInt :: Int
  }
  deriving stock (Eq, Show)

newtype ViewportHeight = ViewportHeight
  { viewportHeightToInt :: Int
  }
  deriving stock (Eq, Show)

data ViewportSize = ViewportSize
  { viewportWidth :: ViewportWidth
  , viewportHeight :: ViewportHeight
  }
  deriving stock (Eq, Show)

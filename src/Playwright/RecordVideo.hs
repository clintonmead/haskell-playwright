module Playwright.RecordVideo
  ( RecordVideo (..)
  ) where

import Playwright.ViewportSize
  ( ViewportSize
  )

-- | Video recording options for a browser context.
data RecordVideo = RecordVideo
  { recordVideoDir :: FilePath
  , recordVideoSize :: Maybe ViewportSize
  }
  deriving stock (Eq, Show)

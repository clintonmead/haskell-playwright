module Playwright.MimeType
  ( MimeType (..)
  ) where

import Data.Text
  ( Text
  )

newtype MimeType = MimeType
  { mimeTypeToText :: Text
  }
  deriving stock (Eq, Show)

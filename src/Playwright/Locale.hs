module Playwright.Locale
  ( Locale (..)
  ) where

import Data.Text
  ( Text
  )

newtype Locale = Locale
  { localeToText :: Text
  }
  deriving stock (Eq, Show)

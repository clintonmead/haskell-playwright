module Playwright.TimezoneId
  ( TimezoneId (..)
  ) where

import Data.Text
  ( Text
  )

newtype TimezoneId = TimezoneId
  { timezoneIdToText :: Text
  }
  deriving stock (Eq, Show)

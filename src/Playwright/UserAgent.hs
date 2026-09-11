module Playwright.UserAgent
  ( UserAgent (..)
  ) where

import Data.Text
  ( Text
  )

newtype UserAgent = UserAgent
  { userAgentToText :: Text
  }
  deriving stock (Eq, Show)

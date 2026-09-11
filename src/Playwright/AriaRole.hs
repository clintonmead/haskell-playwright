module Playwright.AriaRole
  ( AriaRole (..)
  ) where

import Data.Text
  ( Text
  )

newtype AriaRole = AriaRole
  { ariaRoleToText :: Text
  }
  deriving stock (Eq, Show)

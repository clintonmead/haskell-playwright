module Playwright.ResourceType
  ( ResourceType (..)
  ) where

import Data.Text
  ( Text
  )

newtype ResourceType = ResourceType
  { resourceTypeToText :: Text
  }
  deriving stock (Eq, Show)

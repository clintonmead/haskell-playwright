module Playwright.PropertyName
  ( PropertyName (..)
  ) where

import Data.Text
  ( Text
  )

newtype PropertyName = PropertyName
  { propertyNameToText :: Text
  }
  deriving stock (Eq, Show)

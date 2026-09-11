module Playwright.BindingName
  ( BindingName (..)
  ) where

import Data.Text
  ( Text
  )

newtype BindingName = BindingName
  { bindingNameToText :: Text
  }
  deriving stock (Eq, Show)

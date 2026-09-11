module Playwright.SelectOption
  ( SelectOption (..)
  ) where

import Data.Text
  ( Text
  )

data SelectOption
  = SelectValueOrLabel Text
  | SelectValue Text
  | SelectLabel Text
  | SelectIndex Int
  deriving stock (Eq, Show)

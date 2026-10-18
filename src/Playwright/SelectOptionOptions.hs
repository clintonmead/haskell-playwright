module Playwright.SelectOptionOptions
  ( SelectOptionOptions (..)
  , defaultSelectOptionOptions
  ) where

import Playwright.Timeout
  ( Timeout
  )

data SelectOptionOptions = SelectOptionOptions
  { selectOptionOptionsForce :: Maybe Bool
  , selectOptionOptionsTimeout :: Maybe Timeout
  }
  deriving stock (Eq, Show)

defaultSelectOptionOptions :: SelectOptionOptions
defaultSelectOptionOptions =
  SelectOptionOptions
    { selectOptionOptionsForce = Nothing
    , selectOptionOptionsTimeout = Nothing
    }

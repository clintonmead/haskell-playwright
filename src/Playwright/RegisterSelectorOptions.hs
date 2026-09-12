module Playwright.RegisterSelectorOptions
  ( RegisterSelectorOptions (..)
  , defaultRegisterSelectorOptions
  ) where

data RegisterSelectorOptions = RegisterSelectorOptions
  { registerSelectorContentScript :: Maybe Bool
  }
  deriving stock (Eq, Show)

defaultRegisterSelectorOptions :: RegisterSelectorOptions
defaultRegisterSelectorOptions =
  RegisterSelectorOptions
    { registerSelectorContentScript = Nothing
    }

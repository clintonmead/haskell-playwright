module Playwright.ColorScheme
  ( ColorScheme (..)
  ) where

data ColorScheme
  = ColorSchemeDark
  | ColorSchemeLight
  | ColorSchemeNoPreference
  | ColorSchemeNoOverride
  deriving stock (Eq, Ord, Show)

module Playwright.BrowserName
  ( BrowserName (..)
  ) where

data BrowserName
  = Chromium
  | Firefox
  | WebKit
  deriving stock (Eq, Ord, Show)

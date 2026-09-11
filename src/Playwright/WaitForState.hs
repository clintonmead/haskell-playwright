module Playwright.WaitForState
  ( WaitForState (..)
  ) where

data WaitForState
  = WaitForAttached
  | WaitForDetached
  | WaitForVisible
  | WaitForHidden
  deriving stock (Eq, Show)

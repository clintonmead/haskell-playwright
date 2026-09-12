module Playwright.ElementState
  ( ElementState (..)
  ) where

data ElementState
  = ElementVisible
  | ElementHidden
  | ElementStable
  | ElementEnabled
  | ElementDisabled
  | ElementEditable
  deriving stock (Eq, Show)

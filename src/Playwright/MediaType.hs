module Playwright.MediaType
  ( MediaType (..)
  ) where

data MediaType
  = MediaScreen
  | MediaPrint
  | MediaNoOverride
  deriving stock (Eq, Ord, Show)

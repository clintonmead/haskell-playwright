module Playwright.StorageKind
  ( StorageKind (..)
  ) where

data StorageKind
  = StorageLocal
  | StorageSession
  deriving stock (Eq, Ord, Show)

module Playwright.WaitId
  ( WaitId (..)
  ) where

newtype WaitId = WaitId Int
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

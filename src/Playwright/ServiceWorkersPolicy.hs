module Playwright.ServiceWorkersPolicy
  ( ServiceWorkersPolicy (..)
  ) where

data ServiceWorkersPolicy
  = ServiceWorkersAllow
  | ServiceWorkersBlock
  deriving stock (Eq, Show)

module Playwright.Exactness
  ( Exactness (..)
  ) where

data Exactness
  = Inexact
  | Exact
  deriving stock (Eq, Show)

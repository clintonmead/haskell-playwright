module Playwright.LoadState
  ( LoadState (..)
  ) where

data LoadState
  = LoadStateLoad
  | LoadStateDomContentLoaded
  | LoadStateNetworkIdle
  | LoadStateCommit
  deriving stock (Eq, Ord, Show)

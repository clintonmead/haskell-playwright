module Playwright.MouseButton
  ( MouseButton (..)
  ) where

data MouseButton
  = MouseButtonLeft
  | MouseButtonRight
  | MouseButtonMiddle
  deriving stock (Eq, Show)

module Playwright.KeyboardModifier
  ( KeyboardModifier (..)
  ) where

data KeyboardModifier
  = KeyboardModifierAlt
  | KeyboardModifierControl
  | KeyboardModifierControlOrMeta
  | KeyboardModifierMeta
  | KeyboardModifierShift
  deriving stock (Eq, Ord, Show)

module Playwright.DialogType
  ( DialogType (..)
  , dialogTypeFromText
  ) where

import Data.Text
  ( Text
  )

data DialogType
  = DialogAlert
  | DialogBeforeUnload
  | DialogConfirm
  | DialogPrompt
  deriving stock (Eq, Show)

dialogTypeFromText :: Text -> Maybe DialogType
dialogTypeFromText text =
  case text of
    "alert" ->
      Just DialogAlert
    "beforeunload" ->
      Just DialogBeforeUnload
    "confirm" ->
      Just DialogConfirm
    "prompt" ->
      Just DialogPrompt
    _ ->
      Nothing

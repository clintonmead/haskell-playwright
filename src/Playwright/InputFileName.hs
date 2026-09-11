module Playwright.InputFileName
  ( InputFileName (..)
  ) where

import Data.Text
  ( Text
  )

newtype InputFileName = InputFileName
  { inputFileNameToText :: Text
  }
  deriving stock (Eq, Show)

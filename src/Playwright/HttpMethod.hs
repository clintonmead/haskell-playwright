module Playwright.HttpMethod
  ( HttpMethod (..)
  ) where

import Data.Text
  ( Text
  )

newtype HttpMethod = HttpMethod
  { httpMethodToText :: Text
  }
  deriving stock (Eq, Show)

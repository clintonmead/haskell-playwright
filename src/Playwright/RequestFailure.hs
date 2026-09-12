module Playwright.RequestFailure
  ( RequestFailure (..)
  ) where

import Data.Text
  ( Text
  )

newtype RequestFailure = RequestFailure
  { requestFailureText :: Text
  }
  deriving stock (Eq, Show)

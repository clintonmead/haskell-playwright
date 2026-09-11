module Playwright.ContinueOptions
  ( ContinueOptions (..)
  , defaultContinueOptions
  ) where

import Playwright.HttpHeader
  ( HttpHeader
  )

data ContinueOptions = ContinueOptions
  { continueHeaders :: Maybe [HttpHeader]
  }
  deriving stock (Eq, Show)

defaultContinueOptions :: ContinueOptions
defaultContinueOptions =
  ContinueOptions
    { continueHeaders = Nothing
    }

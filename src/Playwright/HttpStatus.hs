module Playwright.HttpStatus
  ( HttpStatus (..)
  , httpStatusOk
  ) where

newtype HttpStatus = HttpStatus
  { httpStatusToInt :: Int
  }
  deriving stock (Eq, Ord, Show)

httpStatusOk :: HttpStatus -> Bool
httpStatusOk (HttpStatus status) =
  status >= 200 && status < 300

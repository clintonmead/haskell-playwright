module Playwright.DomEventType
  ( DomEventType (..)
  ) where

import Data.Text
  ( Text
  )

newtype DomEventType = DomEventType
  { domEventTypeToText :: Text
  }
  deriving stock (Eq, Show)

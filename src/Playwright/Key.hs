module Playwright.Key
  ( Key (..)
  ) where

import Autodocodec
  ( HasCodec
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Data.Text
  ( Text
  )

newtype Key = Key
  { keyToText :: Text
  }
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON, HasCodec)

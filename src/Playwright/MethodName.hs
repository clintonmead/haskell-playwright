module Playwright.MethodName
  ( MethodName (..)
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

newtype MethodName = MethodName
  { methodNameText :: Text
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, HasCodec)

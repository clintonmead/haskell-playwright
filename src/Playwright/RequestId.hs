module Playwright.RequestId
  ( RequestId (..)
  ) where

import Autodocodec
  ( HasCodec
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )

newtype RequestId = RequestId Int
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, ToJSON, FromJSON, HasCodec)

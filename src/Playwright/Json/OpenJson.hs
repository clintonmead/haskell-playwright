module Playwright.Json.OpenJson
  ( OpenJson (..)
  ) where

import Autodocodec
  ( HasCodec
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  , Value
  )

-- | Protocol `json` fields: intentionally open JSON, not a place to stuff
-- structured domain values.
newtype OpenJson = OpenJson
  { openJsonValue :: Value
  }
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON, HasCodec)

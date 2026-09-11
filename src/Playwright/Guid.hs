module Playwright.Guid
  ( Guid
  , guidFromText
  , guidToText
  , rootGuid
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

newtype Guid = Guid Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, HasCodec)

guidFromText :: Text -> Guid
guidFromText = Guid

guidToText :: Guid -> Text
guidToText (Guid text) = text

rootGuid :: Guid
rootGuid = Guid ""

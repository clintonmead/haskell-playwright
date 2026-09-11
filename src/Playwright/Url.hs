module Playwright.Url
  ( Url
  , urlFromText
  , urlToText
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

newtype Url = Url Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, HasCodec)

urlFromText :: Text -> Url
urlFromText = Url

urlToText :: Url -> Text
urlToText (Url text) = text

module Playwright.SerializedError
  ( SerializedError (..)
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , object
  , optionalField'
  , (.=)
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Playwright.SerializedErrorInfo
  ( SerializedErrorInfo
  )
import Playwright.SerializedValue
  ( SerializedValue
  )

data SerializedError = SerializedError
  { serializedErrorError :: Maybe SerializedErrorInfo
  , serializedErrorValue :: Maybe SerializedValue
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec SerializedError)

instance HasCodec SerializedError where
  codec =
    object "SerializedError" $
      SerializedError
        <$> optionalField' "error" .= serializedErrorError
        <*> optionalField' "value" .= serializedErrorValue

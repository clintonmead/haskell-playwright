module Playwright.SerializedErrorInfo
  ( SerializedErrorInfo (..)
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , object
  , optionalField'
  , requiredField'
  , (.=)
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Data.Text
  ( Text
  )

data SerializedErrorInfo = SerializedErrorInfo
  { serializedErrorInfoMessage :: Text
  , serializedErrorInfoName :: Text
  , serializedErrorInfoStack :: Maybe Text
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec SerializedErrorInfo)

instance HasCodec SerializedErrorInfo where
  codec =
    object "SerializedErrorInfo" $
      SerializedErrorInfo
        <$> requiredField' "message" .= serializedErrorInfoMessage
        <*> requiredField' "name" .= serializedErrorInfoName
        <*> optionalField' "stack" .= serializedErrorInfoStack

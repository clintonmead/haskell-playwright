module Playwright.JsErrorValue
  ( JsErrorValue (..)
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , object
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

data JsErrorValue = JsErrorValue
  { jsErrorMessage :: Text
  , jsErrorName :: Text
  , jsErrorStack :: Text
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec JsErrorValue)

instance HasCodec JsErrorValue where
  codec =
    object "JsErrorValue" $
      JsErrorValue
        <$> requiredField' "m" .= jsErrorMessage
        <*> requiredField' "n" .= jsErrorName
        <*> requiredField' "s" .= jsErrorStack

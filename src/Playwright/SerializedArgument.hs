module Playwright.SerializedArgument
  ( SerializedArgument (..)
  , textArgument
  , undefinedArgument
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
import Playwright.Json.ChannelRef
  ( ChannelRef
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadString)
  , SerializedValue (SerializedNode)
  , undefinedValue
  )

data SerializedArgument = SerializedArgument
  { serializedArgumentValue :: SerializedValue
  , serializedArgumentHandles :: [ChannelRef]
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec SerializedArgument)

undefinedArgument :: SerializedArgument
undefinedArgument =
  SerializedArgument
    { serializedArgumentValue = undefinedValue
    , serializedArgumentHandles = []
    }

textArgument :: Text -> SerializedArgument
textArgument text =
  SerializedArgument
    { serializedArgumentValue = SerializedNode (PayloadString text) Nothing
    , serializedArgumentHandles = []
    }

instance HasCodec SerializedArgument where
  codec =
    object "SerializedArgument" $
      SerializedArgument
        <$> requiredField' "value" .= serializedArgumentValue
        <*> requiredField' "handles" .= serializedArgumentHandles

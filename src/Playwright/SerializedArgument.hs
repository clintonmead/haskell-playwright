module Playwright.SerializedArgument
  ( SerializedArgument (..)
  , boolArgument
  , numberArgument
  , channelArgument
  , numbersArgument
  , objectNumberArgument
  , specialArgument
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
  ( SerializedPayload (PayloadArray, PayloadBool, PayloadHandle, PayloadNumber, PayloadObject, PayloadSpecial, PayloadString)
  , SerializedValue (SerializedNode)
  , undefinedValue
  )
import Playwright.SpecialJsValue
  ( SpecialJsValue
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

boolArgument :: Bool -> SerializedArgument
boolArgument flag =
  SerializedArgument
    { serializedArgumentValue = SerializedNode (PayloadBool flag) Nothing
    , serializedArgumentHandles = []
    }

numberArgument :: Double -> SerializedArgument
numberArgument number =
  SerializedArgument
    { serializedArgumentValue = SerializedNode (PayloadNumber number) Nothing
    , serializedArgumentHandles = []
    }

numbersArgument :: [Double] -> SerializedArgument
numbersArgument numbers =
  SerializedArgument
    { serializedArgumentValue =
        SerializedNode
          (PayloadArray (fmap (\number -> SerializedNode (PayloadNumber number) Nothing) numbers))
          Nothing
    , serializedArgumentHandles = []
    }

specialArgument :: SpecialJsValue -> SerializedArgument
specialArgument special =
  SerializedArgument
    { serializedArgumentValue = SerializedNode (PayloadSpecial special) Nothing
    , serializedArgumentHandles = []
    }

channelArgument :: ChannelRef -> SerializedArgument
channelArgument ref =
  SerializedArgument
    { serializedArgumentValue = SerializedNode (PayloadHandle 0) Nothing
    , serializedArgumentHandles = [ref]
    }

objectNumberArgument :: [(Text, Double)] -> SerializedArgument
objectNumberArgument fields =
  SerializedArgument
    { serializedArgumentValue =
        SerializedNode
          ( PayloadObject
              ( fmap
                  ( \(name, number) ->
                      (name, SerializedNode (PayloadNumber number) Nothing)
                  )
                  fields
              )
          )
          Nothing
    , serializedArgumentHandles = []
    }

instance HasCodec SerializedArgument where
  codec =
    object "SerializedArgument" $
      SerializedArgument
        <$> requiredField' "value" .= serializedArgumentValue
        <*> requiredField' "handles" .= serializedArgumentHandles

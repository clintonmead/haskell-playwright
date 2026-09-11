module Playwright.Json.ProtocolBinary
  ( ProtocolBinary
  , protocolBinaryFromByteString
  , protocolBinaryToByteString
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , bimapCodec
  , named
  , textCodec
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString.Base64
  ( decode
  , encode
  )
import Data.Text
  ( Text
  )
import Data.Text.Encoding
  ( decodeLatin1
  , encodeUtf8
  )

newtype ProtocolBinary = ProtocolBinary ByteString
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec ProtocolBinary)

protocolBinaryFromByteString :: ByteString -> ProtocolBinary
protocolBinaryFromByteString = ProtocolBinary

protocolBinaryToByteString :: ProtocolBinary -> ByteString
protocolBinaryToByteString (ProtocolBinary bytes) = bytes

instance HasCodec ProtocolBinary where
  codec =
    named "ProtocolBinary" $
      bimapCodec decodeBinary encodeBinary textCodec

encodeBinary :: ProtocolBinary -> Text
encodeBinary (ProtocolBinary bytes) =
  decodeLatin1 (encode bytes)

decodeBinary :: Text -> Either String ProtocolBinary
decodeBinary text =
  case decode (encodeUtf8 text) of
    Left err ->
      Left err
    Right bytes ->
      Right (ProtocolBinary bytes)

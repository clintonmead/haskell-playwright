module Playwright.Incoming
  ( Incoming (..)
  , CreateParams (..)
  , parseIncoming
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
  , Value (Object)
  , eitherDecodeStrict'
  )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString
  ( ByteString
  )
import Data.Maybe
  ( fromMaybe
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  , ProtocolFailure
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (ChannelRef)
  )
import Playwright.Json.Decode
  ( fromJsonValue
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( ChannelType
  )
import Playwright.RequestId
  ( RequestId
  )

data CreateParams = CreateParams
  { createType :: ChannelType
  , createGuid :: Guid
  , createInitializer :: Value
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec CreateParams)

instance HasCodec CreateParams where
  codec =
    object "CreateParams" $
      CreateParams
        <$> requiredField' "type" .= createType
        <*> requiredField' "guid" .= createGuid
        <*> requiredField' "initializer" .= createInitializer

data Incoming
  = IncomingResponse RequestId (Either ProtocolFailure Value)
  | IncomingCreate Guid CreateParams
  | IncomingAdopt Guid Guid
  | IncomingDispose Guid
  | IncomingEvent Guid MethodName Value
  deriving stock (Eq, Show)

data WireMessage = WireMessage
  { wireId :: Maybe RequestId
  , wireGuid :: Maybe Guid
  , wireMethod :: Maybe Text
  , wireParams :: Maybe Value
  , wireResult :: Maybe Value
  , wireError :: Maybe Value
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec WireMessage)

instance HasCodec WireMessage where
  codec =
    object "WireMessage" $
      WireMessage
        <$> optionalField' "id" .= wireId
        <*> optionalField' "guid" .= wireGuid
        <*> optionalField' "method" .= wireMethod
        <*> optionalField' "params" .= wireParams
        <*> optionalField' "result" .= wireResult
        <*> optionalField' "error" .= wireError

parseIncoming :: ByteString -> Either PlaywrightError Incoming
parseIncoming bytes =
  case eitherDecodeStrict' bytes of
    Left err ->
      Left (DecodeFailure (Text.pack err))
    Right wire ->
      interpretWire wire

interpretWire :: WireMessage -> Either PlaywrightError Incoming
interpretWire wire =
  case wireId wire of
    Just reqId ->
      case wireError wire of
        Just errValue ->
          IncomingResponse reqId . Left <$> parseFailure errValue
        Nothing ->
          Right (IncomingResponse reqId (Right (fromMaybe emptyObject (wireResult wire))))
    Nothing ->
      interpretEvent wire

interpretEvent :: WireMessage -> Either PlaywrightError Incoming
interpretEvent wire =
  case (wireGuid wire, wireMethod wire) of
    (Just guid, Just "__create__") ->
      case wireParams wire of
        Nothing ->
          Left (DecodeFailure "create missing params")
        Just params ->
          IncomingCreate guid <$> fromJsonValue params
    (Just guid, Just "__adopt__") ->
      IncomingAdopt guid <$> parseAdoptGuid (wireParams wire)
    (Just guid, Just "__dispose__") ->
      Right (IncomingDispose guid)
    (Just guid, Just method) ->
      Right (IncomingEvent guid (MethodName method) (fromMaybe emptyObject (wireParams wire)))
    _ ->
      Left (DecodeFailure "event missing guid or method")

parseAdoptGuid :: Maybe Value -> Either PlaywrightError Guid
parseAdoptGuid params =
  case params of
    Nothing ->
      Left (DecodeFailure "adopt missing params")
    Just value -> do
      ChannelRef guid <- fromJsonValue value
      Right guid

parseFailure :: Value -> Either PlaywrightError ProtocolFailure
parseFailure value =
  case fromJsonValue value of
    Right failure ->
      Right failure
    Left _ ->
      case nestedError value of
        Just inner ->
          fromJsonValue inner
        Nothing ->
          Left (DecodeFailure "invalid protocol error")

nestedError :: Value -> Maybe Value
nestedError value =
  case value of
    Object obj ->
      KeyMap.lookup "error" obj
    _ ->
      Nothing

emptyObject :: Value
emptyObject = Object KeyMap.empty

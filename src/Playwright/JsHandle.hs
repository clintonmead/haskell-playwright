module Playwright.JsHandle
  ( JsHandle (..)
  , asElement
  , jsHandleArgument
  , jsHandleFromRef
  , jsonValue
  , jsonValueNumber
  , jsonValueText
  ) where

import Control.Exception
  ( throwIO
  )
import Data.Text
  ( Text
  )
import Playwright.Channel
  ( Channel (Channel, channelConnection, channelGuid)
  , JsHandleTag
  , send
  )
import Playwright.Connection
  ( Connection
  , lookupObject
  )
import Playwright.ElementHandle
  ( ElementHandle (ElementHandle)
  )
import Playwright.Error
  ( PlaywrightError (EvaluateUnexpected)
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (ChannelRef, channelRefGuid)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( ChannelType (ChannelTypeElementHandle)
  , JSHandleJsonValueResult (jSHandleJsonValueResultValue)
  , mkJSHandleJsonValueParams
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteType)
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  , channelArgument
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadNumber, PayloadString)
  , SerializedValue (SerializedNode)
  )

newtype JsHandle = JsHandle
  { jsHandleChannel :: Channel JsHandleTag
  }

jsHandleFromRef :: Connection -> ChannelRef -> JsHandle
jsHandleFromRef conn ref =
  JsHandle (Channel conn (channelRefGuid ref))

jsHandleArgument :: JsHandle -> SerializedArgument
jsHandleArgument handle =
  channelArgument (ChannelRef (channelGuid (jsHandleChannel handle)))

asElement :: JsHandle -> IO (Maybe ElementHandle)
asElement handle = do
  obj <-
    lookupObject
      (channelConnection (jsHandleChannel handle))
      (channelGuid (jsHandleChannel handle))
  case remoteType obj of
    ChannelTypeElementHandle ->
      pure (Just (ElementHandle (Channel (channelConnection (jsHandleChannel handle)) (channelGuid (jsHandleChannel handle)))))
    _ ->
      pure Nothing

jsonValue :: JsHandle -> IO SerializedValue
jsonValue handle = do
  result <-
    send
      (jsHandleChannel handle)
      (MethodName "jsonValue")
      mkJSHandleJsonValueParams
  pure (jSHandleJsonValueResultValue result)

jsonValueNumber :: JsHandle -> IO Double
jsonValueNumber handle = do
  value <- jsonValue handle
  case value of
    SerializedNode (PayloadNumber number) _ ->
      pure number
    _ ->
      throwIO (EvaluateUnexpected value)

jsonValueText :: JsHandle -> IO Text
jsonValueText handle = do
  value <- jsonValue handle
  case value of
    SerializedNode (PayloadString text) _ ->
      pure text
    _ ->
      throwIO (EvaluateUnexpected value)

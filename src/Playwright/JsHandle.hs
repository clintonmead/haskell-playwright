module Playwright.JsHandle
  ( JsHandle (..)
  , jsHandleFromRef
  , jsonValue
  , jsonValueNumber
  ) where

import Control.Exception
  ( throwIO
  )
import Playwright.Channel
  ( Channel (Channel)
  , JsHandleTag
  , send
  )
import Playwright.Connection
  ( Connection
  )
import Playwright.Error
  ( PlaywrightError (EvaluateUnexpected)
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( JSHandleJsonValueResult (jSHandleJsonValueResultValue)
  , mkJSHandleJsonValueParams
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadNumber)
  , SerializedValue (SerializedNode)
  )

newtype JsHandle = JsHandle
  { jsHandleChannel :: Channel JsHandleTag
  }

jsHandleFromRef :: Connection -> ChannelRef -> JsHandle
jsHandleFromRef conn ref =
  JsHandle (Channel conn (channelRefGuid ref))

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

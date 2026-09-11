module Playwright.JsHandle
  ( JsHandle (..)
  , asElement
  , getProperty
  , getPropertyList
  , jsHandleArgument
  , jsHandleFromRef
  , jsonValue
  , jsonValueNumber
  , jsonValueSpecial
  , jsonValueText
  , jsonValueTexts
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
  , JSHandleGetPropertyListResult (jSHandleGetPropertyListResultProperties)
  , JSHandleGetPropertyListResultProperties (jSHandleGetPropertyListResultPropertiesName, jSHandleGetPropertyListResultPropertiesValue)
  , JSHandleGetPropertyResult (jSHandleGetPropertyResultHandle)
  , JSHandleJsonValueResult (jSHandleJsonValueResultValue)
  , mkJSHandleGetPropertyListParams
  , mkJSHandleGetPropertyParams
  , mkJSHandleJsonValueParams
  )
import Playwright.PropertyName
  ( PropertyName (PropertyName, propertyNameToText)
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteType)
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  , channelArgument
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadArray, PayloadNumber, PayloadSpecial, PayloadString)
  , SerializedValue (SerializedNode)
  )
import Playwright.SpecialJsValue
  ( SpecialJsValue
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

jsonValueTexts :: JsHandle -> IO [Text]
jsonValueTexts handle = do
  value <- jsonValue handle
  case value of
    SerializedNode (PayloadArray items) _ ->
      traverse requireJsonText items
    _ ->
      throwIO (EvaluateUnexpected value)

requireJsonText :: SerializedValue -> IO Text
requireJsonText value =
  case value of
    SerializedNode (PayloadString text) _ ->
      pure text
    _ ->
      throwIO (EvaluateUnexpected value)

jsonValueSpecial :: JsHandle -> IO SpecialJsValue
jsonValueSpecial handle = do
  value <- jsonValue handle
  case value of
    SerializedNode (PayloadSpecial special) _ ->
      pure special
    _ ->
      throwIO (EvaluateUnexpected value)

getProperty :: JsHandle -> PropertyName -> IO JsHandle
getProperty handle name = do
  result <-
    send
      (jsHandleChannel handle)
      (MethodName "getProperty")
      (mkJSHandleGetPropertyParams (propertyNameToText name))
  pure
    ( jsHandleFromRef
        (channelConnection (jsHandleChannel handle))
        (jSHandleGetPropertyResultHandle result)
    )

getPropertyList :: JsHandle -> IO [(PropertyName, JsHandle)]
getPropertyList handle = do
  result <-
    send
      (jsHandleChannel handle)
      (MethodName "getPropertyList")
      mkJSHandleGetPropertyListParams
  let conn = channelConnection (jsHandleChannel handle)
  pure
    ( fmap
        ( \prop ->
            ( PropertyName (jSHandleGetPropertyListResultPropertiesName prop)
            , jsHandleFromRef conn (jSHandleGetPropertyListResultPropertiesValue prop)
            )
        )
        (jSHandleGetPropertyListResultProperties result)
    )

module Playwright.JsHandle
  ( JsHandle (..)
  , asElement
  , getProperty
  , getPropertyList
  , jsHandleArgument
  , jsHandleFromRef
  , jsHandleToString
  , jsonValue
  , jsonValueNumber
  , jsonValueSpecial
  , jsonValueText
  , jsonValueTexts
  , waitUntilJsHandlePreview
  ) where

import Playwright.Monad
  ( Playwright
  )
import Control.Concurrent.STM
  ( retry
  , throwSTM
  )
import UnliftIO.Exception
  ( throwIO
  )
import Control.Monad
  ( unless
  )
import Data.Text
  ( Text
  )
import Playwright.Channel
  ( Channel (Channel, channelGuid)
  , JsHandleTag
  , send
  )
import Playwright.Connection
  ( lookupObject
  , waitUntilRemote
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
import Playwright.Json.Decode
  ( fromJsonValue
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( ChannelType (ChannelTypeElementHandle)
  , JSHandleInitializer (jSHandleInitializerPreview)
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
  ( RemoteObject (remoteInitializer, remoteType)
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

jsHandleFromRef :: ChannelRef -> JsHandle
jsHandleFromRef ref =
  JsHandle (Channel (channelRefGuid ref))

jsHandleArgument :: JsHandle -> SerializedArgument
jsHandleArgument handle =
  channelArgument (ChannelRef (channelGuid (jsHandleChannel handle)))

jsHandleToString :: JsHandle -> Playwright Text
jsHandleToString handle = do
  obj <-
    lookupObject
      (channelGuid (jsHandleChannel handle))
  initializer <- either throwIO pure (fromJsonValue (remoteInitializer obj))
  pure (jSHandleInitializerPreview initializer)

waitUntilJsHandlePreview :: JsHandle -> (Text -> Bool) -> Playwright Text
waitUntilJsHandlePreview handle predicate =
  waitUntilRemote
    (channelGuid (jsHandleChannel handle))
    $ \obj -> do
      initializer <- either throwSTM pure (fromJsonValue (remoteInitializer obj))
      let
        preview = jSHandleInitializerPreview initializer
      unless (predicate preview) retry
      pure preview

asElement :: JsHandle -> Playwright (Maybe ElementHandle)
asElement handle = do
  obj <-
    lookupObject
      (channelGuid (jsHandleChannel handle))
  case remoteType obj of
    ChannelTypeElementHandle ->
      pure (Just (ElementHandle (Channel (channelGuid (jsHandleChannel handle)))))
    _ ->
      pure Nothing

jsonValue :: JsHandle -> Playwright SerializedValue
jsonValue handle = do
  result <-
    send
      (jsHandleChannel handle)
      (MethodName "jsonValue")
      mkJSHandleJsonValueParams
  pure (jSHandleJsonValueResultValue result)

jsonValueNumber :: JsHandle -> Playwright Double
jsonValueNumber handle = do
  value <- jsonValue handle
  case value of
    SerializedNode (PayloadNumber number) _ ->
      pure number
    _ ->
      throwIO (EvaluateUnexpected value)

jsonValueText :: JsHandle -> Playwright Text
jsonValueText handle = do
  value <- jsonValue handle
  case value of
    SerializedNode (PayloadString text) _ ->
      pure text
    _ ->
      throwIO (EvaluateUnexpected value)

jsonValueTexts :: JsHandle -> Playwright [Text]
jsonValueTexts handle = do
  value <- jsonValue handle
  case value of
    SerializedNode (PayloadArray items) _ ->
      traverse requireJsonText items
    _ ->
      throwIO (EvaluateUnexpected value)

requireJsonText :: SerializedValue -> Playwright Text
requireJsonText value =
  case value of
    SerializedNode (PayloadString text) _ ->
      pure text
    _ ->
      throwIO (EvaluateUnexpected value)

jsonValueSpecial :: JsHandle -> Playwright SpecialJsValue
jsonValueSpecial handle = do
  value <- jsonValue handle
  case value of
    SerializedNode (PayloadSpecial special) _ ->
      pure special
    _ ->
      throwIO (EvaluateUnexpected value)

getProperty :: JsHandle -> PropertyName -> Playwright JsHandle
getProperty handle name = do
  result <-
    send
      (jsHandleChannel handle)
      (MethodName "getProperty")
      (mkJSHandleGetPropertyParams (propertyNameToText name))
  pure
    ( jsHandleFromRef
        (jSHandleGetPropertyResultHandle result)
    )

getPropertyList :: JsHandle -> Playwright [(PropertyName, JsHandle)]
getPropertyList handle = do
  result <-
    send
      (jsHandleChannel handle)
      (MethodName "getPropertyList")
      mkJSHandleGetPropertyListParams
  pure
    ( fmap
        ( \prop ->
            ( PropertyName (jSHandleGetPropertyListResultPropertiesName prop)
            , jsHandleFromRef (jSHandleGetPropertyListResultPropertiesValue prop)
            )
        )
        (jSHandleGetPropertyListResultProperties result)
    )

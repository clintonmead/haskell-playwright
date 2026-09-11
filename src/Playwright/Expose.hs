module Playwright.Expose
  ( exposeFunction
  ) where

import Control.Exception
  ( SomeException
  , displayException
  , throwIO
  , try
  )
import Data.Aeson
  ( Value
  )
import Data.Text
  ( pack
  )
import Playwright.BindingName
  ( BindingName (bindingNameToText)
  )
import Playwright.Channel
  ( BindingCallTag
  , Channel (Channel, channelConnection, channelGuid)
  , send
  )
import Playwright.Connection
  ( lookupObject
  , registerEventHandler
  )
import Playwright.Disposable
  ( Disposable
  , disposableFromRef
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.Json.Decode
  ( fromJsonValue
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Page
  ( Page (pageChannel)
  )
import Playwright.Protocol
  ( BindingCallInitializer (bindingCallInitializerArgs, bindingCallInitializerName)
  , BindingCallRejectResult
  , BindingCallResolveResult
  , PageBindingCallEvent (pageBindingCallEventBinding)
  , PageExposeBindingResult (pageExposeBindingResultDisposable)
  , mkBindingCallRejectParams
  , mkBindingCallResolveParams
  , mkPageExposeBindingParams
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  )
import Playwright.SerializedError
  ( SerializedError (SerializedError, serializedErrorError, serializedErrorValue)
  )
import Playwright.SerializedErrorInfo
  ( SerializedErrorInfo (SerializedErrorInfo, serializedErrorInfoMessage, serializedErrorInfoName, serializedErrorInfoStack)
  )
import Playwright.SerializedValue
  ( SerializedValue
  )

exposeFunction :: Page -> BindingName -> ([SerializedValue] -> IO SerializedArgument) -> IO Disposable
exposeFunction page name handler = do
  let
    conn = channelConnection (pageChannel page)
    pageGuid = channelGuid (pageChannel page)
  registerEventHandler conn pageGuid (MethodName "bindingCall") $ \params ->
    handleBindingCall page name handler params
  result <-
    send
      (pageChannel page)
      (MethodName "exposeBinding")
      (mkPageExposeBindingParams (bindingNameToText name))
  pure (disposableFromRef conn (pageExposeBindingResultDisposable result))

handleBindingCall ::
  Page ->
  BindingName ->
  ([SerializedValue] -> IO SerializedArgument) ->
  Value ->
  IO ()
handleBindingCall page name handler params = do
  event <- either throwIO pure (fromJsonValue params)
  let
    conn = channelConnection (pageChannel page)
    bindingRef = pageBindingCallEventBinding event
    call = Channel conn (channelRefGuid bindingRef) :: Channel BindingCallTag
  obj <- lookupObject conn (channelRefGuid bindingRef)
  initializer <- either throwIO pure (fromJsonValue (remoteInitializer obj))
  if bindingCallInitializerName initializer == bindingNameToText name
    then do
      outcome <- try (handler (bindingCallInitializerArgs initializer))
      case outcome of
        Left (err :: SomeException) ->
          rejectBinding call err
        Right argument -> do
          _ :: BindingCallResolveResult <-
            send call (MethodName "resolve") (mkBindingCallResolveParams argument)
          pure ()
    else
      pure ()

rejectBinding :: Channel BindingCallTag -> SomeException -> IO ()
rejectBinding call err = do
  let
    info =
      SerializedErrorInfo
        { serializedErrorInfoMessage = pack (displayException err)
        , serializedErrorInfoName = "Error"
        , serializedErrorInfoStack = Nothing
        }
    payload =
      SerializedError
        { serializedErrorError = Just info
        , serializedErrorValue = Nothing
        }
  _ :: BindingCallRejectResult <-
    send call (MethodName "reject") (mkBindingCallRejectParams payload)
  pure ()

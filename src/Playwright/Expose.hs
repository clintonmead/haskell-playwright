module Playwright.Expose
  ( contextExposeFunction
  , exposeFunction
  ) where

import Data.Aeson
  ( Value
  )
import Data.Text
  ( pack
  )
import Playwright.BindingName
  ( BindingName (bindingNameToText)
  )
import Playwright.BrowserContext
  ( BrowserContext (browserContextChannel)
  )
import Playwright.Channel
  ( BindingCallTag
  , Channel (Channel, channelGuid)
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
  ( throwDecode
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.Page
  ( Page (pageChannel)
  )
import Playwright.Protocol
  ( BindingCallInitializer (bindingCallInitializerArgs, bindingCallInitializerName)
  , BindingCallRejectResult
  , BindingCallResolveResult
  , BrowserContextBindingCallEvent (browserContextBindingCallEventBinding)
  , BrowserContextExposeBindingResult (browserContextExposeBindingResultDisposable)
  , PageBindingCallEvent (pageBindingCallEventBinding)
  , PageExposeBindingResult (pageExposeBindingResultDisposable)
  , mkBindingCallRejectParams
  , mkBindingCallResolveParams
  , mkBrowserContextExposeBindingParams
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
import UnliftIO.Exception
  ( SomeException
  , displayException
  , try
  )

exposeFunction :: Page -> BindingName -> ([SerializedValue] -> Playwright SerializedArgument) -> Playwright Disposable
exposeFunction page name handler = do
  let
    guid = channelGuid (pageChannel page)
  registerEventHandler guid (MethodName "bindingCall") $ \params ->
    handlePageBindingCall name handler params
  result <-
    send
      (pageChannel page)
      (MethodName "exposeBinding")
      (mkPageExposeBindingParams (bindingNameToText name))
  pure (disposableFromRef (pageExposeBindingResultDisposable result))

contextExposeFunction :: BrowserContext -> BindingName -> ([SerializedValue] -> Playwright SerializedArgument) -> Playwright Disposable
contextExposeFunction ctx name handler = do
  let
    guid = channelGuid (browserContextChannel ctx)
  registerEventHandler guid (MethodName "bindingCall") $ \params ->
    handleContextBindingCall name handler params
  result <-
    send
      (browserContextChannel ctx)
      (MethodName "exposeBinding")
      (mkBrowserContextExposeBindingParams (bindingNameToText name))
  pure (disposableFromRef (browserContextExposeBindingResultDisposable result))

handlePageBindingCall ::
  BindingName ->
  ([SerializedValue] -> Playwright SerializedArgument) ->
  Value ->
  Playwright ()
handlePageBindingCall name handler params = do
  event <- throwDecode params
  handleNamedBinding name handler (pageBindingCallEventBinding event)

handleContextBindingCall ::
  BindingName ->
  ([SerializedValue] -> Playwright SerializedArgument) ->
  Value ->
  Playwright ()
handleContextBindingCall name handler params = do
  event <- throwDecode params
  handleNamedBinding name handler (browserContextBindingCallEventBinding event)

handleNamedBinding ::
  BindingName ->
  ([SerializedValue] -> Playwright SerializedArgument) ->
  ChannelRef ->
  Playwright ()
handleNamedBinding name handler bindingRef = do
  let
    call = Channel (channelRefGuid bindingRef) :: Channel BindingCallTag
  obj <- lookupObject (channelRefGuid bindingRef)
  initializer <- throwDecode (remoteInitializer obj)
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

rejectBinding :: Channel BindingCallTag -> SomeException -> Playwright ()
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

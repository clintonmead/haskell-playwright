module Playwright.Expose
  ( contextExposeFunction
  , exposeFunction
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
import Playwright.BrowserContext
  ( BrowserContext (browserContextChannel)
  )
import Playwright.Channel
  ( BindingCallTag
  , Channel (Channel, channelConnection, channelGuid)
  , send
  )
import Playwright.Connection
  ( Connection
  , lookupObject
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

exposeFunction :: Page -> BindingName -> ([SerializedValue] -> IO SerializedArgument) -> IO Disposable
exposeFunction page name handler = do
  let
    conn = channelConnection (pageChannel page)
    guid = channelGuid (pageChannel page)
  registerEventHandler conn guid (MethodName "bindingCall") $ \params ->
    handlePageBindingCall conn name handler params
  result <-
    send
      (pageChannel page)
      (MethodName "exposeBinding")
      (mkPageExposeBindingParams (bindingNameToText name))
  pure (disposableFromRef conn (pageExposeBindingResultDisposable result))

contextExposeFunction :: BrowserContext -> BindingName -> ([SerializedValue] -> IO SerializedArgument) -> IO Disposable
contextExposeFunction ctx name handler = do
  let
    conn = channelConnection (browserContextChannel ctx)
    guid = channelGuid (browserContextChannel ctx)
  registerEventHandler conn guid (MethodName "bindingCall") $ \params ->
    handleContextBindingCall conn name handler params
  result <-
    send
      (browserContextChannel ctx)
      (MethodName "exposeBinding")
      (mkBrowserContextExposeBindingParams (bindingNameToText name))
  pure (disposableFromRef conn (browserContextExposeBindingResultDisposable result))

handlePageBindingCall ::
  Connection ->
  BindingName ->
  ([SerializedValue] -> IO SerializedArgument) ->
  Value ->
  IO ()
handlePageBindingCall conn name handler params = do
  event <- either throwIO pure (fromJsonValue params)
  handleNamedBinding conn name handler (pageBindingCallEventBinding event)

handleContextBindingCall ::
  Connection ->
  BindingName ->
  ([SerializedValue] -> IO SerializedArgument) ->
  Value ->
  IO ()
handleContextBindingCall conn name handler params = do
  event <- either throwIO pure (fromJsonValue params)
  handleNamedBinding conn name handler (browserContextBindingCallEventBinding event)

handleNamedBinding ::
  Connection ->
  BindingName ->
  ([SerializedValue] -> IO SerializedArgument) ->
  ChannelRef ->
  IO ()
handleNamedBinding conn name handler bindingRef = do
  let
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

module Playwright.Expose
  ( BindingSource (..)
  , contextExposeBinding
  , contextExposeFunction
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
  ( lookupMainFramePage
  , lookupObject
  , registerEventHandler
  )
import Playwright.Disposable
  ( Disposable
  , disposableFromRef
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  )
import Playwright.Frame
  ( Frame (frameChannel, frameParent)
  , frameFromRef
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (ChannelRef, channelRefGuid)
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
  , pageFromRef
  )
import Playwright.Protocol
  ( BindingCallInitializer (bindingCallInitializerArgs, bindingCallInitializerFrame, bindingCallInitializerName)
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
  , throwIO
  , try
  )

-- | The page, frame and context a binding was called from. Mirrors the
-- @source@ argument of the JS client's @BindingSource@.
data BindingSource = BindingSource
  { bindingSourceContext :: BrowserContext
  , bindingSourcePage :: Page
  , bindingSourceFrame :: Frame
  }

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
contextExposeFunction ctx name handler =
  contextExposeBinding ctx name (\_ args -> handler args)

contextExposeBinding :: BrowserContext -> BindingName -> (BindingSource -> [SerializedValue] -> Playwright SerializedArgument) -> Playwright Disposable
contextExposeBinding ctx name handler = do
  let
    guid = channelGuid (browserContextChannel ctx)
  registerEventHandler guid (MethodName "bindingCall") $ \params ->
    handleContextBindingCall ctx name handler params
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
  handleNamedBinding name (\initializer -> handler (bindingCallInitializerArgs initializer)) (pageBindingCallEventBinding event)

handleContextBindingCall ::
  BrowserContext ->
  BindingName ->
  (BindingSource -> [SerializedValue] -> Playwright SerializedArgument) ->
  Value ->
  Playwright ()
handleContextBindingCall ctx name handler params = do
  event <- throwDecode params
  handleNamedBinding
    name
    ( \initializer -> do
        source <- bindingSourceFor ctx (bindingCallInitializerFrame initializer)
        handler source (bindingCallInitializerArgs initializer)
    )
    (browserContextBindingCallEventBinding event)

-- | Resolve the page, frame and context for a binding call from its frame ref.
-- The page is found from the root (main) frame of the frame's ancestry.
bindingSourceFor :: BrowserContext -> ChannelRef -> Playwright BindingSource
bindingSourceFor ctx frameRef = do
  frame <- frameFromRef frameRef
  rootGuid <- rootFrameGuid frame
  maybePageGuid <- lookupMainFramePage rootGuid
  case maybePageGuid of
    Nothing ->
      throwIO (DecodeFailure "exposeBinding: no page found for the binding's frame")
    Just pageGuid -> do
      page <- pageFromRef (browserContextChannel ctx) (ChannelRef pageGuid)
      pure
        BindingSource
          { bindingSourceContext = ctx
          , bindingSourcePage = page
          , bindingSourceFrame = frame
          }

rootFrameGuid :: Frame -> Playwright Guid
rootFrameGuid frame =
  case frameParent frame of
    Nothing -> pure (channelGuid (frameChannel frame))
    Just parentRef -> do
      parent <- frameFromRef parentRef
      rootFrameGuid parent

handleNamedBinding ::
  BindingName ->
  (BindingCallInitializer -> Playwright SerializedArgument) ->
  ChannelRef ->
  Playwright ()
handleNamedBinding name run bindingRef = do
  let
    call = Channel (channelRefGuid bindingRef) :: Channel BindingCallTag
  obj <- lookupObject (channelRefGuid bindingRef)
  initializer <- throwDecode (remoteInitializer obj)
  if bindingCallInitializerName initializer == bindingNameToText name
    then do
      outcome <- try (run initializer)
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

module Playwright.Channel
  ( Channel (..)
  , RootTag
  , PlaywrightTag
  , BrowserTypeTag
  , BrowserTag
  , BindingCallTag
  , BrowserContextTag
  , PageTag
  , FrameTag
  , ResponseTag
  , ElementHandleTag
  , JsHandleTag
  , DialogTag
  , RequestTag
  , WorkerTag
  , DisposableTag
  , RouteTag
  , ArtifactTag
  , StreamTag
  , APIRequestContextTag
  , WebSocketTag
  , WebSocketRouteTag
  , TracingTag
  , DebuggerTag
  , send
  , sendWithTimeout
  ) where

import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Data.Aeson qualified as Aeson
import Data.Kind
  ( Type
  )
import Playwright.Connection
  ( sendCommand
  , sendCommandWithMetadata
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.MethodName
  ( MethodName
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.Timeout
  ( Timeout
  )

data RootTag
data PlaywrightTag
data BrowserTypeTag
data BrowserTag
data BrowserContextTag
data PageTag
data FrameTag
data ResponseTag
data ElementHandleTag
data JsHandleTag
data DialogTag
data RequestTag
data WorkerTag
data BindingCallTag
data DisposableTag
data RouteTag
data ArtifactTag
data StreamTag
data APIRequestContextTag
data WebSocketTag
data WebSocketRouteTag
data TracingTag
data DebuggerTag

type Channel :: Type -> Type
newtype Channel a = Channel
  { channelGuid :: Guid
  }

send ::
  (ToJSON params, FromJSON result) =>
  Channel a ->
  MethodName ->
  params ->
  Playwright result
send channel method params =
  sendCommand (channelGuid channel) method params

-- | Sends a call whose timeout travels in the message metadata, as the
-- protocol does for calls such as goto, setContent and fetch.
sendWithTimeout ::
  (ToJSON params, FromJSON result) =>
  Channel a ->
  MethodName ->
  Timeout ->
  params ->
  Playwright result
sendWithTimeout channel method timeout params =
  sendCommandWithMetadata
    (channelGuid channel)
    method
    params
    (Aeson.object ["internal" Aeson..= False, "timeout" Aeson..= timeout])

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
  , send
  ) where

import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Data.Kind
  ( Type
  )
import Playwright.Connection
  ( Connection
  , sendCommand
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.MethodName
  ( MethodName
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

type Channel :: Type -> Type
data Channel a = Channel
  { channelConnection :: Connection
  , channelGuid :: Guid
  }

send ::
  (ToJSON params, FromJSON result) =>
  Channel a ->
  MethodName ->
  params ->
  IO result
send channel method params =
  sendCommand (channelConnection channel) (channelGuid channel) method params

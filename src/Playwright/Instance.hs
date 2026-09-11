module Playwright.Instance
  ( Playwright (..)
  , withPlaywright
  , initializePlaywright
  , chromium
  , firefox
  , webkit
  ) where

import Control.Exception
  ( throwIO
  )
import Playwright.BrowserType
  ( BrowserType
  , browserTypeFromRef
  )
import Playwright.Channel
  ( Channel (Channel)
  , PlaywrightTag
  )
import Playwright.Connection
  ( Connection
  , lookupObject
  , sendCommand
  , withConnection
  )
import Playwright.Driver
  ( findDriver
  )
import Playwright.Guid
  ( rootGuid
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
import Playwright.Protocol
  ( PlaywrightInitializer (playwrightInitializerChromium, playwrightInitializerFirefox, playwrightInitializerWebkit)
  , RootInitializeResult (rootInitializeResultPlaywright)
  , SDKLanguage (SDKLanguageJavascript)
  , mkRootInitializeParams
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
  )
import Playwright.Transport.Stdio
  ( withStdioTransport
  )

data Playwright = Playwright
  { playwrightChannel :: Channel PlaywrightTag
  , playwrightChromium :: BrowserType
  , playwrightFirefox :: BrowserType
  , playwrightWebkit :: BrowserType
  }

chromium :: Playwright -> BrowserType
chromium = playwrightChromium

firefox :: Playwright -> BrowserType
firefox = playwrightFirefox

webkit :: Playwright -> BrowserType
webkit = playwrightWebkit

withPlaywright :: (Playwright -> IO a) -> IO a
withPlaywright action = do
  paths <- findDriver
  withStdioTransport paths $ \transport ->
    withConnection transport $ \conn -> do
      pw <- initializePlaywright conn
      action pw

initializePlaywright :: Connection -> IO Playwright
initializePlaywright conn = do
  result <-
    sendCommand
      conn
      rootGuid
      (MethodName "initialize")
      (mkRootInitializeParams SDKLanguageJavascript)
  let
    pwRef :: ChannelRef
    pwRef = rootInitializeResultPlaywright result
  obj <- lookupObject conn (channelRefGuid pwRef)
  initializer <- either throwIO pure (fromJsonValue (remoteInitializer obj))
  chromiumType <- browserTypeFromRef conn (playwrightInitializerChromium initializer)
  firefoxType <- browserTypeFromRef conn (playwrightInitializerFirefox initializer)
  webkitType <- browserTypeFromRef conn (playwrightInitializerWebkit initializer)
  pure
    Playwright
      { playwrightChannel = Channel conn (channelRefGuid pwRef)
      , playwrightChromium = chromiumType
      , playwrightFirefox = firefoxType
      , playwrightWebkit = webkitType
      }

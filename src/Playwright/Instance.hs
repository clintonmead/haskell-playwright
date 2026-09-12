module Playwright.Instance
  ( chromium
  , firefox
  , initializePlaywright
  , runPlaywright
  , webkit
  ) where

import Playwright.BrowserType
  ( BrowserType (browserTypeChannel)
  , browserTypeFromRef
  )
import Playwright.Channel
  ( Channel (channelGuid)
  )
import Playwright.Connection
  ( lookupChromiumGuid
  , lookupFirefoxGuid
  , lookupObject
  , lookupWebkitGuid
  , sendCommand
  , storePlaywrightGuids
  , withConnection
  )
import Playwright.Driver
  ( findDriver
  )
import Playwright.Error
  ( PlaywrightError
  )
import Playwright.Guid
  ( rootGuid
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
import Playwright.Monad
  ( Playwright
  , runPlaywrightWith
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
import UnliftIO.Exception
  ( throwIO
  )

runPlaywright :: Playwright a -> IO a
runPlaywright action = do
  paths <- findDriver
  withStdioTransport paths $ \transport ->
    withConnection transport $ \conn ->
      runPlaywrightWith conn $ do
        initializePlaywright
        action

initializePlaywright :: Playwright ()
initializePlaywright = do
  result <-
    sendCommand
      rootGuid
      (MethodName "initialize")
      (mkRootInitializeParams SDKLanguageJavascript)
  let
    pwRef :: ChannelRef
    pwRef = rootInitializeResultPlaywright result
  obj <- lookupObject (channelRefGuid pwRef)
  initializer <- either throwIO pure (fromJsonValue (remoteInitializer obj) :: Either PlaywrightError PlaywrightInitializer)
  chromiumType <- browserTypeFromRef (playwrightInitializerChromium initializer)
  firefoxType <- browserTypeFromRef (playwrightInitializerFirefox initializer)
  webkitType <- browserTypeFromRef (playwrightInitializerWebkit initializer)
  storePlaywrightGuids
    (channelRefGuid pwRef)
    (channelGuid (browserTypeChannel chromiumType))
    (channelGuid (browserTypeChannel firefoxType))
    (channelGuid (browserTypeChannel webkitType))

chromium :: Playwright BrowserType
chromium =
  lookupChromiumGuid >>= browserTypeFromRef . ChannelRef

firefox :: Playwright BrowserType
firefox =
  lookupFirefoxGuid >>= browserTypeFromRef . ChannelRef

webkit :: Playwright BrowserType
webkit =
  lookupWebkitGuid >>= browserTypeFromRef . ChannelRef

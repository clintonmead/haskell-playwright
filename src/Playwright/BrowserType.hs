module Playwright.BrowserType
  ( BrowserType (..)
  , browserBrowserType
  , browserTypeFromRef
  , browserTypesEqual
  , launch
  , withBrowser
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Data.Text
  ( Text
  )
import Playwright.Browser
  ( Browser (browserChannel)
  , browserFromRef
  , closeBrowser
  )
import Playwright.Channel
  ( BrowserTypeTag
  , Channel (Channel, channelConnection, channelGuid)
  , send
  )
import Playwright.Connection
  ( Connection
  , lookupBrowserTypeGuid
  , lookupObject
  , rememberBrowserType
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (ChannelRef, channelRefGuid)
  )
import Playwright.Json.Decode
  ( fromJsonValue
  )
import Playwright.LaunchOptions
  ( LaunchOptions
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( BrowserTypeInitializer (browserTypeInitializerExecutablePath, browserTypeInitializerName)
  , BrowserTypeLaunchResult (browserTypeLaunchResultBrowser)
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer, remoteParent)
  )

data BrowserType = BrowserType
  { browserTypeChannel :: Channel BrowserTypeTag
  , browserTypeName :: Text
  , browserTypeExecutablePath :: Text
  }

browserTypeFromRef :: Connection -> ChannelRef -> IO BrowserType
browserTypeFromRef conn ref = do
  obj <- lookupObject conn (channelRefGuid ref)
  initializer <- either throwIO pure (fromJsonValue (remoteInitializer obj))
  pure
    BrowserType
      { browserTypeChannel = Channel conn (channelRefGuid ref)
      , browserTypeName = browserTypeInitializerName initializer
      , browserTypeExecutablePath = browserTypeInitializerExecutablePath initializer
      }

launch :: BrowserType -> LaunchOptions -> IO Browser
launch browserType opts = do
  result <-
    send
      (browserTypeChannel browserType)
      (MethodName "launch")
      opts
  browser <-
    browserFromRef
      (channelConnection (browserTypeChannel browserType))
      (browserTypeLaunchResultBrowser result)
  rememberBrowserType
    (channelConnection (browserTypeChannel browserType))
    (channelGuid (browserChannel browser))
    (channelGuid (browserTypeChannel browserType))
  pure browser

browserTypesEqual :: BrowserType -> BrowserType -> Bool
browserTypesEqual left right =
  channelGuid (browserTypeChannel left) == channelGuid (browserTypeChannel right)

browserBrowserType :: Browser -> IO BrowserType
browserBrowserType browser = do
  let
    conn = channelConnection (browserChannel browser)
    guid = channelGuid (browserChannel browser)
  owner <- lookupBrowserTypeGuid conn guid
  case owner of
    Just typeGuid ->
      browserTypeFromRef conn (ChannelRef typeGuid)
    Nothing -> do
      obj <- lookupObject conn guid
      browserTypeFromRef conn (ChannelRef (remoteParent obj))

withBrowser :: BrowserType -> LaunchOptions -> (Browser -> IO a) -> IO a
withBrowser browserType opts =
  bracket (launch browserType opts) closeBrowser

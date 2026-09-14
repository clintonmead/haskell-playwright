module Playwright.BrowserType
  ( BrowserType (..)
  , browserBrowserType
  , browserTypeFromRef
  , browserTypesEqual
  , launch
  , withBrowser
  ) where

import Playwright.Monad
  ( Playwright
  )
import UnliftIO.Exception
  ( bracket
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
  , Channel (Channel, channelGuid)
  , send
  )
import Playwright.Connection
  ( lookupBrowserTypeGuid
  , lookupObject
  , rememberBrowserType
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (ChannelRef, channelRefGuid)
  )
import Playwright.Json.Decode
  ( throwDecode
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

browserTypeFromRef :: ChannelRef -> Playwright BrowserType
browserTypeFromRef ref = do
  obj <- lookupObject (channelRefGuid ref)
  initializer <- throwDecode (remoteInitializer obj)
  pure
    BrowserType
      { browserTypeChannel = Channel (channelRefGuid ref)
      , browserTypeName = browserTypeInitializerName initializer
      , browserTypeExecutablePath = browserTypeInitializerExecutablePath initializer
      }

launch :: BrowserType -> LaunchOptions -> Playwright Browser
launch browserType opts = do
  result <-
    send
      (browserTypeChannel browserType)
      (MethodName "launch")
      opts
  browser <-
    browserFromRef
      (browserTypeLaunchResultBrowser result)
  rememberBrowserType
    (channelGuid (browserChannel browser))
    (channelGuid (browserTypeChannel browserType))
  pure browser

browserTypesEqual :: BrowserType -> BrowserType -> Bool
browserTypesEqual left right =
  channelGuid (browserTypeChannel left) == channelGuid (browserTypeChannel right)

browserBrowserType :: Browser -> Playwright BrowserType
browserBrowserType browser = do
  let
    guid = channelGuid (browserChannel browser)
  owner <- lookupBrowserTypeGuid guid
  case owner of
    Just typeGuid ->
      browserTypeFromRef (ChannelRef typeGuid)
    Nothing -> do
      obj <- lookupObject guid
      browserTypeFromRef (ChannelRef (remoteParent obj))

withBrowser :: BrowserType -> LaunchOptions -> (Browser -> Playwright a) -> Playwright a
withBrowser browserType opts =
  bracket (launch browserType opts) closeBrowser

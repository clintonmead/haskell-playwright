module Playwright.BrowserType
  ( BrowserType (..)
  , browserTypeFromRef
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
  ( Browser
  , browserFromRef
  , closeBrowser
  )
import Playwright.Channel
  ( BrowserTypeTag
  , Channel (Channel, channelConnection)
  , send
  )
import Playwright.Connection
  ( Connection
  , lookupObject
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
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
  ( BrowserTypeInitializer (browserTypeInitializerName)
  , BrowserTypeLaunchResult (browserTypeLaunchResultBrowser)
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
  )

data BrowserType = BrowserType
  { browserTypeChannel :: Channel BrowserTypeTag
  , browserTypeName :: Text
  }

browserTypeFromRef :: Connection -> ChannelRef -> IO BrowserType
browserTypeFromRef conn ref = do
  obj <- lookupObject conn (channelRefGuid ref)
  initializer <- either throwIO pure (fromJsonValue (remoteInitializer obj))
  pure
    BrowserType
      { browserTypeChannel = Channel conn (channelRefGuid ref)
      , browserTypeName = browserTypeInitializerName initializer
      }

launch :: BrowserType -> LaunchOptions -> IO Browser
launch browserType opts = do
  result <-
    send
      (browserTypeChannel browserType)
      (MethodName "launch")
      opts
  browserFromRef
    (channelConnection (browserTypeChannel browserType))
    (browserTypeLaunchResultBrowser result)

withBrowser :: BrowserType -> LaunchOptions -> (Browser -> IO a) -> IO a
withBrowser browserType opts =
  bracket (launch browserType opts) closeBrowser

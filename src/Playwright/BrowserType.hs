module Playwright.BrowserType
  ( BrowserType (..)
  , browserBrowserType
  , browserTypeFromRef
  , browserTypesEqual
  , launch
  , launchPersistentContext
  , withBrowser
  ) where

import Playwright.Monad
  ( Playwright
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import System.Directory
  ( makeAbsolute
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
  , validateNewContextOptions
  )
import Playwright.BrowserContext
  ( BrowserContext
  , contextFromRef
  )
import Playwright.Channel
  ( BrowserTypeTag
  , Channel (Channel, channelGuid)
  , send
  )
import Playwright.Connection
  ( lookupBrowserTypeGuid
  , lookupObject
  , lookupSelectorEngines
  , rememberBrowserContext
  , rememberBrowserType
  , writeContextBaseUrl
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (ChannelRef, channelRefGuid)
  )
import Playwright.Json.Decode
  ( throwDecode
  )
import Playwright.LaunchEnv
  ( applyEnvLaunchOverrides
  )
import Playwright.LaunchOptions
  ( LaunchOptions (launchOptionsDownloadsPath)
  )
import Playwright.LaunchPersistent
  ( toLaunchPersistentParams
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.NewContextOptions
  ( NewContextOptions (newContextBaseURL)
  )
import Playwright.Protocol
  ( BrowserTypeInitializer (browserTypeInitializerExecutablePath, browserTypeInitializerName)
  , BrowserTypeLaunchPersistentContextResult (browserTypeLaunchPersistentContextResultBrowser, browserTypeLaunchPersistentContextResultContext)
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
  resolved <- liftIO (resolveLaunchDownloads opts >>= applyEnvLaunchOverrides)
  result <-
    send
      (browserTypeChannel browserType)
      (MethodName "launch")
      resolved
  browser <-
    browserFromRef
      (browserTypeLaunchResultBrowser result)
  rememberBrowserType
    (channelGuid (browserChannel browser))
    (channelGuid (browserTypeChannel browserType))
  pure browser

launchPersistentContext :: BrowserType -> FilePath -> LaunchOptions -> NewContextOptions -> Playwright BrowserContext
launchPersistentContext browserType userDataDir launchOpts ctxOpts = do
  validateNewContextOptions ctxOpts
  absUserDataDir <- liftIO (makeAbsolute userDataDir)
  resolvedLaunch <- liftIO (resolveLaunchDownloads launchOpts >>= applyEnvLaunchOverrides)
  engines <- lookupSelectorEngines
  result <-
    send
      (browserTypeChannel browserType)
      (MethodName "launchPersistentContext")
      (toLaunchPersistentParams absUserDataDir resolvedLaunch ctxOpts engines)
  browser <-
    browserFromRef
      (browserTypeLaunchPersistentContextResultBrowser result)
  rememberBrowserType
    (channelGuid (browserChannel browser))
    (channelGuid (browserTypeChannel browserType))
  let
    contextRef = browserTypeLaunchPersistentContextResultContext result
  rememberBrowserContext (channelGuid (browserChannel browser)) (channelRefGuid contextRef)
  case newContextBaseURL ctxOpts of
    Nothing ->
      pure ()
    Just base ->
      writeContextBaseUrl (channelRefGuid contextRef) base
  contextFromRef contextRef

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

resolveLaunchDownloads :: LaunchOptions -> IO LaunchOptions
resolveLaunchDownloads opts =
  case launchOptionsDownloadsPath opts of
    Nothing ->
      pure opts
    Just path -> do
      absPath <- makeAbsolute path
      pure opts {launchOptionsDownloadsPath = Just absPath}

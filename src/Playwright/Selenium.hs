module Playwright.Selenium
  ( launchWithSeleniumRemote
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Aeson
  ( Value (Object, String)
  , toJSON
  )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text
  ( Text
  )
import Playwright.BrowserType
  ( BrowserType (browserTypeChannel)
  )
import Playwright.Browser
  ( Browser (browserChannel)
  , browserFromRef
  )
import Playwright.Channel
  ( Channel (channelGuid)
  , send
  )
import Playwright.Connection
  ( rememberBrowserType
  )
import Playwright.LaunchEnv
  ( applyEnvLaunchOverrides
  )
import Playwright.LaunchOptions
  ( LaunchOptions
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.Protocol
  ( BrowserTypeLaunchResult (browserTypeLaunchResultBrowser)
  )

-- | Launches Chromium through a Selenium Grid or WebDriver hub at the given
-- URL (for example @http://127.0.0.1:4444/wd/hub@) instead of starting a local
-- browser. The driver asks the hub for a new session and connects over CDP.
--
-- Only Chromium is supported; other browser types fail with an error from the
-- driver. The URL is passed per launch through the driver's Selenium hook. The
-- @SELENIUM_REMOTE_URL@ environment variable reaches the same code path but
-- applies to the whole driver process.
launchWithSeleniumRemote :: BrowserType -> Text -> LaunchOptions -> Playwright Browser
launchWithSeleniumRemote browserType hubUrl opts = do
  resolved <- liftIO (applyEnvLaunchOverrides opts)
  result <-
    send
      (browserTypeChannel browserType)
      (MethodName "launch")
      (withSeleniumHub hubUrl (toJSON resolved))
  browser <-
    browserFromRef
      (browserTypeLaunchResultBrowser result)
  rememberBrowserType
    (channelGuid (browserChannel browser))
    (channelGuid (browserTypeChannel browserType))
  pure browser

withSeleniumHub :: Text -> Value -> Value
withSeleniumHub hubUrl value =
  case value of
    Object obj ->
      Object (KeyMap.insert "__testHookSeleniumRemoteURL" (String hubUrl) obj)
    other ->
      other

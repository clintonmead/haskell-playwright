module Playwright.BrowserServer
  ( BrowserServerOptions (..)
  , BrowserServer (..)
  , defaultBrowserServerOptions
  , bindBrowser
  , bindBrowserWith
  , unbindBrowser
  ) where

import Data.Aeson
  ( FromJSON (parseJSON)
  , ToJSON
  , Value
  , object
  , withObject
  , (.:)
  , (.=)
  )
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types qualified as Aeson
import Data.Text
  ( Text
  )
import Playwright.Browser
  ( Browser (browserChannel)
  )
import Playwright.Channel
  ( send
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Monad
  ( Playwright
  )

-- | Options for 'bindBrowser', mirroring @browser.bind(title, options)@.
data BrowserServerOptions = BrowserServerOptions
  { browserServerHost :: Maybe Text
  , browserServerPort :: Maybe Int
  , browserServerWsPath :: Maybe Text
  , browserServerWorkspaceDir :: Maybe Text
  , browserServerMetadata :: Maybe Value
  }
  deriving stock (Eq, Show)

defaultBrowserServerOptions :: BrowserServerOptions
defaultBrowserServerOptions =
  BrowserServerOptions
    { browserServerHost = Nothing
    , browserServerPort = Nothing
    , browserServerWsPath = Nothing
    , browserServerWorkspaceDir = Nothing
    , browserServerMetadata = Nothing
    }

-- | A browser server started by 'bindBrowser'. The endpoint is either a
-- @ws://@ URL (when a host or port was given) or a unix pipe path.
newtype BrowserServer = BrowserServer
  { browserServerEndpoint :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON BrowserServer where
  parseJSON =
    withObject "BrowserServer" $ \o ->
      BrowserServer <$> o .: "endpoint"

-- | Starts a browser server with default options.
bindBrowser :: Browser -> Text -> Playwright BrowserServer
bindBrowser browser title =
  bindBrowserWith browser title defaultBrowserServerOptions

-- | Starts a browser server that other clients can connect to. Sends
-- @Browser.startServer@ to the driver.
bindBrowserWith :: Browser -> Text -> BrowserServerOptions -> Playwright BrowserServer
bindBrowserWith browser title options =
  send (browserChannel browser) (MethodName "startServer") params
  where
    params =
      object $
        [ "title" .= title
        ]
          <> optional "host" (browserServerHost options)
          <> optional "port" (browserServerPort options)
          <> optional "wsPath" (browserServerWsPath options)
          <> optional "workspaceDir" (browserServerWorkspaceDir options)
          <> optional "metadata" (browserServerMetadata options)
    optional :: ToJSON a => Text -> Maybe a -> [Aeson.Pair]
    optional key = maybe [] (\v -> [Key.fromText key .= v])

-- | Stops the browser server started by 'bindBrowser'. Sends
-- @Browser.stopServer@ to the driver.
unbindBrowser :: Browser -> Playwright ()
unbindBrowser browser = do
  _ :: Value <- send (browserChannel browser) (MethodName "stopServer") (object [])
  pure ()

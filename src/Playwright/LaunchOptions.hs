module Playwright.LaunchOptions
  ( LaunchOptions (..)
  , defaultLaunchOptions
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , object
  , optionalField'
  , requiredField'
  , (.=)
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Data.Text
  ( pack
  , unpack
  )
import Playwright.Timeout
  ( Timeout
  , defaultLaunchTimeout
  )
import Playwright.ProxySettings
  ( ProxySettings
  )

data LaunchOptions = LaunchOptions
  { launchOptionsHeadless :: Maybe Bool
  , launchOptionsTimeout :: Timeout
  , launchOptionsChromiumSandbox :: Maybe Bool
  , launchOptionsSlowMo :: Maybe Timeout
  , launchOptionsProxy :: Maybe ProxySettings
  , launchOptionsDownloadsPath :: Maybe FilePath
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec LaunchOptions)

defaultLaunchOptions :: LaunchOptions
defaultLaunchOptions =
  LaunchOptions
    { launchOptionsHeadless = Just True
    , launchOptionsTimeout = defaultLaunchTimeout
    , launchOptionsChromiumSandbox = Just False
    , launchOptionsSlowMo = Nothing
    , launchOptionsProxy = Nothing
    , launchOptionsDownloadsPath = Nothing
    }

instance HasCodec LaunchOptions where
  codec =
    object "LaunchOptions" $
      ( \headless timeout sandbox slowMo proxy downloadsPath ->
          LaunchOptions
            { launchOptionsHeadless = headless
            , launchOptionsTimeout = timeout
            , launchOptionsChromiumSandbox = sandbox
            , launchOptionsSlowMo = slowMo
            , launchOptionsProxy = proxy
            , launchOptionsDownloadsPath = fmap unpack downloadsPath
            }
      )
        <$> optionalField' "headless" .= launchOptionsHeadless
        <*> requiredField' "timeout" .= launchOptionsTimeout
        <*> optionalField' "chromiumSandbox" .= launchOptionsChromiumSandbox
        <*> optionalField' "slowMo" .= launchOptionsSlowMo
        <*> optionalField' "proxy" .= launchOptionsProxy
        <*> optionalField' "downloadsPath" .= (fmap pack . launchOptionsDownloadsPath)

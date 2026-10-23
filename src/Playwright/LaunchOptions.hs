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
  ( Text
  , pack
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
  , launchOptionsExecutablePath :: Maybe FilePath
  , launchOptionsArgs :: Maybe [Text]
  , launchOptionsIgnoreDefaultArgs :: Maybe [Text]
  , launchOptionsArtifactsDir :: Maybe FilePath
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
    , launchOptionsExecutablePath = Nothing
    , launchOptionsArgs = Nothing
    , launchOptionsIgnoreDefaultArgs = Nothing
    , launchOptionsArtifactsDir = Nothing
    }

instance HasCodec LaunchOptions where
  codec =
    object "LaunchOptions" $
      ( \headless timeout sandbox slowMo proxy downloadsPath executablePath args ignoreDefaultArgs artifactsDir ->
          LaunchOptions
            { launchOptionsHeadless = headless
            , launchOptionsTimeout = timeout
            , launchOptionsChromiumSandbox = sandbox
            , launchOptionsSlowMo = slowMo
            , launchOptionsProxy = proxy
            , launchOptionsDownloadsPath = fmap unpack downloadsPath
            , launchOptionsExecutablePath = fmap unpack executablePath
            , launchOptionsArgs = args
            , launchOptionsIgnoreDefaultArgs = ignoreDefaultArgs
            , launchOptionsArtifactsDir = fmap unpack artifactsDir
            }
      )
        <$> optionalField' "headless" .= launchOptionsHeadless
        <*> requiredField' "timeout" .= launchOptionsTimeout
        <*> optionalField' "chromiumSandbox" .= launchOptionsChromiumSandbox
        <*> optionalField' "slowMo" .= launchOptionsSlowMo
        <*> optionalField' "proxy" .= launchOptionsProxy
        <*> optionalField' "downloadsPath" .= (fmap pack . launchOptionsDownloadsPath)
        <*> optionalField' "executablePath" .= (fmap pack . launchOptionsExecutablePath)
        <*> optionalField' "args" .= launchOptionsArgs
        <*> optionalField' "ignoreDefaultArgs" .= launchOptionsIgnoreDefaultArgs
        <*> optionalField' "artifactsDir" .= (fmap pack . launchOptionsArtifactsDir)

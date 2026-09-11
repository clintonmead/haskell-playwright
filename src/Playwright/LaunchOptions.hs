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
import Playwright.Timeout
  ( Timeout
  , defaultLaunchTimeout
  )

data LaunchOptions = LaunchOptions
  { launchOptionsHeadless :: Maybe Bool
  , launchOptionsTimeout :: Timeout
  , launchOptionsChromiumSandbox :: Maybe Bool
  , launchOptionsSlowMo :: Maybe Timeout
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
    }

instance HasCodec LaunchOptions where
  codec =
    object "LaunchOptions" $
      LaunchOptions
        <$> optionalField' "headless" .= launchOptionsHeadless
        <*> requiredField' "timeout" .= launchOptionsTimeout
        <*> optionalField' "chromiumSandbox" .= launchOptionsChromiumSandbox
        <*> optionalField' "slowMo" .= launchOptionsSlowMo

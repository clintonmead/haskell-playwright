module Playwright.ProxySettings
  ( ProxyHost (..)
  , ProxySettings (..)
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
  )
import Playwright.HttpCredentials
  ( HttpPassword (HttpPassword, httpPasswordToText)
  , HttpUsername (HttpUsername, httpUsernameToText)
  )

newtype ProxyHost = ProxyHost
  { proxyHostToText :: Text
  }
  deriving stock (Eq, Show)
  deriving newtype (HasCodec)

data ProxySettings = ProxySettings
  { proxySettingsServer :: ProxyHost
  , proxySettingsBypass :: Maybe Text
  , proxySettingsUsername :: Maybe HttpUsername
  , proxySettingsPassword :: Maybe HttpPassword
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec ProxySettings)

instance HasCodec ProxySettings where
  codec =
    object "ProxySettings" $
      ( \server bypass username password ->
          ProxySettings
            { proxySettingsServer = server
            , proxySettingsBypass = bypass
            , proxySettingsUsername = fmap HttpUsername username
            , proxySettingsPassword = fmap HttpPassword password
            }
      )
        <$> requiredField' "server" .= proxySettingsServer
        <*> optionalField' "bypass" .= proxySettingsBypass
        <*> optionalField' "username" .= (fmap httpUsernameToText . proxySettingsUsername)
        <*> optionalField' "password" .= (fmap httpPasswordToText . proxySettingsPassword)

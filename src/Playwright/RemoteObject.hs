module Playwright.RemoteObject
  ( RemoteObject (..)
  ) where

import Data.Aeson
  ( Value
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.Protocol
  ( ChannelType
  )

data RemoteObject = RemoteObject
  { remoteType :: ChannelType
  , remoteGuid :: Guid
  , remoteParent :: Guid
  , remoteInitializer :: Value
  }
  deriving stock (Eq, Show)

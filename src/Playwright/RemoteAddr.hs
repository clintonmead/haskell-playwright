module Playwright.RemoteAddr
  ( RemoteAddr (..)
  , fromProtocolRemoteAddr
  ) where

import Data.Text
  ( Text
  )
import Playwright.Protocol qualified as Protocol
  ( RemoteAddr (remoteAddrIpAddress, remoteAddrPort)
  )

data RemoteAddr = RemoteAddr
  { remoteAddrIpAddress :: Text
  , remoteAddrPort :: Int
  }
  deriving stock (Eq, Show)

fromProtocolRemoteAddr :: Protocol.RemoteAddr -> RemoteAddr
fromProtocolRemoteAddr addr =
  RemoteAddr
    { remoteAddrIpAddress = Protocol.remoteAddrIpAddress addr
    , remoteAddrPort = Protocol.remoteAddrPort addr
    }

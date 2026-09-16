module Playwright.WebSocketFrame
  ( WebSocketFrame (..)
  ) where

import Data.ByteString
  ( ByteString
  )
import Data.Text
  ( Text
  )

data WebSocketFrame
  = WebSocketText Text
  | WebSocketBinary ByteString
  deriving stock (Eq, Show)

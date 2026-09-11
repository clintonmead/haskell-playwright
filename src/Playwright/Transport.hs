module Playwright.Transport
  ( Transport (..)
  ) where

import Data.ByteString
  ( ByteString
  )

-- | Payload-level transport. Stdio applies length-prefix framing internally;
-- in-memory fakes need not.
data Transport = Transport
  { sendPayload :: ByteString -> IO ()
  , recvPayload :: IO (Maybe ByteString)
  , closeTransport :: IO ()
  }

module Playwright.Transport.Memory
  ( newConnectedPair
  ) where

import Control.Concurrent.STM
  ( TQueue
  , atomically
  , newTQueueIO
  , readTQueue
  , writeTQueue
  )
import Data.ByteString
  ( ByteString
  )
import Playwright.Transport
  ( Transport (Transport, closeTransport, recvPayload, sendPayload)
  )

newConnectedPair :: IO (Transport, Transport)
newConnectedPair = do
  aToB <- newTQueueIO
  bToA <- newTQueueIO
  pure (mkTransport aToB bToA, mkTransport bToA aToB)

mkTransport :: TQueue (Maybe ByteString) -> TQueue (Maybe ByteString) -> Transport
mkTransport sendQ recvQ =
  Transport
    { sendPayload = \bytes -> atomically (writeTQueue sendQ (Just bytes))
    , recvPayload = atomically (readTQueue recvQ)
    , closeTransport = atomically (writeTQueue sendQ Nothing)
    }

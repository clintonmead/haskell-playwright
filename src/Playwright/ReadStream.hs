module Playwright.ReadStream
  ( ReadStream
  , readStreamFromChannel
  , readStreamChunk
  , readStreamClose
  ) where

import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as ByteString
import Playwright.Channel
  ( Channel
  , StreamTag
  , send
  )
import Playwright.Json.ProtocolBinary
  ( protocolBinaryToByteString
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.Protocol
  ( StreamCloseResult
  , StreamReadResult (streamReadResultBinary)
  , mkStreamCloseParams
  , mkStreamReadParams
  )

-- | A readable stream of a download's contents, served by the driver.
newtype ReadStream = ReadStream
  { readStreamChannel :: Channel StreamTag
  }

readStreamFromChannel :: Channel StreamTag -> ReadStream
readStreamFromChannel = ReadStream

-- | Reads the next chunk. 'Nothing' means the stream has ended.
readStreamChunk :: ReadStream -> Playwright (Maybe ByteString)
readStreamChunk stream = do
  result :: StreamReadResult <-
    send
      (readStreamChannel stream)
      (MethodName "read")
      mkStreamReadParams
  let
    bytes = protocolBinaryToByteString (streamReadResultBinary result)
  pure $
    if ByteString.null bytes
      then Nothing
      else Just bytes

-- | Closes the stream on the driver.
readStreamClose :: ReadStream -> Playwright ()
readStreamClose stream = do
  _ :: StreamCloseResult <-
    send
      (readStreamChannel stream)
      (MethodName "close")
      mkStreamCloseParams
  pure ()

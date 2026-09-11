module Playwright.Transport.Frame
  ( FrameError (..)
  , maxFrameSize
  , encodeFrame
  , splitFrames
  ) where

import Data.Binary.Get
  ( getWord32le
  , runGetOrFail
  )
import Data.Binary.Put
  ( putByteString
  , putWord32le
  , runPut
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Word
  ( Word32
  )

data FrameError
  = FrameTooLarge Word32
  | FrameCorruptHeader
  deriving stock (Eq, Show)

maxFrameSize :: Word32
maxFrameSize = 64 * 1024 * 1024

encodeFrame :: ByteString -> Either FrameError ByteString
encodeFrame payload = do
  len <- word32Length payload
  if len > maxFrameSize
    then Left (FrameTooLarge len)
    else
      Right
        ( LBS.toStrict
            ( runPut
                ( do
                    putWord32le len
                    putByteString payload
                )
            )
        )

splitFrames :: ByteString -> Either FrameError (ByteString, [ByteString])
splitFrames = go []
  where
    go :: [ByteString] -> ByteString -> Either FrameError (ByteString, [ByteString])
    go acc buffer =
      if BS.length buffer < 4
        then Right (buffer, reverse acc)
        else case runGetOrFail getWord32le (LBS.fromStrict (BS.take 4 buffer)) of
          Left _ ->
            Left FrameCorruptHeader
          Right (_, _, len) ->
            if len > maxFrameSize
              then Left (FrameTooLarge len)
              else
                let
                  need :: Int
                  need = fromIntegral len
                  rest :: ByteString
                  rest = BS.drop 4 buffer
                in
                  if BS.length rest < need
                    then Right (buffer, reverse acc)
                    else go (BS.take need rest : acc) (BS.drop need rest)

word32Length :: ByteString -> Either FrameError Word32
word32Length payload =
  let
    n :: Int
    n = BS.length payload
    maxInt :: Int
    maxInt = fromIntegral (maxBound :: Word32)
  in
    if n > maxInt
      then Left (FrameTooLarge (maxBound :: Word32))
      else Right (fromIntegral n)

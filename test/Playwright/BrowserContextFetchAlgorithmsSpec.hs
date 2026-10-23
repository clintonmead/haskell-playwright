module Playwright.BrowserContextFetchAlgorithmsSpec
  ( tests
  ) where

import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Zlib qualified as Zlib
import Data.ByteString
  ( ByteString
  )
import Data.ByteString.Builder
  ( Builder
  , lazyByteString
  )
import Data.ByteString.Lazy qualified as LBS
import Data.Text
  ( Text
  , unpack
  )
import Data.Text.Encoding
  ( decodeUtf8
  )
import Network.HTTP.Types
  ( hContentEncoding
  , hContentType
  , status200
  )
import Network.Wai
  ( Application
  , responseLBS
  , responseStream
  )
import Playwright
  ( apiResponseText
  , contextRequest
  , fetchGet
  , urlFromText
  )
import Playwright.Harness
  ( assertEqual
  , withChromiumContext
  , withLocalApp
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

tests :: TestTree
tests =
  testGroup
    "browsercontext-fetch-algorithms"
    (concatMap encodingTests encodings)

encodings :: [(ByteString, LBS.ByteString)]
encodings =
  [ ("gzip", GZip.compress payload)
  , ("deflate", Zlib.compress payload)
  , ("br", brotliStr)
  ]

payload :: LBS.ByteString
payload =
  "str"

brotliStr :: LBS.ByteString
brotliStr =
  LBS.pack [11, 1, 128, 115, 116, 114, 3]

encodingTests :: (ByteString, LBS.ByteString) -> [TestTree]
encodingTests (encoding, zipped) =
  [ testGroup
      (unpack (decodeUtf8 encoding) <> " decompression")
      [ testCase "should support decompression" $
          fetchCompressed encoding zipped WithLength "str"
      , testCase ("should not fail if response content-length header is missing (" <> unpack (decodeUtf8 encoding) <> ")") $
          fetchCompressed encoding zipped WithoutLength "str"
      , testCase "should not fail with chunked responses (without Content-Length header)" $
          fetchCompressed encoding zipped Chunked "str"
      , testCase "should not fail with an empty response without content-length header (Z_BUF_ERROR)" $
          fetchCompressed encoding zipped EmptyWithoutLength ""
      , testCase "should not fail with an empty response with content-length header (Z_BUF_ERROR)" $
          fetchCompressed encoding zipped EmptyWithLength ""
      ]
  ]

data BodyMode
  = WithLength
  | WithoutLength
  | Chunked
  | EmptyWithoutLength
  | EmptyWithLength

fetchCompressed :: ByteString -> LBS.ByteString -> BodyMode -> Text -> IO ()
fetchCompressed encoding zipped mode expected =
  withChromiumContext $ \ctx _page ->
    withLocalApp (compressedApp encoding zipped mode) $ \prefix -> do
      request <- contextRequest ctx
      response <- fetchGet request (urlFromText (prefix <> "/compressed"))
      assertEqual "body" expected =<< apiResponseText response

compressedApp :: ByteString -> LBS.ByteString -> BodyMode -> Application
compressedApp encoding zipped mode _request respond =
  case mode of
    WithLength ->
      respond (responseLBS status200 headers zipped)
    EmptyWithLength ->
      respond (responseLBS status200 headers "")
    WithoutLength ->
      respond (responseStream status200 headers (writeBody zipped))
    Chunked ->
      respond
        ( responseStream
            status200
            (("Transfer-Encoding", "chunked") : headers)
            (writeBody zipped)
        )
    EmptyWithoutLength ->
      respond (responseStream status200 headers (writeBody ""))
  where
    headers =
      [(hContentEncoding, encoding), (hContentType, "text/plain")]

writeBody :: LBS.ByteString -> (Builder -> IO ()) -> IO () -> IO ()
writeBody body write flush = do
  write (lazyByteString body)
  flush

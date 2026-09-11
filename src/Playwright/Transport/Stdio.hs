module Playwright.Transport.Stdio
  ( withStdioTransport
  ) where

import Control.Exception
  ( IOException
  , bracket
  , throwIO
  , try
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as BS
import Data.IORef
  ( IORef
  , newIORef
  , readIORef
  , writeIORef
  )
import Playwright.Driver
  ( DriverPaths
  , driverCli
  , driverNode
  )
import Playwright.Error
  ( PlaywrightError (FrameCodec)
  )
import Playwright.Transport
  ( Transport (Transport, closeTransport, recvPayload, sendPayload)
  )
import Playwright.Transport.Frame
  ( encodeFrame
  , splitFrames
  )
import System.Environment
  ( getEnvironment
  )
import System.IO
  ( BufferMode (NoBuffering)
  , Handle
  , hClose
  , hFlush
  , hSetBinaryMode
  , hSetBuffering
  )
import System.Process.Typed
  ( createPipe
  , getStdin
  , getStdout
  , inherit
  , proc
  , setEnv
  , setStderr
  , setStdin
  , setStdout
  , startProcess
  , stopProcess
  )

data RecvBuffer = RecvBuffer
  { recvIncomplete :: ByteString
  , recvQueued :: [ByteString]
  }

withStdioTransport :: DriverPaths -> (Transport -> IO a) -> IO a
withStdioTransport paths action = do
  env <- getEnvironment
  let
    env' :: [(String, String)]
    env' =
      ("PW_LANG_NAME", "javascript")
        : filter (\(key, _) -> key /= "PW_LANG_NAME") env
    config =
      setEnv env'
        $ setStdin createPipe
        $ setStdout createPipe
        $ setStderr inherit
        $ proc (driverNode paths) [driverCli paths, "run-driver"]
  bracket (startProcess config) stopProcess $ \process -> do
    let
      hin :: Handle
      hin = getStdin process
      hout :: Handle
      hout = getStdout process
    hSetBinaryMode hin True
    hSetBinaryMode hout True
    hSetBuffering hin NoBuffering
    hSetBuffering hout NoBuffering
    leftover <- newIORef RecvBuffer {recvIncomplete = BS.empty, recvQueued = []}
    action (mkStdioTransport hin hout leftover)

mkStdioTransport ::
  Handle ->
  Handle ->
  IORef RecvBuffer ->
  Transport
mkStdioTransport hin hout leftover =
  Transport
    { sendPayload = sendFramed hin
    , recvPayload = recvFramed hout leftover
    , closeTransport = hClose hin
    }

sendFramed :: Handle -> ByteString -> IO ()
sendFramed hin payload =
  case encodeFrame payload of
    Left err ->
      throwIO (FrameCodec err)
    Right framed -> do
      BS.hPut hin framed
      hFlush hin

recvFramed :: Handle -> IORef RecvBuffer -> IO (Maybe ByteString)
recvFramed hout leftover = do
  buffer <- readIORef leftover
  case recvQueued buffer of
    payload : rest -> do
      writeIORef leftover buffer {recvQueued = rest}
      pure (Just payload)
    [] -> do
      chunk <- try (BS.hGetSome hout 32768)
      case chunk of
        Left (_ :: IOException) ->
          pure Nothing
        Right bytes ->
          if BS.null bytes
            then pure Nothing
            else case splitFrames (recvIncomplete buffer <> bytes) of
              Left err ->
                throwIO (FrameCodec err)
              Right (incomplete, []) -> do
                writeIORef leftover RecvBuffer {recvIncomplete = incomplete, recvQueued = []}
                recvFramed hout leftover
              Right (incomplete, payload : rest) -> do
                writeIORef leftover RecvBuffer {recvIncomplete = incomplete, recvQueued = rest}
                pure (Just payload)

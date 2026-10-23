module Playwright.LaunchServer
  ( LaunchServerOptions (..)
  , defaultLaunchServerOptions
  , LaunchedServer
  , LaunchServerError (..)
  , launchServer
  , launchedServerEndpoint
  , launchedServerPid
  , closeLaunchedServer
  , killLaunchedServer
  , waitLaunchedServer
  ) where

-- Port of @browserType.launchServer@ (upstream client/browserType.ts and
-- browserServerImpl.ts). Upstream runs the server in the driver process and
-- returns a BrowserServer object. This client has no WebSocket transport, so
-- the server is started as a separate driver process through the hidden
-- @launch-server --browser NAME --config FILE@ CLI command. That command
-- prints the ws endpoint on stdout and keeps running until the browser closes.
--
-- 'closeLaunchedServer' sends SIGTERM. The driver's SIGTERM handler closes
-- the browser, and the process then exits with code 0. 'killLaunchedServer'
-- sends SIGKILL to the driver process only.

import Control.Exception
  ( Exception
  , finally
  , throwIO
  )
import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )
import Data.Aeson
  ( Value
  , encode
  , object
  , (.=)
  )
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe
  ( catMaybes
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Playwright.BrowserType
  ( BrowserType (browserTypeName)
  )
import Playwright.Driver
  ( DriverPaths (driverCli, driverNode)
  , findDriver
  )
import System.Directory
  ( getTemporaryDirectory
  , removeFile
  )
import System.Exit
  ( ExitCode
  )
import System.IO
  ( Handle
  , hClose
  , openTempFile
  )
import System.IO.Error
  ( isEOFError
  )
import System.Process
  ( getPid
  , callProcess
  )
import System.Process.Typed
  ( Process
  , ProcessConfig
  , closed
  , createPipe
  , getStderr
  , getStdout
  , proc
  , setStderr
  , setStdin
  , setStdout
  , startProcess
  , stopProcess
  , unsafeProcessHandle
  , waitExitCode
  )
import UnliftIO.Exception
  ( catch
  )

-- | Options for 'launchServer', mirroring upstream LaunchServerOptions.
-- 'launchServerExtra' carries any other launch option as a raw JSON pair,
-- for example @("channel", Null)@ to exercise the driver's validation.
data LaunchServerOptions = LaunchServerOptions
  { launchServerHost :: Maybe Text
  , launchServerPort :: Maybe Int
  , launchServerWsPath :: Maybe Text
  , launchServerExtra :: [(Text, Value)]
  }
  deriving stock (Show)

defaultLaunchServerOptions :: LaunchServerOptions
defaultLaunchServerOptions =
  LaunchServerOptions
    { launchServerHost = Nothing
    , launchServerPort = Nothing
    , launchServerWsPath = Nothing
    , launchServerExtra = []
    }

-- | A browser server started by 'launchServer'.
data LaunchedServer = LaunchedServer
  { launchedServerEndpoint :: Text
  , launchedServerProcess :: Process () Handle Handle
  }

newtype LaunchServerError = LaunchServerError Text
  deriving stock (Eq, Show)

instance Exception LaunchServerError

-- | Starts a browser server for the given browser type and returns once the
-- ws endpoint has been printed. Throws 'LaunchServerError' with the driver's
-- stderr when the server fails to start.
launchServer :: (MonadIO m) => BrowserType -> LaunchServerOptions -> m LaunchedServer
launchServer browserType options = liftIO $ do
  paths <- findDriver
  tmpDir <- getTemporaryDirectory
  (configPath, configHandle) <- openTempFile tmpDir "pw-launch-server.json"
  LBS.hPut configHandle (encode (configValue options))
  hClose configHandle
  let
    config :: ProcessConfig () Handle Handle
    config =
      setStdin closed
        $ setStdout createPipe
        $ setStderr createPipe
        $ proc
          (driverNode paths)
          [ driverCli paths
          , "launch-server"
          , "--browser"
          , Text.unpack (browserTypeName browserType)
          , "--config"
          , configPath
          ]
  process <- startProcess config
  endpoint <-
    readEndpoint process `finally` removeFile configPath
  pure
    LaunchedServer
      { launchedServerEndpoint = endpoint
      , launchedServerProcess = process
      }

readEndpoint :: Process () Handle Handle -> IO Text
readEndpoint process =
  (Text.strip <$> TIO.hGetLine (getStdout process))
    `catch` \exception ->
      if isEOFError exception
        then do
          _ <- waitExitCode process
          stderrText <- TIO.hGetContents (getStderr process)
          throwIO (LaunchServerError (Text.strip stderrText))
        else throwIO exception

configValue :: LaunchServerOptions -> Value
configValue options =
  object $
    catMaybes
      [ fmap (\h -> "host" .= h) (launchServerHost options)
      , fmap (\p -> "port" .= p) (launchServerPort options)
      , fmap (\w -> "wsPath" .= w) (launchServerWsPath options)
      ]
      <> [Key.fromText k .= v | (k, v) <- launchServerExtra options]

-- | The process id of the driver process running the server, if known.
launchedServerPid :: (MonadIO m) => LaunchedServer -> m (Maybe Int)
launchedServerPid server =
  liftIO $
    fmap fromIntegral <$> getPid (unsafeProcessHandle (launchedServerProcess server))

-- | Closes the browser and the server. Sends SIGTERM and waits for exit.
closeLaunchedServer :: (MonadIO m) => LaunchedServer -> m ()
closeLaunchedServer server =
  liftIO (stopProcess (launchedServerProcess server))

-- | Kills the server process with SIGKILL and waits for it to exit.
killLaunchedServer :: (MonadIO m) => LaunchedServer -> m ()
killLaunchedServer server = liftIO $ do
  mpid <- launchedServerPid server
  case mpid of
    Nothing -> pure ()
    Just pid -> callProcess "kill" ["-KILL", show pid]
  _ <- waitExitCode (launchedServerProcess server)
  pure ()

-- | Waits for the server process to exit and returns its exit code. A
-- process ended by a signal reports a negative code, as in System.Process.
waitLaunchedServer :: (MonadIO m) => LaunchedServer -> m ExitCode
waitLaunchedServer server =
  liftIO (waitExitCode (launchedServerProcess server))

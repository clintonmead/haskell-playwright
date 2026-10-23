module Playwright.BrowserTypeLaunchServerSpec
  ( tests
  ) where

import Data.Aeson
  ( Value (Null)
  )
import Data.Char
  ( isDigit
  , isHexDigit
  )
import Data.Text qualified as Text
import Playwright
  ( LaunchServerError (LaunchServerError)
  , LaunchServerOptions (launchServerHost, launchServerPort, launchServerWsPath, launchServerExtra)
  , LaunchedServer
  , Playwright
  , chromium
  , closeLaunchedServer
  , defaultLaunchServerOptions
  , killLaunchedServer
  , launchServer
  , launchedServerEndpoint
  , launchedServerPid
  , waitLaunchedServer
  )
import Playwright.Harness
  ( assertBool
  , assertFailure
  , withChromiumBrowser
  )
import System.Exit
  ( ExitCode (ExitFailure, ExitSuccess)
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Exception
  ( finally
  , try
  )

-- Skipped upstream cases (tests/library/browsertype-launch-server.spec.ts):
--   "should provide an error when ws endpoint is incorrect": needs
--     BrowserType.connect, which is not implemented in Haskell yet.
--   "should log protocol": the Haskell API has no logger option, so there is
--     no way to pass a protocol logger to launchServer.
--
-- Adapted upstream case:
--   "should fire \"close\" event during kill": Haskell has no event
--     subscription on the server. The test kills the driver with SIGKILL and
--     then checks that the process exited by a signal.
--   "should fire close event": Haskell has no event subscription, so the test
--     checks the exit code from waitLaunchedServer after close instead.

tests :: TestTree
tests =
  testGroup
    "browsertype-launch-server"
    [ testCase "should work" $
        withChromiumBrowser $ \_browser ->
          launchedWith defaultLaunchServerOptions $ \server ->
            assertBool
              "endpoint is non-empty"
              (not (Text.null (launchedServerEndpoint server)))
    , testCase "should validate options" $
        withChromiumBrowser $ \_browser -> do
          browserType <- chromium
          result <-
            try
              ( launchServer
                  browserType
                  defaultLaunchServerOptions
                    { launchServerExtra = [("channel", Null)]
                    }
              )
          case result of
            Left (LaunchServerError message) ->
              assertBool
                ("error mentions channel: " <> Text.unpack message)
                ("channel: expected string, got object" `Text.isInfixOf` message)
            Right server -> do
              closeLaunchedServer server
              assertFailure "launchServer accepted a null channel"
    , testCase "should work with host" $
        withChromiumBrowser $ \_browser -> do
          let host = "0.0.0.0" :: Text.Text
          launchedWith defaultLaunchServerOptions {launchServerHost = Just host} $ \server ->
            assertBool
              "endpoint contains host"
              (host `Text.isInfixOf` launchedServerEndpoint server)
    , testCase "should work with port" $
        withChromiumBrowser $ \_browser -> do
          let port = 8800 :: Int
          launchedWith defaultLaunchServerOptions {launchServerPort = Just port} $ \server ->
            assertBool
              "endpoint contains port"
              (Text.pack (show port) `Text.isInfixOf` launchedServerEndpoint server)
    , testCase "should work with wsPath" $
        withChromiumBrowser $ \_browser -> do
          let wsPath = "/unguessable-token" :: Text.Text
          launchedWith defaultLaunchServerOptions {launchServerWsPath = Just wsPath} $ \server ->
            assertBool
              "endpoint ends with :port/unguessable-token"
              (endsWithPortPath "/unguessable-token" (launchedServerEndpoint server))
    , testCase "should work when wsPath is missing leading slash" $
        withChromiumBrowser $ \_browser -> do
          let wsPath = "unguessable-token" :: Text.Text
          launchedWith defaultLaunchServerOptions {launchServerWsPath = Just wsPath} $ \server ->
            assertBool
              "endpoint ends with :port/unguessable-token"
              (endsWithPortPath "/unguessable-token" (launchedServerEndpoint server))
    , testCase "should default to random wsPath" $
        withChromiumBrowser $ \_browser ->
          launchedWith defaultLaunchServerOptions $ \server -> do
            let segment = Text.takeWhileEnd (/= '/') (launchedServerEndpoint server)
            assertBool
              ("endpoint ends with a 32 hex digit path: " <> Text.unpack segment)
              (Text.length segment == 32 && Text.all isHexDigit segment)
    , testCase "should fire \"close\" event during kill" $
        withChromiumBrowser $ \_browser -> do
          browserType <- chromium
          server <- launchServer browserType defaultLaunchServerOptions
          killLaunchedServer server
          exitCode <- waitLaunchedServer server
          assertBool
            ("server exited by signal after kill: " <> show exitCode)
            (isSignalExit exitCode)
    , testCase "should return child_process instance" $
        withChromiumBrowser $ \_browser ->
          launchedWith defaultLaunchServerOptions $ \server -> do
            pid <- launchedServerPid server
            assertBool
              ("driver process has a positive pid: " <> show pid)
              (maybe False (> 0) pid)
    , testCase "should fire close event" $
        withChromiumBrowser $ \_browser -> do
          browserType <- chromium
          server <- launchServer browserType defaultLaunchServerOptions
          closeLaunchedServer server
          exitCode <- waitLaunchedServer server
          assertBool
            ("exit code is 0 after close: " <> show exitCode)
            (exitCode == ExitSuccess)
    ]

-- | Launches a browser server for the test body and always closes it again.
launchedWith :: LaunchServerOptions -> (LaunchedServer -> Playwright ()) -> Playwright ()
launchedWith options body = do
  browserType <- chromium
  server <- launchServer browserType options
  body server `finally` closeLaunchedServer server

-- | True when the endpoint ends with @path@ preceded by @:port@, as in
-- upstream's @/:\\d+\\/unguessable-token$/@ check.
endsWithPortPath :: Text.Text -> Text.Text -> Bool
endsWithPortPath path endpoint =
  case Text.stripSuffix path endpoint of
    Nothing -> False
    Just prefix ->
      let port = Text.takeWhileEnd isDigit prefix
          host = Text.dropEnd (Text.length port) prefix
       in not (Text.null port) && ":" `Text.isSuffixOf` host

isSignalExit :: ExitCode -> Bool
isSignalExit exitCode =
  case exitCode of
    ExitFailure code -> code < 0
    ExitSuccess -> False

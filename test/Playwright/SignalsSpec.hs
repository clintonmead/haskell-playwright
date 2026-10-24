module Playwright.SignalsSpec
  ( tests
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Maybe
  ( fromMaybe
  )
import Playwright
  ( chromium
  , defaultLaunchServerOptions
  , launchServer
  , launchedServerPid
  , waitLaunchedServer
  )
import Playwright.Harness
  ( assertEqual
  , withChromiumBrowser
  )
import System.Exit
  ( ExitCode (ExitFailure, ExitSuccess)
  )
import System.Process
  ( callProcess
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

-- Port of tests/library/signals.spec.ts. Upstream starts the server inside a
-- Node parent process and inspects that parent's exit code and signal. This
-- client has no such parent. The tests send the signal to the driver process
-- that runs `launch-server` (see 'launchedServerPid') and check how that
-- process exits.
--
-- Skipped upstream cases:
--   "should close the browser when the node process closes": the test closes
--     the Node parent process. There is no Node parent in Haskell.
--   "should remove temp dir on process.exit": needs the upstream temp dir and
--     a process.exit hook in the Node parent. Not observable from Haskell.
--   "should report browser close signal 2": needs the browser 'close' event
--     with exitCode and signal from the browser process. launchServer does
--     not expose browser events or the browser pid.
--   "should kill the browser on double SIGINT and remove temp dir": needs the
--     temp dir and the Node parent's exit code, as above.
--   "should kill the browser on SIGINT + SIGTERM": the second signal must
--     arrive while the first graceful close is still running. The driver
--     exits with 130 before a second signal can be sent reliably, so the
--     case is timing dependent and the browser's SIGKILL is not observable.
--   "should kill the browser on SIGTERM + SIGINT": same timing dependency.
--   "should not prevent default SIGTERM handling after browser close": needs
--     the Node parent's signal, as above.

tests :: TestTree
tests =
  testGroup
    "signals"
    [ testCase "should close the browser on SIGTERM" $
        withChromiumBrowser $ \_browser -> do
          exitCode <- signalServer "TERM"
          assertEqual "driver exit code after SIGTERM" ExitSuccess exitCode
    , testCase "should close the browser on SIGINT" $
        withChromiumBrowser $ \_browser -> do
          exitCode <- signalServer "INT"
          assertEqual "driver exit code after SIGINT" (ExitFailure 130) exitCode
    , testCase "should close the browser on SIGHUP" $
        withChromiumBrowser $ \_browser -> do
          exitCode <- signalServer "HUP"
          assertEqual "driver exit code after SIGHUP" ExitSuccess exitCode
    ]
  where
    signalServer signalName = do
      browserType <- chromium
      server <- launchServer browserType defaultLaunchServerOptions
      pid <- fromMaybe 0 <$> launchedServerPid server
      liftIO (callProcess "kill" ["-" <> signalName, show pid])
      waitLaunchedServer server

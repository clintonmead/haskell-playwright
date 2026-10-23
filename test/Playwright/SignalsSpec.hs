module Playwright.SignalsSpec
  ( tests
  ) where

import Test.Tasty
  ( TestTree
  , testGroup
  )

-- Skipped upstream cases (tests/library/signals.spec.ts). All ten use the
-- `startRemoteServer` fixture, which runs `browserType.launchServer` inside a
-- separate Node child process, then checks what happens to the browser when
-- that process is closed, killed, or sent a signal. The Haskell client has
-- no `launchServer`, and no way to start that Node-side server process.
--
--   "should close the browser when the node process closes"
--   "should remove temp dir on process.exit"
--   "should report browser close signal 2"
--   "should close the browser on SIGINT"
--   "should close the browser on SIGTERM"
--   "should close the browser on SIGHUP"
--   "should kill the browser on double SIGINT and remove temp dir"
--   "should kill the browser on SIGINT + SIGTERM"
--   "should kill the browser on SIGTERM + SIGINT"
--   "should not prevent default SIGTERM handling after browser close"

tests :: TestTree
tests =
  testGroup
    "signals"
    []

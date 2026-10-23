module Playwright.LauncherSpec
  ( tests
  ) where

import Test.Tasty
  ( TestTree
  , testGroup
  )

-- Skipped upstream cases (tests/library/launcher.spec.ts):
--   "should have an errors object": the Haskell client has no `playwright.errors`
--     namespace. Errors are PlaywrightError constructors.
--   "should have a devices object": the Haskell client has no device descriptor
--     registry (`playwright.devices`).
--   "should kill browser process on timeout after close": relies on the Node-only
--     `__testHookGracefullyClose` / `__testHookBrowserCloseTimeout` launch hooks.
--   "should throw a friendly error if its headed and there is no xserver on linux
--     running": the driver only rewrites Chromium's startup log into the
--     "Looks like you launched a headed browser without having a XServer running"
--     message when Chromium logs "Missing X server". On this host Chromium 1248
--     starts headed with DISPLAY unset (checked by running the binary directly
--     with `env -u DISPLAY`), so the message never appears. A port would be
--     asserted with `assertProtocolContainsAll` after `unsetEnv "DISPLAY"`, and
--     it needs a host without a display server that makes Chromium fail.

tests :: TestTree
tests =
  testGroup
    "launcher"
    []

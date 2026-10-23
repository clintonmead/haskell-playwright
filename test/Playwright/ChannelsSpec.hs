module Playwright.ChannelsSpec
  ( tests
  ) where

import Test.Tasty
  ( TestTree
  , testGroup
  )

-- Skipped upstream cases (tests/library/channels.spec.ts). All six depend on
-- Node-only internals, so none has a Haskell equivalent:
--   "should scope context handles": compares `_debugScopeState()` trees of the
--     client and driver dispatcher objects (the `expectScopeState` fixture).
--   "should scope CDPSession handles": same `_debugScopeState()` comparison.
--   "should scope browser handles": same `_debugScopeState()` comparison.
--   "should not generate dispatchers for subresources w/o listeners": same
--     `_debugScopeState()` comparison.
--   "should work with the domain module": uses Node's `domain` module to catch
--     errors thrown by WebSocket socket errors. No Haskell counterpart.
--   "exposeFunction should not leak": compares the full dispatcher scope tree
--     after `exposeFunction` through `_debugScopeState()`.
--
-- The Haskell client has no `_debugScopeState()` hook, so these cases stay
-- out of the suite until the driver exposes an equivalent introspection
-- channel. Recorded in progress/FUNCTIONALITY_TODO.md.

tests :: TestTree
tests =
  testGroup
    "channels"
    []

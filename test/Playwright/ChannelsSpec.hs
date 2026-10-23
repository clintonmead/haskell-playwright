module Playwright.ChannelsSpec
  ( tests
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import Data.IORef
  ( modifyIORef'
  , newIORef
  , readIORef
  )
import Playwright
  ( BindingName (BindingName)
  , evaluate
  , exposeFunction
  , defaultGotoOptions
  , goto
  , undefinedArgument
  , urlFromText
  )
import Playwright.Harness
  ( assertEqual
  , withAssets
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

-- Upstream cases from tests/library/channels.spec.ts.
--
-- Ported (partial):
--   "exposeFunction should not leak": the call-count half. The upstream
--     `expectScopeState` check after the calls is skipped, see below.
--
-- Skipped upstream cases:
--   "should scope context handles", "should scope CDPSession handles",
--   "should scope browser handles", "should not generate dispatchers for
--   subresources w/o listeners": these compare `_debugScopeState()` trees. That
--     method is an in-process JS method on the client and server dispatcher
--     objects (packages/playwright-core/src/client/channelOwner.ts and
--     server/dispatchers/dispatcher.ts), not a protocol message. A remote
--     client cannot call it, so there is nothing to port.
--   "should work with the domain module": needs Node's `domain` module to
--     catch WebSocket errors. Its socket-error assertion is already ported as
--     WebSocketSpec "should emit error".
--   "exposeFunction should not leak" (scope-state half): the same in-process
--     `_debugScopeState()` comparison as above.
--
-- Recorded in progress/FUNCTIONALITY_TODO.md.

tests :: TestTree
tests =
  testGroup
    "channels"
    [ testCase "exposeFunction should not leak" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          called <- liftIO (newIORef (0 :: Int))
          _ <-
            exposeFunction page (BindingName "myFunction") $ \_ -> do
              liftIO (modifyIORef' called (+ 1))
              pure undefinedArgument
          _ <-
            evaluate
              page
              "async () => { for (let i = 0; i < 10; ++i) await window.myFunction({ foo: 'bar' }); }"
          count <- liftIO (readIORef called)
          assertEqual "called" (10 :: Int) count
    ]

module Playwright.PageListenersSpec
  ( tests
  ) where

import Data.List
  ( isInfixOf
  )
import Playwright
  ( ListenerRemoval (RemoveIgnoringErrors, RemoveWaiting)
  , evaluate
  , onConsole
  , removeAllConsoleListeners
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( async
  , wait
  , withAsync
  )
import UnliftIO.Concurrent
  ( threadDelay
  )
import UnliftIO.Exception
  ( SomeException
  , throwIO
  , try
  )
import UnliftIO.IORef
  ( newIORef
  , readIORef
  , writeIORef
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , putMVar
  , takeMVar
  )

-- Upstream: tests/page/page-listeners.spec.ts. The handlers here run on the
-- handler queue and block on a release, so each evaluate runs in an async
-- while the test thread drives the handler. Otherwise evaluate would wait for
-- the blocked handler and never return.
tests :: TestTree
tests =
  testGroup
    "page-listeners"
    [ testCase "should not throw with ignoreErrors" $
        withChromiumPage $ \page -> do
          reached <- newEmptyMVar
          release <- newEmptyMVar
          onConsole page $ \_ -> do
            putMVar reached ()
            takeMVar release
            throwIO (userError "Error in console handler")
          withAsync (evaluate page "() => console.log(1)") $ \evaluated -> do
            takeMVar reached
            removeAllConsoleListeners page RemoveIgnoringErrors
            putMVar release ()
            _ <- wait evaluated
            pure ()
    , testCase "should wait" $
        withChromiumPage $ \page -> do
          reached <- newEmptyMVar
          value <- newIORef (0 :: Int)
          onConsole page $ \_ -> do
            writeIORef value 42
            putMVar reached ()
          withAsync (evaluate page "() => console.log(1)") $ \evaluated -> do
            takeMVar reached
            removeAllConsoleListeners page RemoveWaiting
            _ <- wait evaluated
            assertEqual "value" 42 =<< readIORef value
    , testCase "wait should throw" $
        withChromiumPage $ \page -> do
          reached <- newEmptyMVar
          release <- newEmptyMVar
          onConsole page $ \_ -> do
            putMVar reached ()
            takeMVar release
            throwIO (userError "Error in handler")
          withAsync (evaluate page "() => console.log(1)") $ \evaluated -> do
            takeMVar reached
            removing <- async (removeAllConsoleListeners page RemoveWaiting)
            -- Let the removal register before the handler finishes, so its
            -- error is collected for the removal rather than dropped.
            threadDelay 200000
            putMVar release ()
            outcome <- try (wait removing)
            _ <- wait evaluated
            case outcome of
              Left (err :: SomeException) ->
                assertBool ("error: " <> show err) ("Error in handler" `isInfixOf` show err)
              Right () ->
                assertFailure "expected the removal to rethrow the handler error"
    ]

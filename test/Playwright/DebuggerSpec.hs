module Playwright.DebuggerSpec
  ( tests
  ) where

import Data.Text qualified as T
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( Async
  , wait
  , withAsync
  )
import Playwright
  ( Debugger
  , PausedDetails (pausedDetailsTitle)
  , Playwright
  , armPausedStateChanged
  , click
  , debuggerFromContext
  , debuggerRequestPause
  , debuggerResume
  , pausePage
  , setContent
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , locate
  , withChromiumContext
  )

-- Official cases that stay out of this spec:
--
-- * "should stream api calls via _enable", "should report error status for
--   failed api calls", "should expose the action point while paused on an
--   input action", "should replay ongoing calls when enabled late" and
--   "should keep pause working when api calls are enabled": every
--   apiCallsUpdated event made by the pinned driver (playwright-core 1.64.1)
--   carries a numeric apiCalls[].id, which the driver's own protocol
--   validator rejects with "apiCalls[0].id: expected string, got number". The
--   driver process dies on that throw, so these cases cannot run.
--   Haskell's debuggerEnable and onApiCallsUpdated are implemented and
--   exported, but cannot be exercised until the driver is fixed.
-- * "should pause at next and resume" and "should pause at pause call" are
--   ported below.
-- * "should pause on runTo": debuggerRunTo is implemented, but the case needs
--   call-site file/line metadata that the Haskell client does not supply.
-- * The two internal-call cases: the Haskell client has no public internal
--   wrapper that issues internal API calls.

tests :: TestTree
tests =
  testGroup
    "debugger"
    [ testCase "should pause at next and resume" $
        withChromiumContext $ \ctx page -> do
          setContent page "<div>click me</div>"
          dbg <- debuggerFromContext ctx
          debuggerRequestPause dbg
          waitPaused <- armPausedStateChanged dbg
          withAsync (click (locate page "div")) $ \clicking -> do
            details <- requirePaused =<< waitPaused
            assertBool "paused on the click" (T.isInfixOf "Click" (pausedDetailsTitle details))
            resumeAndFinish dbg clicking
    , testCase "should pause at pause call" $
        withChromiumContext $ \ctx page -> do
          setContent page "<div>click me</div>"
          dbg <- debuggerFromContext ctx
          debuggerRequestPause dbg
          waitPaused <- armPausedStateChanged dbg
          withAsync (pausePage page) $ \pausing -> do
            details <- requirePaused =<< waitPaused
            assertBool "paused on the pause call" (T.isInfixOf "Pause" (pausedDetailsTitle details))
            resumeAndFinish dbg pausing
    ]

requirePaused :: Maybe PausedDetails -> Playwright PausedDetails
requirePaused = maybe (assertFailure "expected the debugger to be paused") pure

-- | Resumes the debugger, checks the resume event clears the paused details,
-- and waits for the paused action to finish.
resumeAndFinish :: Debugger -> Async () -> Playwright ()
resumeAndFinish dbg action = do
  waitResumed <- armPausedStateChanged dbg
  debuggerResume dbg
  resumed <- waitResumed
  assertEqual "resume clears the paused details" Nothing (fmap pausedDetailsTitle resumed)
  wait action

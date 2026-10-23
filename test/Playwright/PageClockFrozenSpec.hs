module Playwright.PageClockFrozenSpec
  ( tests
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import System.Environment
  ( lookupEnv
  )
import Playwright
  ( ClockTime (ClockTimeMillis)
  , clockInstallAt
  , clockMillisFromInt
  , clockPauseAt
  , evaluateNumber
  )
import Playwright.Harness
  ( assertEqual
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

-- The upstream suite runs these under a CI matrix that sets PW_CLOCK. Upstream
-- installs the clock on every context when PW_CLOCK is set; here each test
-- installs the clock itself, and it only runs when PW_CLOCK selects it.
tests :: TestTree
tests =
  testGroup
    "page-clock.frozen"
    [ testCase "clock should be frozen" $
        withClockMode "frozen" $
          withChromiumPage $ \page -> do
            clockInstallAt page (ClockTimeMillis (clockMillisFromInt 0))
            clockPauseAt page (ClockTimeMillis (clockMillisFromInt 1000))
            assertEqual "now" (1000 :: Double) =<< evaluateNumber page "() => Date.now()"
    , testCase "clock should be realtime" $
        withClockMode "realtime" $
          withChromiumPage $ \page -> do
            clockInstallAt page (ClockTimeMillis (clockMillisFromInt 0))
            now <- evaluateNumber page "() => Date.now()"
            assertEqual "under 10000" True (now < 10000)
    ]

-- | Runs the action only when PW_CLOCK equals the given mode, mirroring
-- upstream's `it.skip(process.env.PW_CLOCK !== mode)`.
withClockMode :: String -> IO () -> IO ()
withClockMode mode action = do
  value <- lookupEnv "PW_CLOCK"
  if value == Just mode
    then action
    else liftIO (putStrLn ("skipped: needs PW_CLOCK=" <> mode))

module Playwright.PageClockSpec
  ( tests
  ) where

import Control.Concurrent.Async
  ( wait
  , withAsync
  )
import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVarIO
  )
import Playwright
  ( BindingName (BindingName)
  , ClockMillis (ClockMillis)
  , ClockTicks (ClockTicksMillis, ClockTicksString)
  , ClockTime (ClockTimeMillis)
  , Page
  , clockFastForward
  , clockInstall
  , clockInstallAt
  , clockMillisFromInt
  , clockPauseAt
  , clockRunFor
  , clockSetFixedTime
  , clockSetSystemTime
  , defaultGotoOptions
  , evaluate
  , evaluateBool
  , evaluateNumber
  , exposeFunction
  , goto
  , milliseconds
  , urlFromText
  , waitForPopup
  , waitForTimeout
  )
import Playwright.Harness
  ( assertProtocolContains
  , withChromiumPage
  )
import Playwright.SerializedArgument
  ( undefinedArgument
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadNumber)
  , SerializedValue (SerializedNode)
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , assertFailure
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-clock"
    [ testGroup
        "runFor"
        [ testCase "triggers immediately without specified delay" $
            withPausedClock $ \page calls -> do
              _ <- evaluate page "async () => { setTimeout(window.stub); }"
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 0))
              assertCallCount calls 1
        , testCase "does not trigger without sufficient delay" $
            withPausedClock $ \page calls -> do
              _ <- evaluate page "async () => { setTimeout(window.stub, 100); }"
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 10))
              assertNoCalls calls
        , testCase "triggers after sufficient delay" $
            withPausedClock $ \page calls -> do
              _ <- evaluate page "async () => { setTimeout(window.stub, 100); }"
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 100))
              assertCallCount calls 1
        , testCase "triggers simultaneous timers" $
            withPausedClock $ \page calls -> do
              _ <-
                evaluate
                  page
                  "async () => { setTimeout(window.stub, 100); setTimeout(window.stub, 100); }"
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 100))
              assertCallCount calls 2
        , testCase "waits after setTimeout was called" $
            withPausedClock $ \page calls -> do
              _ <- evaluate page "async () => { setTimeout(window.stub, 150); }"
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 50))
              assertNoCalls calls
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 100))
              assertCallCount calls 1
        , testCase "triggers event when some throw" $
            withPausedClock $ \page calls -> do
              _ <-
                evaluate
                  page
                  "async () => { setTimeout(() => { throw new Error(); }, 100); setTimeout(window.stub, 120); }"
              assertProtocolContains
                "Error"
                (clockRunFor page (ClockTicksMillis (clockMillisFromInt 120)))
              assertCallCount calls 1
        , testCase "creates updated Date while ticking" $
            withPausedClock $ \page calls -> do
              clockSetSystemTime page (ClockTimeMillis (clockMillisFromInt 0))
              _ <-
                evaluate
                  page
                  "async () => { setInterval(() => { window.stub(new Date().getTime()); }, 10); }"
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 100))
              listed <- readTVarIO calls
              case traverse callNumber listed of
                Nothing ->
                  assertFailure "expected numeric stub args"
                Just numbers ->
                  assertEqual
                    "ticks"
                    ([10, 20, 30, 40, 50, 60, 70, 80, 90, 100] :: [Double])
                    numbers
        , testCase "passes 8 seconds" $
            withPausedClock $ \page calls -> do
              _ <- evaluate page "async () => { setInterval(window.stub, 4000); }"
              clockRunFor page (ClockTicksString "08")
              assertCallCount calls 2
        , testCase "returns the current now value" $
            withPausedClock $ \page _ -> do
              clockSetSystemTime page (ClockTimeMillis (clockMillisFromInt 0))
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 200))
              assertEqual "now" (200 :: Double) =<< evaluateNumber page "() => Date.now()"
        , testCase "throws for invalid format" $
            withPausedClock $ \page _ -> do
              _ <- evaluate page "async () => { setInterval(window.stub, 10000); }"
              assertProtocolContains
                "Clock only understands numbers"
                (clockRunFor page (ClockTicksString "12:02:34:10"))
        ]
    , testGroup
        "fastForward"
        [ testCase "ignores timers which would not be run" $
            withPausedClock $ \page calls -> do
              _ <-
                evaluate
                  page
                  "async () => { setTimeout(() => { window.stub('should not be logged'); }, 1000); }"
              clockFastForward page (ClockTicksMillis (clockMillisFromInt 500))
              assertNoCalls calls
        , testCase "pushes back execution time for skipped timers" $
            withPausedClock $ \page calls -> do
              _ <-
                evaluate
                  page
                  "async () => { setTimeout(() => { window.stub(Date.now()); }, 1000); }"
              clockFastForward page (ClockTicksMillis (clockMillisFromInt 2000))
              listed <- readTVarIO calls
              case listed of
                [args] ->
                  assertEqual "now" (Just (3000 :: Double)) (callNumber args)
                other ->
                  assertFailure ("expected one stub call, got " ++ show (length other))
        , testCase "supports string time arguments" $
            withPausedClock $ \page calls -> do
              _ <-
                evaluate
                  page
                  "async () => { setTimeout(() => { window.stub(Date.now()); }, 100000); }"
              clockFastForward page (ClockTicksString "01:50")
              listed <- readTVarIO calls
              case listed of
                [args] ->
                  assertEqual "now" (Just (111000 :: Double)) (callNumber args)
                other ->
                  assertFailure ("expected one stub call, got " ++ show (length other))
        ]
    , testGroup
        "stubTimers"
        [ testCase "sets initial timestamp" $
            withPausedClock $ \page _ -> do
              clockSetSystemTime page (ClockTimeMillis (clockMillisFromInt 1400))
              assertEqual "now" (1400 :: Double) =<< evaluateNumber page "() => Date.now()"
        , testCase "replaces global setTimeout" $
            withPausedClock $ \page calls -> do
              _ <- evaluate page "async () => { setTimeout(window.stub, 1000); }"
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 1000))
              assertCallCount calls 1
        , testCase "replaces global clearTimeout" $
            withPausedClock $ \page calls -> do
              _ <- evaluate page "async () => { const to = setTimeout(window.stub, 1000); clearTimeout(to); }"
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 1000))
              assertNoCalls calls
        , testCase "replaces global setInterval" $
            withPausedClock $ \page calls -> do
              _ <- evaluate page "async () => { setInterval(window.stub, 500); }"
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 1000))
              assertCallCount calls 2
        , testCase "fakes Date constructor" $
            withPausedClock $ \page _ ->
              assertEqual "now" (1000 :: Double) =<< evaluateNumber page "() => new Date().getTime()"
        ]
    , testGroup
        "setFixedTime"
        [ testCase "does not fake methods" $
            withChromiumPage $ \page -> do
              clockSetFixedTime page (ClockTimeMillis (clockMillisFromInt 0))
              _ <- evaluate page "() => new Promise(f => setTimeout(f, 1))"
              pure ()
        , testCase "allows setting time multiple times" $
            withChromiumPage $ \page -> do
              clockSetFixedTime page (ClockTimeMillis (clockMillisFromInt 100))
              assertEqual "first" (100 :: Double) =<< evaluateNumber page "() => Date.now()"
              clockSetFixedTime page (ClockTimeMillis (clockMillisFromInt 200))
              assertEqual "second" (200 :: Double) =<< evaluateNumber page "() => Date.now()"
        , testCase "fixed time is not affected by clock manipulation" $
            withChromiumPage $ \page -> do
              clockSetFixedTime page (ClockTimeMillis (clockMillisFromInt 100))
              assertEqual "before" (100 :: Double) =<< evaluateNumber page "() => Date.now()"
              clockFastForward page (ClockTicksMillis (clockMillisFromInt 20))
              assertEqual "after" (100 :: Double) =<< evaluateNumber page "() => Date.now()"
        ]
    , testGroup
        "while running"
        [ testCase "should progress time" $
            withChromiumPage $ \page -> do
              clockInstallAt page (ClockTimeMillis (clockMillisFromInt 0))
              _ <- goto page (urlFromText "data:text/html,") defaultGotoOptions
              sleepMillis page 1000
              now <- evaluateNumber page "() => Date.now()"
              assertEqual "lower" True (now >= 1000)
              assertEqual "upper" True (now <= 2000)
        , testCase "should runFor" $
            withChromiumPage $ \page -> do
              clockInstallAt page (ClockTimeMillis (clockMillisFromInt 0))
              _ <- goto page (urlFromText "data:text/html,") defaultGotoOptions
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 10000))
              now <- evaluateNumber page "() => Date.now()"
              assertEqual "lower" True (now >= 10000)
              assertEqual "upper" True (now <= 11000)
        , testCase "should pause and fastForward" $
            withChromiumPage $ \page -> do
              clockInstallAt page (ClockTimeMillis (clockMillisFromInt 0))
              _ <- goto page (urlFromText "data:text/html,") defaultGotoOptions
              clockPauseAt page (ClockTimeMillis (clockMillisFromInt 1000))
              clockFastForward page (ClockTicksMillis (clockMillisFromInt 1000))
              assertEqual "now" (2000 :: Double) =<< evaluateNumber page "() => Date.now()"
        , testCase "should set system time on pause" $
            withChromiumPage $ \page -> do
              clockInstallAt page (ClockTimeMillis (clockMillisFromInt 0))
              _ <- goto page (urlFromText "data:text/html,") defaultGotoOptions
              clockPauseAt page (ClockTimeMillis (clockMillisFromInt 1000))
              assertEqual "now" (1000 :: Double) =<< evaluateNumber page "() => Date.now()"
        ]
    , testGroup
        "popup"
        [ testCase "should tick after popup" $
            withChromiumPage $ \page -> do
              clockInstallAt page (ClockTimeMillis (clockMillisFromInt 0))
              clockPauseAt page (ClockTimeMillis (ClockMillis 1443139200000))
              withAsync (waitForPopup page) $ \handle -> do
                _ <- evaluate page "() => window.open('about:blank')"
                popup <- wait handle
                assertEqual "popup" (1443139200000 :: Double) =<< evaluateNumber popup "() => Date.now()"
                clockRunFor page (ClockTicksMillis (clockMillisFromInt 1000))
                assertEqual "after" (1443139201000 :: Double) =<< evaluateNumber popup "() => Date.now()"
        ]
    , testGroup
        "Date.now"
        [ testCase "check Date.now is an integer" $
            withChromiumPage $ \page -> do
              clockInstall page
              _ <- goto page (urlFromText "data:text/html,") defaultGotoOptions
              sleepMillis page 1000
              assertEqual "integer" True =<< evaluateBool page "() => Number.isInteger(Date.now())"
        , testCase "check Date.now is an integer after fractional runFor" $
            withPausedClock $ \page _ -> do
              clockRunFor page (ClockTicksMillis (ClockMillis (1 / 2)))
              assertEqual "now" (1001 :: Double) =<< evaluateNumber page "() => Date.now()"
        ]
    ]

withPausedClock :: (Page -> TVar [[SerializedValue]] -> IO a) -> IO a
withPausedClock action =
  withChromiumPage $ \page -> do
    calls <- newTVarIO ([] :: [[SerializedValue]])
    _ <-
      exposeFunction page (BindingName "stub") $ \args -> do
        atomically (modifyTVar' calls (++ [args]))
        pure undefinedArgument
    clockInstallAt page (ClockTimeMillis (clockMillisFromInt 0))
    clockPauseAt page (ClockTimeMillis (clockMillisFromInt 1000))
    action page calls

assertCallCount :: TVar [[SerializedValue]] -> Int -> IO ()
assertCallCount var expected = do
  listed <- readTVarIO var
  assertEqual "calls" expected (length listed)

assertNoCalls :: TVar [[SerializedValue]] -> IO ()
assertNoCalls var = do
  listed <- readTVarIO var
  assertEqual "calls" (0 :: Int) (length listed)

sleepMillis :: Page -> Double -> IO ()
sleepMillis page n =
  case milliseconds n of
    Nothing ->
      assertFailure "timeout"
    Just timeout ->
      waitForTimeout page timeout

callNumber :: [SerializedValue] -> Maybe Double
callNumber args =
  case args of
    [SerializedNode (PayloadNumber number) _] ->
      Just number
    _ ->
      Nothing

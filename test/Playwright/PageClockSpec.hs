module Playwright.PageClockSpec
  ( tests
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Text
  ( Text
  )
import GHC.Clock
  ( getMonotonicTime
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , responseLBS
  )
import UnliftIO.Async
  ( wait
  , withAsync
  )
import UnliftIO.STM
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
  , ClockTime (ClockTimeMillis, ClockTimeString)
  , ConsoleMessage (consoleMessageText)
  , LoadState (LoadStateLoad)
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
  , setContent
  , urlFromText
  , waitForConsoleWhere
  , waitForLoadState
  , waitForPopup
  , waitForTimeout
  , Playwright
  )
import Playwright.Harness
  ( assertEqual
  , assertFailure
  , assertProtocolContains
  , withChromiumPage
  , withLocalApp
  )
import Playwright.SerializedArgument
  ( undefinedArgument
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadNumber, PayloadString)
  , SerializedValue (SerializedNode)
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
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
        , testCase "triggers multiple simultaneous timers" $
            withPausedClock $ \page calls -> do
              _ <-
                evaluate
                  page
                  "async () => { setTimeout(window.stub, 100); setTimeout(window.stub, 100); setTimeout(window.stub, 99); setTimeout(window.stub, 100); }"
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 100))
              assertCallCount calls 4
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
        , testCase "passes 1 minute" $
            withPausedClock $ \page calls -> do
              _ <- evaluate page "async () => { setInterval(window.stub, 6000); }"
              clockRunFor page (ClockTicksString "01:00")
              assertCallCount calls 10
        , testCase "passes 2 hours, 34 minutes and 10 seconds" $
            withPausedClock $ \page calls -> do
              _ <- evaluate page "async () => { setInterval(window.stub, 10000); }"
              clockRunFor page (ClockTicksString "02:34:10")
              assertCallCount calls 925
        , testCase "returns the current now value" $
            withPausedClock $ \page _ -> do
              clockSetSystemTime page (ClockTimeMillis (clockMillisFromInt 0))
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 200))
              assertEqual "now" (200 :: Double) =<< evaluateNumber page "() => Date.now()"
        , testCase "throws for invalid format" $
            withPausedClock $ \page calls -> do
              _ <- evaluate page "async () => { setInterval(window.stub, 10000); }"
              assertProtocolContains
                "Clock only understands numbers"
                (clockRunFor page (ClockTicksString "12:02:34:10"))
              assertNoCalls calls
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
        , testCase "should throw for invalid date" $
            -- The upstream Date-object variant is not ported: ClockTime has no Date constructor.
            withPausedClock $ \page _ ->
              assertProtocolContains
                "Invalid date: invalid"
                (clockSetSystemTime page (ClockTimeString "invalid"))
        , testCase "replaces global setTimeout" $
            withPausedClock $ \page calls -> do
              _ <- evaluate page "async () => { setTimeout(window.stub, 1000); }"
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 1000))
              assertCallCount calls 1
        , testCase "global fake setTimeout should return id" $
            withPausedClock $ \page _ ->
              assertEqual "number" True
                =<< evaluateBool page "() => typeof setTimeout(window.stub, 1000) === 'number'"
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
        , testCase "replaces global clearInterval" $
            withPausedClock $ \page calls -> do
              _ <- evaluate page "async () => { const to = setInterval(window.stub, 500); clearInterval(to); }"
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 1000))
              assertNoCalls calls
        , testCase "replaces global performance.now" $
            withPausedClock $ \page _ ->
              withAsync (evaluate page performanceNowScript) $ \pending -> do
                -- Give the page time to register its fake timer before the clock is advanced.
                sleepMillis page 200
                clockRunFor page (ClockTicksMillis (clockMillisFromInt 1000))
                _ <- wait pending
                assertEqual "prev" (1000 :: Double) =<< evaluateNumber page "() => window.perfPrev"
                assertEqual "next" (2000 :: Double) =<< evaluateNumber page "() => window.perfNext"
        , testCase "fakes Date constructor" $
            withPausedClock $ \page _ ->
              assertEqual "now" (1000 :: Double) =<< evaluateNumber page "() => new Date().getTime()"
        , testCase "replaces global performance.timeOrigin" $
            withChromiumPage $ \page -> do
              clockInstallAt page (ClockTimeMillis (clockMillisFromInt 1000))
              clockPauseAt page (ClockTimeMillis (clockMillisFromInt 2000))
              withAsync (evaluate page performanceNowScript) $ \pending -> do
                sleepMillis page 200
                clockRunFor page (ClockTicksMillis (clockMillisFromInt 1000))
                _ <- wait pending
                assertEqual "timeOrigin" (1000 :: Double) =<< evaluateNumber page "() => performance.timeOrigin"
                assertEqual "prev" (1000 :: Double) =<< evaluateNumber page "() => window.perfPrev"
                assertEqual "next" (2000 :: Double) =<< evaluateNumber page "() => window.perfNext"
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
        , testCase "allows installing fake timers after settings time" $
            withStubPage $ \page calls -> do
              clockSetFixedTime page (ClockTimeMillis (clockMillisFromInt 100))
              assertEqual "first" (100 :: Double) =<< evaluateNumber page "() => Date.now()"
              clockSetFixedTime page (ClockTimeMillis (clockMillisFromInt 200))
              _ <- evaluate page "async () => { setTimeout(() => window.stub(Date.now())); }"
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 0))
              listed <- readTVarIO calls
              assertEqual "calls" [Just (200 :: Double)] (map callNumber listed)
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
        , testCase "should fastForward" $
            withChromiumPage $ \page -> do
              clockInstallAt page (ClockTimeMillis (clockMillisFromInt 0))
              start <- liftIO getMonotonicTime
              _ <- goto page (urlFromText "data:text/html,") defaultGotoOptions
              clockFastForward page (ClockTicksMillis (clockMillisFromInt 10000))
              now <- evaluateNumber page "() => Date.now()"
              end <- liftIO getMonotonicTime
              let realElapsedMillis = (end - start) * 1000
              assertEqual "lower" True (now >= 10000)
              assertEqual "upper" True (now <= 10000 + realElapsedMillis + 1000)
        , testCase "should pause" $
            withChromiumPage $ \page -> do
              clockInstallAt page (ClockTimeMillis (clockMillisFromInt 0))
              _ <- goto page (urlFromText "data:text/html,") defaultGotoOptions
              clockPauseAt page (ClockTimeMillis (clockMillisFromInt 60000))
              -- Wait in real time to make sure the clock is paused and not running.
              sleepMillis page 1111
              assertEqual "now" (60000 :: Double) =<< evaluateNumber page "() => Date.now()"
        , testCase "should reject an invalid target time with an active requestAnimationFrame loop" $
            withChromiumPage $ \page -> do
              clockInstall page
              setContent page "<script>function tick() { requestAnimationFrame(tick); } requestAnimationFrame(tick);</script>"
              now <- evaluateNumber page "() => Date.now()"
              let invalidTime = ClockMillis (fromIntegral (round (now * 1000000) :: Integer))
              assertProtocolContains
                "Invalid date"
                (clockPauseAt page (ClockTimeMillis invalidTime))
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
        "while on pause"
        [ testCase "fastForward should not run nested immediate" $
            withNavigatedPausedClock $ \page calls -> do
              _ <-
                evaluate
                  page
                  "() => { setTimeout(() => { window.stub('outer'); setTimeout(() => window.stub('inner'), 0); }, 1000); }"
              clockFastForward page (ClockTicksMillis (clockMillisFromInt 1000))
              assertTextCalls calls ["outer"]
              clockFastForward page (ClockTicksMillis (clockMillisFromInt 1))
              assertTextCalls calls ["outer", "inner"]
        , testCase "runFor should not run nested immediate" $
            withNavigatedPausedClock $ \page calls -> do
              _ <-
                evaluate
                  page
                  "() => { setTimeout(() => { window.stub('outer'); setTimeout(() => window.stub('inner'), 0); }, 1000); }"
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 1000))
              assertTextCalls calls ["outer"]
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 1))
              assertTextCalls calls ["outer", "inner"]
        , testCase "runFor should not run nested immediate from microtask" $
            withNavigatedPausedClock $ \page calls -> do
              _ <-
                evaluate
                  page
                  "() => { setTimeout(() => { window.stub('outer'); void Promise.resolve().then(() => setTimeout(() => window.stub('inner'), 0)); }, 1000); }"
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 1000))
              assertTextCalls calls ["outer"]
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 1))
              assertTextCalls calls ["outer", "inner"]
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
        , testCase "should tick before popup" $
            withChromiumPage $ \page -> do
              clockInstallAt page (ClockTimeMillis (clockMillisFromInt 0))
              clockPauseAt page (ClockTimeMillis (ClockMillis 1443139200000))
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 1000))
              withAsync (waitForPopup page) $ \handle -> do
                _ <- evaluate page "() => window.open('about:blank')"
                popup <- wait handle
                assertEqual "popup" (1443139201000 :: Double) =<< evaluateNumber popup "() => Date.now()"
        , testCase "should run time before popup" $
            withChromiumPage $ \page ->
              withLocalApp popupApp $ \prefix -> do
                _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
                -- Wait for 2 second in real life to check that it is past in popup.
                sleepMillis page 2000
                withAsync (waitForPopup page) $ \handle -> do
                  _ <- evaluate page ("() => window.open('" <> prefix <> "/popup.html')")
                  popup <- wait handle
                  waitForLoadState popup LoadStateLoad
                  assertEqual "time" True =<< evaluateBool popup "() => window.time >= 2000"
        , testCase "should not run time before popup on pause" $
            withChromiumPage $ \page ->
              withLocalApp popupApp $ \prefix -> do
                clockInstallAt page (ClockTimeMillis (clockMillisFromInt 0))
                clockPauseAt page (ClockTimeMillis (clockMillisFromInt 1000))
                _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
                -- Wait for 2 second in real life to check that it is past in popup.
                sleepMillis page 2000
                withAsync (waitForPopup page) $ \handle -> do
                  _ <- evaluate page ("() => window.open('" <> prefix <> "/popup.html')")
                  popup <- wait handle
                  waitForLoadState popup LoadStateLoad
                  assertEqual "time" (1000 :: Double) =<< evaluateNumber popup "() => window.time"
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
    , testGroup
        "AbortSignal"
        [ testCase "AbortSignal.timeout" $
            withChromiumPage $ \page -> do
              clockInstallAt page (ClockTimeMillis (clockMillisFromInt 0))
              _ <-
                evaluate
                  page
                  "() => { const signal = AbortSignal.any([AbortSignal.timeout(100)]); const handle = { signal, event: false, handler: false }; signal.addEventListener('abort', () => handle.event = true); signal.onabort = () => handle.handler = true; window.abortHandle = handle; }"
              assertEqual "before" True
                =<< evaluateBool page "() => window.abortHandle.signal.aborted === false && window.abortHandle.event === false && window.abortHandle.handler === false"
              clockRunFor page (ClockTicksMillis (clockMillisFromInt 200))
              assertEqual "after" True
                =<< evaluateBool
                  page
                  "() => window.abortHandle.signal.aborted === true && window.abortHandle.event === true && window.abortHandle.handler === true && window.abortHandle.signal.reason.name === 'TimeoutError' && window.abortHandle.signal.reason.message === 'signal timed out' && window.abortHandle.signal.reason.code === 23"
              assertEqual "abort" True =<< evaluateBool page "() => AbortSignal.abort().aborted"
        ]
    , testGroup
        "blocking execution"
        [ testCase "correctly increments Date.now()/performance.now() during blocking execution" $
            withChromiumPage $ \page ->
              withLocalApp blockingReproApp $ \prefix -> do
                clockSetSystemTime page (ClockTimeMillis (ClockMillis 1767225600000))
                withAsync (waitForConsoleWhere page (\message -> consoleMessageText message == "done")) $ \pending -> do
                  _ <- goto page (urlFromText (prefix <> "/repro.html")) defaultGotoOptions
                  _ <- wait pending
                  pure ()
        ]
    ]

performanceNowScript :: Text
performanceNowScript =
  "async () => { window.perfPrev = performance.now(); await new Promise(f => setTimeout(f, 1000)); window.perfNext = performance.now(); }"

popupApp :: Application
popupApp _ respond =
  respond (responseLBS status200 [(hContentType, "text/html")] "<script>window.time = Date.now()</script>")

blockingReproApp :: Application
blockingReproApp _ respond =
  respond
    ( responseLBS
        status200
        [(hContentType, "text/html")]
        "<html><body><script>{ const start = performance.now(); while (performance.now() - start < 100) { } } { const start = Date.now(); while (Date.now() - start < 100) { } } console.log('done');</script></body></html>"
    )

withStubPage :: (Page -> TVar [[SerializedValue]] -> Playwright a) -> IO a
withStubPage action =
  withChromiumPage $ \page -> do
    calls <- newTVarIO ([] :: [[SerializedValue]])
    _ <-
      exposeFunction page (BindingName "stub") $ \args -> do
        atomically (modifyTVar' calls (++ [args]))
        pure undefinedArgument
    action page calls

withPausedClock :: (Page -> TVar [[SerializedValue]] -> Playwright a) -> IO a
withPausedClock action =
  withStubPage $ \page calls -> do
    clockInstallAt page (ClockTimeMillis (clockMillisFromInt 0))
    clockPauseAt page (ClockTimeMillis (clockMillisFromInt 1000))
    action page calls

withNavigatedPausedClock :: (Page -> TVar [[SerializedValue]] -> Playwright a) -> IO a
withNavigatedPausedClock action =
  withStubPage $ \page calls -> do
    clockInstallAt page (ClockTimeMillis (clockMillisFromInt 0))
    _ <- goto page (urlFromText "data:text/html,") defaultGotoOptions
    clockPauseAt page (ClockTimeMillis (clockMillisFromInt 1000))
    action page calls

assertCallCount :: TVar [[SerializedValue]] -> Int -> Playwright ()
assertCallCount var expected = do
  listed <- readTVarIO var
  assertEqual "calls" expected (length listed)

assertNoCalls :: TVar [[SerializedValue]] -> Playwright ()
assertNoCalls var = do
  listed <- readTVarIO var
  assertEqual "calls" (0 :: Int) (length listed)

assertTextCalls :: TVar [[SerializedValue]] -> [Text] -> Playwright ()
assertTextCalls var expected = do
  listed <- readTVarIO var
  assertEqual "calls" (map Just expected) (map callText listed)

sleepMillis :: Page -> Double -> Playwright ()
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

callText :: [SerializedValue] -> Maybe Text
callText args =
  case args of
    [SerializedNode (PayloadString text) _] ->
      Just text
    _ ->
      Nothing

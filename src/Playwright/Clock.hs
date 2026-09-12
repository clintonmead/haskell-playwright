module Playwright.Clock
  ( ClockMillis (..)
  , ClockTicks (..)
  , ClockTime (..)
  , clockFastForward
  , clockInstall
  , clockInstallAt
  , clockMillisFromInt
  , clockPauseAt
  , clockResume
  , clockRunFor
  , clockSetFixedTime
  , clockSetSystemTime
  ) where

import Data.Scientific
  ( Scientific
  )
import Data.Text
  ( Text
  )
import Playwright.Channel
  ( send
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Page
  ( Page (pageContextChannel)
  )
import Playwright.Protocol
  ( BrowserContextClockFastForwardParams (browserContextClockFastForwardParamsTicksNumber, browserContextClockFastForwardParamsTicksString)
  , BrowserContextClockFastForwardResult
  , BrowserContextClockInstallParams (browserContextClockInstallParamsTimeNumber, browserContextClockInstallParamsTimeString)
  , BrowserContextClockInstallResult
  , BrowserContextClockPauseAtParams (browserContextClockPauseAtParamsTimeNumber, browserContextClockPauseAtParamsTimeString)
  , BrowserContextClockPauseAtResult
  , BrowserContextClockResumeResult
  , BrowserContextClockRunForParams (browserContextClockRunForParamsTicksNumber, browserContextClockRunForParamsTicksString)
  , BrowserContextClockRunForResult
  , BrowserContextClockSetFixedTimeParams (browserContextClockSetFixedTimeParamsTimeNumber, browserContextClockSetFixedTimeParamsTimeString)
  , BrowserContextClockSetFixedTimeResult
  , BrowserContextClockSetSystemTimeParams (browserContextClockSetSystemTimeParamsTimeNumber, browserContextClockSetSystemTimeParamsTimeString)
  , BrowserContextClockSetSystemTimeResult
  , mkBrowserContextClockFastForwardParams
  , mkBrowserContextClockInstallParams
  , mkBrowserContextClockPauseAtParams
  , mkBrowserContextClockResumeParams
  , mkBrowserContextClockRunForParams
  , mkBrowserContextClockSetFixedTimeParams
  , mkBrowserContextClockSetSystemTimeParams
  )

newtype ClockMillis = ClockMillis
  { clockMillisToScientific :: Scientific
  }
  deriving stock (Eq, Show)

data ClockTime
  = ClockTimeMillis ClockMillis
  | ClockTimeString Text
  deriving stock (Eq, Show)

data ClockTicks
  = ClockTicksMillis ClockMillis
  | ClockTicksString Text
  deriving stock (Eq, Show)

clockMillisFromInt :: Int -> ClockMillis
clockMillisFromInt value =
  ClockMillis (fromIntegral value)

clockInstall :: Page -> IO ()
clockInstall page = do
  _ :: BrowserContextClockInstallResult <-
    send (pageContextChannel page) (MethodName "clockInstall") mkBrowserContextClockInstallParams
  pure ()

clockInstallAt :: Page -> ClockTime -> IO ()
clockInstallAt page time = do
  let
    (number, string) = timeFields time
  _ :: BrowserContextClockInstallResult <-
    send
      (pageContextChannel page)
      (MethodName "clockInstall")
      mkBrowserContextClockInstallParams
        { browserContextClockInstallParamsTimeNumber = number
        , browserContextClockInstallParamsTimeString = string
        }
  pure ()

clockPauseAt :: Page -> ClockTime -> IO ()
clockPauseAt page time = do
  let
    (number, string) = timeFields time
  _ :: BrowserContextClockPauseAtResult <-
    send
      (pageContextChannel page)
      (MethodName "clockPauseAt")
      mkBrowserContextClockPauseAtParams
        { browserContextClockPauseAtParamsTimeNumber = number
        , browserContextClockPauseAtParamsTimeString = string
        }
  pure ()

clockRunFor :: Page -> ClockTicks -> IO ()
clockRunFor page ticks = do
  let
    (number, string) = tickFields ticks
  _ :: BrowserContextClockRunForResult <-
    send
      (pageContextChannel page)
      (MethodName "clockRunFor")
      mkBrowserContextClockRunForParams
        { browserContextClockRunForParamsTicksNumber = number
        , browserContextClockRunForParamsTicksString = string
        }
  pure ()

clockFastForward :: Page -> ClockTicks -> IO ()
clockFastForward page ticks = do
  let
    (number, string) = tickFields ticks
  _ :: BrowserContextClockFastForwardResult <-
    send
      (pageContextChannel page)
      (MethodName "clockFastForward")
      mkBrowserContextClockFastForwardParams
        { browserContextClockFastForwardParamsTicksNumber = number
        , browserContextClockFastForwardParamsTicksString = string
        }
  pure ()

clockResume :: Page -> IO ()
clockResume page = do
  _ :: BrowserContextClockResumeResult <-
    send (pageContextChannel page) (MethodName "clockResume") mkBrowserContextClockResumeParams
  pure ()

clockSetFixedTime :: Page -> ClockTime -> IO ()
clockSetFixedTime page time = do
  let
    (number, string) = timeFields time
  _ :: BrowserContextClockSetFixedTimeResult <-
    send
      (pageContextChannel page)
      (MethodName "clockSetFixedTime")
      mkBrowserContextClockSetFixedTimeParams
        { browserContextClockSetFixedTimeParamsTimeNumber = number
        , browserContextClockSetFixedTimeParamsTimeString = string
        }
  pure ()

clockSetSystemTime :: Page -> ClockTime -> IO ()
clockSetSystemTime page time = do
  let
    (number, string) = timeFields time
  _ :: BrowserContextClockSetSystemTimeResult <-
    send
      (pageContextChannel page)
      (MethodName "clockSetSystemTime")
      mkBrowserContextClockSetSystemTimeParams
        { browserContextClockSetSystemTimeParamsTimeNumber = number
        , browserContextClockSetSystemTimeParamsTimeString = string
        }
  pure ()

timeFields :: ClockTime -> (Maybe Scientific, Maybe Text)
timeFields time =
  case time of
    ClockTimeMillis millis ->
      (Just (clockMillisToScientific millis), Nothing)
    ClockTimeString text ->
      (Nothing, Just text)

tickFields :: ClockTicks -> (Maybe Scientific, Maybe Text)
tickFields ticks =
  case ticks of
    ClockTicksMillis millis ->
      (Just (clockMillisToScientific millis), Nothing)
    ClockTicksString text ->
      (Nothing, Just text)

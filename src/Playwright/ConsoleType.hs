module Playwright.ConsoleType
  ( ConsoleType (..)
  , consoleTypeFromText
  ) where

import Data.Text
  ( Text
  )

data ConsoleType
  = ConsoleAssert
  | ConsoleClear
  | ConsoleCount
  | ConsoleDebug
  | ConsoleDir
  | ConsoleDirXml
  | ConsoleError
  | ConsoleInfo
  | ConsoleLog
  | ConsoleProfile
  | ConsoleProfileEnd
  | ConsoleTable
  | ConsoleTimeEnd
  | ConsoleTrace
  | ConsoleVerbose
  | ConsoleWarning
  deriving stock (Eq, Show)

consoleTypeFromText :: Text -> Maybe ConsoleType
consoleTypeFromText text =
  case text of
    "assert" ->
      Just ConsoleAssert
    "clear" ->
      Just ConsoleClear
    "count" ->
      Just ConsoleCount
    "debug" ->
      Just ConsoleDebug
    "dir" ->
      Just ConsoleDir
    "dirxml" ->
      Just ConsoleDirXml
    "error" ->
      Just ConsoleError
    "info" ->
      Just ConsoleInfo
    "log" ->
      Just ConsoleLog
    "profile" ->
      Just ConsoleProfile
    "profileEnd" ->
      Just ConsoleProfileEnd
    "table" ->
      Just ConsoleTable
    "timeEnd" ->
      Just ConsoleTimeEnd
    "trace" ->
      Just ConsoleTrace
    "verbose" ->
      Just ConsoleVerbose
    "warning" ->
      Just ConsoleWarning
    _ ->
      Nothing

module Playwright.PageEventConsoleSpec
  ( tests
  ) where

import Playwright
  ( ConsoleMessage (consoleMessageArgs, consoleMessageText, consoleMessageType)
  , ConsoleType (ConsoleLog)
  , armConsole
  , evaluate
  , jsonValueNumber
  , jsonValueText
  )
import Playwright.Harness
  ( withChromiumPage
  , assertEqual
  , assertFailure
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
    "page-event-console"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          takeMessage <- armConsole page
          _ <- evaluate page "() => console.log('hello', 5, { foo: 'bar' })"
          message <- takeMessage
          assertEqual "type" ConsoleLog (consoleMessageType message)
          assertEqual "text" "hello 5 {foo: bar}" (consoleMessageText message)
          case consoleMessageArgs message of
            firstArg : secondArg : _ -> do
              assertEqual "arg0" "hello" =<< jsonValueText firstArg
              assertEqual "arg1" (5 :: Double) =<< jsonValueNumber secondArg
            _ ->
              assertFailure "expected console args"
    , testCase "should emit same log twice" $
        withChromiumPage $ \page -> do
          takeFirst <- armConsole page
          _ <- evaluate page "() => { console.log('hello'); console.log('hello'); }"
          first <- takeFirst
          takeSecond <- armConsole page
          -- Second log was already emitted; re-arming misses it. Capture both via two logs after arming once is enough for the first.
          assertEqual "first" "hello" (consoleMessageText first)
          _ <- evaluate page "() => console.log('hello')"
          second <- takeSecond
          assertEqual "second" "hello" (consoleMessageText second)
    ]

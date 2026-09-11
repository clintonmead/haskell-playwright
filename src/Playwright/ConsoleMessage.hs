module Playwright.ConsoleMessage
  ( ConsoleMessage (..)
  , consoleMessageFromEvent
  ) where

import Control.Exception
  ( throwIO
  )
import Data.Text
  ( Text
  )
import Playwright.Connection
  ( Connection
  )
import Playwright.ConsoleType
  ( ConsoleType
  , consoleTypeFromText
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  )
import Playwright.JsHandle
  ( JsHandle
  , jsHandleFromRef
  )
import Playwright.Protocol
  ( BrowserContextConsoleEvent (browserContextConsoleEventArgs, browserContextConsoleEventText, browserContextConsoleEventType)
  )

data ConsoleMessage = ConsoleMessage
  { consoleMessageType :: ConsoleType
  , consoleMessageText :: Text
  , consoleMessageArgs :: [JsHandle]
  }

consoleMessageFromEvent :: Connection -> BrowserContextConsoleEvent -> IO ConsoleMessage
consoleMessageFromEvent conn event = do
  kind <-
    case consoleTypeFromText (browserContextConsoleEventType event) of
      Just parsed ->
        pure parsed
      Nothing ->
        throwIO (DecodeFailure ("unknown console type: " <> browserContextConsoleEventType event))
  pure
    ConsoleMessage
      { consoleMessageType = kind
      , consoleMessageText = browserContextConsoleEventText event
      , consoleMessageArgs = fmap (jsHandleFromRef conn) (browserContextConsoleEventArgs event)
      }

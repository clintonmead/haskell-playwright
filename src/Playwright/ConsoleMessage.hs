module Playwright.ConsoleMessage
  ( ConsoleMessage (..)
  , consoleMessageFromEvent
  ) where

import Playwright.Monad
  ( Playwright
  )
import UnliftIO.Exception
  ( throwIO
  )
import Data.Text
  ( Text
  )
import Playwright.ConsoleType
  ( ConsoleType
  , consoleTypeFromText
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  )
import Playwright.Channel
  ( BrowserContextTag
  , Channel
  )
import Playwright.JsHandle
  ( JsHandle
  , jsHandleFromRef
  )
import Playwright.Json.ChannelRef
  ( ChannelRef
  )
import Playwright.Page
  ( Page
  , pageFromRef
  )
import Playwright.Protocol
  ( BrowserContextConsoleEvent (browserContextConsoleEventArgs, browserContextConsoleEventPage, browserContextConsoleEventText, browserContextConsoleEventTimestamp, browserContextConsoleEventType, browserContextConsoleEventWorker)
  )

data ConsoleMessage = ConsoleMessage
  { consoleMessageType :: ConsoleType
  , consoleMessageText :: Text
  , consoleMessageArgs :: [JsHandle]
  , consoleMessagePage :: Maybe Page
  , consoleMessageWorkerRef :: Maybe ChannelRef
  , -- | Milliseconds since the epoch, as in the JS client's message.timestamp().
    consoleMessageTimestamp :: Double
  }

consoleMessageFromEvent :: Channel BrowserContextTag -> BrowserContextConsoleEvent -> Playwright ConsoleMessage
consoleMessageFromEvent contextChannel event = do
  kind <-
    case consoleTypeFromText (browserContextConsoleEventType event) of
      Just parsed ->
        pure parsed
      Nothing ->
        throwIO (DecodeFailure ("unknown console type: " <> browserContextConsoleEventType event))
  matchedPage <-
    case browserContextConsoleEventPage event of
      Nothing ->
        pure Nothing
      Just ref ->
        Just <$> pageFromRef contextChannel ref
  pure
    ConsoleMessage
      { consoleMessageType = kind
      , consoleMessageText = browserContextConsoleEventText event
      , consoleMessageArgs = fmap (jsHandleFromRef) (browserContextConsoleEventArgs event)
      , consoleMessagePage = matchedPage
      , consoleMessageWorkerRef = browserContextConsoleEventWorker event
      , consoleMessageTimestamp = realToFrac (browserContextConsoleEventTimestamp event)
      }

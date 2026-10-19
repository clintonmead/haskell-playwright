module Playwright.ConsoleMessage
  ( ConsoleLocation (..)
  , ConsoleMessage (..)
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
import Playwright.Url
  ( urlToText
  )
import Playwright.Page
  ( Page
  , pageFromRef
  )
import Playwright.Protocol
  ( BrowserContextConsoleEvent (browserContextConsoleEventArgs, browserContextConsoleEventLocation, browserContextConsoleEventPage, browserContextConsoleEventText, browserContextConsoleEventTimestamp, browserContextConsoleEventType, browserContextConsoleEventWorker)
  , BrowserContextConsoleEventLocation (browserContextConsoleEventLocationColumnNumber, browserContextConsoleEventLocationLineNumber, browserContextConsoleEventLocationUrl)
  )

data ConsoleMessage = ConsoleMessage
  { consoleMessageType :: ConsoleType
  , consoleMessageText :: Text
  , consoleMessageArgs :: [JsHandle]
  , consoleMessagePage :: Maybe Page
  , consoleMessageWorkerRef :: Maybe ChannelRef
  , -- | Milliseconds since the epoch, as in the JS client's message.timestamp().
    consoleMessageTimestamp :: Double
  , -- | Where the message was logged, as in the JS client's message.location().
    consoleMessageLocation :: ConsoleLocation
  }

data ConsoleLocation = ConsoleLocation
  { consoleLocationUrl :: Text
  , consoleLocationLineNumber :: Int
  , consoleLocationColumnNumber :: Int
  }
  deriving stock (Eq, Show)

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
      , consoleMessageLocation = consoleLocationFromEvent (browserContextConsoleEventLocation event)
      }

consoleLocationFromEvent :: BrowserContextConsoleEventLocation -> ConsoleLocation
consoleLocationFromEvent location =
  ConsoleLocation
    { consoleLocationUrl = urlToText (browserContextConsoleEventLocationUrl location)
    , consoleLocationLineNumber = browserContextConsoleEventLocationLineNumber location
    , consoleLocationColumnNumber = browserContextConsoleEventLocationColumnNumber location
    }

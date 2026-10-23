module Playwright.PageConsole
  ( ConsoleFilter (..)
  , clearConsoleMessages
  , consoleFilterProtocol
  , consoleMessages
  , consoleMessagesWith
  ) where

import Playwright.Monad
  ( Playwright
  )
import UnliftIO.Exception
  ( throwIO
  )
import Playwright.ConsoleMessage
  ( ConsoleLocation (ConsoleLocation, consoleLocationColumnNumber, consoleLocationLineNumber, consoleLocationUrl)
  , ConsoleMessage (ConsoleMessage, consoleMessageArgs, consoleMessageLocation, consoleMessagePage, consoleMessageText, consoleMessageTimestamp, consoleMessageType, consoleMessageWorkerRef)
  )
import Playwright.ConsoleType
  ( consoleTypeFromText
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  )
import Playwright.Channel
  ( send
  )
import Playwright.JsHandle
  ( jsHandleFromRef
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Page
  ( Page (pageChannel)
  )
import Playwright.Protocol
  ( ConsoleMessagesFilter (ConsoleMessagesFilterAll, ConsoleMessagesFilterSinceNavigation)
  , PageClearConsoleMessagesResult
  , PageConsoleMessagesResult (pageConsoleMessagesResultMessages)
  , PageConsoleMessagesResultMessages (pageConsoleMessagesResultMessagesArgs, pageConsoleMessagesResultMessagesLocation, pageConsoleMessagesResultMessagesText, pageConsoleMessagesResultMessagesTimestamp, pageConsoleMessagesResultMessagesType)
  , PageConsoleMessagesResultMessagesLocation (pageConsoleMessagesResultMessagesLocationColumnNumber, pageConsoleMessagesResultMessagesLocationLineNumber, pageConsoleMessagesResultMessagesLocationUrl)
  , mkPageClearConsoleMessagesParams
  , mkPageConsoleMessagesParams
  , PageConsoleMessagesParams (pageConsoleMessagesParamsFilter)
  )
import Playwright.Url
  ( urlToText
  )

-- | Which messages a page-level query returns. 'ConsoleFilterSinceNavigation'
-- is the default used by 'consoleMessages' and matches upstream Playwright.
data ConsoleFilter
  = ConsoleFilterAll
  | ConsoleFilterSinceNavigation
  deriving stock (Eq, Show)

-- | Console messages the page has logged since its last navigation, oldest first.
consoleMessages :: Page -> Playwright [ConsoleMessage]
consoleMessages page =
  consoleMessagesWith page ConsoleFilterSinceNavigation

-- | Console messages the page has logged, narrowed by the given filter.
consoleMessagesWith :: Page -> ConsoleFilter -> Playwright [ConsoleMessage]
consoleMessagesWith page filterChoice = do
  result <-
    send
      (pageChannel page)
      (MethodName "consoleMessages")
      (mkPageConsoleMessagesParams {pageConsoleMessagesParamsFilter = Just (consoleFilterProtocol filterChoice)})
  traverse (pageConsoleMessageFromItem page) (pageConsoleMessagesResultMessages result)

-- | Forgets the console messages the page has stored so far.
clearConsoleMessages :: Page -> Playwright ()
clearConsoleMessages page = do
  _ :: PageClearConsoleMessagesResult <-
    send (pageChannel page) (MethodName "clearConsoleMessages") mkPageClearConsoleMessagesParams
  pure ()

-- | The wire value for a filter. Shared with the other page-level getters.
consoleFilterProtocol :: ConsoleFilter -> ConsoleMessagesFilter
consoleFilterProtocol filterChoice =
  case filterChoice of
    ConsoleFilterAll -> ConsoleMessagesFilterAll
    ConsoleFilterSinceNavigation -> ConsoleMessagesFilterSinceNavigation

pageConsoleMessageFromItem :: Page -> PageConsoleMessagesResultMessages -> Playwright ConsoleMessage
pageConsoleMessageFromItem page item = do
  kind <-
    case consoleTypeFromText (pageConsoleMessagesResultMessagesType item) of
      Just parsed ->
        pure parsed
      Nothing ->
        throwIO (DecodeFailure ("unknown console type: " <> pageConsoleMessagesResultMessagesType item))
  let
    location = pageConsoleMessagesResultMessagesLocation item
  pure
    ConsoleMessage
      { consoleMessageType = kind
      , consoleMessageText = pageConsoleMessagesResultMessagesText item
      , consoleMessageArgs = fmap jsHandleFromRef (pageConsoleMessagesResultMessagesArgs item)
      , consoleMessagePage = Just page
      , consoleMessageWorkerRef = Nothing
      , consoleMessageTimestamp = realToFrac (pageConsoleMessagesResultMessagesTimestamp item)
      , consoleMessageLocation =
          ConsoleLocation
            { consoleLocationUrl = urlToText (pageConsoleMessagesResultMessagesLocationUrl location)
            , consoleLocationLineNumber = pageConsoleMessagesResultMessagesLocationLineNumber location
            , consoleLocationColumnNumber = pageConsoleMessagesResultMessagesLocationColumnNumber location
            }
      }

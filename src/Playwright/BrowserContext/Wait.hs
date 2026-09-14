module Playwright.BrowserContext.Wait
  ( waitForContextConsole
  , waitForContextDialog
  ) where

import Playwright.Monad
  ( Playwright
  )
import Playwright.BrowserContext
  ( BrowserContext (browserContextChannel)
  )
import Playwright.BrowserContext.Network
  ( subscribeBrowserContext
  )
import Playwright.Channel
  ( Channel (channelGuid)
  )
import Playwright.Connection
  ( waitForChannelEvent
  )
import Playwright.ConsoleMessage
  ( ConsoleMessage
  , consoleMessageFromEvent
  )
import Playwright.Dialog
  ( Dialog
  , dialogFromRef
  )
import Playwright.Json.Decode
  ( throwDecode
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( BrowserContextDialogEvent (browserContextDialogEventDialog)
  , BrowserContextUpdateSubscriptionParamsEvent (BrowserContextUpdateSubscriptionParamsEventConsole, BrowserContextUpdateSubscriptionParamsEventDialog)
  )
import Playwright.RaceTimeout
  ( waitWithTimeout
  )
import Playwright.Timeout
  ( defaultTimeout
  )

waitForContextConsole :: BrowserContext -> Playwright ConsoleMessage
waitForContextConsole ctx = do
  subscribeBrowserContext ctx BrowserContextUpdateSubscriptionParamsEventConsole
  waitWithTimeout defaultTimeout $ do
    params <-
      waitForChannelEvent
        (channelGuid (browserContextChannel ctx))
        (MethodName "console")
    event <- throwDecode params
    consoleMessageFromEvent (browserContextChannel ctx) event

waitForContextDialog :: BrowserContext -> Playwright Dialog
waitForContextDialog ctx = do
  subscribeBrowserContext ctx BrowserContextUpdateSubscriptionParamsEventDialog
  waitWithTimeout defaultTimeout $ do
    params <-
      waitForChannelEvent
        (channelGuid (browserContextChannel ctx))
        (MethodName "dialog")
    event <- throwDecode params
    dialogFromRef (browserContextDialogEventDialog event)

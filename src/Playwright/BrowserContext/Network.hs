module Playwright.BrowserContext.Network
  ( onContextRequest
  , onContextRequestFailed
  , onContextRequestFinished
  , onContextResponse
  , subscribeBrowserContext
  , waitForContextRequestFinished
  ) where

import Playwright.Monad
  ( Playwright
  )
import Playwright.BrowserContext
  ( BrowserContext (browserContextChannel)
  )
import Playwright.Channel
  ( Channel (channelGuid)
  , send
  )
import Playwright.Connection
  ( registerEventHandler
  , waitForChannelEvent
  )
import Playwright.Json.Decode
  ( throwDecode
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( BrowserContextRequestEvent (browserContextRequestEventRequest)
  , BrowserContextRequestFailedEvent (browserContextRequestFailedEventRequest)
  , BrowserContextRequestFinishedEvent (browserContextRequestFinishedEventRequest)
  , BrowserContextResponseEvent (browserContextResponseEventResponse)
  , BrowserContextUpdateSubscriptionParamsEvent (BrowserContextUpdateSubscriptionParamsEventRequest, BrowserContextUpdateSubscriptionParamsEventRequestFailed, BrowserContextUpdateSubscriptionParamsEventRequestFinished, BrowserContextUpdateSubscriptionParamsEventResponse)
  , BrowserContextUpdateSubscriptionResult
  , mkBrowserContextUpdateSubscriptionParams
  )
import Playwright.RaceTimeout
  ( waitWithTimeout
  )
import Playwright.Request
  ( Request
  , requestFromRef
  )
import Playwright.Response
  ( Response
  , responseFromRef
  )
import Playwright.Timeout
  ( defaultTimeout
  )

subscribeBrowserContext :: BrowserContext -> BrowserContextUpdateSubscriptionParamsEvent -> Playwright ()
subscribeBrowserContext ctx event = do
  _ :: BrowserContextUpdateSubscriptionResult <-
    send
      (browserContextChannel ctx)
      (MethodName "updateSubscription")
      (mkBrowserContextUpdateSubscriptionParams True event)
  pure ()

onContextRequest :: BrowserContext -> (Request -> Playwright ()) -> Playwright ()
onContextRequest ctx run = do
  subscribeBrowserContext ctx BrowserContextUpdateSubscriptionParamsEventRequest
  registerEventHandler
    (channelGuid (browserContextChannel ctx))
    (MethodName "request")
    $ \params -> do
      event <- throwDecode params
      run (requestFromRef (browserContextRequestEventRequest event))

onContextResponse :: BrowserContext -> (Response -> Playwright ()) -> Playwright ()
onContextResponse ctx run = do
  subscribeBrowserContext ctx BrowserContextUpdateSubscriptionParamsEventResponse
  registerEventHandler
    (channelGuid (browserContextChannel ctx))
    (MethodName "response")
    $ \params -> do
      event <- throwDecode params
      run (responseFromRef (browserContextResponseEventResponse event))

onContextRequestFailed :: BrowserContext -> (Request -> Playwright ()) -> Playwright ()
onContextRequestFailed ctx run = do
  subscribeBrowserContext ctx BrowserContextUpdateSubscriptionParamsEventRequestFailed
  registerEventHandler
    (channelGuid (browserContextChannel ctx))
    (MethodName "requestFailed")
    $ \params -> do
      event <- throwDecode params
      run (requestFromRef (browserContextRequestFailedEventRequest event))

onContextRequestFinished :: BrowserContext -> (Request -> Playwright ()) -> Playwright ()
onContextRequestFinished ctx run = do
  subscribeBrowserContext ctx BrowserContextUpdateSubscriptionParamsEventRequestFinished
  registerEventHandler
    (channelGuid (browserContextChannel ctx))
    (MethodName "requestFinished")
    $ \params -> do
      event <- throwDecode params
      run (requestFromRef (browserContextRequestFinishedEventRequest event))

waitForContextRequestFinished :: BrowserContext -> Playwright Request
waitForContextRequestFinished ctx = do
  subscribeBrowserContext ctx BrowserContextUpdateSubscriptionParamsEventRequestFinished
  waitWithTimeout defaultTimeout $ do
    params <-
      waitForChannelEvent
        (channelGuid (browserContextChannel ctx))
        (MethodName "requestFinished")
    event <- throwDecode params
    pure (requestFromRef (browserContextRequestFinishedEventRequest event))

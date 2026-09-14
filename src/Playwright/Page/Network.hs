module Playwright.Page.Network
  ( onRequest
  , onRequestFailed
  , onRequestFinished
  , onResponse
  , requests
  , responseFinished
  ) where

import Playwright.Monad
  ( Playwright
  )
import Playwright.Channel
  ( Channel (channelGuid)
  , send
  )
import Playwright.Connection
  ( registerEventHandler
  , waitUntilRequestSettled
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.Json.Decode
  ( throwDecode
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Page
  ( Page (pageChannel, pageContextChannel)
  , subscribeContext
  )
import Playwright.Protocol
  ( BrowserContextRequestEvent (browserContextRequestEventPage, browserContextRequestEventRequest)
  , BrowserContextRequestFailedEvent (browserContextRequestFailedEventPage, browserContextRequestFailedEventRequest)
  , BrowserContextRequestFinishedEvent (browserContextRequestFinishedEventPage, browserContextRequestFinishedEventRequest)
  , BrowserContextResponseEvent (browserContextResponseEventPage, browserContextResponseEventResponse)
  , BrowserContextUpdateSubscriptionParamsEvent (BrowserContextUpdateSubscriptionParamsEventRequest, BrowserContextUpdateSubscriptionParamsEventRequestFailed, BrowserContextUpdateSubscriptionParamsEventRequestFinished, BrowserContextUpdateSubscriptionParamsEventResponse)
  , PageRequestsResult (pageRequestsResultRequests)
  , mkPageRequestsParams
  )
import Playwright.Request
  ( Request (requestChannel)
  , requestFromRef
  )
import Playwright.Response
  ( Response
  , responseFromRef
  , responseRequest
  )

onRequest :: Page -> (Request -> Playwright ()) -> Playwright ()
onRequest page run = do
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventRequest
  registerEventHandler
    (channelGuid (pageContextChannel page))
    (MethodName "request")
    $ \params -> do
      event <- throwDecode params
      whenForPage page (browserContextRequestEventPage event) $
        run (requestFromRef (browserContextRequestEventRequest event))

onResponse :: Page -> (Response -> Playwright ()) -> Playwright ()
onResponse page run = do
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventResponse
  registerEventHandler
    (channelGuid (pageContextChannel page))
    (MethodName "response")
    $ \params -> do
      event <- throwDecode params
      whenForPage page (browserContextResponseEventPage event) $
        run (responseFromRef (browserContextResponseEventResponse event))

onRequestFailed :: Page -> (Request -> Playwright ()) -> Playwright ()
onRequestFailed page run = do
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventRequestFailed
  registerEventHandler
    (channelGuid (pageContextChannel page))
    (MethodName "requestFailed")
    $ \params -> do
      event <- throwDecode params
      whenForPage page (browserContextRequestFailedEventPage event) $
        run (requestFromRef (browserContextRequestFailedEventRequest event))

onRequestFinished :: Page -> (Request -> Playwright ()) -> Playwright ()
onRequestFinished page run = do
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventRequestFinished
  registerEventHandler
    (channelGuid (pageContextChannel page))
    (MethodName "requestFinished")
    $ \params -> do
      event <- throwDecode params
      whenForPage page (browserContextRequestFinishedEventPage event) $
        run (requestFromRef (browserContextRequestFinishedEventRequest event))

requests :: Page -> Playwright [Request]
requests page = do
  result <-
    send
      (pageChannel page)
      (MethodName "requests")
      mkPageRequestsParams
  pure
    ( fmap
        (requestFromRef)
        (pageRequestsResultRequests result)
    )

responseFinished :: Response -> Playwright ()
responseFinished response = do
  request <- responseRequest response
  waitUntilRequestSettled
    (channelGuid (requestChannel request))

whenForPage :: Page -> Maybe ChannelRef -> Playwright () -> Playwright ()
whenForPage page maybeRef run =
  case maybeRef of
    Just ref
      | channelRefGuid ref == channelGuid (pageChannel page) ->
          run
    _ ->
      pure ()

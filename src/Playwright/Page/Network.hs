module Playwright.Page.Network
  ( onRequest
  , onRequestFailed
  , onRequestFinished
  , onResponse
  , requests
  , responseFinished
  ) where

import Control.Exception
  ( throwIO
  )
import Playwright.Channel
  ( Channel (channelConnection, channelGuid)
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
  ( fromJsonValue
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

onRequest :: Page -> (Request -> IO ()) -> IO ()
onRequest page run = do
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventRequest
  registerEventHandler
    (channelConnection (pageContextChannel page))
    (channelGuid (pageContextChannel page))
    (MethodName "request")
    $ \params -> do
      event <- either throwIO pure (fromJsonValue params)
      whenForPage page (browserContextRequestEventPage event) $
        run (requestFromRef (channelConnection (pageContextChannel page)) (browserContextRequestEventRequest event))

onResponse :: Page -> (Response -> IO ()) -> IO ()
onResponse page run = do
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventResponse
  registerEventHandler
    (channelConnection (pageContextChannel page))
    (channelGuid (pageContextChannel page))
    (MethodName "response")
    $ \params -> do
      event <- either throwIO pure (fromJsonValue params)
      whenForPage page (browserContextResponseEventPage event) $
        run (responseFromRef (channelConnection (pageContextChannel page)) (browserContextResponseEventResponse event))

onRequestFailed :: Page -> (Request -> IO ()) -> IO ()
onRequestFailed page run = do
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventRequestFailed
  registerEventHandler
    (channelConnection (pageContextChannel page))
    (channelGuid (pageContextChannel page))
    (MethodName "requestFailed")
    $ \params -> do
      event <- either throwIO pure (fromJsonValue params)
      whenForPage page (browserContextRequestFailedEventPage event) $
        run (requestFromRef (channelConnection (pageContextChannel page)) (browserContextRequestFailedEventRequest event))

onRequestFinished :: Page -> (Request -> IO ()) -> IO ()
onRequestFinished page run = do
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventRequestFinished
  registerEventHandler
    (channelConnection (pageContextChannel page))
    (channelGuid (pageContextChannel page))
    (MethodName "requestFinished")
    $ \params -> do
      event <- either throwIO pure (fromJsonValue params)
      whenForPage page (browserContextRequestFinishedEventPage event) $
        run (requestFromRef (channelConnection (pageContextChannel page)) (browserContextRequestFinishedEventRequest event))

requests :: Page -> IO [Request]
requests page = do
  result <-
    send
      (pageChannel page)
      (MethodName "requests")
      mkPageRequestsParams
  pure
    ( fmap
        (requestFromRef (channelConnection (pageChannel page)))
        (pageRequestsResultRequests result)
    )

responseFinished :: Response -> IO ()
responseFinished response = do
  request <- responseRequest response
  waitUntilRequestSettled
    (channelConnection (requestChannel request))
    (channelGuid (requestChannel request))

whenForPage :: Page -> Maybe ChannelRef -> IO () -> IO ()
whenForPage page maybeRef run =
  case maybeRef of
    Just ref
      | channelRefGuid ref == channelGuid (pageChannel page) ->
          run
    _ ->
      pure ()

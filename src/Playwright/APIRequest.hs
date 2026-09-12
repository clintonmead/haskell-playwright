module Playwright.APIRequest
  ( newRequestContext
  , newRequestContextWith
  ) where

import Playwright.APIRequestContext
  ( APIRequestContext (APIRequestContext, apiRequestContextChannel, apiRequestContextTimeoutOwner)
  )
import Playwright.Channel
  ( Channel (Channel, channelConnection)
  , send
  )
import Playwright.Connection
  ( setTimeoutDefault
  )
import Playwright.Instance
  ( Playwright (playwrightChannel)
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.NewRequestOptions
  ( NewRequestOptions (newRequestTimeout)
  , defaultNewRequestOptions
  , toPlaywrightNewRequestParams
  )
import Playwright.Protocol
  ( PlaywrightNewRequestResult (playwrightNewRequestResultRequest)
  )

newRequestContext :: Playwright -> IO APIRequestContext
newRequestContext pw =
  newRequestContextWith pw defaultNewRequestOptions

newRequestContextWith :: Playwright -> NewRequestOptions -> IO APIRequestContext
newRequestContextWith pw opts = do
  let channel = playwrightChannel pw
  result <-
    send
      channel
      (MethodName "newRequest")
      (toPlaywrightNewRequestParams opts)
  let guid = channelRefGuid (playwrightNewRequestResultRequest result)
  setTimeoutDefault (channelConnection channel) guid (newRequestTimeout opts)
  pure
    APIRequestContext
      { apiRequestContextChannel = Channel (channelConnection channel) guid
      , apiRequestContextTimeoutOwner = guid
      }

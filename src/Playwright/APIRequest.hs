module Playwright.APIRequest
  ( newRequestContext
  , newRequestContextWith
  ) where

import Playwright.APIRequestContext
  ( APIRequestContext (APIRequestContext, apiRequestContextChannel, apiRequestContextTimeoutOwner)
  )
import Playwright.Channel
  ( Channel (Channel)
  )
import Playwright.Connection
  ( lookupPlaywrightGuid
  , sendCommand
  , setTimeoutDefault
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.NewRequestOptions
  ( NewRequestOptions (newRequestTimeout)
  , defaultNewRequestOptions
  , toPlaywrightNewRequestParams
  )
import Playwright.Protocol
  ( PlaywrightNewRequestResult (playwrightNewRequestResultRequest)
  )

newRequestContext :: Playwright APIRequestContext
newRequestContext =
  newRequestContextWith defaultNewRequestOptions

newRequestContextWith :: NewRequestOptions -> Playwright APIRequestContext
newRequestContextWith opts = do
  guid <- lookupPlaywrightGuid
  result <-
    sendCommand
      guid
      (MethodName "newRequest")
      (toPlaywrightNewRequestParams opts)
  let
    requestGuid = channelRefGuid (playwrightNewRequestResultRequest result)
  setTimeoutDefault requestGuid (newRequestTimeout opts)
  pure
    APIRequestContext
      { apiRequestContextChannel = Channel requestGuid
      , apiRequestContextTimeoutOwner = requestGuid
      }

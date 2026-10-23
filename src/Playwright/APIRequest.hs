module Playwright.APIRequest
  ( newRequestContext
  , newRequestContextWith
  ) where

import Playwright.APIRequestContext
  ( APIRequestContext (APIRequestContext, apiRequestContextChannel, apiRequestContextTimeoutOwner)
  )
import Playwright.ClientCertificate
  ( loadClientCertificates
  , toRequestClientCertificates
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
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Maybe
  ( fromMaybe
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.NewRequestOptions
  ( NewRequestOptions (newRequestClientCertificates, newRequestTimeout)
  , defaultNewRequestOptions
  , toPlaywrightNewRequestParams
  )
import Playwright.Protocol
  ( PlaywrightNewRequestParams (playwrightNewRequestParamsClientCertificates)
  , PlaywrightNewRequestResult (playwrightNewRequestResultRequest)
  )

newRequestContext :: Playwright APIRequestContext
newRequestContext =
  newRequestContextWith defaultNewRequestOptions

newRequestContextWith :: NewRequestOptions -> Playwright APIRequestContext
newRequestContextWith opts = do
  guid <- lookupPlaywrightGuid
  certs <- liftIO (loadClientCertificates (fromMaybe [] (newRequestClientCertificates opts)))
  result <-
    sendCommand
      guid
      (MethodName "newRequest")
      ((toPlaywrightNewRequestParams opts) { playwrightNewRequestParamsClientCertificates = toRequestClientCertificates certs })
  let
    requestGuid = channelRefGuid (playwrightNewRequestResultRequest result)
  setTimeoutDefault requestGuid (newRequestTimeout opts)
  pure
    APIRequestContext
      { apiRequestContextChannel = Channel requestGuid
      , apiRequestContextTimeoutOwner = requestGuid
      }

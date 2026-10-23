module Playwright.APIRequest
  ( newRequestContext
  , newRequestContextWith
  ) where

import Data.ByteString qualified as BS
import Playwright.APIRequestContext
  ( APIRequestContext (APIRequestContext, apiRequestContextChannel, apiRequestContextTimeoutOwner)
  )
import Playwright.StorageState
  ( decodeStorageState
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
  ( NewRequestOptions (newRequestClientCertificates, newRequestStorageState, newRequestStorageStatePath, newRequestTimeout)
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

-- | When 'newRequestStorageStatePath' is set, read that file as a storage
-- state and use it in place of 'newRequestStorageState'.
resolveStorageStatePath :: NewRequestOptions -> IO NewRequestOptions
resolveStorageStatePath opts =
  case newRequestStorageStatePath opts of
    Nothing -> pure opts
    Just path -> do
      bytes <- BS.readFile path
      case decodeStorageState bytes of
        Left message ->
          ioError (userError ("invalid storage state file " <> path <> ": " <> message))
        Right state ->
          pure opts {newRequestStorageState = Just state}

newRequestContextWith :: NewRequestOptions -> Playwright APIRequestContext
newRequestContextWith opts0 = do
  guid <- lookupPlaywrightGuid
  certs <- liftIO (loadClientCertificates (fromMaybe [] (newRequestClientCertificates opts0)))
  opts <- liftIO (resolveStorageStatePath opts0)
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

module Playwright.APIRequestContext
  ( APIRequestContext (..)
  , apiRequestContextFor
  , fetchGet
  , fetchGetWith
  , fetchRequest
  , fetchRequestWith
  , fetchFromUrl
  , fetchFromUrlWith
  ) where

import Control.Exception
  ( throwIO
  )
import Data.Aeson
  ( encode
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString.Lazy
  ( toStrict
  )
import Data.Maybe
  ( fromMaybe
  )
import Data.Text
  ( Text
  )
import Data.Text.Encoding
  ( decodeUtf8
  , encodeUtf8
  )
import Playwright.APIResponse
  ( APIResponse (APIResponse, apiResponseChannel, apiResponsePayload)
  )
import Playwright.Channel
  ( APIRequestContextTag
  , Channel (Channel, channelConnection)
  , send
  )
import Playwright.Connection
  ( Connection
  , lookupObject
  , lookupTimeoutDefaults
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  )
import Playwright.FetchOptions
  ( FetchOptions (fetchHeaders, fetchMaxRedirects, fetchMethod, fetchPostData, fetchTimeout, fetchUrl)
  , FetchPostData (FetchPostBytes, FetchPostJson, FetchPostText)
  , defaultFetchOptions
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.HttpHeader
  ( HttpHeader
  , toNameValue
  )
import Playwright.HttpMethod
  ( HttpMethod (HttpMethod, httpMethodToText)
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.Json.Decode
  ( fromJsonValue
  )
import Playwright.Json.ProtocolBinary
  ( protocolBinaryFromByteString
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( APIRequestContextFetchParams (aPIRequestContextFetchParamsHeaders, aPIRequestContextFetchParamsJsonData, aPIRequestContextFetchParamsMaxRedirects, aPIRequestContextFetchParamsMethod, aPIRequestContextFetchParamsPostData)
  , APIRequestContextFetchResult (aPIRequestContextFetchResultResponse)
  , BrowserContextInitializer (browserContextInitializerRequestContext)
  , ChannelType (ChannelTypeBrowserContext)
  , mkAPIRequestContextFetchParams
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer, remoteParent, remoteType)
  )
import Playwright.Request
  ( Request
  , requestHeaders
  , requestMethod
  , requestPostData
  , requestUrl
  )
import Playwright.Timeout
  ( Timeout
  )
import Playwright.TimeoutDefaults
  ( emptyTimeoutDefaults
  , resolveActionTimeout
  )
import Playwright.Url
  ( Url
  )

data APIRequestContext = APIRequestContext
  { apiRequestContextChannel :: Channel APIRequestContextTag
  , apiRequestContextTimeoutOwner :: Guid
  }

data PostBody = PostBody
  { postBodyBytes :: Maybe ByteString
  , postBodyJson :: Maybe Text
  }

apiRequestContextFor :: Connection -> Guid -> IO APIRequestContext
apiRequestContextFor conn guid = do
  contextGuid <- findBrowserContextGuid conn guid
  obj <- lookupObject conn contextGuid
  initializer <- either throwIO pure (fromJsonValue (remoteInitializer obj))
  pure
    APIRequestContext
      { apiRequestContextChannel =
          Channel conn (channelRefGuid (browserContextInitializerRequestContext initializer))
      , apiRequestContextTimeoutOwner = contextGuid
      }

fetchRequest :: APIRequestContext -> Request -> IO APIResponse
fetchRequest ctx request =
  fetchRequestWith ctx request defaultFetchOptions

fetchRequestWith :: APIRequestContext -> Request -> FetchOptions -> IO APIResponse
fetchRequestWith ctx request opts = do
  url <- maybe (requestUrl request) pure (fetchUrl opts)
  method <- maybe (requestMethod request) pure (fetchMethod opts)
  headers <- maybe (requestHeaders request) pure (fetchHeaders opts)
  post <- resolveRequestPost (fetchPostData opts) request
  sendFetch ctx url (Just method) (Just headers) post (fetchMaxRedirects opts) (fetchTimeout opts)

fetchFromUrl :: APIRequestContext -> Url -> IO APIResponse
fetchFromUrl ctx url =
  fetchFromUrlWith ctx url defaultFetchOptions

fetchFromUrlWith :: APIRequestContext -> Url -> FetchOptions -> IO APIResponse
fetchFromUrlWith ctx url opts =
  sendFetch
    ctx
    (fromMaybe url (fetchUrl opts))
    (fetchMethod opts)
    (fetchHeaders opts)
    (postFromOverlay (fetchPostData opts))
    (fetchMaxRedirects opts)
    (fetchTimeout opts)

fetchGet :: APIRequestContext -> Url -> IO APIResponse
fetchGet ctx url =
  fetchGetWith ctx url defaultFetchOptions

fetchGetWith :: APIRequestContext -> Url -> FetchOptions -> IO APIResponse
fetchGetWith ctx url opts =
  fetchFromUrlWith ctx url opts {fetchMethod = Just (HttpMethod "GET")}

sendFetch ::
  APIRequestContext ->
  Url ->
  Maybe HttpMethod ->
  Maybe [HttpHeader] ->
  PostBody ->
  Maybe Int ->
  Maybe Timeout ->
  IO APIResponse
sendFetch ctx url method headers post maxRedirects timeoutOverride = do
  timeout <- resolveFetchTimeout ctx timeoutOverride
  result <-
    send
      (apiRequestContextChannel ctx)
      (MethodName "fetch")
      (mkAPIRequestContextFetchParams timeout url)
        { aPIRequestContextFetchParamsHeaders = fmap (fmap toNameValue) headers
        , aPIRequestContextFetchParamsJsonData = postBodyJson post
        , aPIRequestContextFetchParamsMaxRedirects = maxRedirects
        , aPIRequestContextFetchParamsMethod = fmap httpMethodToText method
        , aPIRequestContextFetchParamsPostData = fmap protocolBinaryFromByteString (postBodyBytes post)
        }
  pure
    APIResponse
      { apiResponseChannel = apiRequestContextChannel ctx
      , apiResponsePayload = aPIRequestContextFetchResultResponse result
      }

resolveFetchTimeout :: APIRequestContext -> Maybe Timeout -> IO Timeout
resolveFetchTimeout ctx timeoutOverride =
  case timeoutOverride of
    Just timeout ->
      pure timeout
    Nothing -> do
      defaults <-
        lookupTimeoutDefaults
          (channelConnection (apiRequestContextChannel ctx))
          (apiRequestContextTimeoutOwner ctx)
      pure (resolveActionTimeout emptyTimeoutDefaults defaults)

findBrowserContextGuid :: Connection -> Guid -> IO Guid
findBrowserContextGuid conn guid = do
  obj <- lookupObject conn guid
  if remoteType obj == ChannelTypeBrowserContext
    then pure guid
    else
      if remoteParent obj == guid
        then throwIO (DecodeFailure "No browser context for APIRequestContext")
        else findBrowserContextGuid conn (remoteParent obj)

resolveRequestPost :: Maybe FetchPostData -> Request -> IO PostBody
resolveRequestPost overlay request =
  case overlay of
    Just payload ->
      pure (postFromOverlay (Just payload))
    Nothing -> do
      bytes <- requestPostData request
      pure (PostBody {postBodyBytes = bytes, postBodyJson = Nothing})

postFromOverlay :: Maybe FetchPostData -> PostBody
postFromOverlay overlay =
  case overlay of
    Nothing ->
      PostBody {postBodyBytes = Nothing, postBodyJson = Nothing}
    Just (FetchPostBytes bytes) ->
      PostBody {postBodyBytes = Just bytes, postBodyJson = Nothing}
    Just (FetchPostText text) ->
      PostBody {postBodyBytes = Just (encodeUtf8 text), postBodyJson = Nothing}
    Just (FetchPostJson value) ->
      PostBody
        { postBodyBytes = Nothing
        , postBodyJson = Just (decodeUtf8 (toStrict (encode value)))
        }

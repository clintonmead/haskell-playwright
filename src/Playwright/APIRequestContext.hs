module Playwright.APIRequestContext
  ( APIRequestContext (..)
  , apiRequestContextFor
  , apiRequestStorageState
  , disposeAPIRequestContext
  , fetchDelete
  , fetchDeleteWith
  , fetchFromUrl
  , fetchFromUrlWith
  , fetchGet
  , fetchGetWith
  , fetchHead
  , fetchHeadWith
  , fetchPatch
  , fetchPatchWith
  , fetchPost
  , fetchPostWith
  , fetchPut
  , fetchPutWith
  , fetchRequest
  , fetchRequestWith
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
  ( FetchFormField (fetchFormFieldName, fetchFormFieldValue)
  , FetchMultipartField (FetchMultipartField, FetchMultipartFileField)
  , FetchMultipartFile (fetchMultipartFileBytes, fetchMultipartFileMimeType, fetchMultipartFileName)
  , FetchOptions (fetchEncodedParams, fetchFailOnStatusCode, fetchFormData, fetchHeaders, fetchIgnoreHTTPSErrors, fetchMaxRedirects, fetchMaxRetries, fetchMethod, fetchMultipart, fetchPostData, fetchQueryParams, fetchTimeout, fetchUrl)
  , FetchPostData (FetchPostBytes, FetchPostJson, FetchPostText)
  , FetchQueryParam (fetchQueryParamName, fetchQueryParamValue)
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
  ( APIRequestContextDisposeResult
  , APIRequestContextFetchParams (aPIRequestContextFetchParamsEncodedParams, aPIRequestContextFetchParamsFailOnStatusCode, aPIRequestContextFetchParamsFormData, aPIRequestContextFetchParamsHeaders, aPIRequestContextFetchParamsIgnoreHTTPSErrors, aPIRequestContextFetchParamsJsonData, aPIRequestContextFetchParamsMaxRedirects, aPIRequestContextFetchParamsMaxRetries, aPIRequestContextFetchParamsMethod, aPIRequestContextFetchParamsMultipartData, aPIRequestContextFetchParamsParams, aPIRequestContextFetchParamsPostData)
  , APIRequestContextFetchResult (aPIRequestContextFetchResultResponse)
  , APIRequestContextStorageStateResult
  , BrowserContextInitializer (browserContextInitializerRequestContext)
  , ChannelType (ChannelTypeBrowserContext)
  , FormField (formFieldFile, formFieldValue)
  , FormFieldFile (formFieldFileMimeType)
  , NameValue
  , mkAPIRequestContextDisposeParams
  , mkAPIRequestContextFetchParams
  , mkAPIRequestContextStorageStateParams
  , mkFormField
  , mkFormFieldFile
  , mkNameValue
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
import Playwright.StorageState
  ( StorageState
  , fromRequestStorageState
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
  sendFetch ctx url (Just method) (Just headers) post opts

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
    opts

fetchGet :: APIRequestContext -> Url -> IO APIResponse
fetchGet ctx url =
  fetchGetWith ctx url defaultFetchOptions

fetchGetWith :: APIRequestContext -> Url -> FetchOptions -> IO APIResponse
fetchGetWith ctx url opts =
  fetchFromUrlWith ctx url (withFetchMethod "GET" opts)

fetchDelete :: APIRequestContext -> Url -> IO APIResponse
fetchDelete ctx url =
  fetchDeleteWith ctx url defaultFetchOptions

fetchDeleteWith :: APIRequestContext -> Url -> FetchOptions -> IO APIResponse
fetchDeleteWith ctx url opts =
  fetchFromUrlWith ctx url (withFetchMethod "DELETE" opts)

fetchHead :: APIRequestContext -> Url -> IO APIResponse
fetchHead ctx url =
  fetchHeadWith ctx url defaultFetchOptions

fetchHeadWith :: APIRequestContext -> Url -> FetchOptions -> IO APIResponse
fetchHeadWith ctx url opts =
  fetchFromUrlWith ctx url (withFetchMethod "HEAD" opts)

fetchPatch :: APIRequestContext -> Url -> IO APIResponse
fetchPatch ctx url =
  fetchPatchWith ctx url defaultFetchOptions

fetchPatchWith :: APIRequestContext -> Url -> FetchOptions -> IO APIResponse
fetchPatchWith ctx url opts =
  fetchFromUrlWith ctx url (withFetchMethod "PATCH" opts)

fetchPost :: APIRequestContext -> Url -> IO APIResponse
fetchPost ctx url =
  fetchPostWith ctx url defaultFetchOptions

fetchPostWith :: APIRequestContext -> Url -> FetchOptions -> IO APIResponse
fetchPostWith ctx url opts =
  fetchFromUrlWith ctx url (withFetchMethod "POST" opts)

fetchPut :: APIRequestContext -> Url -> IO APIResponse
fetchPut ctx url =
  fetchPutWith ctx url defaultFetchOptions

fetchPutWith :: APIRequestContext -> Url -> FetchOptions -> IO APIResponse
fetchPutWith ctx url opts =
  fetchFromUrlWith ctx url (withFetchMethod "PUT" opts)

withFetchMethod :: Text -> FetchOptions -> FetchOptions
withFetchMethod name opts =
  opts {fetchMethod = Just (HttpMethod name)}

sendFetch ::
  APIRequestContext ->
  Url ->
  Maybe HttpMethod ->
  Maybe [HttpHeader] ->
  PostBody ->
  FetchOptions ->
  IO APIResponse
sendFetch ctx url method headers post opts = do
  timeout <- resolveFetchTimeout ctx (fetchTimeout opts)
  result <-
    send
      (apiRequestContextChannel ctx)
      (MethodName "fetch")
      (mkAPIRequestContextFetchParams timeout url)
        { aPIRequestContextFetchParamsEncodedParams = fetchEncodedParams opts
        , aPIRequestContextFetchParamsFailOnStatusCode = fetchFailOnStatusCode opts
        , aPIRequestContextFetchParamsFormData = fmap (fmap formNameValue) (fetchFormData opts)
        , aPIRequestContextFetchParamsHeaders = fmap (fmap toNameValue) headers
        , aPIRequestContextFetchParamsIgnoreHTTPSErrors = fetchIgnoreHTTPSErrors opts
        , aPIRequestContextFetchParamsJsonData = postBodyJson post
        , aPIRequestContextFetchParamsMaxRedirects = fetchMaxRedirects opts
        , aPIRequestContextFetchParamsMaxRetries = fetchMaxRetries opts
        , aPIRequestContextFetchParamsMethod = fmap httpMethodToText method
        , aPIRequestContextFetchParamsMultipartData = fmap (fmap toFormField) (fetchMultipart opts)
        , aPIRequestContextFetchParamsParams = fmap (fmap queryNameValue) (fetchQueryParams opts)
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

queryNameValue :: FetchQueryParam -> NameValue
queryNameValue param =
  mkNameValue (fetchQueryParamName param) (fetchQueryParamValue param)

formNameValue :: FetchFormField -> NameValue
formNameValue field =
  mkNameValue (fetchFormFieldName field) (fetchFormFieldValue field)

toFormField :: FetchMultipartField -> FormField
toFormField field =
  case field of
    FetchMultipartField name value ->
      (mkFormField name) {formFieldValue = Just value}
    FetchMultipartFileField name file ->
      (mkFormField name)
        { formFieldFile =
            Just
              ( ( mkFormFieldFile
                    (protocolBinaryFromByteString (fetchMultipartFileBytes file))
                    (fetchMultipartFileName file)
                )
                  { formFieldFileMimeType = fetchMultipartFileMimeType file
                  }
              )
        }

apiRequestStorageState :: APIRequestContext -> IO StorageState
apiRequestStorageState ctx = do
  result :: APIRequestContextStorageStateResult <-
    send
      (apiRequestContextChannel ctx)
      (MethodName "storageState")
      mkAPIRequestContextStorageStateParams
  pure (fromRequestStorageState result)

disposeAPIRequestContext :: APIRequestContext -> IO ()
disposeAPIRequestContext ctx = do
  _ :: APIRequestContextDisposeResult <-
    send
      (apiRequestContextChannel ctx)
      (MethodName "dispose")
      mkAPIRequestContextDisposeParams
  pure ()

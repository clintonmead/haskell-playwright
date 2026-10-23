module Playwright.NewRequestOptions
  ( NewRequestOptions (..)
  , defaultNewRequestOptions
  , toPlaywrightNewRequestParams
  ) where

import Data.Maybe
  ( maybeToList
  )
import Playwright.HttpCredentials
  ( HttpCredentials (httpCredentialsOrigin, httpCredentialsPassword, httpCredentialsSend, httpCredentialsUsername)
  , HttpPassword (httpPasswordToText)
  , HttpUsername (httpUsernameToText)
  )
import Playwright.HttpCredentialsSend
  ( HttpCredentialsSend (HttpCredentialsSendAlways, HttpCredentialsSendUnauthorized)
  )
import Playwright.HttpHeader
  ( HttpHeader
  , toNameValue
  )
import Playwright.Protocol qualified as Protocol
import Playwright.Protocol
  ( PlaywrightNewRequestParams (playwrightNewRequestParamsBaseURL, playwrightNewRequestParamsExtraHTTPHeaders, playwrightNewRequestParamsFailOnStatusCode, playwrightNewRequestParamsHttpCredentials, playwrightNewRequestParamsIgnoreHTTPSErrors, playwrightNewRequestParamsMaxRedirects, playwrightNewRequestParamsProxy, playwrightNewRequestParamsStorageState, playwrightNewRequestParamsUserAgent)
  , PlaywrightNewRequestParamsProxy (playwrightNewRequestParamsProxyBypass, playwrightNewRequestParamsProxyPassword, playwrightNewRequestParamsProxyUsername)
  , mkPlaywrightNewRequestParamsProxy
  , mkPlaywrightNewRequestParams
  )
import Playwright.ProxySettings
  ( ProxyHost (proxyHostToText)
  , ProxySettings (proxySettingsBypass, proxySettingsPassword, proxySettingsServer, proxySettingsUsername)
  )
import Playwright.StorageState
  ( StorageState
  , toRequestStorageState
  )
import Playwright.Timeout
  ( Timeout
  )
import Playwright.Url
  ( Url
  , urlToText
  )
import Playwright.UserAgent
  ( UserAgent (userAgentToText)
  )

data NewRequestOptions = NewRequestOptions
  { newRequestBaseURL :: Maybe Url
  , newRequestExtraHTTPHeaders :: Maybe [HttpHeader]
  , newRequestFailOnStatusCode :: Maybe Bool
  , newRequestHttpCredentials :: Maybe HttpCredentials
  , newRequestHttpCredentialsList :: [HttpCredentials]
  , newRequestIgnoreHTTPSErrors :: Maybe Bool
  , newRequestMaxRedirects :: Maybe Int
  , newRequestProxy :: Maybe ProxySettings
  , newRequestStorageState :: Maybe StorageState
  , newRequestTimeout :: Maybe Timeout
  , newRequestUserAgent :: Maybe UserAgent
  }
  deriving stock (Eq, Show)

defaultNewRequestOptions :: NewRequestOptions
defaultNewRequestOptions =
  NewRequestOptions
    { newRequestBaseURL = Nothing
    , newRequestExtraHTTPHeaders = Nothing
    , newRequestFailOnStatusCode = Nothing
    , newRequestHttpCredentials = Nothing
    , newRequestHttpCredentialsList = []
    , newRequestIgnoreHTTPSErrors = Nothing
    , newRequestMaxRedirects = Nothing
    , newRequestProxy = Nothing
    , newRequestStorageState = Nothing
    , newRequestTimeout = Nothing
    , newRequestUserAgent = Nothing
    }

toPlaywrightNewRequestParams :: NewRequestOptions -> PlaywrightNewRequestParams
toPlaywrightNewRequestParams opts =
  mkPlaywrightNewRequestParams
    { playwrightNewRequestParamsBaseURL = newRequestBaseURL opts
    , playwrightNewRequestParamsExtraHTTPHeaders = fmap (fmap toNameValue) (newRequestExtraHTTPHeaders opts)
    , playwrightNewRequestParamsFailOnStatusCode = newRequestFailOnStatusCode opts
    , playwrightNewRequestParamsHttpCredentials = toProtocolCredentialsList opts
    , playwrightNewRequestParamsIgnoreHTTPSErrors = newRequestIgnoreHTTPSErrors opts
    , playwrightNewRequestParamsMaxRedirects = newRequestMaxRedirects opts
    , playwrightNewRequestParamsProxy = fmap toProtocolProxy (newRequestProxy opts)
    , playwrightNewRequestParamsStorageState = fmap toRequestStorageState (newRequestStorageState opts)
    , playwrightNewRequestParamsUserAgent = fmap userAgentToText (newRequestUserAgent opts)
    }

toProtocolProxy :: ProxySettings -> PlaywrightNewRequestParamsProxy
toProtocolProxy settings =
  ( mkPlaywrightNewRequestParamsProxy (proxyHostToText (proxySettingsServer settings))
  )
    { playwrightNewRequestParamsProxyBypass = proxySettingsBypass settings
    , playwrightNewRequestParamsProxyUsername = fmap httpUsernameToText (proxySettingsUsername settings)
    , playwrightNewRequestParamsProxyPassword = fmap httpPasswordToText (proxySettingsPassword settings)
    }

-- | The single 'newRequestHttpCredentials' comes first, then every entry of
-- 'newRequestHttpCredentialsList'. Nothing is sent when neither is set.
toProtocolCredentialsList :: NewRequestOptions -> Maybe [Protocol.HttpCredentials]
toProtocolCredentialsList opts =
  case concatMap toProtocolCredentials (maybeToList (newRequestHttpCredentials opts) <> newRequestHttpCredentialsList opts) of
    [] -> Nothing
    creds -> Just creds

toProtocolCredentials :: HttpCredentials -> [Protocol.HttpCredentials]
toProtocolCredentials creds =
  [ Protocol.HttpCredentials
      { Protocol.httpCredentialsOrigin = fmap urlToText (httpCredentialsOrigin creds)
      , Protocol.httpCredentialsPassword = httpPasswordToText (httpCredentialsPassword creds)
      , Protocol.httpCredentialsSend = fmap toProtocolCredentialsSend (httpCredentialsSend creds)
      , Protocol.httpCredentialsUsername = httpUsernameToText (httpCredentialsUsername creds)
      }
  ]

toProtocolCredentialsSend :: HttpCredentialsSend -> Protocol.HttpCredentialsSend
toProtocolCredentialsSend send =
  case send of
    HttpCredentialsSendAlways ->
      Protocol.HttpCredentialsSendAlways
    HttpCredentialsSendUnauthorized ->
      Protocol.HttpCredentialsSendUnauthorized

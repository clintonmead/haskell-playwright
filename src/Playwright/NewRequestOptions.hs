module Playwright.NewRequestOptions
  ( NewRequestOptions (..)
  , defaultNewRequestOptions
  , toPlaywrightNewRequestParams
  ) where

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
  ( PlaywrightNewRequestParams (playwrightNewRequestParamsBaseURL, playwrightNewRequestParamsExtraHTTPHeaders, playwrightNewRequestParamsFailOnStatusCode, playwrightNewRequestParamsHttpCredentials, playwrightNewRequestParamsIgnoreHTTPSErrors, playwrightNewRequestParamsMaxRedirects, playwrightNewRequestParamsStorageState, playwrightNewRequestParamsUserAgent)
  , mkPlaywrightNewRequestParams
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
  , newRequestIgnoreHTTPSErrors :: Maybe Bool
  , newRequestMaxRedirects :: Maybe Int
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
    , newRequestIgnoreHTTPSErrors = Nothing
    , newRequestMaxRedirects = Nothing
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
    , playwrightNewRequestParamsHttpCredentials = fmap toProtocolCredentials (newRequestHttpCredentials opts)
    , playwrightNewRequestParamsIgnoreHTTPSErrors = newRequestIgnoreHTTPSErrors opts
    , playwrightNewRequestParamsMaxRedirects = newRequestMaxRedirects opts
    , playwrightNewRequestParamsStorageState = fmap toRequestStorageState (newRequestStorageState opts)
    , playwrightNewRequestParamsUserAgent = fmap userAgentToText (newRequestUserAgent opts)
    }

-- | The protocol takes a list of credentials; a request has at most one.
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

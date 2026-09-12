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
import Playwright.Protocol
  ( PlaywrightNewRequestParams (playwrightNewRequestParamsBaseURL, playwrightNewRequestParamsExtraHTTPHeaders, playwrightNewRequestParamsFailOnStatusCode, playwrightNewRequestParamsHttpCredentials, playwrightNewRequestParamsIgnoreHTTPSErrors, playwrightNewRequestParamsMaxRedirects, playwrightNewRequestParamsStorageState, playwrightNewRequestParamsUserAgent)
  , PlaywrightNewRequestParamsHttpCredentials (playwrightNewRequestParamsHttpCredentialsOrigin, playwrightNewRequestParamsHttpCredentialsSend)
  , PlaywrightNewRequestParamsHttpCredentialsSend (PlaywrightNewRequestParamsHttpCredentialsSendAlways, PlaywrightNewRequestParamsHttpCredentialsSendUnauthorized)
  , mkPlaywrightNewRequestParams
  , mkPlaywrightNewRequestParamsHttpCredentials
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

toProtocolCredentials :: HttpCredentials -> PlaywrightNewRequestParamsHttpCredentials
toProtocolCredentials creds =
  ( mkPlaywrightNewRequestParamsHttpCredentials
      (httpPasswordToText (httpCredentialsPassword creds))
      (httpUsernameToText (httpCredentialsUsername creds))
  )
    { playwrightNewRequestParamsHttpCredentialsOrigin =
        fmap urlToText (httpCredentialsOrigin creds)
    , playwrightNewRequestParamsHttpCredentialsSend =
        fmap toProtocolCredentialsSend (httpCredentialsSend creds)
    }

toProtocolCredentialsSend :: HttpCredentialsSend -> PlaywrightNewRequestParamsHttpCredentialsSend
toProtocolCredentialsSend send =
  case send of
    HttpCredentialsSendAlways ->
      PlaywrightNewRequestParamsHttpCredentialsSendAlways
    HttpCredentialsSendUnauthorized ->
      PlaywrightNewRequestParamsHttpCredentialsSendUnauthorized

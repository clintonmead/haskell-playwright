module Playwright.FetchOptions
  ( FetchOptions (..)
  , FetchFormField (..)
  , FetchPostData (..)
  , FetchQueryParam (..)
  , defaultFetchOptions
  ) where

import Data.Aeson
  ( Value
  )
import Data.ByteString
  ( ByteString
  )
import Data.Text
  ( Text
  )
import Playwright.HttpHeader
  ( HttpHeader
  )
import Playwright.HttpMethod
  ( HttpMethod
  )
import Playwright.Timeout
  ( Timeout
  )
import Playwright.Url
  ( Url
  )

data FetchPostData
  = FetchPostBytes ByteString
  | FetchPostJson Value
  | FetchPostText Text
  deriving stock (Eq, Show)

data FetchQueryParam = FetchQueryParam
  { fetchQueryParamName :: Text
  , fetchQueryParamValue :: Text
  }
  deriving stock (Eq, Show)

data FetchFormField = FetchFormField
  { fetchFormFieldName :: Text
  , fetchFormFieldValue :: Text
  }
  deriving stock (Eq, Show)

data FetchOptions = FetchOptions
  { fetchEncodedParams :: Maybe Text
  , fetchFailOnStatusCode :: Maybe Bool
  , fetchFormData :: Maybe [FetchFormField]
  , fetchHeaders :: Maybe [HttpHeader]
  , fetchIgnoreHTTPSErrors :: Maybe Bool
  , fetchMaxRedirects :: Maybe Int
  , fetchMaxRetries :: Maybe Int
  , fetchMethod :: Maybe HttpMethod
  , fetchPostData :: Maybe FetchPostData
  , fetchQueryParams :: Maybe [FetchQueryParam]
  , fetchTimeout :: Maybe Timeout
  , fetchUrl :: Maybe Url
  }
  deriving stock (Eq, Show)

defaultFetchOptions :: FetchOptions
defaultFetchOptions =
  FetchOptions
    { fetchEncodedParams = Nothing
    , fetchFailOnStatusCode = Nothing
    , fetchFormData = Nothing
    , fetchHeaders = Nothing
    , fetchIgnoreHTTPSErrors = Nothing
    , fetchMaxRedirects = Nothing
    , fetchMaxRetries = Nothing
    , fetchMethod = Nothing
    , fetchPostData = Nothing
    , fetchQueryParams = Nothing
    , fetchTimeout = Nothing
    , fetchUrl = Nothing
    }

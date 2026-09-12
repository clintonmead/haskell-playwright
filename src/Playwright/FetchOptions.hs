module Playwright.FetchOptions
  ( FetchOptions (..)
  , FetchPostData (..)
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

data FetchOptions = FetchOptions
  { fetchFailOnStatusCode :: Maybe Bool
  , fetchHeaders :: Maybe [HttpHeader]
  , fetchMaxRedirects :: Maybe Int
  , fetchMethod :: Maybe HttpMethod
  , fetchPostData :: Maybe FetchPostData
  , fetchTimeout :: Maybe Timeout
  , fetchUrl :: Maybe Url
  }
  deriving stock (Eq, Show)

defaultFetchOptions :: FetchOptions
defaultFetchOptions =
  FetchOptions
    { fetchFailOnStatusCode = Nothing
    , fetchHeaders = Nothing
    , fetchMaxRedirects = Nothing
    , fetchMethod = Nothing
    , fetchPostData = Nothing
    , fetchTimeout = Nothing
    , fetchUrl = Nothing
    }

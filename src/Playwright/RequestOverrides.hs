module Playwright.RequestOverrides
  ( RequestOverrides (..)
  , emptyRequestOverrides
  , overlayContinueOptions
  ) where

import Data.ByteString
  ( ByteString
  )
import Control.Applicative
  ( (<|>)
  )
import Playwright.ContinueOptions
  ( ContinueOptions (continueHeaders, continueMethod, continuePostData, continueUrl)
  )
import Playwright.HttpHeader
  ( HttpHeader
  )
import Playwright.HttpMethod
  ( HttpMethod
  )
import Playwright.Url
  ( Url
  )

data RequestOverrides = RequestOverrides
  { requestOverrideHeaders :: Maybe [HttpHeader]
  , requestOverrideMethod :: Maybe HttpMethod
  , requestOverridePostData :: Maybe ByteString
  , requestOverrideUrl :: Maybe Url
  }
  deriving stock (Eq, Show)

emptyRequestOverrides :: RequestOverrides
emptyRequestOverrides =
  RequestOverrides
    { requestOverrideHeaders = Nothing
    , requestOverrideMethod = Nothing
    , requestOverridePostData = Nothing
    , requestOverrideUrl = Nothing
    }

overlayContinueOptions :: RequestOverrides -> ContinueOptions -> RequestOverrides
overlayContinueOptions current opts =
  RequestOverrides
    { requestOverrideHeaders = continueHeaders opts <|> requestOverrideHeaders current
    , requestOverrideMethod = continueMethod opts <|> requestOverrideMethod current
    , requestOverridePostData = continuePostData opts <|> requestOverridePostData current
    , requestOverrideUrl = continueUrl opts <|> requestOverrideUrl current
    }

module Playwright.ContinueOptions
  ( ContinueOptions (..)
  , defaultContinueOptions
  ) where

import Data.ByteString
  ( ByteString
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

data ContinueOptions = ContinueOptions
  { continueHeaders :: Maybe [HttpHeader]
  , continueMethod :: Maybe HttpMethod
  , continuePostData :: Maybe ByteString
  , continueUrl :: Maybe Url
  }
  deriving stock (Eq, Show)

defaultContinueOptions :: ContinueOptions
defaultContinueOptions =
  ContinueOptions
    { continueHeaders = Nothing
    , continueMethod = Nothing
    , continuePostData = Nothing
    , continueUrl = Nothing
    }

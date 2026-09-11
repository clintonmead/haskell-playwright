module Playwright.HttpHeader
  ( HeaderName (..)
  , HeaderValue (..)
  , HttpHeader (..)
  ) where

import Data.Text
  ( Text
  )

newtype HeaderName = HeaderName
  { headerNameToText :: Text
  }
  deriving stock (Eq, Show)

newtype HeaderValue = HeaderValue
  { headerValueToText :: Text
  }
  deriving stock (Eq, Show)

data HttpHeader = HttpHeader
  { httpHeaderName :: HeaderName
  , httpHeaderValue :: HeaderValue
  }
  deriving stock (Eq, Show)

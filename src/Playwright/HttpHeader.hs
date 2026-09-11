module Playwright.HttpHeader
  ( HeaderName (..)
  , HeaderValue (..)
  , HttpHeader (..)
  , toNameValue
  ) where

import Data.Text
  ( Text
  )
import Playwright.Protocol
  ( NameValue
  , mkNameValue
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

toNameValue :: HttpHeader -> NameValue
toNameValue header =
  mkNameValue (headerNameToText (httpHeaderName header)) (headerValueToText (httpHeaderValue header))

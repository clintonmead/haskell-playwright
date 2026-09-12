module Playwright.NetworkSizes
  ( BodySize (..)
  , HeadersSize (..)
  , NetworkSizes (..)
  ) where

newtype BodySize = BodySize
  { bodySizeToInt :: Int
  }
  deriving stock (Eq, Ord, Show)

newtype HeadersSize = HeadersSize
  { headersSizeToInt :: Int
  }
  deriving stock (Eq, Ord, Show)

data NetworkSizes = NetworkSizes
  { networkRequestBodySize :: BodySize
  , networkRequestHeadersSize :: HeadersSize
  , networkResponseBodySize :: BodySize
  , networkResponseHeadersSize :: HeadersSize
  }
  deriving stock (Eq, Show)

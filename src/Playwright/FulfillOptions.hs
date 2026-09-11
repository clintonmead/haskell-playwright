module Playwright.FulfillOptions
  ( ContentType (..)
  , FulfillBody (..)
  , FulfillOptions (..)
  , defaultFulfillOptions
  ) where

import Data.ByteString
  ( ByteString
  )
import Data.Text
  ( Text
  )
import Playwright.HttpHeader
  ( HttpHeader
  )
import Playwright.HttpStatus
  ( HttpStatus
  )

newtype ContentType = ContentType
  { contentTypeToText :: Text
  }
  deriving stock (Eq, Show)

data FulfillBody
  = FulfillText Text
  | FulfillBytes ByteString
  deriving stock (Eq, Show)

data FulfillOptions = FulfillOptions
  { fulfillBody :: Maybe FulfillBody
  , fulfillContentType :: Maybe ContentType
  , fulfillHeaders :: [HttpHeader]
  , fulfillStatus :: Maybe HttpStatus
  }
  deriving stock (Eq, Show)

defaultFulfillOptions :: FulfillOptions
defaultFulfillOptions =
  FulfillOptions
    { fulfillBody = Nothing
    , fulfillContentType = Nothing
    , fulfillHeaders = []
    , fulfillStatus = Nothing
    }

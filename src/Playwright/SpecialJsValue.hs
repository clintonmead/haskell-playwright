module Playwright.SpecialJsValue
  ( SpecialJsValue (..)
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , stringConstCodec
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Data.List.NonEmpty
  ( NonEmpty ((:|))
  )

data SpecialJsValue
  = SpecialNull
  | SpecialUndefined
  | SpecialNaN
  | SpecialInfinity
  | SpecialNegInfinity
  | SpecialNegZero
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec SpecialJsValue)

instance HasCodec SpecialJsValue where
  codec =
    stringConstCodec $
      (SpecialNull, "null")
        :| [ (SpecialUndefined, "undefined")
           , (SpecialNaN, "NaN")
           , (SpecialInfinity, "Infinity")
           , (SpecialNegInfinity, "-Infinity")
           , (SpecialNegZero, "-0")
           ]

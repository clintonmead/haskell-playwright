module Playwright.Timeout
  ( Timeout
  , milliseconds
  , timeoutToDouble
  , defaultTimeout
  , defaultLaunchTimeout
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , bimapCodec
  , named
  , scientificCodec
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Data.Scientific
  ( Scientific
  , fromFloatDigits
  , toRealFloat
  )

newtype Timeout = Timeout Double
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec Timeout)

milliseconds :: Double -> Maybe Timeout
milliseconds n =
  if n >= 0
    then Just (Timeout n)
    else Nothing

timeoutToDouble :: Timeout -> Double
timeoutToDouble (Timeout n) = n

defaultTimeout :: Timeout
defaultTimeout = Timeout 30000

defaultLaunchTimeout :: Timeout
defaultLaunchTimeout = Timeout 180000

instance HasCodec Timeout where
  codec =
    named "Timeout" $
      bimapCodec decodeTimeout encodeTimeout scientificCodec

decodeTimeout :: Scientific -> Either String Timeout
decodeTimeout scientific =
  case milliseconds (toRealFloat scientific) of
    Just timeout ->
      Right timeout
    Nothing ->
      Left "Timeout must be non-negative"

encodeTimeout :: Timeout -> Scientific
encodeTimeout = fromFloatDigits . timeoutToDouble

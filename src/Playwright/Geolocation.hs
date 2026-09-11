module Playwright.Geolocation
  ( Accuracy (..)
  , Geolocation (..)
  , Latitude (..)
  , Longitude (..)
  , latitudeFromInt
  , longitudeFromInt
  ) where

import Data.Scientific
  ( Scientific
  )

newtype Latitude = Latitude
  { latitudeToScientific :: Scientific
  }
  deriving stock (Eq, Show)

newtype Longitude = Longitude
  { longitudeToScientific :: Scientific
  }
  deriving stock (Eq, Show)

newtype Accuracy = Accuracy
  { accuracyToScientific :: Scientific
  }
  deriving stock (Eq, Show)

data Geolocation = Geolocation
  { geolocationAccuracy :: Maybe Accuracy
  , geolocationLatitude :: Latitude
  , geolocationLongitude :: Longitude
  }
  deriving stock (Eq, Show)

latitudeFromInt :: Int -> Latitude
latitudeFromInt value =
  Latitude (fromIntegral value)

longitudeFromInt :: Int -> Longitude
longitudeFromInt value =
  Longitude (fromIntegral value)

module Playwright.DeviceScaleFactor
  ( DeviceScaleFactor (..)
  , deviceScaleFactorFromInt
  ) where

import Data.Scientific
  ( Scientific
  )

newtype DeviceScaleFactor = DeviceScaleFactor
  { deviceScaleFactorToScientific :: Scientific
  }
  deriving stock (Eq, Show)

deviceScaleFactorFromInt :: Int -> DeviceScaleFactor
deviceScaleFactorFromInt value =
  DeviceScaleFactor (fromIntegral value)

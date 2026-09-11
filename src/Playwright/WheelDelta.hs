module Playwright.WheelDelta
  ( WheelDelta (..)
  , wheelDeltaFromInt
  ) where

import Data.Scientific
  ( Scientific
  )

newtype WheelDelta = WheelDelta
  { wheelDeltaToScientific :: Scientific
  }
  deriving stock (Eq, Show)

wheelDeltaFromInt :: Int -> WheelDelta
wheelDeltaFromInt n =
  WheelDelta (fromIntegral n)

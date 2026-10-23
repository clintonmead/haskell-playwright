module Playwright.AriaSnapshotOptions
  ( AriaSnapshotOptions (..)
  , defaultAriaSnapshotOptions
  ) where

import Playwright.AriaSnapshotMode
  ( AriaSnapshotMode
  )
import Playwright.Timeout
  ( Timeout
  )

data AriaSnapshotOptions = AriaSnapshotOptions
  { ariaSnapshotOptionsMode :: Maybe AriaSnapshotMode
  , ariaSnapshotOptionsDepth :: Maybe Int
  , ariaSnapshotOptionsBoxes :: Maybe Bool
  , ariaSnapshotOptionsTimeout :: Maybe Timeout
  }
  deriving stock (Eq, Show)

defaultAriaSnapshotOptions :: AriaSnapshotOptions
defaultAriaSnapshotOptions =
  AriaSnapshotOptions
    { ariaSnapshotOptionsMode = Nothing
    , ariaSnapshotOptionsDepth = Nothing
    , ariaSnapshotOptionsBoxes = Nothing
    , ariaSnapshotOptionsTimeout = Nothing
    }

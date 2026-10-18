module Playwright.DragAndDropOptions
  ( DragAndDropOptions (..)
  , defaultDragAndDropOptions
  ) where

import Playwright.Point
  ( Point
  )
import Playwright.Timeout
  ( Timeout
  )

data DragAndDropOptions = DragAndDropOptions
  { dragAndDropOptionsForce :: Maybe Bool
  , dragAndDropOptionsSourcePosition :: Maybe Point
  , dragAndDropOptionsSteps :: Maybe Int
  , dragAndDropOptionsTargetPosition :: Maybe Point
  , dragAndDropOptionsTimeout :: Maybe Timeout
  , dragAndDropOptionsTrial :: Maybe Bool
  }
  deriving stock (Eq, Show)

defaultDragAndDropOptions :: DragAndDropOptions
defaultDragAndDropOptions =
  DragAndDropOptions
    { dragAndDropOptionsForce = Nothing
    , dragAndDropOptionsSourcePosition = Nothing
    , dragAndDropOptionsSteps = Nothing
    , dragAndDropOptionsTargetPosition = Nothing
    , dragAndDropOptionsTimeout = Nothing
    , dragAndDropOptionsTrial = Nothing
    }

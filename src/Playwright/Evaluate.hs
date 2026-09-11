module Playwright.Evaluate
  ( evaluate
  , evaluateBool
  , evaluateOnLocator
  , evaluateOnLocatorBool
  , evaluateOnLocatorText
  , evaluateOnLocatorWith
  , evaluateText
  ) where

import Control.Exception
  ( throwIO
  )
import Data.Text
  ( Text
  )
import Playwright.Error
  ( PlaywrightError (EvaluateUnexpected)
  )
import Playwright.Frame
  ( evalOnSelectorFrame
  , evaluateFrame
  )
import Playwright.Locator
  ( Locator (locatorFrame, locatorSelector)
  )
import Playwright.Page
  ( Page (pageMainFrame)
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  , undefinedArgument
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadBool, PayloadString)
  , SerializedValue (SerializedNode)
  )

evaluate :: Page -> Text -> IO SerializedValue
evaluate page = evaluateFrame (pageMainFrame page)

evaluateOnLocator :: Locator -> Text -> IO SerializedValue
evaluateOnLocator loc expression =
  evaluateOnLocatorWith loc expression undefinedArgument

evaluateOnLocatorWith :: Locator -> Text -> SerializedArgument -> IO SerializedValue
evaluateOnLocatorWith loc expression arg =
  evalOnSelectorFrame (locatorFrame loc) (locatorSelector loc) expression arg

evaluateText :: Page -> Text -> IO Text
evaluateText page expression =
  requireText =<< evaluate page expression

evaluateBool :: Page -> Text -> IO Bool
evaluateBool page expression =
  requireBool =<< evaluate page expression

evaluateOnLocatorText :: Locator -> Text -> IO Text
evaluateOnLocatorText loc expression =
  requireText =<< evaluateOnLocator loc expression

evaluateOnLocatorBool :: Locator -> Text -> IO Bool
evaluateOnLocatorBool loc expression =
  requireBool =<< evaluateOnLocator loc expression

requireText :: SerializedValue -> IO Text
requireText value =
  case value of
    SerializedNode (PayloadString text) _ ->
      pure text
    _ ->
      throwIO (EvaluateUnexpected value)

requireBool :: SerializedValue -> IO Bool
requireBool value =
  case value of
    SerializedNode (PayloadBool flag) _ ->
      pure flag
    _ ->
      throwIO (EvaluateUnexpected value)

module Playwright.Evaluate
  ( evaluate
  , evaluateBool
  , evaluateNumber
  , evaluateOnLocator
  , evaluateOnLocatorAllTexts
  , evaluateOnLocatorBool
  , evaluateOnLocatorText
  , evaluateOnLocatorWith
  , evaluateSpecial
  , evaluateText
  , evaluateTexts
  , evaluateWith
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
  ( evalOnSelectorAllFrame
  , evalOnSelectorFrame
  , evaluateFrame
  , evaluateFrameWith
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
  ( SerializedPayload (PayloadArray, PayloadBool, PayloadNumber, PayloadSpecial, PayloadString)
  , SerializedValue (SerializedNode)
  )
import Playwright.SpecialJsValue
  ( SpecialJsValue
  )

evaluate :: Page -> Text -> IO SerializedValue
evaluate page = evaluateFrame (pageMainFrame page)

evaluateWith :: Page -> Text -> SerializedArgument -> IO SerializedValue
evaluateWith page =
  evaluateFrameWith (pageMainFrame page)

evaluateOnLocator :: Locator -> Text -> IO SerializedValue
evaluateOnLocator loc expression =
  evaluateOnLocatorWith loc expression undefinedArgument

evaluateOnLocatorWith :: Locator -> Text -> SerializedArgument -> IO SerializedValue
evaluateOnLocatorWith loc expression arg =
  evalOnSelectorFrame (locatorFrame loc) (locatorSelector loc) expression arg

evaluateOnLocatorAllTexts :: Locator -> Text -> IO [Text]
evaluateOnLocatorAllTexts loc expression =
  requireTexts
    =<< evalOnSelectorAllFrame (locatorFrame loc) (locatorSelector loc) expression undefinedArgument

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

evaluateNumber :: Page -> Text -> IO Double
evaluateNumber page expression =
  requireNumber =<< evaluate page expression

evaluateSpecial :: Page -> Text -> IO SpecialJsValue
evaluateSpecial page expression =
  requireSpecial =<< evaluate page expression

evaluateTexts :: Page -> Text -> IO [Text]
evaluateTexts page expression =
  requireTexts =<< evaluate page expression

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

requireNumber :: SerializedValue -> IO Double
requireNumber value =
  case value of
    SerializedNode (PayloadNumber number) _ ->
      pure number
    _ ->
      throwIO (EvaluateUnexpected value)

requireSpecial :: SerializedValue -> IO SpecialJsValue
requireSpecial value =
  case value of
    SerializedNode (PayloadSpecial special) _ ->
      pure special
    _ ->
      throwIO (EvaluateUnexpected value)

requireTexts :: SerializedValue -> IO [Text]
requireTexts value =
  case value of
    SerializedNode (PayloadArray items) _ ->
      traverse requireText items
    _ ->
      throwIO (EvaluateUnexpected value)

module Playwright.Evaluate
  ( evaluate
  , evaluateBool
  , evaluateBoolWith
  , evaluateNumber
  , evaluateNumberWith
  , evaluateNumbersWith
  , evaluateOnLocator
  , evaluateOnLocatorAllTexts
  , evaluateOnLocatorBool
  , evaluateOnLocatorText
  , evaluateOnLocatorWith
  , evaluateSpecial
  , evaluateSpecialWith
  , evaluateText
  , evaluateTextWith
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

evaluateTextWith :: Page -> Text -> SerializedArgument -> IO Text
evaluateTextWith page expression arg =
  requireText =<< evaluateWith page expression arg

evaluateBool :: Page -> Text -> IO Bool
evaluateBool page expression =
  requireBool =<< evaluate page expression

evaluateBoolWith :: Page -> Text -> SerializedArgument -> IO Bool
evaluateBoolWith page expression arg =
  requireBool =<< evaluateWith page expression arg

evaluateOnLocatorText :: Locator -> Text -> IO Text
evaluateOnLocatorText loc expression =
  requireText =<< evaluateOnLocator loc expression

evaluateOnLocatorBool :: Locator -> Text -> IO Bool
evaluateOnLocatorBool loc expression =
  requireBool =<< evaluateOnLocator loc expression

evaluateNumber :: Page -> Text -> IO Double
evaluateNumber page expression =
  requireNumber =<< evaluate page expression

evaluateNumberWith :: Page -> Text -> SerializedArgument -> IO Double
evaluateNumberWith page expression arg =
  requireNumber =<< evaluateWith page expression arg

evaluateNumbersWith :: Page -> Text -> SerializedArgument -> IO [Double]
evaluateNumbersWith page expression arg =
  requireNumbers =<< evaluateWith page expression arg

evaluateSpecial :: Page -> Text -> IO SpecialJsValue
evaluateSpecial page expression =
  requireSpecial =<< evaluate page expression

evaluateSpecialWith :: Page -> Text -> SerializedArgument -> IO SpecialJsValue
evaluateSpecialWith page expression arg =
  requireSpecial =<< evaluateWith page expression arg

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

requireNumbers :: SerializedValue -> IO [Double]
requireNumbers value =
  case value of
    SerializedNode (PayloadArray items) _ ->
      traverse requireNumber items
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

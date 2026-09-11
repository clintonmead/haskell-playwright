module Playwright.Json.Decode
  ( fromJsonValue
  ) where

import Data.Aeson
  ( FromJSON
  , Result (Error, Success)
  , Value
  , fromJSON
  )
import Data.Text qualified as Text
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  )

fromJsonValue :: (FromJSON a) => Value -> Either PlaywrightError a
fromJsonValue value =
  case fromJSON value of
    Error err ->
      Left (DecodeFailure (Text.pack err))
    Success decoded ->
      Right decoded

module Playwright.Json.Decode
  ( fromJsonValue
  , throwDecode
  ) where

import Control.Monad.Catch
  ( MonadThrow
  , throwM
  )
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

throwDecode :: (MonadThrow m, FromJSON a) => Value -> m a
throwDecode value =
  either throwM pure (fromJsonValue value)

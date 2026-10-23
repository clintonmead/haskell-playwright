module Playwright.AttributeValue
  ( AttributeValue (..)
  , renderAttributeValue
  , renderNumber
  ) where

import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import Data.Text qualified as T

-- | The right-hand side of an attribute clause. Regular expressions keep
-- their source text and canonically ordered flags.
data AttributeValue
  = AttributeString Text
  | AttributeNumber Scientific
  | AttributeBool Bool
  | AttributeRegex Text Text
  deriving stock (Eq, Show)

-- | Renders a value the way JavaScript's template-string conversion does,
-- which is what the upstream error messages show.
renderAttributeValue :: AttributeValue -> Text
renderAttributeValue value = case value of
  AttributeString s -> s
  AttributeNumber n -> renderNumber n
  AttributeBool True -> "true"
  AttributeBool False -> "false"
  AttributeRegex source flags -> "/" <> source <> "/" <> flags

renderNumber :: Scientific -> Text
renderNumber n = case floatingOrInteger n of
  Right i -> T.pack (show (i :: Integer))
  Left d -> T.pack (show (d :: Double))

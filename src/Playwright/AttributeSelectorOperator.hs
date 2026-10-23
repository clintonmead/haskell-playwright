module Playwright.AttributeSelectorOperator
  ( AttributeSelectorOperator (..)
  , renderOperator
  ) where

import Data.Text (Text)

-- | The comparison used by one attribute clause of a selector such as
-- @BookItem[name *= "foo" i]@. 'OpTruthy' is the bare @[name]@ form.
data AttributeSelectorOperator
  = OpTruthy
  | OpEqual
  | OpContains
  | OpDashMatch
  | OpPrefix
  | OpSuffix
  | OpWord
  deriving stock (Eq, Show)

renderOperator :: AttributeSelectorOperator -> Text
renderOperator op = case op of
  OpTruthy -> "<truthy>"
  OpEqual -> "="
  OpContains -> "*="
  OpDashMatch -> "|="
  OpPrefix -> "^="
  OpSuffix -> "$="
  OpWord -> "~="

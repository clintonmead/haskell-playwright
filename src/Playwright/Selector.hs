module Playwright.Selector
  ( Selector
  , selectorFromText
  , selectorToText
  , nestedSelector
  ) where

import Autodocodec
  ( HasCodec
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Data.Text
  ( Text
  )

newtype Selector = Selector Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, HasCodec)

selectorFromText :: Text -> Selector
selectorFromText = Selector

selectorToText :: Selector -> Text
selectorToText (Selector text) = text

nestedSelector :: Selector -> Selector -> Selector
nestedSelector (Selector parent) (Selector child) =
  Selector (parent <> " >> " <> child)

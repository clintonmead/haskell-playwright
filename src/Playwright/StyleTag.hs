module Playwright.StyleTag
  ( StyleTag (..)
  , emptyStyleTag
  ) where

import Data.Text
  ( Text
  )
import Playwright.Url
  ( Url
  )

data StyleTag = StyleTag
  { styleTagContent :: Maybe Text
  , styleTagUrl :: Maybe Url
  , styleTagPath :: Maybe FilePath
  }
  deriving stock (Eq, Show)

emptyStyleTag :: StyleTag
emptyStyleTag =
  StyleTag
    { styleTagContent = Nothing
    , styleTagUrl = Nothing
    , styleTagPath = Nothing
    }

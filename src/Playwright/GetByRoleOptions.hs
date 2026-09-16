module Playwright.GetByRoleOptions
  ( GetByRoleOptions (..)
  , defaultGetByRoleOptions
  ) where

import Data.Text
  ( Text
  )
import Playwright.Exactness
  ( Exactness (Inexact)
  )

data GetByRoleOptions = GetByRoleOptions
  { getByRoleChecked :: Maybe Bool
  , getByRoleDisabled :: Maybe Bool
  , getByRoleExact :: Exactness
  , getByRoleExpanded :: Maybe Bool
  , getByRoleIncludeHidden :: Maybe Bool
  , getByRoleLevel :: Maybe Int
  , getByRoleName :: Maybe Text
  , getByRoleNameRegex :: Maybe Text
  , getByRolePressed :: Maybe Bool
  , getByRoleSelected :: Maybe Bool
  }
  deriving stock (Eq, Show)

defaultGetByRoleOptions :: GetByRoleOptions
defaultGetByRoleOptions =
  GetByRoleOptions
    { getByRoleChecked = Nothing
    , getByRoleDisabled = Nothing
    , getByRoleExact = Inexact
    , getByRoleExpanded = Nothing
    , getByRoleIncludeHidden = Nothing
    , getByRoleLevel = Nothing
    , getByRoleName = Nothing
    , getByRoleNameRegex = Nothing
    , getByRolePressed = Nothing
    , getByRoleSelected = Nothing
    }

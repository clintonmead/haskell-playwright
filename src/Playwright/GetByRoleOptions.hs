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
  { getByRoleName :: Maybe Text
  , getByRoleExact :: Exactness
  }
  deriving stock (Eq, Show)

defaultGetByRoleOptions :: GetByRoleOptions
defaultGetByRoleOptions =
  GetByRoleOptions
    { getByRoleName = Nothing
    , getByRoleExact = Inexact
    }

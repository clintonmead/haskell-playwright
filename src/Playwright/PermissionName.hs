module Playwright.PermissionName
  ( PermissionName (..)
  ) where

import Data.Text
  ( Text
  )

newtype PermissionName = PermissionName
  { permissionNameToText :: Text
  }
  deriving stock (Eq, Show)

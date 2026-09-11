module Playwright.StorageItem
  ( StorageItem (..)
  , StorageName (..)
  , StorageValue (..)
  ) where

import Data.Text
  ( Text
  )

newtype StorageName = StorageName
  { storageNameToText :: Text
  }
  deriving stock (Eq, Show)

newtype StorageValue = StorageValue
  { storageValueToText :: Text
  }
  deriving stock (Eq, Show)

data StorageItem = StorageItem
  { storageItemName :: StorageName
  , storageItemValue :: StorageValue
  }
  deriving stock (Eq, Show)

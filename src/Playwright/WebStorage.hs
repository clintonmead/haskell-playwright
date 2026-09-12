module Playwright.WebStorage
  ( WebStorage (..)
  , localStorage
  , sessionStorage
  , storageClear
  , storageGetItem
  , storageItems
  , storageRemoveItem
  , storageSetItem
  ) where

import Playwright.Monad
  ( Playwright
  )
import Playwright.Channel
  ( send
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Page
  ( Page (pageChannel)
  )
import Playwright.Protocol
  ( NameValue (nameValueName, nameValueValue)
  , PageWebStorageClearParamsKind (PageWebStorageClearParamsKindLocal, PageWebStorageClearParamsKindSession)
  , PageWebStorageClearResult
  , PageWebStorageGetItemParamsKind (PageWebStorageGetItemParamsKindLocal, PageWebStorageGetItemParamsKindSession)
  , PageWebStorageGetItemResult (pageWebStorageGetItemResultValue)
  , PageWebStorageItemsParamsKind (PageWebStorageItemsParamsKindLocal, PageWebStorageItemsParamsKindSession)
  , PageWebStorageItemsResult (pageWebStorageItemsResultItems)
  , PageWebStorageRemoveItemParamsKind (PageWebStorageRemoveItemParamsKindLocal, PageWebStorageRemoveItemParamsKindSession)
  , PageWebStorageRemoveItemResult
  , PageWebStorageSetItemParamsKind (PageWebStorageSetItemParamsKindLocal, PageWebStorageSetItemParamsKindSession)
  , PageWebStorageSetItemResult
  , mkPageWebStorageClearParams
  , mkPageWebStorageGetItemParams
  , mkPageWebStorageItemsParams
  , mkPageWebStorageRemoveItemParams
  , mkPageWebStorageSetItemParams
  )
import Playwright.StorageItem
  ( StorageItem (StorageItem, storageItemName, storageItemValue)
  , StorageName (StorageName, storageNameToText)
  , StorageValue (StorageValue, storageValueToText)
  )
import Playwright.StorageKind
  ( StorageKind (StorageLocal, StorageSession)
  )

data WebStorage = WebStorage
  { webStoragePage :: Page
  , webStorageKind :: StorageKind
  }

localStorage :: Page -> WebStorage
localStorage page =
  WebStorage page StorageLocal

sessionStorage :: Page -> WebStorage
sessionStorage page =
  WebStorage page StorageSession

storageItems :: WebStorage -> Playwright [StorageItem]
storageItems storage = do
  result <-
    send
      (pageChannel (webStoragePage storage))
      (MethodName "webStorageItems")
      (mkPageWebStorageItemsParams (itemsKind (webStorageKind storage)))
  pure (fmap fromNameValue (pageWebStorageItemsResultItems result))

storageGetItem :: WebStorage -> StorageName -> Playwright (Maybe StorageValue)
storageGetItem storage name = do
  result <-
    send
      (pageChannel (webStoragePage storage))
      (MethodName "webStorageGetItem")
      ( mkPageWebStorageGetItemParams
          (getItemKind (webStorageKind storage))
          (storageNameToText name)
      )
  pure (fmap StorageValue (pageWebStorageGetItemResultValue result))

storageSetItem :: WebStorage -> StorageName -> StorageValue -> Playwright ()
storageSetItem storage name value = do
  _ :: PageWebStorageSetItemResult <-
    send
      (pageChannel (webStoragePage storage))
      (MethodName "webStorageSetItem")
      ( mkPageWebStorageSetItemParams
          (setItemKind (webStorageKind storage))
          (storageNameToText name)
          (storageValueToText value)
      )
  pure ()

storageRemoveItem :: WebStorage -> StorageName -> Playwright ()
storageRemoveItem storage name = do
  _ :: PageWebStorageRemoveItemResult <-
    send
      (pageChannel (webStoragePage storage))
      (MethodName "webStorageRemoveItem")
      ( mkPageWebStorageRemoveItemParams
          (removeItemKind (webStorageKind storage))
          (storageNameToText name)
      )
  pure ()

storageClear :: WebStorage -> Playwright ()
storageClear storage = do
  _ :: PageWebStorageClearResult <-
    send
      (pageChannel (webStoragePage storage))
      (MethodName "webStorageClear")
      (mkPageWebStorageClearParams (clearKind (webStorageKind storage)))
  pure ()

fromNameValue :: NameValue -> StorageItem
fromNameValue item =
  StorageItem
    { storageItemName = StorageName (nameValueName item)
    , storageItemValue = StorageValue (nameValueValue item)
    }

itemsKind :: StorageKind -> PageWebStorageItemsParamsKind
itemsKind kind =
  case kind of
    StorageLocal ->
      PageWebStorageItemsParamsKindLocal
    StorageSession ->
      PageWebStorageItemsParamsKindSession

getItemKind :: StorageKind -> PageWebStorageGetItemParamsKind
getItemKind kind =
  case kind of
    StorageLocal ->
      PageWebStorageGetItemParamsKindLocal
    StorageSession ->
      PageWebStorageGetItemParamsKindSession

setItemKind :: StorageKind -> PageWebStorageSetItemParamsKind
setItemKind kind =
  case kind of
    StorageLocal ->
      PageWebStorageSetItemParamsKindLocal
    StorageSession ->
      PageWebStorageSetItemParamsKindSession

removeItemKind :: StorageKind -> PageWebStorageRemoveItemParamsKind
removeItemKind kind =
  case kind of
    StorageLocal ->
      PageWebStorageRemoveItemParamsKindLocal
    StorageSession ->
      PageWebStorageRemoveItemParamsKindSession

clearKind :: StorageKind -> PageWebStorageClearParamsKind
clearKind kind =
  case kind of
    StorageLocal ->
      PageWebStorageClearParamsKindLocal
    StorageSession ->
      PageWebStorageClearParamsKindSession

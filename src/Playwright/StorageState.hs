module Playwright.StorageState
  ( OriginStorageState (..)
  , StorageState (..)
  , emptyStorageState
  , fromNetworkStorage
  , fromRequestStorageState
  , fromStorageStateResult
  , toNewContextStorageState
  , toSetStorageState
  ) where

import Playwright.Cookie
  ( Cookie
  , fromNetworkCookie
  , toSetNetworkFromCookie
  )
import Playwright.Protocol
  ( APIRequestContextStorageStateResult (aPIRequestContextStorageStateResultCookies, aPIRequestContextStorageStateResultOrigins)
  , BrowserContextSetStorageStateParamsStorageState (browserContextSetStorageStateParamsStorageStateCookies, browserContextSetStorageStateParamsStorageStateOrigins)
  , BrowserContextStorageStateResult (browserContextStorageStateResultCookies, browserContextStorageStateResultOrigins)
  , BrowserNewContextParamsStorageState (browserNewContextParamsStorageStateCookies, browserNewContextParamsStorageStateOrigins)
  , NameValue (nameValueName, nameValueValue)
  , NetworkCookie
  , OriginStorage (originStorageLocalStorage, originStorageOrigin)
  , SetOriginStorage
  , mkBrowserContextSetStorageStateParamsStorageState
  , mkBrowserNewContextParamsStorageState
  , mkNameValue
  , mkSetOriginStorage
  )
import Playwright.StorageItem
  ( StorageItem (StorageItem, storageItemName, storageItemValue)
  , StorageName (StorageName, storageNameToText)
  , StorageValue (StorageValue, storageValueToText)
  )
import Playwright.Url
  ( Url
  , urlFromText
  , urlToText
  )

data OriginStorageState = OriginStorageState
  { originStateOrigin :: Url
  , originStateLocalStorage :: [StorageItem]
  }
  deriving stock (Eq, Show)

data StorageState = StorageState
  { storageStateCookies :: [Cookie]
  , storageStateOrigins :: [OriginStorageState]
  }
  deriving stock (Eq, Show)

emptyStorageState :: StorageState
emptyStorageState =
  StorageState
    { storageStateCookies = []
    , storageStateOrigins = []
    }

fromStorageStateResult :: BrowserContextStorageStateResult -> StorageState
fromStorageStateResult result =
  fromNetworkStorage
    (browserContextStorageStateResultCookies result)
    (browserContextStorageStateResultOrigins result)

fromRequestStorageState :: APIRequestContextStorageStateResult -> StorageState
fromRequestStorageState result =
  fromNetworkStorage
    (aPIRequestContextStorageStateResultCookies result)
    (aPIRequestContextStorageStateResultOrigins result)

fromNetworkStorage :: [NetworkCookie] -> [OriginStorage] -> StorageState
fromNetworkStorage cookies origins =
  StorageState
    { storageStateCookies = fmap fromNetworkCookie cookies
    , storageStateOrigins = fmap fromOrigin origins
    }

toSetStorageState :: StorageState -> BrowserContextSetStorageStateParamsStorageState
toSetStorageState state =
  mkBrowserContextSetStorageStateParamsStorageState
    { browserContextSetStorageStateParamsStorageStateCookies =
        Just (fmap toSetNetworkFromCookie (storageStateCookies state))
    , browserContextSetStorageStateParamsStorageStateOrigins =
        Just (fmap toSetOrigin (storageStateOrigins state))
    }

toNewContextStorageState :: StorageState -> BrowserNewContextParamsStorageState
toNewContextStorageState state =
  mkBrowserNewContextParamsStorageState
    { browserNewContextParamsStorageStateCookies =
        Just (fmap toSetNetworkFromCookie (storageStateCookies state))
    , browserNewContextParamsStorageStateOrigins =
        Just (fmap toSetOrigin (storageStateOrigins state))
    }

fromOrigin :: OriginStorage -> OriginStorageState
fromOrigin origin =
  OriginStorageState
    { originStateOrigin = urlFromText (originStorageOrigin origin)
    , originStateLocalStorage = fmap fromNameValue (originStorageLocalStorage origin)
    }

toSetOrigin :: OriginStorageState -> SetOriginStorage
toSetOrigin origin =
  mkSetOriginStorage
    (fmap toNameValue (originStateLocalStorage origin))
    (urlToText (originStateOrigin origin))

fromNameValue :: NameValue -> StorageItem
fromNameValue pair =
  StorageItem
    { storageItemName = StorageName (nameValueName pair)
    , storageItemValue = StorageValue (nameValueValue pair)
    }

toNameValue :: StorageItem -> NameValue
toNameValue item =
  mkNameValue
    (storageNameToText (storageItemName item))
    (storageValueToText (storageItemValue item))

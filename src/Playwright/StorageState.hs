module Playwright.StorageState
  ( OriginStorageState (..)
  , StorageState (..)
  , StorageStateOptions (..)
  , decodeStorageState
  , defaultStorageStateOptions
  , emptyStorageState
  , encodeStorageState
  , fromNetworkStorage
  , fromRequestStorageState
  , fromStorageStateResult
  , toNewContextStorageState
  , toRequestStorageState
  , toSetStorageState
  ) where

import Data.Aeson
  ( Object
  , Result (Error, Success)
  , Value (Object)
  , eitherDecodeStrict
  , encode
  , fromJSON
  , toJSON
  )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Playwright.Cookie
  ( Cookie
  , fromNetworkCookie
  , toNetworkCookie
  , toSetNetworkFromCookie
  )
import Playwright.Protocol
  ( APIRequestContextStorageStateResult (APIRequestContextStorageStateResult, aPIRequestContextStorageStateResultCookies, aPIRequestContextStorageStateResultOrigins)
  , BrowserContextSetStorageStateParamsStorageState (browserContextSetStorageStateParamsStorageStateCookies, browserContextSetStorageStateParamsStorageStateOrigins)
  , BrowserContextStorageStateResult (browserContextStorageStateResultCookies, browserContextStorageStateResultOrigins)
  , BrowserNewContextParamsStorageState (browserNewContextParamsStorageStateCookies, browserNewContextParamsStorageStateOrigins)
  , IndexedDBDatabase
  , NameValue (nameValueName, nameValueValue)
  , NetworkCookie
  , OPFSEntry
  , OriginStorage (OriginStorage, originStorageIndexedDB, originStorageLocalStorage, originStorageOpfs, originStorageOrigin)
  , PlaywrightNewRequestParamsStorageState (playwrightNewRequestParamsStorageStateCookies, playwrightNewRequestParamsStorageStateOrigins)
  , SetOriginStorage (setOriginStorageIndexedDB, setOriginStorageOpfs)
  , mkBrowserContextSetStorageStateParamsStorageState
  , mkBrowserNewContextParamsStorageState
  , mkNameValue
  , mkPlaywrightNewRequestParamsStorageState
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

-- | One origin's storage. 'originStateIndexedDB' and 'originStateOpfs' are
-- 'Nothing' when the driver did not report that kind of storage; they are
-- only populated when the export asked for them.
data OriginStorageState = OriginStorageState
  { originStateOrigin :: Url
  , originStateLocalStorage :: [StorageItem]
  , originStateIndexedDB :: Maybe [IndexedDBDatabase]
  , originStateOpfs :: Maybe [OPFSEntry]
  }
  deriving stock (Eq, Show)

data StorageState = StorageState
  { storageStateCookies :: [Cookie]
  , storageStateOrigins :: [OriginStorageState]
  }
  deriving stock (Eq, Show)

-- | Options for exporting a storage state. 'storageStateOptionsPath' writes
-- the exported state to that file as JSON after it has been read.
data StorageStateOptions = StorageStateOptions
  { storageStateOptionsIndexedDB :: Bool
  , storageStateOptionsOpfs :: Bool
  , storageStateOptionsPath :: Maybe FilePath
  }
  deriving stock (Eq, Show)

defaultStorageStateOptions :: StorageStateOptions
defaultStorageStateOptions =
  StorageStateOptions
    { storageStateOptionsIndexedDB = False
    , storageStateOptionsOpfs = False
    , storageStateOptionsPath = Nothing
    }

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

toRequestStorageState :: StorageState -> PlaywrightNewRequestParamsStorageState
toRequestStorageState state =
  mkPlaywrightNewRequestParamsStorageState
    { playwrightNewRequestParamsStorageStateCookies =
        Just (fmap toNetworkCookie (storageStateCookies state))
    , playwrightNewRequestParamsStorageStateOrigins =
        Just (fmap toSetOrigin (storageStateOrigins state))
    }

-- | Render a storage state in the Playwright file format: an object with
-- @cookies@ and @origins@, where each origin carries @origin@,
-- @localStorage@ and optionally @indexedDB@ and @opfs@.
encodeStorageState :: StorageState -> LBS.ByteString
encodeStorageState state =
  encode (toJSON result)
  where
    result =
      APIRequestContextStorageStateResult
        { aPIRequestContextStorageStateResultCookies = fmap toNetworkCookie (storageStateCookies state)
        , aPIRequestContextStorageStateResultOrigins = fmap toOriginStorage (storageStateOrigins state)
        }

-- | Parse a storage state file. Missing @cookies@ or @origins@ keys read as
-- empty lists, so @{}@ is an empty storage state.
decodeStorageState :: BS.ByteString -> Either String StorageState
decodeStorageState bytes = do
  value <- eitherDecodeStrict bytes
  object <- case value of
    Object fields -> pure (withDefaults fields)
    _ -> Left "storage state must be a JSON object"
  case fromJSON (Object object) of
    Success result -> pure (fromRequestStorageState result)
    Error message -> Left message
  where
    withDefaults :: Object -> Object
    withDefaults fields =
      KeyMap.union fields (KeyMap.fromList [("cookies", toJSON ([] :: [NetworkCookie])), ("origins", toJSON ([] :: [OriginStorage]))])

fromOrigin :: OriginStorage -> OriginStorageState
fromOrigin origin =
  OriginStorageState
    { originStateOrigin = urlFromText (originStorageOrigin origin)
    , originStateLocalStorage = fmap fromNameValue (originStorageLocalStorage origin)
    , originStateIndexedDB = originStorageIndexedDB origin
    , originStateOpfs = originStorageOpfs origin
    }

toOriginStorage :: OriginStorageState -> OriginStorage
toOriginStorage origin =
  OriginStorage
    { originStorageIndexedDB = originStateIndexedDB origin
    , originStorageLocalStorage = fmap toNameValue (originStateLocalStorage origin)
    , originStorageOpfs = originStateOpfs origin
    , originStorageOrigin = urlToText (originStateOrigin origin)
    }

toSetOrigin :: OriginStorageState -> SetOriginStorage
toSetOrigin origin =
  (mkSetOriginStorage
    (fmap toNameValue (originStateLocalStorage origin))
    (urlToText (originStateOrigin origin)))
    { setOriginStorageIndexedDB = originStateIndexedDB origin
    , setOriginStorageOpfs = originStateOpfs origin
    }

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

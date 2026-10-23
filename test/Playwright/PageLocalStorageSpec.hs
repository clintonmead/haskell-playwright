module Playwright.PageLocalStorageSpec
  ( tests
  ) where

import Data.List
  ( sortOn
  )
import Data.Text
  ( Text
  , replace
  )
import Playwright
  ( StorageItem (storageItemName, storageItemValue)
  , StorageName (StorageName, storageNameToText)
  , StorageValue (StorageValue, storageValueToText)
  , defaultGotoOptions
  , evaluateText
  , goto
  , localStorage
  , sessionStorage
  , storageClear
  , storageGetItem
  , storageItems
  , storageRemoveItem
  , storageSetItem
  , urlFromText
  )
import Playwright.Harness
  ( withAssets
  , assertEqual
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-localstorage"
    [ testCase "localStorage.items returns empty array on fresh origin" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "empty" [] =<< storageItems (localStorage page)
    , testCase "localStorage.getItem returns null for missing key" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          missing <- storageGetItem (localStorage page) (StorageName "absent")
          assertEqual "missing" True (maybe True (const False) missing)
    , testCase "localStorage.setItem persists and surfaces in items()/getItem()" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          storageSetItem (localStorage page) (StorageName "alpha") (StorageValue "1")
          storageSetItem (localStorage page) (StorageName "beta") (StorageValue "2")
          items <- storageItems (localStorage page)
          let sorted = sortOn (storageNameToText . storageItemName) items
          assertEqual
            "items"
            [("alpha", "1"), ("beta", "2")]
            (fmap pairItem sorted)
          got <- storageGetItem (localStorage page) (StorageName "alpha")
          assertEqual "getItem" (Just "1") (fmap storageValueToText got)
          assertEqual "evaluate" "1" =<< evaluateText page "() => localStorage.getItem('alpha')"
    , testCase "localStorage.setItem overwrites existing value" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          storageSetItem (localStorage page) (StorageName "k") (StorageValue "first")
          storageSetItem (localStorage page) (StorageName "k") (StorageValue "second")
          got <- storageGetItem (localStorage page) (StorageName "k")
          assertEqual "second" (Just "second") (fmap storageValueToText got)
    , testCase "localStorage.removeItem removes a single item" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          storageSetItem (localStorage page) (StorageName "a") (StorageValue "1")
          storageSetItem (localStorage page) (StorageName "b") (StorageValue "2")
          storageRemoveItem (localStorage page) (StorageName "a")
          items <- storageItems (localStorage page)
          assertEqual "remaining" [("b", "2")] (fmap pairItem items)
    , testCase "localStorage.clear empties storage" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          storageSetItem (localStorage page) (StorageName "a") (StorageValue "1")
          storageSetItem (localStorage page) (StorageName "b") (StorageValue "2")
          storageClear (localStorage page)
          assertEqual "empty" [] =<< storageItems (localStorage page)
    , testCase "sessionStorage round-trip" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "empty" [] =<< storageItems (sessionStorage page)
          storageSetItem (sessionStorage page) (StorageName "s1") (StorageValue "v1")
          storageSetItem (sessionStorage page) (StorageName "s2") (StorageValue "v2")
          items <- storageItems (sessionStorage page)
          let sorted = sortOn (storageNameToText . storageItemName) items
          assertEqual "items" [("s1", "v1"), ("s2", "v2")] (fmap pairItem sorted)
          storageRemoveItem (sessionStorage page) (StorageName "s1")
          remaining <- storageItems (sessionStorage page)
          assertEqual "s2" [("s2", "v2")] (fmap pairItem remaining)
          storageClear (sessionStorage page)
          assertEqual "cleared" [] =<< storageItems (sessionStorage page)
    , testCase "localStorage and sessionStorage are independent" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          storageSetItem (localStorage page) (StorageName "shared") (StorageValue "local")
          storageSetItem (sessionStorage page) (StorageName "shared") (StorageValue "session")
          localGot <- storageGetItem (localStorage page) (StorageName "shared")
          sessionGot <- storageGetItem (sessionStorage page) (StorageName "shared")
          assertEqual "local" (Just "local") (fmap storageValueToText localGot)
          assertEqual "session" (Just "session") (fmap storageValueToText sessionGot)
          storageClear (localStorage page)
          assertEqual "local empty" [] =<< storageItems (localStorage page)
          still <- storageGetItem (sessionStorage page) (StorageName "shared")
          assertEqual "session kept" (Just "session") (fmap storageValueToText still)
    , testCase "storage methods are scoped to the current origin" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          storageSetItem (localStorage page) (StorageName "k") (StorageValue "origin-1")
          _ <- goto page (urlFromText (crossOrigin prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "other origin items" [] =<< storageItems (localStorage page)
          storageSetItem (localStorage page) (StorageName "k") (StorageValue "origin-2")
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          got <- storageGetItem (localStorage page) (StorageName "k")
          assertEqual "original origin" (Just "origin-1") (fmap storageValueToText got)
    ]

-- | The same test server reached through a different host name, which makes
-- a different origin.
crossOrigin :: Text -> Text
crossOrigin =
  replace "127.0.0.1" "localhost"

pairItem :: StorageItem -> (Text, Text)
pairItem item =
  (storageNameToText (storageItemName item), storageValueToText (storageItemValue item))

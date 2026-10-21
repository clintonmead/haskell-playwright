module Playwright.BrowserContextStorageStateSpec
  ( tests
  ) where

import Data.Maybe
  ( listToMaybe
  )
import Data.Text
  ( Text
  )
import Playwright
  ( Cookie (cookieName)
  , CookieName (CookieName, cookieNameToText)
  , CookieValue (CookieValue)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody)
  , NewContextOptions (newContextStorageState)
  , OriginStorageState (OriginStorageState, originStateLocalStorage, originStateOrigin)
  , Page
  , StorageItem (StorageItem, storageItemName, storageItemValue)
  , StorageName (StorageName, storageNameToText)
  , StorageState (storageStateCookies, storageStateOrigins)
  , StorageValue (StorageValue, storageValueToText)
  , UrlMatch (UrlMatchGlob)
  , addCookies
  , cookieForUrl
  , defaultFulfillOptions
  , defaultGotoOptions
  , defaultNewContextOptions
  , emptyStorageState
  , evaluate
  , evaluateText
  , fulfillRoute
  , goto
  , route
  , setStorageState
  , storageState
  , urlFromText
  , urlToText
  , Playwright
  )
import Playwright.Harness
  ( withAssetsContextWith
  , withChromiumContext
  , withChromiumContextWith
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
    "browsercontext-storage-state"
    [ testCase "should capture cookies" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          addCookies
            ctx
            [cookieForUrl (urlFromText (prefix <> "/empty.html")) (CookieName "password") (CookieValue "123456")]
          state <- storageState ctx
          let
            names = fmap (cookieNameToText . cookieName) (storageStateCookies state)
          assertEqual "cookie" True (elem "password" names)
    , testCase "should capture local storage" $
        withChromiumContext $ \ctx page -> do
          fulfillBlank page
          _ <- goto page (urlFromText "https://www.example.com") defaultGotoOptions
          _ <- evaluate page "() => { localStorage['name1'] = 'value1'; }"
          _ <- goto page (urlFromText "https://www.domain.com") defaultGotoOptions
          _ <- evaluate page "() => { localStorage['name2'] = 'value2'; }"
          state <- storageState ctx
          assertEqual "example" (Just "value1") (originValue state "https://www.example.com" "name1")
          assertEqual "domain" (Just "value2") (originValue state "https://www.domain.com" "name2")
    , testCase "should set local storage" $
        withChromiumContextWith
          defaultNewContextOptions {newContextStorageState = Just seeded}
          $ \_ page -> do
            fulfillBlank page
            _ <- goto page (urlFromText "https://www.example.com") defaultGotoOptions
            assertEqual "value" "value1" =<< evaluateText page "() => window.localStorage['name1']"
    , testCase "should replace storage with setStorageState" $
        withChromiumContextWith
          defaultNewContextOptions {newContextStorageState = Just seeded}
          $ \ctx page -> do
            fulfillBlank page
            _ <- goto page (urlFromText "https://www.example.com") defaultGotoOptions
            assertEqual "before" "value1" =<< evaluateText page "() => window.localStorage['name1']"
            setStorageState ctx replaced
            _ <- goto page (urlFromText "https://www.example.com") defaultGotoOptions
            assertEqual "after" "value2" =<< evaluateText page "() => window.localStorage['name1']"
    ]

seeded :: StorageState
seeded =
  emptyStorageState
    { storageStateOrigins =
        [ OriginStorageState
            { originStateOrigin = urlFromText "https://www.example.com"
            , originStateLocalStorage =
                [StorageItem (StorageName "name1") (StorageValue "value1")]
            }
        ]
    }

replaced :: StorageState
replaced =
  emptyStorageState
    { storageStateOrigins =
        [ OriginStorageState
            { originStateOrigin = urlFromText "https://www.example.com"
            , originStateLocalStorage =
                [StorageItem (StorageName "name1") (StorageValue "value2")]
            }
        ]
    }

fulfillBlank :: Page -> Playwright ()
fulfillBlank page =
  route page (UrlMatchGlob "**/*") $ \handled ->
    fulfillRoute handled defaultFulfillOptions {fulfillBody = Just (FulfillText " ")}

originValue :: StorageState -> Text -> Text -> Maybe Text
originValue state origin name = do
  originState <-
    listToMaybe
      ( filter
          (\item -> urlToText (originStateOrigin item) == origin)
          (storageStateOrigins state)
      )
  item <-
    listToMaybe
      ( filter
          (\entry -> storageNameToText (storageItemName entry) == name)
          (originStateLocalStorage originState)
      )
  pure (storageValueToText (storageItemValue item))

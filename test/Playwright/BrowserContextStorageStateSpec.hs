module Playwright.BrowserContextStorageStateSpec
  ( tests
  ) where

import Data.Maybe
  ( listToMaybe
  )
import Data.Text
  ( Text
  )
import Data.Text
  ( isInfixOf
  )
import UnliftIO.Exception
  ( try
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright
  ( BrowserContext
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody)
  , UrlMatch (UrlMatchGlob)
  , defaultFulfillOptions
  , fulfillRoute
  , newPage
  , route
  , NewContextOptions (newContextStorageState)
  , OriginStorageState (OriginStorageState)
  , StorageItem (StorageItem)
  , StorageName (StorageName)
  , StorageValue (StorageValue)
  , StorageState (StorageState)
  , defaultNewContextOptions
  , newContextWith
  , Cookie (cookieName)
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
  ( assertBool
  , assertFailure
  , withChromiumBrowser
  , withAssetsContextWith
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
    , testCase "should report good error if the url is not valid" $
        withChromiumBrowser $ \browser -> do
          result <-
            try
              ( newContextWith
                  browser
                  defaultNewContextOptions
                    { newContextStorageState =
                        Just
                          ( StorageState
                              []
                              [OriginStorageState (urlFromText "foo") [StorageItem (StorageName "name1") (StorageValue "value1")]]
                          )
                    }
              )
              :: Playwright (Either PlaywrightError BrowserContext)
          case result of
            Left (ProtocolRpc failure) -> do
              assertBool "setting" ("Error setting storage state:" `isInfixOf` protocolFailureMessage failure)
              assertBool "origin" ("foo" `isInfixOf` protocolFailureMessage failure)
            Left err ->
              assertFailure ("expected ProtocolRpc, got: " <> show err)
            Right _ ->
              assertFailure "expected the storage state to be refused"
    , testCase "should not restore localStorage twice" $
        withChromiumBrowser $ \browser -> do
          context <-
            newContextWith
              browser
              defaultNewContextOptions
                { newContextStorageState =
                    Just
                      ( StorageState
                          []
                          [OriginStorageState (urlFromText "https://www.example.com") [StorageItem (StorageName "name1") (StorageValue "value1")]]
                      )
                }
          page <- newPage context
          route page (UrlMatchGlob "**/*") $ \handled ->
            fulfillRoute handled defaultFulfillOptions {fulfillBody = Just (FulfillText "<html></html>")}
          _ <- goto page (urlFromText "https://www.example.com") defaultGotoOptions
          assertEqual "first" "value1" =<< evaluateText page "() => localStorage.getItem('name1')"
          _ <- evaluateText page "() => { localStorage.setItem('name1', 'value2'); return 'set'; }"
          _ <- goto page (urlFromText "https://www.example.com") defaultGotoOptions
          assertEqual "second" "value2" =<< evaluateText page "() => localStorage.getItem('name1')"
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

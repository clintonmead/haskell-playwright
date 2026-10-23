module Playwright.BrowserContextStorageStateSpec
  ( tests
  ) where

import Data.Maybe
  ( listToMaybe
  )
import Data.Text
  ( Text
  , isInfixOf
  , replace
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , rawPathInfo
  , responseLBS
  )
import UnliftIO.Exception
  ( try
  )
import UnliftIO.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright
  ( BrowserContext
  , Cookie (cookieName)
  , CookieName (CookieName, cookieNameToText)
  , CookieValue (CookieValue)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody)
  , NewContextOptions (newContextStorageState)
  , OriginStorageState (OriginStorageState, originStateLocalStorage, originStateOrigin)
  , Page
  , Playwright
  , StorageItem (StorageItem, storageItemName, storageItemValue)
  , StorageName (StorageName, storageNameToText)
  , StorageState (StorageState, storageStateCookies, storageStateOrigins)
  , StorageValue (StorageValue, storageValueToText)
  , UrlMatch (UrlMatchGlob)
  , addCookies
  , closeContext
  , cookieForUrl
  , defaultFulfillOptions
  , defaultGotoOptions
  , defaultNewContextOptions
  , emptyStorageState
  , evaluate
  , evaluateOnFrameText
  , evaluateText
  , fulfillRoute
  , goto
  , newContextWith
  , newPage
  , onContextPageClose
  , onContextPageLoad
  , onRequest
  , onRequestFailed
  , onRequestFinished
  , onResponse
  , route
  , setStorageState
  , storageState
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , attachFrame
  , withAssetsContextWith
  , withChromiumBrowser
  , withChromiumContext
  , withChromiumContextWith
  , withLocalApp
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
    , testCase "should set local storage in third-party context" $
        withChromiumBrowser $ \browser ->
          withLocalApp blankApp $ \prefix -> do
            let cross = replace "127.0.0.1" "localhost" prefix
            ctx <-
              newContextWith
                browser
                defaultNewContextOptions {newContextStorageState = Just (crossOriginState cross)}
            page <- newPage ctx
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            frame <- attachFrame page "frame1" (cross <> "/empty.html")
            assertEqual "third-party" "value1" =<< evaluateOnFrameText frame "() => window.localStorage.getItem('name1')"
            closeContext ctx
    , testCase "should roundtrip local storage in third-party context" $
        withChromiumBrowser $ \browser ->
          withLocalApp blankApp $ \prefix -> do
            let cross = replace "127.0.0.1" "localhost" prefix
            ctx <- newContextWith browser defaultNewContextOptions
            page <- newPage ctx
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            frame <- attachFrame page "frame1" (cross <> "/empty.html")
            _ <- evaluateOnFrameText frame "() => { window.localStorage.setItem('name1', 'value1'); return 'set'; }"
            state <- storageState ctx
            ctx2 <- newContextWith browser defaultNewContextOptions {newContextStorageState = Just state}
            page2 <- newPage ctx2
            _ <- goto page2 (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            frame2 <- attachFrame page2 "frame1" (cross <> "/empty.html")
            assertEqual "roundtrip" "value1" =<< evaluateOnFrameText frame2 "() => window.localStorage.getItem('name1')"
            closeContext ctx2
            closeContext ctx
    , testCase "should not emit events about internal page" $
        -- Partial port: context-level request and response events are not exposed
        -- by this library, so only the page-level request events are subscribed to here.
        withChromiumContext $ \ctx page -> do
          fulfillWith "<html></html>" page
          _ <- goto page (urlFromText "https://www.example.com") defaultGotoOptions
          _ <- evaluate page "() => { localStorage['name1'] = 'value1'; }"
          _ <- goto page (urlFromText "https://www.domain.com") defaultGotoOptions
          _ <- evaluate page "() => { localStorage['name2'] = 'value2'; }"
          events <- newIORef (0 :: Int)
          onContextPageLoad ctx (countEvent events)
          onContextPageClose ctx (countEvent events)
          onRequest page (countEvent events)
          onResponse page (countEvent events)
          onRequestFailed page (countEvent events)
          onRequestFinished page (countEvent events)
          _ <- storageState ctx
          count <- readIORef events
          assertEqual "events" 0 count
    , testCase "should work when service worker is intefering" $
        withChromiumContext $ \ctx page ->
          withLocalApp serviceWorkerApp $ \prefix -> do
            _ <- goto page (urlFromText prefix) defaultGotoOptions
            _ <- evaluate page "() => window['activationPromise']"
            state <- storageState ctx
            assertEqual "foo" (Just "bar") (originValue state prefix "foo")
    -- Skipped: these titles have no Haskell counterpart in the current API. See QUESTIONS.md.
    -- "should round-trip through the file"
    -- "should round-trip OPFS"
    -- "should handle missing file"
    -- "should handle malformed file"
    -- "should serialize storageState with lone surrogates"
    -- "should support IndexedDB"
    -- "should support empty indexedDB"
    -- "should not leave IndexedDB connections open"
    -- "should round-trip WebAuthn credentials with storageState"
    -- "setStorageState should replace credentials"
    -- "setStorageState should handle missing file"
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

crossOriginState :: Text -> StorageState
crossOriginState origin =
  emptyStorageState
    { storageStateOrigins =
        [ OriginStorageState
            { originStateOrigin = urlFromText origin
            , originStateLocalStorage =
                [StorageItem (StorageName "name1") (StorageValue "value1")]
            }
        ]
    }

fulfillWith :: Text -> Page -> Playwright ()
fulfillWith body page =
  route page (UrlMatchGlob "**/*") $ \handled ->
    fulfillRoute handled defaultFulfillOptions {fulfillBody = Just (FulfillText body)}

fulfillBlank :: Page -> Playwright ()
fulfillBlank = fulfillWith " "

countEvent :: IORef Int -> a -> Playwright ()
countEvent events _ = modifyIORef' events (+ 1)

blankApp :: Application
blankApp _ respond =
  respond (responseLBS status200 [(hContentType, "text/html")] "<html></html>")

-- Serves a page that registers a service worker, and a worker that passes every fetch through.
serviceWorkerApp :: Application
serviceWorkerApp request respond =
  respond $
    case rawPathInfo request of
      "/sw.js" ->
        responseLBS
          status200
          [(hContentType, "application/javascript")]
          ( "self.addEventListener('install', () => self.skipWaiting());\n"
              <> "self.addEventListener('activate', event => event.waitUntil(self.clients.claim()));\n"
              <> "self.addEventListener('fetch', event => event.respondWith(fetch(event.request)));\n"
          )
      _ ->
        responseLBS
          status200
          [(hContentType, "text/html")]
          ( "<script>\n"
              <> "window.localStorage.foo = 'bar';\n"
              <> "window.activationPromise = new Promise(resolve => { navigator.serviceWorker.oncontrollerchange = resolve; });\n"
              <> "navigator.serviceWorker.register('/sw.js');\n"
              <> "</script>\n"
          )

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

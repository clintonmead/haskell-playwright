module Playwright.GlobalFetchCookieSpec
  ( tests
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import Control.Concurrent
  ( Chan
  , newChan
  , readChan
  , writeChan
  )
import Data.ByteString
  ( ByteString
  )
import Data.Text
  ( Text
  , pack
  )
import Network.HTTP.Types
  ( RequestHeaders
  , hContentType
  , status200
  , status404
  )
import Network.HTTP.Types qualified as HTTP
import Network.Wai
  ( Application
  , pathInfo
  , requestHeaders
  , responseLBS
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( APIRequestContext
  , Cookie (Cookie, cookieDomain, cookieExpires, cookieHttpOnly, cookieName, cookiePath, cookieSameSite, cookieSecure, cookieValue)
  , CookieDomain (CookieDomain)
  , CookieExpires (CookieExpires)
  , CookieHttpOnly (CookieHttpOnly)
  , CookieName (CookieName, cookieNameToText)
  , CookiePath (CookiePath)
  , CookieSameSite (CookieSameSiteLax)
  , CookieSecure (CookieSecure)
  , CookieValue (CookieValue, cookieValueToText)
  , NewRequestOptions (newRequestStorageState)
  , OriginStorageState (OriginStorageState, originStateLocalStorage, originStateOrigin)
  , StorageItem (StorageItem, storageItemName, storageItemValue)
  , StorageName (StorageName)
  , StorageState (StorageState, storageStateCookies, storageStateOrigins)
  , StorageValue (StorageValue)
  , apiRequestStorageState
  , apiResponseOk
  , defaultNewRequestOptions
  , disposeAPIRequestContext
  , emptyStorageState
  , fetchGet
  , newRequestContext
  , newRequestContextWith
  , sessionExpires
  , urlFromText
  , runPlaywright
  , Playwright
  )
import Playwright.Harness
  ( assertEqual
  )
import Test.Tasty
  ( TestTree
  , localOption
  , mkTimeout
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

tests :: TestTree
tests =
  localOption (mkTimeout 30000000) $
    testGroup
      "global-fetch-cookie"
      [ testCase "should filter outgoing cookies by path" $
          pathFilterTest
      , testCase "should send secure cookie over http for localhost" $
          localhostSecureCookieTest
      , testCase "should send secure cookie over http for subdomains of localhost" $
          subdomainSecureCookieTest
      , testCase "should send not expired cookies" $
          notExpiredCookieTest
      , testCase "should remove expired cookies" $
          removeExpiredCookieTest
      , testCase "should remove cookie with negative max-age" $
          negativeMaxAgeTest
      , testCase "should remove cookie with expires far in the past" $
          pastExpiresTest
      , testCase "should override cookie from Set-Cookie header" $
          overrideCookieTest
      , testCase "should override cookie from Set-Cookie header even if it expired" $
          overrideExpiredCookieTest
      , testCase "should preserve local storage on import/export of storage state" $
          storageStateRoundTripTest
      , testCase "should work with empty storage state" $
          emptyStorageStateTest
      ]

pathFilterTest :: IO ()
pathFilterTest =
  withCookieServer pathFilterApp $ \request prefix captured -> do
    _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
    _ <- fetchGet request (urlFromText (prefix <> "/input/button.html"))
    cookie <- liftIO (readChan captured)
    assertEqual "cookie" (Just "b=v; c=v") cookie

pathFilterApp :: Chan (Maybe ByteString) -> Application
pathFilterApp captured request respond =
  case pathInfo request of
    ["setcookie.html"] ->
      respond
        ( responseLBS
            status200
            [ ("Set-Cookie", "a=v; path=/input/subfolder")
            , ("Set-Cookie", "b=v; path=/input")
            , ("Set-Cookie", "c=v;")
            ]
            ""
        )
    ["input", "button.html"] ->
      captureCookie captured request respond
    _ ->
      respond (responseLBS status404 [] "")

localhostSecureCookieTest :: IO ()
localhostSecureCookieTest =
  withCookieServerOn "localhost" secureCookieApp $ \request prefix captured -> do
    _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
    _ <- fetchGet request (urlFromText (prefix <> "/empty.html"))
    cookie <- liftIO (readChan captured)
    assertEqual "cookie" (Just "a=v; b=v") cookie

subdomainSecureCookieTest :: IO ()
subdomainSecureCookieTest =
  withCookieServerOn "a.b.localhost" secureCookieApp $ \request prefix captured -> do
    _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
    _ <- fetchGet request (urlFromText (prefix <> "/empty.html"))
    cookie <- liftIO (readChan captured)
    assertEqual "cookie" (Just "a=v; b=v") cookie

secureCookieApp :: Chan (Maybe ByteString) -> Application
secureCookieApp captured request respond =
  case pathInfo request of
    ["setcookie.html"] ->
      respond
        ( responseLBS
            status200
            [("Set-Cookie", "a=v; secure"), ("Set-Cookie", "b=v")]
            ""
        )
    ["empty.html"] ->
      captureCookie captured request respond
    _ ->
      respond (responseLBS status404 [] "")

notExpiredCookieTest :: IO ()
notExpiredCookieTest =
  withCookieServer (setThenEmpty [("Set-Cookie", "a=v"), ("Set-Cookie", "b=v; expires=" <> futureHttpDate)]) $
    \request prefix captured -> do
      _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
      _ <- fetchGet request (urlFromText (prefix <> "/empty.html"))
      cookie <- liftIO (readChan captured)
      assertEqual "cookie" (Just "a=v; b=v") cookie

removeExpiredCookieTest :: IO ()
removeExpiredCookieTest =
  withCookieServer (setThenEmpty [("Set-Cookie", "a=v"), ("Set-Cookie", "b=v; expires=" <> pastHttpDate)]) $
    \request prefix captured -> do
      _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
      _ <- fetchGet request (urlFromText (prefix <> "/empty.html"))
      cookie <- liftIO (readChan captured)
      assertEqual "cookie" (Just "a=v") cookie

negativeMaxAgeTest :: IO ()
negativeMaxAgeTest =
  withCookieServer negativeMaxAgeApp $ \request prefix captured -> do
    _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
    _ <- fetchGet request (urlFromText (prefix <> "/removecookie.html"))
    _ <- fetchGet request (urlFromText (prefix <> "/empty.html"))
    cookie <- liftIO (readChan captured)
    assertEqual "cookie" (Just "c=v") cookie

negativeMaxAgeApp :: Chan (Maybe ByteString) -> Application
negativeMaxAgeApp captured request respond =
  case pathInfo request of
    ["setcookie.html"] ->
      respond
        ( responseLBS
            status200
            [ ("Set-Cookie", "a=v; max-age=100000")
            , ("Set-Cookie", "b=v; max-age=100000")
            , ("Set-Cookie", "c=v")
            ]
            ""
        )
    ["removecookie.html"] ->
      respond
        ( responseLBS
            status200
            [("Set-Cookie", "a=v; max-age=-2000000000"), ("Set-Cookie", "b=v; max-age=-1")]
            ""
        )
    ["empty.html"] ->
      captureCookie captured request respond
    _ ->
      respond (responseLBS status404 [] "")

pastExpiresTest :: IO ()
pastExpiresTest =
  withCookieServer pastExpiresApp $ \request prefix captured -> do
    _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
    _ <- fetchGet request (urlFromText (prefix <> "/removecookie.html"))
    _ <- fetchGet request (urlFromText (prefix <> "/empty.html"))
    cookie <- liftIO (readChan captured)
    assertEqual "cookie" Nothing cookie

pastExpiresApp :: Chan (Maybe ByteString) -> Application
pastExpiresApp captured request respond =
  case pathInfo request of
    ["setcookie.html"] ->
      respond (responseLBS status200 [("Set-Cookie", "a=v; max-age=1000000")] "")
    ["removecookie.html"] ->
      respond
        ( responseLBS
            status200
            [("Set-Cookie", "a=v; expires=" <> pastHttpDate)]
            ""
        )
    ["empty.html"] ->
      captureCookie captured request respond
    _ ->
      respond (responseLBS status404 [] "")

overrideCookieTest :: IO ()
overrideCookieTest =
  withCookieServer overrideApp $ \request prefix _captured -> do
    _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
    _ <- fetchGet request (urlFromText (prefix <> "/updatecookie.html"))
    state <- apiRequestStorageState request
    case storageStateCookies state of
      [cookie] -> do
        assertEqual "name" "a" (cookieNameToText (cookieName cookie))
        assertEqual "value" "new" (cookieValueToText (cookieValue cookie))
        assertEqual "expires" (CookieExpires (fromInteger 4102444800)) (cookieExpires cookie)
      cookies ->
        assertEqual "one cookie" 1 (length cookies)

overrideApp :: Chan (Maybe ByteString) -> Application
overrideApp _captured request respond =
  case pathInfo request of
    ["setcookie.html"] ->
      respond
        ( responseLBS
            status200
            [("Set-Cookie", "a=old; expires=" <> futureHttpDate)]
            ""
        )
    ["updatecookie.html"] ->
      respond
        ( responseLBS
            status200
            [("Set-Cookie", "a=new; expires=" <> laterHttpDate)]
            ""
        )
    _ ->
      respond (responseLBS status404 [] "")

overrideExpiredCookieTest :: IO ()
overrideExpiredCookieTest =
  withCookieServer unsetCookieApp $ \request prefix captured -> do
    _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
    _ <- fetchGet request (urlFromText (prefix <> "/unsetsetcookie.html"))
    _ <- fetchGet request (urlFromText (prefix <> "/empty.html"))
    cookie <- liftIO (readChan captured)
    assertEqual "cookie" Nothing cookie

unsetCookieApp :: Chan (Maybe ByteString) -> Application
unsetCookieApp captured request respond =
  case pathInfo request of
    ["setcookie.html"] ->
      respond
        ( responseLBS
            status200
            [("Set-Cookie", "a=ok"), ("Set-Cookie", "b=ok; expires=" <> futureHttpDate)]
            ""
        )
    ["unsetsetcookie.html"] ->
      respond
        ( responseLBS
            status200
            [ ("Set-Cookie", "a=; expires=" <> pastHttpDate)
            , ("Set-Cookie", "b=; expires=" <> pastHttpDate)
            ]
            ""
        )
    ["empty.html"] ->
      captureCookie captured request respond
    _ ->
      respond (responseLBS status404 [] "")

storageStateRoundTripTest :: IO ()
storageStateRoundTripTest =
  withApplication (pure emptyHtmlApp) $ \port ->
    runPlaywright $ do
      request <-
        newRequestContextWith
          defaultNewRequestOptions {newRequestStorageState = Just seededState}
      _ <- fetchGet request (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
      exported <- apiRequestStorageState request
      assertEqual "cookies" (storageStateCookies seededState) (storageStateCookies exported)
      assertEqual "origins" (storageStateOrigins seededState) (storageStateOrigins exported)
      disposeAPIRequestContext request

emptyStorageStateTest :: IO ()
emptyStorageStateTest =
  withApplication (pure emptyHtmlApp) $ \port ->
    runPlaywright $ do
      request <-
        newRequestContextWith
          defaultNewRequestOptions {newRequestStorageState = Just emptyStorageState}
      state <- apiRequestStorageState request
      assertEqual "empty" emptyStorageState state
      response <- fetchGet request (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
      assertEqual "ok" True (apiResponseOk response)
      disposeAPIRequestContext request

seededState :: StorageState
seededState =
  StorageState
    { storageStateCookies =
        [ Cookie
            { cookieName = CookieName "a"
            , cookieValue = CookieValue "b"
            , cookieDomain = CookieDomain "a.b.one.com"
            , cookiePath = CookiePath "/"
            , cookieExpires = sessionExpires
            , cookieHttpOnly = CookieHttpOnly False
            , cookieSecure = CookieSecure False
            , cookieSameSite = CookieSameSiteLax
            }
        ]
    , storageStateOrigins =
        [ OriginStorageState
            { originStateOrigin = urlFromText "https://www.example.com"
            , originStateLocalStorage =
                [ StorageItem
                    { storageItemName = StorageName "name1"
                    , storageItemValue = StorageValue "value1"
                    }
                ]
            }
        ]
    }

emptyHtmlApp :: Application
emptyHtmlApp _request respond =
  respond (responseLBS status200 [(hContentType, "text/html")] "<html></html>")

setThenEmpty :: RequestHeaders -> Chan (Maybe ByteString) -> Application
setThenEmpty setHeaders captured request respond =
  case pathInfo request of
    ["setcookie.html"] ->
      respond (responseLBS status200 setHeaders "")
    ["empty.html"] ->
      captureCookie captured request respond
    _ ->
      respond (responseLBS status404 [] "")

captureCookie :: Chan (Maybe ByteString) -> Application
captureCookie captured request respond = do
  writeChan captured (lookupHeader "cookie" (requestHeaders request))
  respond (responseLBS status200 [(hContentType, "text/html")] "<html></html>")

withCookieServer :: (Chan (Maybe ByteString) -> Application) -> (APIRequestContext -> Text -> Chan (Maybe ByteString) -> Playwright ()) -> IO ()
withCookieServer =
  withCookieServerOn "127.0.0.1"

withCookieServerOn ::
  Text ->
  (Chan (Maybe ByteString) -> Application) ->
  (APIRequestContext -> Text -> Chan (Maybe ByteString) -> Playwright ()) ->
  IO ()
withCookieServerOn host app action = do
  captured <- newChan
  withApplication (pure (app captured)) $ \port ->
    runPlaywright $ do
      request <- newRequestContext
      action request ("http://" <> host <> ":" <> pack (show port)) captured
      disposeAPIRequestContext request

lookupHeader :: HTTP.HeaderName -> RequestHeaders -> Maybe ByteString
lookupHeader =
  lookup

futureHttpDate :: ByteString
futureHttpDate =
  "Thu, 01 Jan 2099 00:00:00 GMT"

laterHttpDate :: ByteString
laterHttpDate =
  "Sat, 01 Jan 2100 00:00:00 GMT"

pastHttpDate :: ByteString
pastHttpDate =
  "Thu, 01 Jan 1970 00:00:00 GMT"

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
import Data.Char
  ( isDigit
  )
import Data.Text
  ( Text
  , pack
  , takeWhileEnd
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
      , testCase "should store cookie from Set-Cookie header" $
          storeCookieTest
      , testCase "should filter outgoing cookies by domain" $
          filterByDomainTest
      , testCase "should do case-insensitive match of cookie domain" $
          caseInsensitiveCookieDomainTest
      , testCase "should do case-insensitive match of request domain" $
          caseInsensitiveRequestDomainTest
      , testCase "should store cookie from Set-Cookie header even if it contains equal signs" $
          equalSignsCookieTest
      , testCase "should export cookies to storage state" $
          exportCookiesTest
      , testCase "should send cookies from storage state" $
          sendFromStorageStateTest
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

storeCookieTest :: IO ()
storeCookieTest =
  withCookieServerOn "a.b.localhost" storeCookieApp $ \request prefix captured -> do
    _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
    _ <- fetchGet request (urlFromText (atHost "b.localhost" prefix <> "/input/button.html"))
    cookie <- liftIO (readChan captured)
    assertEqual "cookie" (Just "c=d") cookie

storeCookieApp :: Chan (Maybe ByteString) -> Application
storeCookieApp captured request respond =
  case pathInfo request of
    ["setcookie.html"] ->
      respond
        ( responseLBS
            status200
            [ ("Set-Cookie", "a=b")
            , ("Set-Cookie", "c=d; max-age=3600; domain=b.localhost; path=/input")
            , ("Set-Cookie", "e=f; domain=b.localhost; path=/input/subfolder")
            ]
            ""
        )
    ["input", "button.html"] ->
      captureCookie captured request respond
    _ ->
      respond (responseLBS status404 [] "")

filterByDomainTest :: IO ()
filterByDomainTest =
  withCookieServerOn "a.b.one.localhost" (setThenEmpty [("Set-Cookie", "a=v; domain=one.localhost"), ("Set-Cookie", "b=v; domain=.b.one.localhost"), ("Set-Cookie", "c=v; domain=other.com")]) $
    \request prefix captured -> do
      _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
      _ <- fetchGet request (urlFromText (atHost "www.b.one.localhost" prefix <> "/empty.html"))
      cookie <- liftIO (readChan captured)
      assertEqual "cookie" (Just "a=v; b=v") cookie
      _ <- fetchGet request (urlFromText (atHost "127.0.0.1" prefix <> "/empty.html"))
      unrelated <- liftIO (readChan captured)
      assertEqual "unrelated host cookie" Nothing unrelated

caseInsensitiveCookieDomainTest :: IO ()
caseInsensitiveCookieDomainTest =
  withCookieServerOn "a.b.one.localhost" (setThenEmpty [("Set-Cookie", "a=v; domain=ONE.LOCALHOST"), ("Set-Cookie", "b=v; domain=.B.one.localhost")]) $
    \request prefix captured -> do
      _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
      _ <- fetchGet request (urlFromText (atHost "www.b.one.localhost" prefix <> "/empty.html"))
      cookie <- liftIO (readChan captured)
      assertEqual "cookie" (Just "a=v; b=v") cookie

caseInsensitiveRequestDomainTest :: IO ()
caseInsensitiveRequestDomainTest =
  withCookieServerOn "a.b.one.localhost" (setThenEmpty [("Set-Cookie", "a=v; domain=one.localhost"), ("Set-Cookie", "b=v; domain=.b.one.localhost")]) $
    \request prefix captured -> do
      _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
      _ <- fetchGet request (urlFromText (atHost "WWW.B.ONE.LOCALHOST" prefix <> "/empty.html"))
      cookie <- liftIO (readChan captured)
      assertEqual "cookie" (Just "a=v; b=v") cookie

equalSignsCookieTest :: IO ()
equalSignsCookieTest =
  withCookieServerOn "a.b.localhost" (setThenEmpty [("Set-Cookie", "f=value == value=; secure; httpOnly; path=/some=value")]) $
    \request prefix _captured -> do
      _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
      state <- apiRequestStorageState request
      assertEqual
        "state"
        StorageState
          { storageStateCookies =
              [ Cookie
                  { cookieName = CookieName "f"
                  , cookieValue = CookieValue "value == value="
                  , cookieDomain = CookieDomain "a.b.localhost"
                  , cookiePath = CookiePath "/some=value"
                  , cookieExpires = sessionExpires
                  , cookieHttpOnly = CookieHttpOnly True
                  , cookieSecure = CookieSecure True
                  , cookieSameSite = CookieSameSiteLax
                  }
              ]
          , storageStateOrigins = []
          }
        state

exportCookiesTest :: IO ()
exportCookiesTest =
  withCookieServerOn "a.b.localhost" exportApp $ \request prefix _captured -> do
    _ <- fetchGet request (urlFromText (prefix <> "/setcookie.html"))
    state <- apiRequestStorageState request
    assertEqual
      "state"
      StorageState
        { storageStateCookies =
            [ mkCookie "a" "b" "a.b.localhost" "/" sessionExpires
            , mkCookie "c" "d" ".b.localhost" "/input" (CookieExpires (fromInteger 4102444800))
            , mkCookie "e" "f" ".b.localhost" "/input/subfolder" sessionExpires
            ]
        , storageStateOrigins = []
        }
      state

exportApp :: Chan (Maybe ByteString) -> Application
exportApp _captured request respond =
  case pathInfo request of
    ["setcookie.html"] ->
      respond
        ( responseLBS
            status200
            [ ("Set-Cookie", "a=b")
            , ("Set-Cookie", "c=d; expires=" <> laterHttpDate <> "; domain=b.localhost; path=/input")
            , ("Set-Cookie", "e=f; domain=b.localhost; path=/input/subfolder")
            ]
            ""
        )
    _ ->
      respond (responseLBS status404 [] "")

sendFromStorageStateTest :: IO ()
sendFromStorageStateTest = do
  captured <- newChan
  withApplication (pure (captureCookie captured)) $ \port ->
    runPlaywright $ do
      request <-
        newRequestContextWith
          defaultNewRequestOptions
            { newRequestStorageState =
                Just
                  StorageState
                    { storageStateCookies =
                        [ mkCookie "a" "b" "a.b.localhost" "/" sessionExpires
                        , mkCookie "c" "d" ".b.localhost" "/first/" (CookieExpires (fromInteger 4070908800))
                        , mkCookie "e" "f" ".b.localhost" "/first/second" sessionExpires
                        ]
                    , storageStateOrigins = []
                    }
            }
      _ <- fetchGet request (urlFromText ("http://www.a.b.localhost:" <> pack (show port) <> "/first/second/third/not_found.html"))
      cookie <- liftIO (readChan captured)
      assertEqual "cookie" (Just "c=d; e=f") cookie
      disposeAPIRequestContext request

mkCookie :: Text -> Text -> Text -> Text -> CookieExpires -> Cookie
mkCookie name value domain path expires =
  Cookie
    { cookieName = CookieName name
    , cookieValue = CookieValue value
    , cookieDomain = CookieDomain domain
    , cookiePath = CookiePath path
    , cookieExpires = expires
    , cookieHttpOnly = CookieHttpOnly False
    , cookieSecure = CookieSecure False
    , cookieSameSite = CookieSameSiteLax
    }

-- Replaces the host of a "http://host:port" prefix, keeping the port.
atHost :: Text -> Text -> Text
atHost host prefix =
  "http://" <> host <> ":" <> takeWhileEnd isDigit prefix

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

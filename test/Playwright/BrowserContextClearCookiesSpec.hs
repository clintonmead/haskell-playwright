module Playwright.BrowserContextClearCookiesSpec
  ( tests
  ) where

import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text
  ( Text
  , pack
  , replace
  )
import Data.Text qualified as Text
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , responseLBS
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( ClearCookiesOptions (clearCookiesDomain, clearCookiesName, clearCookiesNameRegex, clearCookiesPath)
  , CookieDomain (CookieDomain)
  , CookieName (CookieName)
  , CookiePath (CookiePath)
  , CookieValue (CookieValue)
  , addCookies
  , clearCookies
  , clearCookiesNamed
  , clearCookiesWith
  , closeContext
  , cookieForHost
  , cookieForUrl
  , cookies
  , defaultClearCookiesOptions
  , defaultGotoOptions
  , evaluateText
  , evaluateTexts
  , goto
  , newContext
  , reload
  , urlFromText
  , waitForFunction
  )
import Playwright.Harness
  ( withChromiumBrowser
  , withChromiumContext
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "browsercontext-clearcookies"
    [ testCase "should clear cookies" $
        withApplication (pure emptyHtml) $ \port ->
          withChromiumContext $ \ctx page -> do
            let
              prefix = prefixFor port
              empty = prefix <> "/empty.html"
            _ <- goto page (urlFromText empty) defaultGotoOptions
            addCookies ctx [cookieForUrl (urlFromText empty) (CookieName "cookie1") (CookieValue "1")]
            assertEqual "document" "cookie1=1" =<< evaluateText page "() => document.cookie"
            clearCookies ctx
            assertEqual "jar" [] =<< cookies ctx
            _ <- reload page
            assertEqual "after reload" "" =<< evaluateText page "() => document.cookie"
    , testCase "should isolate cookies when clearing" $
        withApplication (pure emptyHtml) $ \port ->
          withChromiumBrowser $ \browser -> do
            let
              empty = urlFromText (prefixFor port <> "/empty.html")
            ctx1 <- newContext browser
            ctx2 <- newContext browser
            addCookies ctx1 [cookieForUrl empty (CookieName "page1cookie") (CookieValue "page1value")]
            addCookies ctx2 [cookieForUrl empty (CookieName "page2cookie") (CookieValue "page2value")]
            assertEqual "ctx1" (1 :: Int) . length =<< cookies ctx1
            assertEqual "ctx2" (1 :: Int) . length =<< cookies ctx2
            clearCookies ctx1
            assertEqual "ctx1 cleared" (0 :: Int) . length =<< cookies ctx1
            assertEqual "ctx2 kept" (1 :: Int) . length =<< cookies ctx2
            clearCookies ctx2
            assertEqual "ctx1 still empty" (0 :: Int) . length =<< cookies ctx1
            assertEqual "ctx2 cleared" (0 :: Int) . length =<< cookies ctx2
            closeContext ctx2
            closeContext ctx1
    , testCase "should remove cookies by name" $
        withApplication (pure emptyHtml) $ \port ->
          withChromiumContext $ \ctx page -> do
            let
              prefix = prefixFor port
              host = hostFromPrefix prefix
            addCookies
              ctx
              [ cookieForHost (CookieDomain host) (CookiePath "/") (CookieName "cookie1") (CookieValue "1")
              , cookieForHost (CookieDomain host) (CookiePath "/") (CookieName "cookie2") (CookieValue "2")
              ]
            _ <- goto page (urlFromText prefix) defaultGotoOptions
            assertEqual "both" "cookie1=1; cookie2=2" =<< evaluateText page "() => document.cookie"
            clearCookiesNamed ctx (CookieName "cookie1")
            assertEqual "remaining" "cookie2=2" =<< evaluateText page "() => document.cookie"
    , testCase "should remove cookies by name regex" $
        withApplication (pure emptyHtml) $ \port ->
          withChromiumContext $ \ctx page -> do
            let
              prefix = prefixFor port
              host = hostFromPrefix prefix
            addCookies
              ctx
              [ cookieForHost (CookieDomain host) (CookiePath "/") (CookieName "cookie1") (CookieValue "1")
              , cookieForHost (CookieDomain host) (CookiePath "/") (CookieName "cookie2") (CookieValue "2")
              ]
            _ <- goto page (urlFromText prefix) defaultGotoOptions
            assertEqual "both" "cookie1=1; cookie2=2" =<< evaluateText page "() => document.cookie"
            clearCookiesWith
              ctx
              defaultClearCookiesOptions {clearCookiesNameRegex = Just "coo.*1"}
            assertEqual "remaining" "cookie2=2" =<< evaluateText page "() => document.cookie"
    , testCase "should remove cookies by domain" $
        withApplication (pure emptyHtml) $ \port ->
          withChromiumContext $ \ctx page -> do
            let
              prefix = prefixFor port
              cross = replace "127.0.0.1" "localhost" prefix
              localHost = hostFromPrefix prefix
              crossHost = hostFromPrefix cross
            addCookies
              ctx
              [ cookieForHost (CookieDomain localHost) (CookiePath "/") (CookieName "cookie1") (CookieValue "1")
              , cookieForHost (CookieDomain crossHost) (CookiePath "/") (CookieName "cookie2") (CookieValue "2")
              ]
            _ <- goto page (urlFromText prefix) defaultGotoOptions
            assertEqual "local" "cookie1=1" =<< evaluateText page "() => document.cookie"
            _ <- goto page (urlFromText cross) defaultGotoOptions
            assertEqual "cross" "cookie2=2" =<< evaluateText page "() => document.cookie"
            clearCookiesWith
              ctx
              defaultClearCookiesOptions {clearCookiesDomain = Just (CookieDomain crossHost)}
            assertEqual "cross cleared" "" =<< evaluateText page "() => document.cookie"
            _ <- goto page (urlFromText prefix) defaultGotoOptions
            assertEqual "local kept" "cookie1=1" =<< evaluateText page "() => document.cookie"
    , testCase "should remove cookies by path" $
        withApplication (pure emptyHtml) $ \port ->
          withChromiumContext $ \ctx page -> do
            let
              prefix = prefixFor port
              host = hostFromPrefix prefix
            addCookies
              ctx
              [ cookieForHost (CookieDomain host) (CookiePath "/api/v1") (CookieName "cookie1") (CookieValue "1")
              , cookieForHost (CookieDomain host) (CookiePath "/api/v2") (CookieName "cookie2") (CookieValue "2")
              , cookieForHost (CookieDomain host) (CookiePath "/") (CookieName "cookie3") (CookieValue "3")
              ]
            _ <- goto page (urlFromText (prefix <> "/api/v1")) defaultGotoOptions
            assertEqual "v1" "cookie1=1; cookie3=3" =<< evaluateText page "() => document.cookie"
            clearCookiesWith
              ctx
              defaultClearCookiesOptions {clearCookiesPath = Just (CookiePath "/api/v1")}
            assertEqual "v1 after" "cookie3=3" =<< evaluateText page "() => document.cookie"
            _ <- goto page (urlFromText (prefix <> "/api/v2")) defaultGotoOptions
            assertEqual "v2" "cookie2=2; cookie3=3" =<< evaluateText page "() => document.cookie"
            _ <- goto page (urlFromText (prefix <> "/")) defaultGotoOptions
            assertEqual "root" "cookie3=3" =<< evaluateText page "() => document.cookie"
    , testCase "should remove cookies by name and domain" $
        withApplication (pure emptyHtml) $ \port ->
          withChromiumContext $ \ctx page -> do
            let
              prefix = prefixFor port
              cross = replace "127.0.0.1" "localhost" prefix
              localHost = hostFromPrefix prefix
              crossHost = hostFromPrefix cross
            addCookies
              ctx
              [ cookieForHost (CookieDomain localHost) (CookiePath "/") (CookieName "cookie1") (CookieValue "1")
              , cookieForHost (CookieDomain crossHost) (CookiePath "/") (CookieName "cookie1") (CookieValue "1")
              ]
            _ <- goto page (urlFromText prefix) defaultGotoOptions
            assertEqual "local" "cookie1=1" =<< evaluateText page "() => document.cookie"
            clearCookiesWith
              ctx
              defaultClearCookiesOptions
                { clearCookiesName = Just (CookieName "cookie1")
                , clearCookiesDomain = Just (CookieDomain localHost)
                }
            assertEqual "local cleared" "" =<< evaluateText page "() => document.cookie"
            _ <- goto page (urlFromText cross) defaultGotoOptions
            assertEqual "cross kept" "cookie1=1" =<< evaluateText page "() => document.cookie"
    , testCase "should not transiently delete non-matching cookies when filtering" $
        withApplication (pure emptyHtml) $ \port ->
          withChromiumContext $ \ctx page -> do
            let
              prefix = prefixFor port
              host = hostFromPrefix prefix
            addCookies
              ctx
              [ cookieForHost (CookieDomain host) (CookiePath "/") (CookieName "keep_me") (CookieValue "1")
              , cookieForHost (CookieDomain host) (CookiePath "/") (CookieName "delete_me") (CookieValue "2")
              ]
            _ <- goto page (urlFromText prefix) defaultGotoOptions
            _ <-
              evaluateText
                page
                "() => { window.__cookieEvents = []; cookieStore.addEventListener('change', event => { for (const changed of event.changed) window.__cookieEvents.push('changed ' + changed.name); for (const deleted of event.deleted) window.__cookieEvents.push('deleted ' + deleted.name); }); return 'ok'; }"
            clearCookiesNamed ctx (CookieName "delete_me")
            _ <- waitForFunction page "() => window.__cookieEvents.length > 0"
            assertEqual "events" ["deleted delete_me"]
              =<< evaluateTexts page "() => window.__cookieEvents"
            assertEqual "kept" "keep_me=1" =<< evaluateText page "() => document.cookie"
    ]

-- Official `should remove partitioned cookies by name` needs HTTPS / ignoreHTTPSErrors.

emptyHtml :: Application
emptyHtml _ respond =
  respond (responseLBS status200 [(hContentType, "text/html")] LBS8.empty)

prefixFor :: Int -> Text
prefixFor port =
  "http://127.0.0.1:" <> pack (show port)

hostFromPrefix :: Text -> Text
hostFromPrefix prefix =
  Text.takeWhile (/= ':') (Text.drop 7 prefix)

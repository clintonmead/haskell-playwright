module Playwright.BrowserContextCookiesSpec
  ( tests
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text
  ( pack
  )
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
  ( Cookie (cookieHttpOnly, cookieName, cookieSameSite, cookieValue)
  , CookieHttpOnly (cookieHttpOnlyToBool)
  , CookieName (cookieNameToText)
  , CookieSameSite (CookieSameSiteLax, CookieSameSiteStrict)
  , CookieValue (cookieValueToText)
  , cookies
  , defaultGotoOptions
  , evaluateText
  , goto
  , urlFromText
  )
import Playwright.Harness
  ( withChromiumContext
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
    "browsercontext-cookies"
    [ testCase "should return no cookies in pristine browser context" $
        withChromiumContext $ \ctx _page ->
          assertEqual "empty" [] =<< cookies ctx
    , testCase "should get a cookie" $
        withApplication (pure emptyHtml) $ \port ->
          withChromiumContext $ \ctx page -> do
            let prefix = "http://127.0.0.1:" <> pack (show port)
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            documentCookie <- evaluateText page "() => { document.cookie = 'username=John Doe'; return document.cookie; }"
            assertEqual "document" "username=John Doe" documentCookie
            jar <- cookies ctx
            assertEqual "count" 1 (length jar)
            case jar of
              [cookie] -> do
                assertEqual "name" "username" (cookieNameToText (cookieName cookie))
                assertEqual "value" "John Doe" (cookieValueToText (cookieValue cookie))
                assertEqual "httpOnly" False (cookieHttpOnlyToBool (cookieHttpOnly cookie))
                assertEqual "sameSite" CookieSameSiteLax (cookieSameSite cookie)
              _ ->
                assertEqual "one cookie" 1 (length jar)
    , testCase "should properly report httpOnly cookie" $
        withApplication (pure httpOnlyCookie) $ \port ->
          withChromiumContext $ \ctx page -> do
            _ <-
              goto
                page
                (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
                defaultGotoOptions
            jar <- cookies ctx
            assertEqual "count" 1 (length jar)
            case jar of
              [cookie] -> do
                assertEqual "name" "name" (cookieNameToText (cookieName cookie))
                assertEqual "httpOnly" True (cookieHttpOnlyToBool (cookieHttpOnly cookie))
              _ ->
                assertEqual "one cookie" 1 (length jar)
    , testCase "should properly report Strict sameSite cookie" $
        withApplication (pure strictCookie) $ \port ->
          withChromiumContext $ \ctx page -> do
            _ <-
              goto
                page
                (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
                defaultGotoOptions
            jar <- cookies ctx
            case jar of
              [cookie] ->
                assertEqual "sameSite" CookieSameSiteStrict (cookieSameSite cookie)
              _ ->
                assertEqual "one cookie" 1 (length jar)
    ]

emptyHtml :: Application
emptyHtml _req respond =
  respond
    ( responseLBS
        status200
        [(hContentType, BS8.pack "text/html")]
        (LBS8.pack "<html></html>")
    )

httpOnlyCookie :: Application
httpOnlyCookie _req respond =
  respond
    ( responseLBS
        status200
        [ (hContentType, BS8.pack "text/html")
        , ("Set-Cookie", BS8.pack "name=value;HttpOnly; Path=/")
        ]
        (LBS8.pack "<html></html>")
    )

strictCookie :: Application
strictCookie _req respond =
  respond
    ( responseLBS
        status200
        [ (hContentType, BS8.pack "text/html")
        , ("Set-Cookie", BS8.pack "name=value;SameSite=Strict")
        ]
        (LBS8.pack "<html></html>")
    )

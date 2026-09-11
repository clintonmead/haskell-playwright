module Playwright.BrowserContextAddCookiesSpec
  ( tests
  ) where

import Playwright
  ( Cookie (cookieName)
  , CookieName (CookieName, cookieNameToText)
  , CookieValue (CookieValue)
  , addCookies
  , clearCookies
  , cookieForUrl
  , cookies
  , defaultGotoOptions
  , evaluateText
  , goto
  , pageContext
  , urlFromText
  )
import Playwright.Harness
  ( withAssets
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
    "browsercontext-add-cookies"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          addCookies
            (pageContext page)
            [cookieForUrl (urlFromText (prefix <> "/empty.html")) (CookieName "password") (CookieValue "123456")]
          assertEqual "document" "password=123456" =<< evaluateText page "() => document.cookie"
    , testCase "should round-trip a url cookie" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          addCookies
            (pageContext page)
            [cookieForUrl (urlFromText (prefix <> "/empty.html")) (CookieName "password") (CookieValue "123456")]
          jar <- cookies (pageContext page)
          assertEqual "names" ["password"] (fmap (cookieNameToText . cookieName) jar)
    , testCase "should clear cookies" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          addCookies
            (pageContext page)
            [cookieForUrl (urlFromText (prefix <> "/empty.html")) (CookieName "cookie1") (CookieValue "1")]
          assertEqual "before" "cookie1=1" =<< evaluateText page "() => document.cookie"
          clearCookies (pageContext page)
          assertEqual "cleared" [] =<< cookies (pageContext page)
    ]

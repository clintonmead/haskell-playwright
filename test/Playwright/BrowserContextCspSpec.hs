module Playwright.BrowserContextCspSpec
  ( tests
  ) where

import UnliftIO.Exception
  ( try
  )
import Control.Monad
  ( void
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , responseLBS
  )
import Playwright
  ( ElementHandle
  , NewContextOptions (newContextBypassCSP)
  , evaluateText
  , newContext
  , newContextWith
  , newPage
  , Playwright
  , ScriptTag (scriptTagContent)
  , addScriptTag
  , defaultGotoOptions
  , defaultNewContextOptions
  , emptyScriptTag
  , evaluateBool
  , evaluateNumber
  , goto
  , urlFromText
  )
import Playwright.Error
  ( PlaywrightError
  )
import Playwright.Harness
  ( withAssetsWith
  , withLocalApp
  , withChromiumBrowser
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
    "browsercontext-csp"
    [ testCase "should honor CSP meta tag" $
        withAssetsWith defaultNewContextOptions $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/csp.html")) defaultGotoOptions
          assertEqual "inline blocked" False
            =<< evaluateBool page "() => window.__inlineScriptValue === 42"
          void
            ( try @Playwright @PlaywrightError
                ( addScriptTag
                    page
                    emptyScriptTag {scriptTagContent = Just "window[\"__injected\"] = 42;"}
                )
            )
          assertEqual "injected blocked" False
            =<< evaluateBool page "() => window.__injected === 42"
    , testCase "should bypass CSP meta tag" $
        withAssetsWith defaultNewContextOptions {newContextBypassCSP = Just True} $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/csp.html")) defaultGotoOptions
          assertEqual "inline" True
            =<< evaluateBool page "() => window.__inlineScriptValue === 42"
          _ <-
            addScriptTag
              page
              emptyScriptTag {scriptTagContent = Just "window[\"__injected\"] = 42;"}
          assertEqual "injected" (42 :: Double)
            =<< evaluateNumber page "() => window.__injected"
    , testCase "should bypass CSP header" $
        withChromiumBrowser $ \browser ->
          withLocalApp cspInlineApp $ \base -> do
            strict <- newContext browser
            strictPage <- newPage strict
            _ <- goto strictPage (urlFromText (base <> "/empty.html")) defaultGotoOptions
            assertEqual "strict inline" "undefined" =<< evaluateText strictPage "() => String(window['__inlineScriptValue'])"
            _ <- try (addScriptTag strictPage emptyScriptTag {scriptTagContent = Just "window['__injected'] = 42;"}) :: Playwright (Either PlaywrightError ElementHandle)
            assertEqual "strict injected" "undefined" =<< evaluateText strictPage "() => String(window['__injected'])"
            bypassing <- newContextWith browser defaultNewContextOptions {newContextBypassCSP = Just True}
            bypassingPage <- newPage bypassing
            _ <- goto bypassingPage (urlFromText (base <> "/empty.html")) defaultGotoOptions
            assertEqual "bypass inline" "42" =<< evaluateText bypassingPage "() => String(window['__inlineScriptValue'])"
            _ <- addScriptTag bypassingPage emptyScriptTag {scriptTagContent = Just "window['__injected'] = 42;"}
            assertEqual "bypass injected" "42" =<< evaluateText bypassingPage "() => String(window['__injected'])"
    ]

-- | Serves a page whose Content-Security-Policy forbids inline scripts, with an
-- inline script that sets a value.
cspInlineApp :: Application
cspInlineApp _ respond =
  respond
    ( responseLBS
        status200
        [ ("Content-Security-Policy", "default-src \"self\"")
        , (hContentType, "text/html")
        ]
        "<script type='text/javascript'>window.__inlineScriptValue = 42;</script>"
    )

module Playwright.PageAddInitScriptSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad
  ( void
  , when
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Text
  ( Text
  , isPrefixOf
  , replace
  )
import UnliftIO.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
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
  ( ConsoleMessage (consoleMessageText)
  , addInitScript
  , addScriptTag
  , onConsole
  , defaultGotoOptions
  , emptyScriptTag
  , evaluateNumber
  , goto
  , newContext
  , newPage
  , Playwright
  , ScriptTag (scriptTagContent)
  , urlFromText
  )
import Playwright.Error
  ( PlaywrightError
  )
import Playwright.Harness
  ( assertEqual
  , withAssets
  , withChromiumBrowser
  , withLocalApp
  )
import UnliftIO.Exception
  ( try
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
    "page-add-init-script"
    [ testCase "should work with content" $
        withAssets $ \prefix page -> do
          addInitScript page "window[\"injected\"] = 123"
          _ <- goto page (urlFromText (prefix <> "/tamperable.html")) defaultGotoOptions
          assertEqual "injected" (123 :: Double) =<< evaluateNumber page "() => window.result"
    , testCase "should support multiple scripts" $
        withAssets $ \prefix page -> do
          addInitScript page "window['script1'] = 1;"
          addInitScript page "window['script2'] = 2;"
          _ <- goto page (urlFromText (prefix <> "/tamperable.html")) defaultGotoOptions
          assertEqual "script1" (1 :: Double) =<< evaluateNumber page "() => window.script1"
          assertEqual "script2" (2 :: Double) =<< evaluateNumber page "() => window.script2"
    , testCase "should work with trailing comments" $
        withAssets $ \prefix page -> do
          addInitScript page "// comment"
          addInitScript page "window.secret = 42;"
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "secret" (42 :: Double) =<< evaluateNumber page "() => window.secret"
    , testCase "should evaluate before anything else on the page" $
        withAssets $ \prefix page -> do
          addInitScript page "window['injected'] = 123;"
          _ <- goto page (urlFromText (prefix <> "/tamperable.html")) defaultGotoOptions
          assertEqual "result" (123 :: Double) =<< evaluateNumber page "() => window.result"
    , testCase "should work with CSP" $
        withChromiumBrowser $ \browser ->
          withLocalApp cspScriptApp $ \base -> do
            page <- newPage =<< newContext browser
            addInitScript page "window['injected'] = 123;"
            _ <- goto page (urlFromText (base <> "/empty.html")) defaultGotoOptions
            assertEqual "injected" (123 :: Double) =<< evaluateNumber page "() => window.injected"
            void
              ( try @Playwright @PlaywrightError
                  (addScriptTag page emptyScriptTag {scriptTagContent = Just "window.e = 10;"})
              )
            assertEqual "blocked" (0 :: Double) =<< evaluateNumber page "() => window.e === undefined ? 0 : 1"
    , testCase "init script should run only once in iframe" $
        withAssets $ \prefix page -> do
          messages <- newIORef []
          onConsole page $ \message ->
            when ("init script:" `isPrefixOf` consoleMessageText message) $
              modifyIORef' messages (<> [consoleMessageText message])
          addInitScript page "console.log('init script:', location.pathname || 'no url yet')"
          _ <- goto page (urlFromText (prefix <> "/frames/one-frame.html")) defaultGotoOptions
          assertEqual "messages"
            ["init script: /frames/one-frame.html", "init script: /frames/frame.html"]
            =<< awaitMessages messages 2 100
    , testCase "should work after a cross origin navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (replace "127.0.0.1" "localhost" prefix <> "/empty.html")) defaultGotoOptions
          addInitScript page "window['injected'] = 123;"
          _ <- goto page (urlFromText (prefix <> "/tamperable.html")) defaultGotoOptions
          assertEqual "result" (123 :: Double) =<< evaluateNumber page "() => window.result"
    ]

-- | Serves every page with a Content-Security-Policy that only allows scripts
-- from the page's own origin, so an inline script is refused.
cspScriptApp :: Application
cspScriptApp _ respond =
  respond
    ( responseLBS
        status200
        [ ("Content-Security-Policy", "script-src 'self'")
        , (hContentType, "text/html")
        ]
        "<html></html>"
    )

-- | Polls the console messages until the expected count arrives, or the
-- attempts run out.
awaitMessages :: IORef [Text] -> Int -> Int -> Playwright [Text]
awaitMessages messages count attempts = do
  current <- readIORef messages
  if length current >= count || attempts <= 0
    then pure current
    else do
      liftIO (threadDelay 50000)
      awaitMessages messages count (attempts - 1)

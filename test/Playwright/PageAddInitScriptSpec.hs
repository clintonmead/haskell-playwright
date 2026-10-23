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
import Data.Text.IO qualified as TIO
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
  , addPageInitScriptExposing
  , addScriptTag
  , dispose
  , onConsole
  , defaultGotoOptions
  , emptyScriptTag
  , evaluateBool
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
  , testAssetsDir
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
    , testCase "should work with a path" $ do
        -- The library takes script text, not a path, so the test reads the file
        -- itself and adds its contents as the init script.
        assets <- testAssetsDir
        script <- TIO.readFile (assets <> "/injectedfile.js")
        withAssets $ \prefix page -> do
          addInitScript page script
          _ <- goto page (urlFromText (prefix <> "/tamperable.html")) defaultGotoOptions
          assertEqual "result" (123 :: Double) =<< evaluateNumber page "() => window.result"
    , testCase "should remove init script after dispose" $
        withAssets $ \prefix page -> do
          -- The function-expression source matches upstream; the empty function
          -- list makes this the only page-level form that returns a dispose handle.
          disposable <- addPageInitScriptExposing page "function() { window['injected'] = 123; }" []
          _ <- goto page (urlFromText (prefix <> "/tamperable.html")) defaultGotoOptions
          assertEqual "before" (123 :: Double) =<< evaluateNumber page "() => window.result"
          dispose disposable
          _ <- goto page (urlFromText (prefix <> "/tamperable.html")) defaultGotoOptions
          assertEqual "after" True =<< evaluateBool page "() => window.result === undefined"
    , testCase "should remove one of multiple init scripts after dispose" $
        withAssets $ \prefix page -> do
          disposable1 <- addPageInitScriptExposing page "function() { window['script1'] = 1; }" []
          addInitScript page "window['script2'] = 2;"
          _ <- goto page (urlFromText (prefix <> "/tamperable.html")) defaultGotoOptions
          assertEqual "script1" (1 :: Double) =<< evaluateNumber page "() => window.script1"
          assertEqual "script2" (2 :: Double) =<< evaluateNumber page "() => window.script2"
          dispose disposable1
          _ <- goto page (urlFromText (prefix <> "/tamperable.html")) defaultGotoOptions
          assertEqual "script1 removed" True =<< evaluateBool page "() => window.script1 === undefined"
          assertEqual "script2 kept" (2 :: Double) =<< evaluateNumber page "() => window.script2"
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

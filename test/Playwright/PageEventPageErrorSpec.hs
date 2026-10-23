module Playwright.PageEventPageErrorSpec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding
  ( encodeUtf8
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , responseLBS
  )
import Data.Text
  ( Text
  , isInfixOf
  , pack
  )
import Playwright
  ( Page
  , PageError (pageErrorMessage, pageErrorName, pageErrorStack)
  , Playwright
  , SpecialJsValue (SpecialNull, SpecialUndefined)
  , clearPageErrors
  , defaultGotoOptions
  , evaluate
  , evaluateWith
  , goto
  , numberArgument
  , onConsole
  , pageErrors
  , setContent
  , specialArgument
  , textArgument
  , urlFromText
  , waitForPageError
  , waitForTimeout
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  )
import Playwright.Harness
  ( requireTimeout
  , withAssets
  , withLocalApp
  , withChromiumPage
  , assertBool
  , assertEqual
  )
import UnliftIO.IORef
  ( modifyIORef'
  , newIORef
  , readIORef
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
    "page-event-pageerror"
    [ testCase "should fire" $
        withAssets $ \prefix page ->
          withAsync (waitForPageError page) $ \handle -> do
            _ <- goto page (urlFromText (prefix <> "/error.html")) defaultGotoOptions
            err <- wait handle
            assertEqual "name" "Error" (pageErrorName err)
            assertEqual "message" "Fancy error!" (pageErrorMessage err)
    , testCase "should fire illegal character error" $
        withChromiumPage $ \page ->
          withLocalApp illegalCharacterApp $ \prefix ->
            withAsync (waitForPageError page) $ \handle -> do
              _ <- goto page (urlFromText (prefix <> "/error.html")) defaultGotoOptions
              err <- wait handle
              assertBool "illegal character" ("Invalid or unexpected token" `isInfixOf` pageErrorMessage err)
    , testCase "should not receive console message for pageError" $
        withAssets $ \prefix page -> do
          messageCount <- newIORef (0 :: Int)
          onConsole page (\_ -> liftIO (modifyIORef' messageCount (+ 1)))
          withAsync (waitForPageError page) $ \handle -> do
            _ <- goto page (urlFromText (prefix <> "/error.html")) defaultGotoOptions
            _ <- wait handle
            count <- readIORef messageCount
            assertEqual "console messages" 1 count
    , testCase "should contain sourceURL" $
        withAssets $ \prefix page ->
          withAsync (waitForPageError page) $ \handle -> do
            _ <- goto page (urlFromText (prefix <> "/error.html")) defaultGotoOptions
            err <- wait handle
            assertBool "myscript.js" ("myscript.js" `isInfixOf` maybe "" id (pageErrorStack err))
    , testCase "should contain the Error.name property" $
        withChromiumPage $ \page ->
          withAsync (waitForPageError page) $ \handle -> do
            _ <-
              evaluate
                page
                "() => { setTimeout(() => { const error = new Error('my-message'); error.name = 'my-name'; throw error; }, 0); }"
            err <- wait handle
            assertEqual "name" "my-name" (pageErrorName err)
            assertEqual "message" "my-message" (pageErrorMessage err)
    , testCase "should support an empty Error.name property" $
        withChromiumPage $ \page -> do
          _ <-
            evaluate
              page
              "() => { setTimeout(() => { const error = new Error('my-message'); error.name = ''; throw error; }, 0); }"
          err <- waitForPageError page
          assertEqual "name" "" (pageErrorName err)
          assertEqual "message" "my-message" (pageErrorMessage err)
    , testCase "should handle odd values" $
        withChromiumPage $ \page -> do
          oddValueMessage page (specialArgument SpecialNull) "null"
          oddValueMessage page (specialArgument SpecialUndefined) "undefined"
          oddValueMessage page (numberArgument 0) "0"
          oddValueMessage page (textArgument "") ""
    , testCase "should handle object" $
        withChromiumPage $ \page -> do
          _ <- evaluate page "() => { setTimeout(() => { throw {}; }, 0); }"
          err <- waitForPageError page
          assertEqual "message" "Object" (pageErrorMessage err)
    , testCase "should handle window" $
        withChromiumPage $ \page -> do
          _ <- evaluate page "() => { setTimeout(() => { throw window; }, 0); }"
          err <- waitForPageError page
          assertEqual "message" "Window" (pageErrorMessage err)
    , testCase "should emit error from unhandled rejects" $
        withChromiumPage $ \page -> do
          setContent page "\n        <script>\n          Promise.reject(new Error('sad :('));\n        </script>\n    "
          err <- waitForPageError page
          assertBool "message" ("sad :(" `isInfixOf` pageErrorMessage err)
    , testCase "pageErrors should work" $
        withChromiumPage $ \page -> do
          _ <-
            evaluate
              page
              "async () => { for (let i = 0; i < 301; i++) { setTimeout(() => { throw new Error('error' + i); }, 0); } await new Promise(f => setTimeout(f, 2000)); }"
          errors <- pageErrors page
          let messages = map pageErrorMessage errors
              expected = ["error" <> pack (show i) | i <- [201 .. 300 :: Int]]
          assertBool "at least 100 errors" (length messages >= 100)
          assertEqual "last errors" expected (drop (length messages - length expected) messages)
    , testCase "clearPageErrors should work" $
        withChromiumPage $ \page -> do
          _ <-
            evaluate
              page
              "() => { setTimeout(() => { throw new Error('error1'); }, 0); setTimeout(() => { throw new Error('error2'); }, 0); }"
          waitForTimeout page =<< requireTimeout 1000
          errors <- pageErrors page
          assertBool "error1" ("error1" `elem` map pageErrorMessage errors)
          assertBool "error2" ("error2" `elem` map pageErrorMessage errors)
          clearPageErrors page
          assertEqual "cleared" [] =<< (map pageErrorMessage <$> pageErrors page)
          _ <- evaluate page "() => { setTimeout(() => { throw new Error('error3'); }, 0); }"
          waitForTimeout page =<< requireTimeout 1000
          remaining <- map pageErrorMessage <$> pageErrors page
          assertEqual "one error" 1 (length remaining)
          assertBool "error3" (any ("error3" `isInfixOf`) remaining)
    -- Skipped: "should remove a listener of a non-existing event handler". Upstream calls
    -- `page.removeListener('pageerror', ...)`. The Haskell client has no listener
    -- registry for page errors (only one-shot `waitForPageError` waiters), so there is no
    -- `removeListener` to call.
    -- "should remove a listener of a non-existing event handler"
    ]

-- | Throws the given value from a timeout and checks the page error's message.
oddValueMessage :: Page -> SerializedArgument -> Text -> Playwright ()
oddValueMessage page value expected = do
  _ <- evaluateWith page "value => { setTimeout(() => { throw value; }, 0); }" value
  err <- waitForPageError page
  assertEqual "message" expected (pageErrorMessage err)

-- | Serves a page whose script has a full-width semicolon, which is an illegal
-- character, so the page raises a syntax error.
illegalCharacterApp :: Application
illegalCharacterApp _ respond =
  respond
    ( responseLBS
        status200
        [(hContentType, "text/html; charset=utf-8")]
        (LBS.fromStrict (encodeUtf8 illegalCharacterPage))
    )

illegalCharacterPage :: Text
illegalCharacterPage =
  "<!doctype html>\n<html lang=\"en\">\n<head><meta charset=\"UTF-8\"><title>vite-project</title></head>\n<body>\n<div id=\"app\"></div>\n<script>\n  let a=10；//   !!!note: ； not ;\n</script>\n</body>\n</html>\n"

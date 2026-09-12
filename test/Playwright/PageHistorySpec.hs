module Playwright.PageHistorySpec
  ( tests
  ) where

import Data.IORef
  ( IORef
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.Text
  ( Text
  , isInfixOf
  , pack
  , replace
  )
import Data.Text.Encoding
  ( encodeUtf8
  )
import Network.HTTP.Types
  ( hContentType
  , hLocation
  , status200
  , status302
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( SpecialJsValue (SpecialUndefined)
  , content
  , defaultGotoOptions
  , evaluate
  , evaluateSpecial
  , goBack
  , goForward
  , goto
  , reload
  , responseOk
  , responseUrl
  , url
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( withAssets
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , assertFailure
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-history"
    [ testCase "page.goBack should work" $
        withAssets $ \prefix page -> do
          none <- goBack page
          case none of
            Nothing ->
              pure ()
            Just _ ->
              assertFailure "expected no history"
          let
            empty :: Text
            empty = prefix <> "/empty.html"
            grid :: Text
            grid = prefix <> "/grid.html"
          _ <- goto page (urlFromText empty) defaultGotoOptions
          _ <- goto page (urlFromText grid) defaultGotoOptions
          back <- goBack page
          case back of
            Nothing ->
              assertFailure "expected goBack response"
            Just response -> do
              assertEqual "ok" True =<< responseOk response
              responseLocation <- urlToText <$> responseUrl response
              assertEqual "empty url" True (empty `isInfixOf` responseLocation)
          forward <- goForward page
          case forward of
            Nothing ->
              assertFailure "expected goForward response"
            Just response -> do
              assertEqual "ok" True =<< responseOk response
              responseLocation <- urlToText <$> responseUrl response
              assertEqual "grid url" True ("/grid.html" `isInfixOf` responseLocation)
          noneAgain <- goForward page
          case noneAgain of
            Nothing ->
              pure ()
            Just _ ->
              assertFailure "expected end of history"
    , testCase "page.goBack should work with HistoryAPI" $
        withAssets $ \prefix page -> do
          let
            empty :: Text
            empty = prefix <> "/empty.html"
          _ <- goto page (urlFromText empty) defaultGotoOptions
          _ <-
            evaluate
              page
              "() => { history.pushState({}, '', '/first.html'); history.pushState({}, '', '/second.html'); }"
          assertEqual "second" (prefix <> "/second.html") =<< (urlToText <$> url page)
          _ <- goBack page
          assertEqual "first" (prefix <> "/first.html") =<< (urlToText <$> url page)
          _ <- goBack page
          assertEqual "empty" empty =<< (urlToText <$> url page)
          _ <- goForward page
          assertEqual "first again" (prefix <> "/first.html") =<< (urlToText <$> url page)
    , testCase "page.reload should work" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- evaluate page "() => { window._foo = 10; }"
          _ <- reload page
          special <- evaluateSpecial page "() => window._foo"
          assertEqual "cleared" SpecialUndefined special
    , testCase "page.reload should work with data url" $
        withChromiumPage $ \page -> do
          _ <- goto page (urlFromText "data:text/html,hello") defaultGotoOptions
          assertEqual "hello" True . ("hello" `isInfixOf`) =<< content page
          response <- reload page
          case response of
            Nothing ->
              pure ()
            Just _ ->
              assertFailure "expected no response"
          assertEqual "hello after" True . ("hello" `isInfixOf`) =<< content page
    , testCase "page.reload should work with same origin redirect" $
        reloadRedirect False
    , testCase "page.reload should work with cross-origin redirect" $
        reloadRedirect True
    , testCase "page.reload should work on a page with a hash" $
        withAssets $ \prefix page -> do
          let hashed = prefix <> "/empty.html#hash"
          _ <- goto page (urlFromText hashed) defaultGotoOptions
          _ <- reload page
          assertEqual "hash" hashed . urlToText =<< url page
    , testCase "page.reload should work on a page with a hash at the end" $
        withAssets $ \prefix page -> do
          let hashed = prefix <> "/empty.html#"
          _ <- goto page (urlFromText hashed) defaultGotoOptions
          _ <- reload page
          assertEqual "hash" hashed . urlToText =<< url page
    ]

reloadRedirect :: Bool -> IO ()
reloadRedirect cross =
  withChromiumPage $ \page -> do
    served <- newIORef False
    destination <- newIORef ("" :: Text)
    withApplication (pure (reloadRedirectApp served destination)) $ \port -> do
      let
        prefix = "http://127.0.0.1:" <> pack (show port)
        expected =
          ( if cross
              then replace "127.0.0.1" "localhost" prefix
              else prefix
          )
            <> "/title.html"
      writeIORef destination expected
      _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
      _ <- reload page
      assertEqual "url" expected . urlToText =<< url page

reloadRedirectApp :: IORef Bool -> IORef Text -> Application
reloadRedirectApp served destination request respond =
  case pathInfo request of
    ["empty.html"] -> do
      already <- readIORef served
      if already
        then do
          location <- readIORef destination
          respond (responseLBS status302 [(hLocation, encodeUtf8 location)] "")
        else do
          writeIORef served True
          respond (responseLBS status200 [(hContentType, "text/html")] "")
    ["title.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] "<title>t</title>")
    _ ->
      respond (responseLBS status200 [] "")


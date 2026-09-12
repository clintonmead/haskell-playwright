module Playwright.PageGotoSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad
  ( forever
  )
import Data.Text
  ( Text
  , pack
  )
import Network.Wai
  ( Application
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( defaultGotoOptions
  , evaluate
  , goto
  , milliseconds
  , pageContext
  , setContextDefaultNavigationTimeout
  , setContextDefaultTimeout
  , setDefaultNavigationTimeout
  , setDefaultTimeout
  , url
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( assertProtocolContains
  , withAssets
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
    "page-goto"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          let
            empty :: Text
            empty = prefix <> "/empty.html"
          _ <- goto page (urlFromText empty) defaultGotoOptions
          current <- urlToText <$> url page
          assertEqual "url" empty current
    , testCase "page.url should work" $
        withAssets $ \prefix page -> do
          blank <- urlToText <$> url page
          assertEqual "blank" "about:blank" blank
          let
            empty :: Text
            empty = prefix <> "/empty.html"
          _ <- goto page (urlFromText empty) defaultGotoOptions
          current <- urlToText <$> url page
          assertEqual "empty" empty current
    , testCase "page.url should include hashes" $
        withAssets $ \prefix page -> do
          let
            hashed :: Text
            hashed = prefix <> "/empty.html#hash"
          _ <- goto page (urlFromText hashed) defaultGotoOptions
          current <- urlToText <$> url page
          assertEqual "hash" hashed current
          _ <- evaluate page "() => { window.location.hash = 'dynamic'; }"
          updated <- urlToText <$> url page
          assertEqual "dynamic" (prefix <> "/empty.html#dynamic") updated
    , testCase "should fail when exceeding default maximum navigation timeout" $
        withChromiumPage $ \page ->
          withHangServer $ \prefix ->
            case milliseconds 2 of
              Nothing ->
                assertFailure "timeout"
              Just timeout -> do
                setContextDefaultNavigationTimeout (pageContext page) timeout
                assertProtocolContains
                  "Timeout 2ms exceeded"
                  (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
    , testCase "should fail when exceeding default maximum timeout" $
        withChromiumPage $ \page ->
          withHangServer $ \prefix ->
            case (milliseconds 1, milliseconds 2) of
              (Just pageTimeout, Just contextTimeout) -> do
                setContextDefaultTimeout (pageContext page) contextTimeout
                setDefaultTimeout page pageTimeout
                assertProtocolContains
                  "Timeout 1ms exceeded"
                  (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
              _ ->
                assertFailure "timeout"
    , testCase "should fail when exceeding browser context timeout" $
        withChromiumPage $ \page ->
          withHangServer $ \prefix ->
            case milliseconds 2 of
              Nothing ->
                assertFailure "timeout"
              Just timeout -> do
                setContextDefaultTimeout (pageContext page) timeout
                assertProtocolContains
                  "Timeout 2ms exceeded"
                  (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
    , testCase "should prioritize default navigation timeout over default timeout" $
        withChromiumPage $ \page ->
          withHangServer $ \prefix ->
            case (milliseconds 0, milliseconds 1) of
              (Just disabled, Just navigationTimeout) -> do
                setDefaultTimeout page disabled
                setDefaultNavigationTimeout page navigationTimeout
                assertProtocolContains
                  "Timeout 1ms exceeded"
                  (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
              _ ->
                assertFailure "timeout"
    ]

hangApp :: Application
hangApp _request _respond =
  forever (threadDelay 1000000)

withHangServer :: (Text -> IO a) -> IO a
withHangServer action =
  withApplication (pure hangApp) $ \port ->
    action ("http://127.0.0.1:" <> pack (show port))

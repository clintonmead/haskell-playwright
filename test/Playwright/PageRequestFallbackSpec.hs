module Playwright.PageRequestFallbackSpec
  ( tests
  ) where

import Data.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import Playwright
  ( FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillStatus)
  , HttpStatus (HttpStatus)
  , UrlGlob (UrlGlob)
  , abortRoute
  , defaultFulfillOptions
  , defaultGotoOptions
  , fallbackRoute
  , fulfillRoute
  , goto
  , responseText
  , route
  , urlFromText
  )
import Playwright.Harness
  ( assertProtocolContains
  , withAssets
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
    "page-request-fallback"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          route page (UrlGlob "**/*") fallbackRoute
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          pure ()
    , testCase "should fall back" $
        withAssets $ \prefix page -> do
          intercepted <- newIORef ([] :: [Int])
          route page (UrlGlob "**/empty.html") $ \handled -> do
            modifyIORef' intercepted (1 :)
            fallbackRoute handled
          route page (UrlGlob "**/empty.html") $ \handled -> do
            modifyIORef' intercepted (2 :)
            fallbackRoute handled
          route page (UrlGlob "**/empty.html") $ \handled -> do
            modifyIORef' intercepted (3 :)
            fallbackRoute handled
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "order" [3, 2, 1] . reverse =<< readIORef intercepted
    , testCase "should not chain fulfill" $
        withAssets $ \prefix page -> do
          failed <- newIORef False
          route page (UrlGlob "**/empty.html") $ \_ ->
            writeTrue failed
          route page (UrlGlob "**/empty.html") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus 200)
                , fulfillBody = Just (FulfillText "fulfilled")
                }
          route page (UrlGlob "**/empty.html") fallbackRoute
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "body" "fulfilled" =<< responseText value
          assertEqual "chained" False =<< readIORef failed
    , testCase "should not chain abort" $
        withAssets $ \prefix page -> do
          failed <- newIORef False
          route page (UrlGlob "**/empty.html") $ \_ ->
            writeTrue failed
          route page (UrlGlob "**/empty.html") abortRoute
          route page (UrlGlob "**/empty.html") fallbackRoute
          assertProtocolContains
            "net::ERR_FAILED"
            (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
          assertEqual "chained" False =<< readIORef failed
    ]

writeTrue :: IORef Bool -> IO ()
writeTrue ref =
  writeIORef ref True

module Playwright.PageEventLoadSpec
  ( tests
  ) where

import UnliftIO.IORef
  ( IORef
  , atomicModifyIORef'
  , modifyIORef'
  , newIORef
  , readIORef
  )
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Network.HTTP.Types
  ( hContentType
  , status200
  , status404
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
import Playwright
  ( click
  , defaultGotoOptions
  , evaluateBool
  , frameByName
  , goto
  , locator
  , milliseconds
  , onLoad
  , selectorFromText
  , urlFromText
  , waitForFunctionOnFrame
  , waitForTimeout
  )
import Playwright.Harness
  ( withAssets
  , withChromiumPage
  , withLocalApp
  , assertEqual
  , assertFailure
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
    "page-event-load"
    [ testCase "should fire once" $
        withAssets $ \prefix page -> do
          count <- newIORef (0 :: Int)
          onLoad page (modifyIORef' count (+ 1))
          _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
          case milliseconds 1000 of
            Just timeout ->
              waitForTimeout page timeout
            Nothing ->
              assertFailure "expected timeout"
          fired <- readIORef count
          assertEqual "load" 1 fired
    , testCase "should fire once with iframe navigation" $
        withChromiumPage $ \page -> do
          hits <- newIORef (0 :: Int)
          withLocalApp (trackingApp hits) $ \prefix -> do
            count <- newIORef (0 :: Int)
            onLoad page (modifyIORef' count (+ 1))
            _ <- goto page (urlFromText (prefix <> "/home")) defaultGotoOptions
            tracker <- frameByName page "tracker"
            case tracker of
              Nothing ->
                assertFailure "the tracking iframe is missing"
              Just frame -> do
                _ <- waitForFunctionOnFrame frame "() => document.body.innerText.includes('request count: 1')"
                click (locator page (selectorFromText "input[type=submit]"))
                case milliseconds 1000 of
                  Just timeout ->
                    waitForTimeout page timeout
                  Nothing ->
                    assertFailure "expected timeout"
                fired <- readIORef count
                assertEqual "load" 1 fired
                logged <- evaluateBool page "() => JSON.stringify(window.eventLog) === JSON.stringify(['load'])"
                assertEqual "event log" True logged
    ]

trackingApp :: IORef Int -> Application
trackingApp hits request respond =
  case pathInfo request of
    ["home"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] homeHtml)
    ["tracker"] -> do
      n <- atomicModifyIORef' hits (\current -> (current + 1, current + 1))
      respond (responseLBS status200 [] (LBS8.pack ("request count: " <> show n)))
    _ ->
      respond (responseLBS status404 [] "")

homeHtml :: LBS8.ByteString
homeHtml =
  LBS8.pack . unlines $
    [ "<!DOCTYPE html>"
    , "<html>"
    , "  <head>"
    , "  </head>"
    , "  <body>"
    , "    <script>"
    , "      window.eventLog = [];"
    , "      window.addEventListener('load', () => {"
    , "        window.eventLog.push('load');"
    , "      });"
    , "    </script>"
    , "    <form id=\"trackerForm\" action=\"/tracker\" method=\"post\" target=\"tracker\">"
    , "      <input type=\"submit\">"
    , "    </form>"
    , "    <iframe name=\"tracker\" src=\"/tracker\"></iframe>"
    , "  </body>"
    , "</html>"
    ]

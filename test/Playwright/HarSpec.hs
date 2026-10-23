module Playwright.HarSpec
  ( tests
  ) where

import Control.Monad
  ( void
  )
import Data.Either
  ( isLeft
  )
import Data.Text
  ( Text
  )
import Playwright
  ( BrowserContext
  , ContentType (ContentType)
  , ContinueOptions (continueUrl)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType)
  , HarNotFound (HarNotFoundFallback)
  , HarRouteOptions (harRouteNotFound, harRouteUrl)
  , Page
  , Playwright
  , UrlMatch (UrlMatchGlob, UrlMatchRegex)
  , contextRoute
  , contextRouteFromHar
  , contextRouteFromHarWith
  , defaultContinueOptions
  , defaultFulfillOptions
  , defaultGotoOptions
  , defaultHarRouteOptions
  , defaultNewContextOptions
  , evaluateText
  , fallbackRouteWith
  , fulfillRoute
  , goto
  , newContext
  , newPage
  , requestUrl
  , routeFromHar
  , routeFromHarWith
  , routeRequest
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , testAssetsDir
  , withAssetsContextWith
  , withChromiumBrowser
  , withChromiumContext
  )
import System.Directory
  ( getTemporaryDirectory
  , removeFile
  )
import System.IO
  ( hClose
  , hPutStr
  , openTempFile
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Exception
  ( SomeException
  , bracket
  , try
  )

tests :: TestTree
tests =
  testGroup
    "har"
    [ testCase "should context.routeFromHAR, matching the method and following redirects" $
        withChromiumContext $ \ctx _ -> do
          file <- harFile
          contextRouteFromHar ctx file
          page <- newPage ctx
          gotoUrl page "http://no.playwright/"
          assertEqual "window.value" "foo" =<< windowValue page
          assertEqual "body background" "rgb(255, 0, 0)" =<< pageBackground page
    , testCase "should page.routeFromHAR, matching the method and following redirects" $
        withChromiumContext $ \ctx _ -> do
          file <- harFile
          page <- newPage ctx
          routeFromHar page file
          gotoUrl page "http://no.playwright/"
          assertEqual "window.value" "foo" =<< windowValue page
          assertEqual "body background" "rgb(255, 0, 0)" =<< pageBackground page
    , testCase "fallback:continue should continue when not found in har" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx _ -> do
          file <- harFile
          contextRouteFromHarWith ctx file defaultHarRouteOptions {harRouteNotFound = HarNotFoundFallback}
          page <- newPage ctx
          gotoUrl page (prefix <> "/one-style.html")
          assertEqual "body background" "rgb(255, 192, 203)" =<< pageBackground page
    , testCase "by default should abort requests not found in har" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx _ -> do
          file <- harFile
          contextRouteFromHar ctx file
          page <- newPage ctx
          outcome <- try (gotoUrl page (prefix <> "/empty.html"))
          assertBool "navigation should fail" (isLeft (outcome :: Either SomeException ()))
    , testCase "fallback:continue should continue requests on bad har" $
        withBadHar $ \badFile ->
          withAssetsContextWith defaultNewContextOptions $ \prefix ctx _ -> do
            contextRouteFromHarWith ctx badFile defaultHarRouteOptions {harRouteNotFound = HarNotFoundFallback}
            page <- newPage ctx
            gotoUrl page (prefix <> "/one-style.html")
            assertEqual "body background" "rgb(255, 192, 203)" =<< pageBackground page
    , testCase "should only handle requests matching url filter" $
        withChromiumContext $ \ctx _ -> do
          file <- harFile
          contextRouteFromHarWith
            ctx
            file
            defaultHarRouteOptions
              { harRouteNotFound = HarNotFoundFallback
              , harRouteUrl = UrlMatchGlob "**/*.js"
              }
          page <- newPage ctx
          serveScriptPage ctx
          gotoUrl page "http://no.playwright/"
          assertEqual "window.value" "foo" =<< windowValue page
          assertEqual "body background" "rgba(0, 0, 0, 0)" =<< pageBackground page
    , testCase "should only context.routeFromHAR requests matching url filter" $
        withChromiumContext $ \ctx _ -> do
          file <- harFile
          contextRouteFromHarWith ctx file defaultHarRouteOptions {harRouteUrl = UrlMatchGlob "**/*.js"}
          page <- newPage ctx
          serveScriptPage ctx
          gotoUrl page "http://no.playwright/"
          assertEqual "window.value" "foo" =<< windowValue page
          assertEqual "body background" "rgba(0, 0, 0, 0)" =<< pageBackground page
    , testCase "should only page.routeFromHAR requests matching url filter" $
        withChromiumContext $ \ctx _ -> do
          file <- harFile
          page <- newPage ctx
          routeFromHarWith page file defaultHarRouteOptions {harRouteUrl = UrlMatchGlob "**/*.js"}
          serveScriptPage ctx
          gotoUrl page "http://no.playwright/"
          assertEqual "window.value" "foo" =<< windowValue page
          assertEqual "body background" "rgba(0, 0, 0, 0)" =<< pageBackground page
    , testCase "should apply overrides before routing from har" $
        withChromiumContext $ \ctx _ -> do
          file <- harFile
          contextRouteFromHarWith ctx file defaultHarRouteOptions {harRouteUrl = UrlMatchGlob "**/*.js"}
          page <- newPage ctx
          contextRoute ctx (UrlMatchGlob "http://no.playwright/my-script.js") $ \handled ->
            fallbackRouteWith
              handled
              defaultContinueOptions {continueUrl = Just (urlFromText "http://no.playwright/script2.js")}
          contextRoute ctx (UrlMatchGlob "http://test.example/") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillContentType = Just (ContentType "text/html")
                , fulfillBody =
                    Just
                      ( FulfillText
                          "<script src=\"http://no.playwright/my-script.js\"></script><div>hello</div>"
                      )
                }
          gotoUrl page "http://test.example/"
          assertEqual "window.value" "foo" =<< windowValue page
    , testCase "should support regex filter" $
        withChromiumContext $ \ctx _ -> do
          file <- harFile
          contextRouteFromHarWith
            ctx
            file
            defaultHarRouteOptions
              { harRouteUrl = UrlMatchRegex ".*(\\.js|.*\\.css|no.playwright\\/)$" ""
              }
          page <- newPage ctx
          gotoUrl page "http://no.playwright/"
          assertEqual "window.value" "foo" =<< windowValue page
          assertEqual "body background" "rgb(255, 0, 0)" =<< pageBackground page
    , testCase "newPage should fulfill from har, matching the method and following redirects" $
        withChromiumBrowser $ \browser -> do
          file <- harFile
          ctx <- newContext browser
          page <- newPage ctx
          routeFromHar page file
          gotoUrl page "http://no.playwright/"
          assertEqual "window.value" "foo" =<< windowValue page
          assertEqual "body background" "rgb(255, 0, 0)" =<< pageBackground page
    ]

harFile :: Playwright FilePath
harFile = do
  dir <- testAssetsDir
  pure (dir <> "/har-fulfill.har")

gotoUrl :: Page -> Text -> Playwright ()
gotoUrl page url =
  void (goto page (urlFromText url) defaultGotoOptions)

windowValue :: Page -> Playwright Text
windowValue page =
  evaluateText page "() => String(window.value)"

pageBackground :: Page -> Playwright Text
pageBackground page =
  evaluateText page "() => getComputedStyle(document.body).backgroundColor"

-- | Serves the page that loads script.js, checking the request URL on the way.
serveScriptPage :: BrowserContext -> Playwright ()
serveScriptPage ctx =
  contextRoute ctx (UrlMatchGlob "http://no.playwright/") $ \handled -> do
    request <- routeRequest handled
    url <- requestUrl request
    assertEqual "route url" "http://no.playwright/" (urlToText url)
    fulfillRoute
      handled
      defaultFulfillOptions
        { fulfillContentType = Just (ContentType "text/html")
        , fulfillBody = Just (FulfillText "<script src=\"./script.js\"></script><div>hello</div>")
        }

-- | Writes a HAR file with no entries, which the bad-HAR case routes through.
withBadHar :: (FilePath -> IO a) -> IO a
withBadHar action = do
  dir <- getTemporaryDirectory
  bracket (openTempFile dir "bad-har.har") (\(path, handle) -> hClose handle >> removeFile path) $ \(path, handle) -> do
    hPutStr handle "{\"log\":{}}"
    hClose handle
    action path

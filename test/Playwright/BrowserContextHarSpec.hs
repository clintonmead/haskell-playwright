module Playwright.BrowserContextHarSpec
  ( tests
  ) where

import Control.Monad
  ( forM_
  , void
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Either
  ( isLeft
  )
import Data.IORef
  ( IORef
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.List
  ( sort
  )
import Data.Maybe
  ( fromMaybe
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Network.HTTP.Types
  ( hContentType
  , status200
  , status500
  )
import Network.Wai qualified as Wai
import Playwright
  ( Browser
  , BrowserContext
  , ContentType (ContentType)
  , ContinueOptions (continuePostData, continueUrl)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType)
  , HarNotFound (HarNotFoundAbort, HarNotFoundFallback)
  , HarRouteOptions (harRouteNotFound, harRouteUpdate, harRouteUpdateContent, harRouteUpdateMode, harRouteUrl)
  , Page
  , Playwright
  , RecordHar (RecordHar)
  , RecordHarContent (RecordHarContentAttach, RecordHarContentEmbed)
  , RecordHarMode (RecordHarModeFull, RecordHarModeMinimal)
  , Response
  , UrlMatch (UrlMatchGlob, UrlMatchRegex)
  , closeContext
  , closePage
  , contextRoute
  , defaultContinueOptions
  , defaultFulfillOptions
  , fallbackRouteWith
  , fulfillRoute
  , goBack
  , goForward
  , newBrowserPage
  , pageClickSelector
  , reload
  , requestPostData
  , requestUrl
  , route
  , routeRequest
  , responseRequest
  , routeFromHar
  , selectorFromText
  , setContent
  , urlToText
  , waitForURL
  , content
  , contextRouteFromHar
  , contextRouteFromHarWith
  , defaultGotoOptions
  , defaultHarRouteOptions
  , NewContextOptions (newContextRecordHar)
  , defaultNewContextOptions
  , evaluateText
  , goto
  , newContextWith
  , newPage
  , routeFromHarWith
  , unrouteAll
  , unrouteContextAll
  , urlFromText
  )
import Playwright.Harness
  ( Recorded (recordedBody)
  , assertBool
  , assertEqual
  , recordRequest
  , testAssetsDir
  , withAssetsPrefix
  , withChromiumBrowser
  , withLocalApp
  )
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removePathForcibly
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
    "browsercontext-har"
    [ testCase "should round-trip har.zip" $
        withTempDir "round-trip-zip" $ \dir ->
          withAssetsPrefix $ \prefix ->
            withChromiumBrowser $ \browser -> do
              let harPath = dir <> "/har.zip"
              recordOnClose browser (RecordHar harPath Nothing (Just RecordHarModeMinimal) Nothing) (prefix <> "/one-style.html")
              written <- liftIO (doesFileExist harPath)
              assertBool "har.zip should be written on context close" written
              replayFrom browser harPath (prefix <> "/one-style.html")
    , testCase "should produce extracted zip" $
        withTempDir "extracted-zip" $ \dir ->
          withAssetsPrefix $ \prefix ->
            withChromiumBrowser $ \browser -> do
              let harPath = dir <> "/har.har"
                  opts = RecordHar harPath (Just RecordHarContentAttach) (Just RecordHarModeMinimal) Nothing
              recordOnClose browser opts (prefix <> "/one-style.html")
              written <- liftIO (doesFileExist harPath)
              assertBool "har should be written on context close" written
              har <- liftIO (TextIO.readFile harPath)
              assertBool "har should not embed the stylesheet body" (not (Text.isInfixOf "background-color" har))
              replayFrom browser harPath (prefix <> "/one-style.html")
    , testCase "should round-trip extracted har.zip" $
        skipCase
          "needs unzipping of the recorded har.zip into a plain HAR; the library has no zip extraction and the test deps provide none"
    , testCase "should fulfill from har with content in a file" $
        withChromiumBrowser $ \browser -> do
          dir <- testAssetsDir
          ctx <- newContextWith browser defaultNewContextOptions
          contextRouteFromHar ctx (dir <> "/har-sha1.har")
          page <- newPage ctx
          gotoUrl page "http://no.playwright/"
          assertEqual "page content" "<html><head></head><body>Hello, world</body></html>" =<< content page
          closeContext ctx
    , testCase "page.unrouteAll should stop page.routeFromHAR" $
        withAssetsPrefix $ \prefix ->
          withChromiumBrowser $ \browser -> do
            dir <- testAssetsDir
            ctx <- newContextWith browser defaultNewContextOptions
            page <- newPage ctx
            routeFromHarWith page (dir <> "/har-sha1.har") abortingHarOptions
            assertNavigationFails page (prefix <> "/empty.html")
            unrouteAll page
            gotoUrl page (prefix <> "/empty.html")
            closeContext ctx
    , testCase "context.unrouteAll should stop context.routeFromHAR" $
        withAssetsPrefix $ \prefix ->
          withChromiumBrowser $ \browser -> do
            dir <- testAssetsDir
            ctx <- newContextWith browser defaultNewContextOptions
            contextRouteFromHarWith ctx (dir <> "/har-sha1.har") abortingHarOptions
            page <- newPage ctx
            assertNavigationFails page (prefix <> "/empty.html")
            unrouteContextAll ctx
            gotoUrl page (prefix <> "/empty.html")
            closeContext ctx
    , testCase "should change document URL after redirected navigation" $
        withChromiumBrowser $ \browser -> do
          dir <- testAssetsDir
          ctx <- newContextWith browser defaultNewContextOptions
          contextRouteFromHar ctx (dir <> "/har-redirect.har")
          page <- newPage ctx
          response <- goto page (urlFromText "https://theverge.com/") defaultGotoOptions
          assertEqual "document url" "https://www.theverge.com/" =<< hrefOf page
          assertEqual "response url" (Just "https://www.theverge.com/") =<< responseUrlText response
          closeContext ctx
    , testCase "should change document URL after redirected navigation on click" $
        withAssetsPrefix $ \prefix ->
          withChromiumBrowser $ \browser -> do
            dir <- testAssetsDir
            ctx <- newContextWith browser defaultNewContextOptions
            contextRouteFromHarWith ctx (dir <> "/har-redirect.har") theVergeHarOptions
            page <- newPage ctx
            gotoUrl page (prefix <> "/empty.html")
            setContent page "<a href=\"https://theverge.com/\">click me</a>"
            pageClickSelector page (selectorFromText "text=click me")
            waitForURL page (UrlMatchGlob "https://www.theverge.com/")
            assertEqual "document url" "https://www.theverge.com/" =<< hrefOf page
            closeContext ctx
    , testCase "should goBack to redirected navigation" $
        withAssetsPrefix $ \prefix ->
          withChromiumBrowser $ \browser -> do
            dir <- testAssetsDir
            ctx <- newContextWith browser defaultNewContextOptions
            contextRouteFromHarWith ctx (dir <> "/har-redirect.har") theVergeHarOptions
            page <- newPage ctx
            gotoUrl page "https://theverge.com/"
            gotoUrl page (prefix <> "/empty.html")
            assertBool "should be on empty page" . Text.isSuffixOf "/empty.html" =<< hrefOf page
            response <- goBack page
            assertEqual "document url" "https://www.theverge.com/" =<< hrefOf page
            assertEqual "response url" (Just "https://www.theverge.com/") =<< responseUrlText response
            closeContext ctx
    , testCase "should goForward to redirected navigation" $
        withAssetsPrefix $ \prefix ->
          withChromiumBrowser $ \browser -> do
            dir <- testAssetsDir
            ctx <- newContextWith browser defaultNewContextOptions
            contextRouteFromHarWith ctx (dir <> "/har-redirect.har") theVergeHarOptions
            page <- newPage ctx
            gotoUrl page (prefix <> "/empty.html")
            gotoUrl page "https://theverge.com/"
            assertEqual "document url" "https://www.theverge.com/" =<< hrefOf page
            _ <- goBack page
            assertBool "should be on empty page" . Text.isSuffixOf "/empty.html" =<< hrefOf page
            response <- goForward page
            assertEqual "document url" "https://www.theverge.com/" =<< hrefOf page
            assertEqual "response url" (Just "https://www.theverge.com/") =<< responseUrlText response
            closeContext ctx
    , testCase "should reload redirected navigation" $
        withChromiumBrowser $ \browser -> do
          dir <- testAssetsDir
          ctx <- newContextWith browser defaultNewContextOptions
          contextRouteFromHarWith ctx (dir <> "/har-redirect.har") theVergeHarOptions
          page <- newPage ctx
          gotoUrl page "https://theverge.com/"
          assertEqual "document url" "https://www.theverge.com/" =<< hrefOf page
          response <- reload page
          assertEqual "document url" "https://www.theverge.com/" =<< hrefOf page
          assertEqual "response url" (Just "https://www.theverge.com/") =<< responseUrlText response
          closeContext ctx
    , testCase "should context.routeFromHAR, matching the method and following redirects" $
        withChromiumBrowser $ \browser -> do
          dir <- testAssetsDir
          ctx <- newContextWith browser defaultNewContextOptions
          contextRouteFromHar ctx (dir <> "/har-fulfill.har")
          page <- newPage ctx
          gotoUrl page "http://no.playwright/"
          assertHarFulfilled page
          closeContext ctx
    , testCase "should page.routeFromHAR, matching the method and following redirects" $
        withChromiumBrowser $ \browser -> do
          dir <- testAssetsDir
          ctx <- newContextWith browser defaultNewContextOptions
          page <- newPage ctx
          routeFromHar page (dir <> "/har-fulfill.har")
          gotoUrl page "http://no.playwright/"
          assertHarFulfilled page
          closeContext ctx
    , testCase "fallback:continue should continue when not found in har" $
        withAssetsPrefix $ \prefix ->
          withChromiumBrowser $ \browser -> do
            dir <- testAssetsDir
            ctx <- newContextWith browser defaultNewContextOptions
            contextRouteFromHarWith ctx (dir <> "/har-fulfill.har") fallbackHarOptions
            page <- newPage ctx
            gotoUrl page (prefix <> "/one-style.html")
            assertPinkBackground page
            closeContext ctx
    , testCase "by default should abort requests not found in har" $
        withAssetsPrefix $ \prefix ->
          withChromiumBrowser $ \browser -> do
            dir <- testAssetsDir
            ctx <- newContextWith browser defaultNewContextOptions
            contextRouteFromHar ctx (dir <> "/har-fulfill.har")
            page <- newPage ctx
            assertNavigationFails page (prefix <> "/empty.html")
            closeContext ctx
    , testCase "fallback:continue should continue requests on bad har" $
        withTempDir "bad-har" $ \dir ->
          withAssetsPrefix $ \prefix ->
            withChromiumBrowser $ \browser -> do
              let harPath = dir <> "/test.har"
              liftIO (TextIO.writeFile harPath "{\"log\":{}}")
              ctx <- newContextWith browser defaultNewContextOptions
              contextRouteFromHarWith ctx harPath fallbackHarOptions
              page <- newPage ctx
              gotoUrl page (prefix <> "/one-style.html")
              assertPinkBackground page
              closeContext ctx
    , testCase "should only handle requests matching url filter" $
        withChromiumBrowser $ \browser -> do
          dir <- testAssetsDir
          ctx <- newContextWith browser defaultNewContextOptions
          contextRouteFromHarWith ctx (dir <> "/har-fulfill.har") jsFallbackHarOptions
          page <- newPage ctx
          servePlaygroundPage ctx
          gotoUrl page "http://no.playwright/"
          assertEqual "window.value" "foo" =<< evaluateText page "() => window.value"
          assertEqual "background" "rgba(0, 0, 0, 0)" =<< pageBackground page
          closeContext ctx
    , testCase "should only context.routeFromHAR requests matching url filter" $
        withChromiumBrowser $ \browser -> do
          dir <- testAssetsDir
          ctx <- newContextWith browser defaultNewContextOptions
          contextRouteFromHarWith ctx (dir <> "/har-fulfill.har") jsHarOptions
          page <- newPage ctx
          servePlaygroundPage ctx
          gotoUrl page "http://no.playwright/"
          assertEqual "window.value" "foo" =<< evaluateText page "() => window.value"
          assertEqual "background" "rgba(0, 0, 0, 0)" =<< pageBackground page
          closeContext ctx
    , testCase "should only page.routeFromHAR requests matching url filter" $
        withChromiumBrowser $ \browser -> do
          dir <- testAssetsDir
          ctx <- newContextWith browser defaultNewContextOptions
          page <- newPage ctx
          routeFromHarWith page (dir <> "/har-fulfill.har") jsHarOptions
          servePlaygroundPage ctx
          gotoUrl page "http://no.playwright/"
          assertEqual "window.value" "foo" =<< evaluateText page "() => window.value"
          assertEqual "background" "rgba(0, 0, 0, 0)" =<< pageBackground page
          closeContext ctx
    , testCase "should apply overrides before routing from har" $
        withChromiumBrowser $ \browser -> do
          dir <- testAssetsDir
          ctx <- newContextWith browser defaultNewContextOptions
          contextRouteFromHarWith ctx (dir <> "/har-fulfill.har") jsHarOptions
          page <- newPage ctx
          contextRoute ctx (UrlMatchGlob "http://no.playwright/my-script.js") $ \handled ->
            fallbackRouteWith handled defaultContinueOptions {continueUrl = Just (urlFromText "http://no.playwright/script2.js")}
          contextRoute ctx (UrlMatchGlob "http://test.example/") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillContentType = Just (ContentType "text/html")
                , fulfillBody = Just (FulfillText "<script src=\"http://no.playwright/my-script.js\"></script><div>hello</div>")
                }
          gotoUrl page "http://test.example/"
          assertEqual "window.value" "foo" =<< evaluateText page "() => window.value"
          closeContext ctx
    , testCase "should support regex filter" $
        withChromiumBrowser $ \browser -> do
          dir <- testAssetsDir
          ctx <- newContextWith browser defaultNewContextOptions
          contextRouteFromHarWith ctx (dir <> "/har-fulfill.har") defaultHarRouteOptions {harRouteUrl = UrlMatchRegex ".*(\\.js|.*\\.css|no.playwright/)$" ""}
          page <- newPage ctx
          gotoUrl page "http://no.playwright/"
          assertHarFulfilled page
          closeContext ctx
    , testCase "newPage should fulfill from har, matching the method and following redirects" $
        withChromiumBrowser $ \browser -> do
          dir <- testAssetsDir
          page <- newBrowserPage browser
          routeFromHar page (dir <> "/har-fulfill.har")
          gotoUrl page "http://no.playwright/"
          assertHarFulfilled page
          closePage page
    , testCase "should round-trip har with postData" $
        withTempDir "post-data" $ \dir -> do
          live <- newIORef True
          withChromiumBrowser $ \browser ->
            withLocalApp (echoApp live echoBody) $ \prefix -> do
              let harPath = dir <> "/har.zip"
              ctx1 <- newRecordingContext browser harPath
              page1 <- newPage ctx1
              gotoUrl page1 (prefix <> "/empty.html")
              forM_ ["1", "2", "3"] $ \body ->
                assertEqual "recorded echo" body =<< echoPost page1 "/echo" body
              closeContext ctx1
              liftIO (writeIORef live False)
              ctx2 <- newContextWith browser defaultNewContextOptions
              contextRouteFromHar ctx2 harPath
              page2 <- newPage ctx2
              gotoUrl page2 (prefix <> "/empty.html")
              forM_ ["1", "2", "3"] $ \body ->
                assertEqual "replayed echo" body =<< echoPost page2 "/echo" body
              failed <- try (echoPost page2 "/echo" "4")
              assertBool "an unrecorded request should fail" (isLeft (failed :: Either SomeException Text))
              closeContext ctx2
    , testCase "should record overridden requests to har" $
        withTempDir "overridden" $ \dir -> do
          live <- newIORef True
          withChromiumBrowser $ \browser ->
            withLocalApp (echoApp live echoBody) $ \prefix -> do
              let harPath = dir <> "/har.zip"
              ctx1 <- newRecordingContext browser harPath
              page1 <- newPage ctx1
              gotoUrl page1 (prefix <> "/empty.html")
              route page1 (UrlMatchGlob "**/echo_redir") $ \handled -> do
                request <- routeRequest handled
                body <- requestPostData request
                let bumped = BS8.pack (show (maybe 0 fst (BS8.readInt (fromMaybe BS8.empty body)) + (10 :: Int)))
                fallbackRouteWith
                  handled
                  defaultContinueOptions
                    { continueUrl = Just (urlFromText (prefix <> "/echo"))
                    , continuePostData = Just bumped
                    }
              assertEqual "overridden 1" "11" =<< echoPost page1 "/echo_redir" "1"
              assertEqual "overridden 2" "12" =<< echoPost page1 "/echo_redir" "2"
              closeContext ctx1
              liftIO (writeIORef live False)
              ctx2 <- newContextWith browser defaultNewContextOptions
              contextRouteFromHar ctx2 harPath
              page2 <- newPage ctx2
              gotoUrl page2 (prefix <> "/empty.html")
              assertEqual "replayed 11" "11" =<< echoPost page2 "/echo" "11"
              assertEqual "replayed 12" "12" =<< echoPost page2 "/echo" "12"
              closeContext ctx2
    , testCase "should disambiguate by header" $
        withTempDir "disambiguate" $ \dir -> do
          live <- newIORef True
          withChromiumBrowser $ \browser ->
            withLocalApp (echoApp live echoBazHeader) $ \prefix -> do
              let harPath = dir <> "/har.zip"
              ctx1 <- newRecordingContext browser harPath
              page1 <- newPage ctx1
              gotoUrl page1 (prefix <> "/empty.html")
              forM_ ["baz1", "baz2", "baz3"] $ \baz ->
                assertEqual "recorded baz" baz =<< echoWithBaz page1 baz
              closeContext ctx1
              liftIO (writeIORef live False)
              ctx2 <- newContextWith browser defaultNewContextOptions
              contextRouteFromHar ctx2 harPath
              page2 <- newPage ctx2
              gotoUrl page2 (prefix <> "/empty.html")
              forM_ ["baz1", "baz2", "baz3"] $ \baz ->
                assertEqual "replayed baz" baz =<< echoWithBaz page2 baz
              assertEqual "unmatched baz falls back to the first entry" "baz1" =<< echoWithBaz page2 "baz4"
              closeContext ctx2
    , testCase "should update har.zip for context" $
        withTempDir "update-context" $ \dir ->
          withAssetsPrefix $ \prefix ->
            withChromiumBrowser $ \browser -> do
              let harPath = dir <> "/har.zip"
              ctx1 <- newContextWith browser defaultNewContextOptions
              contextRouteFromHarWith ctx1 harPath updateHarOptions
              page1 <- newPage ctx1
              gotoUrl page1 (prefix <> "/one-style.html")
              closeContext ctx1
              ctx2 <- newContextWith browser defaultNewContextOptions
              contextRouteFromHarWith ctx2 harPath defaultHarRouteOptions {harRouteNotFound = HarNotFoundAbort}
              page2 <- newPage ctx2
              gotoUrl page2 (prefix <> "/one-style.html")
              assertOneStyle page2
              closeContext ctx2
    , testCase "should ignore boundary when matching multipart/form-data body" $
        withTempDir "multipart" $ \dir -> do
          recorded <- newIORef []
          withChromiumBrowser $ \browser ->
            withLocalApp (formApp recorded) $ \prefix -> do
              let harPath = dir <> "/har.zip"
              ctx1 <- newContextWith browser defaultNewContextOptions
              contextRouteFromHarWith ctx1 harPath updateHarOptions
              page1 <- newPage ctx1
              gotoUrl page1 (prefix <> "/empty.html")
              pageClickSelector page1 (selectorFromText "button")
              waitForURL page1 (UrlMatchGlob "**/form.html")
              assertBool "form page rendered" . Text.isInfixOf "done" =<< content page1
              posted <- liftIO (map recordedBody <$> readIORef recorded)
              assertBool "form was posted as multipart" (any (BS8.isInfixOf "---") posted)
              closeContext ctx1
              ctx2 <- newContextWith browser defaultNewContextOptions
              contextRouteFromHar ctx2 harPath
              page2 <- newPage ctx2
              gotoUrl page2 (prefix <> "/empty.html")
              pageClickSelector page2 (selectorFromText "button")
              waitForURL page2 (UrlMatchGlob "**/form.html")
              assertBool "replayed form page rendered" . Text.isInfixOf "done" =<< content page2
              closeContext ctx2
    , testCase "should record single set-cookie headers" $
        withTempDir "single-cookie" $ \dir ->
          withChromiumBrowser $ \browser ->
            replaySetCookies browser dir ["first=foo"] "first=foo"
    , testCase "should record multiple set-cookie headers" $
        withTempDir "multiple-cookies" $ \dir ->
          withChromiumBrowser $ \browser ->
            replaySetCookies browser dir ["first=foo", "second=bar"] "first=foo; second=bar"
    , testCase "should update har.zip for page" $
        withTempDir "update-page" $ \dir ->
          withAssetsPrefix $ \prefix ->
            withChromiumBrowser $ \browser -> do
              let harPath = dir <> "/har.zip"
              ctx1 <- newContextWith browser defaultNewContextOptions
              page1 <- newPage ctx1
              routeFromHarWith page1 harPath updateHarOptions
              gotoUrl page1 (prefix <> "/one-style.html")
              closeContext ctx1
              ctx2 <- newContextWith browser defaultNewContextOptions
              page2 <- newPage ctx2
              routeFromHar page2 harPath
              gotoUrl page2 (prefix <> "/one-style.html")
              assertOneStyle page2
              closeContext ctx2
    , testCase "should update har.zip for page with different options" $
        withTempDir "update-page-options" $ \dir ->
          withAssetsPrefix $ \prefix ->
            withChromiumBrowser $ \browser -> do
              let harPath = dir <> "/har.zip"
              ctx1 <- newContextWith browser defaultNewContextOptions
              page1 <- newPage ctx1
              routeFromHarWith page1 harPath updateEmbedFullOptions
              gotoUrl page1 (prefix <> "/one-style.html")
              closeContext ctx1
              ctx2 <- newContextWith browser defaultNewContextOptions
              page2 <- newPage ctx2
              routeFromHar page2 harPath
              gotoUrl page2 (prefix <> "/one-style.html")
              assertOneStyle page2
              closeContext ctx2
    , testCase "should update extracted har.zip for page" $
        withTempDir "update-page-extracted" $ \dir ->
          withAssetsPrefix $ \prefix ->
            withChromiumBrowser $ \browser -> do
              let harPath = dir <> "/har.har"
              ctx1 <- newContextWith browser defaultNewContextOptions
              page1 <- newPage ctx1
              routeFromHarWith page1 harPath updateHarOptions
              gotoUrl page1 (prefix <> "/one-style.html")
              closeContext ctx1
              ctx2 <- newContextWith browser defaultNewContextOptions
              page2 <- newPage ctx2
              routeFromHar page2 harPath
              gotoUrl page2 (prefix <> "/one-style.html")
              assertOneStyle page2
              closeContext ctx2
    , testCase "should ignore aborted requests" $
        skipCase
          "needs req.destroy() to drop a request socket mid-flight; the Wai/Warp harness cannot reproduce that"
    ]

-- | Reports a case that cannot run in this harness yet, with the concrete
-- reason. The case passes as a no-op, matching the other specs' convention.
skipCase :: String -> IO ()
skipCase reason =
  putStrLn ("skipped: " <> reason)

-- | HAR routing that aborts any request the HAR does not contain.
abortingHarOptions :: HarRouteOptions
abortingHarOptions =
  defaultHarRouteOptions {harRouteNotFound = HarNotFoundAbort}

-- | HAR routing limited to the theverge.com redirect fixture.
theVergeHarOptions :: HarRouteOptions
theVergeHarOptions =
  defaultHarRouteOptions {harRouteUrl = UrlMatchRegex ".*theverge.*" ""}

-- | HAR routing that continues requests the HAR does not contain.
fallbackHarOptions :: HarRouteOptions
fallbackHarOptions =
  defaultHarRouteOptions {harRouteNotFound = HarNotFoundFallback}

-- | HAR routing limited to scripts, aborting the rest.
jsHarOptions :: HarRouteOptions
jsHarOptions =
  defaultHarRouteOptions {harRouteUrl = UrlMatchGlob "**/*.js"}

-- | HAR routing limited to scripts, continuing the rest.
jsFallbackHarOptions :: HarRouteOptions
jsFallbackHarOptions =
  jsHarOptions {harRouteNotFound = HarNotFoundFallback}

-- | HAR routing that records traffic into the file.
updateHarOptions :: HarRouteOptions
updateHarOptions =
  defaultHarRouteOptions {harRouteUpdate = True}

-- | Recording that embeds response bodies in full, rather than attaching them.
updateEmbedFullOptions :: HarRouteOptions
updateEmbedFullOptions =
  updateHarOptions
    { harRouteUpdateContent = RecordHarContentEmbed
    , harRouteUpdateMode = RecordHarModeFull
    }

-- | Asserts the page ran the HAR's script and painted the HAR's stylesheet, not the POSTed one.
assertHarFulfilled :: Page -> Playwright ()
assertHarFulfilled page = do
  -- HAR contains a redirect for the script that should be followed automatically.
  assertEqual "window.value" "foo" =<< evaluateText page "() => window.value"
  -- HAR contains a POST for the css file that should not be used.
  assertEqual "background" "rgb(255, 0, 0)" =<< pageBackground page

-- | Asserts the page rendered the one-style fixture with the HAR's body and stylesheet.
assertOneStyle :: Page -> Playwright ()
assertOneStyle page = do
  assertBool "one-style content" . Text.isInfixOf "hello, world!" =<< content page
  assertPinkBackground page

assertPinkBackground :: Page -> Playwright ()
assertPinkBackground page =
  assertEqual "background" "rgb(255, 192, 203)" =<< pageBackground page

-- | Serves http://no.playwright/ from the context with a script and a div, as the upstream cases do.
servePlaygroundPage :: BrowserContext -> Playwright ()
servePlaygroundPage ctx =
  contextRoute ctx (UrlMatchGlob "http://no.playwright/") $ \handled -> do
    url <- urlToText <$> (routeRequest handled >>= requestUrl)
    assertEqual "routed url" "http://no.playwright/" url
    fulfillRoute
      handled
      defaultFulfillOptions
        { fulfillContentType = Just (ContentType "text/html")
        , fulfillBody = Just (FulfillText "<script src=\"./script.js\"></script><div>hello</div>")
        }

-- | A context that records its traffic to a HAR file, written when the context closes.
newRecordingContext :: Browser -> FilePath -> Playwright BrowserContext
newRecordingContext browser harPath =
  newContextWith
    browser
    defaultNewContextOptions
      { newContextRecordHar = Just (RecordHar harPath Nothing (Just RecordHarModeMinimal) Nothing)
      }

-- | An app whose @/echo@ answers with what @answer@ picks, until the flag is cleared. After that
-- @/echo@ fails, so replies can only come from a HAR. Any other path serves an empty page.
echoApp :: IORef Bool -> (Wai.Request -> LBS.ByteString -> LBS.ByteString) -> Wai.Application
echoApp live answer request respond = do
  body <- Wai.strictRequestBody request
  isLive <- readIORef live
  case Wai.rawPathInfo request of
    "/echo"
      | isLive -> respond (Wai.responseLBS status200 [(hContentType, "text/plain")] (answer request body))
      | otherwise -> respond (Wai.responseLBS status500 [] "stale")
    _ -> respond (htmlResponse "")

echoBody :: Wai.Request -> LBS.ByteString -> LBS.ByteString
echoBody _ body = body

echoBazHeader :: Wai.Request -> LBS.ByteString -> LBS.ByteString
echoBazHeader request _ =
  LBS.fromStrict (fromMaybe "" (lookup "baz" (Wai.requestHeaders request)))

-- | Posts a body to a path from the page, returning the response text.
echoPost :: Page -> Text -> Text -> Playwright Text
echoPost page path body =
  evaluateText page ("() => fetch('" <> path <> "', { method: 'POST', body: '" <> body <> "' }).then(r => r.text())")

-- | Posts an empty body with headers that a HAR entry is matched on.
echoWithBaz :: Page -> Text -> Playwright Text
echoWithBaz page baz =
  evaluateText
    page
    ( "() => fetch('/echo', { method: 'POST', body: '', headers: { foo: 'foo-value', bar: 'bar-value', baz: '"
        <> baz
        <> "' } }).then(r => r.text())"
    )

-- | A page with a multipart form that posts to form.html, which records what it receives.
formApp :: IORef [Recorded] -> Wai.Application
formApp recorded request respond
  | Wai.rawPathInfo request == "/form.html" = do
      recordRequest recorded request
      respond (htmlResponse "<div>done</div>")
  | otherwise =
      respond (htmlResponse formPage)
  where
    formPage :: LBS.ByteString
    formPage =
      "<form id=\"form\" action=\"form.html\" enctype=\"multipart/form-data\" method=\"POST\">"
        <> "<input id=\"file\" type=\"file\" multiple />"
        <> "<button type=\"submit\">Upload</button></form>"

-- | Answers every request with the given set-cookie headers.
cookieApp :: [BS8.ByteString] -> Wai.Application
cookieApp cookies _ respond =
  respond (Wai.responseLBS status200 ((hContentType, "text/html") : [("set-cookie", cookie) | cookie <- cookies]) "")

htmlResponse :: LBS.ByteString -> Wai.Response
htmlResponse =
  Wai.responseLBS status200 [(hContentType, "text/html")]

-- | Records one page that sets cookies through a context HAR update, then replays it from the file.
replaySetCookies :: Browser -> FilePath -> [BS8.ByteString] -> Text -> Playwright ()
replaySetCookies browser dir cookies expected =
  withLocalApp (cookieApp cookies) $ \prefix -> do
    let harPath = dir <> "/har.zip"
    ctx1 <- newContextWith browser defaultNewContextOptions
    contextRouteFromHarWith ctx1 harPath updateHarOptions
    page1 <- newPage ctx1
    gotoUrl page1 (prefix <> "/empty.html")
    assertEqual "recorded cookies" expected =<< sortedCookies page1
    closeContext ctx1
    ctx2 <- newContextWith browser defaultNewContextOptions
    contextRouteFromHar ctx2 harPath
    page2 <- newPage ctx2
    gotoUrl page2 (prefix <> "/empty.html")
    assertEqual "replayed cookies" expected =<< sortedCookies page2
    closeContext ctx2
  where
    sortedCookies page = do
      cookie <- evaluateText page "() => document.cookie"
      pure (Text.intercalate "; " (sort (Text.splitOn "; " cookie)))

-- | Records one navigation with the given HAR options, closing the context so the HAR is written.
recordOnClose :: Browser -> RecordHar -> Text -> Playwright ()
recordOnClose browser harOpts url = do
  ctx <- newContextWith browser defaultNewContextOptions {newContextRecordHar = Just harOpts}
  page <- newPage ctx
  gotoUrl page url
  closeContext ctx

-- | Asserts that navigating to the URL fails, as it does when the HAR has no entry for it.
assertNavigationFails :: Page -> Text -> Playwright ()
assertNavigationFails page url = do
  outcome <- try (gotoUrl page url)
  assertBool "navigation should fail" (isLeft (outcome :: Either SomeException ()))

-- | The URL of the request behind a navigation response, if there was one.
responseUrlText :: Maybe Response -> Playwright (Maybe Text)
responseUrlText Nothing = pure Nothing
responseUrlText (Just response) = do
  request <- responseRequest response
  Just . urlToText <$> requestUrl request

hrefOf :: Page -> Playwright Text
hrefOf page =
  evaluateText page "() => location.href"

-- | Replays a recorded HAR in a fresh context and checks the page rendered from it.
replayFrom :: Browser -> FilePath -> Text -> Playwright ()
replayFrom browser harPath url = do
  ctx <- newContextWith browser defaultNewContextOptions
  contextRouteFromHar ctx harPath
  page <- newPage ctx
  gotoUrl page url
  body <- content page
  assertBool "replayed body" (Text.isInfixOf "hello, world!" body)
  assertEqual "background" "rgb(255, 192, 203)" =<< pageBackground page
  closeContext ctx

gotoUrl :: Page -> Text -> Playwright ()
gotoUrl page url =
  void (goto page (urlFromText url) defaultGotoOptions)

pageBackground :: Page -> Playwright Text
pageBackground page =
  evaluateText page "() => getComputedStyle(document.body).backgroundColor"

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir name action = do
  root <- getTemporaryDirectory
  let dir = root <> "/pw-har-" <> name
  bracket
    (removePathForcibly dir >> createDirectoryIfMissing True dir >> pure dir)
    removeDirectoryRecursive
    action

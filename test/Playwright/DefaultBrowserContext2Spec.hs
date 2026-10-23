module Playwright.DefaultBrowserContext2Spec
  ( tests
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import Data.List
  ( find
  )
import Data.Text
  ( Text
  , isInfixOf
  , toLower
  )
import Playwright
  ( AriaRole (AriaRole)
  , BrowserContext
  , ColorScheme (ColorSchemeDark)
  , Contrast (ContrastMore)
  , ForcedColors (ForcedColorsActive)
  , Geolocation (Geolocation, geolocationAccuracy, geolocationLatitude, geolocationLongitude)
  , GetByRoleOptions (getByRoleName)
  , HeaderName (HeaderName, headerNameToText)
  , HeaderValue (HeaderValue, headerValueToText)
  , HttpHeader (HttpHeader, httpHeaderName, httpHeaderValue)
  , LaunchOptions (launchOptionsArgs)
  , Locale (Locale)
  , NewContextOptions (newContextColorScheme, newContextContrast, newContextExtraHTTPHeaders, newContextForcedColors, newContextGeolocation, newContextHasTouch, newContextIsMobile, newContextLocale, newContextPermissions, newContextReducedMotion, newContextTimezoneId, newContextViewport)
  , Page
  , PermissionName (PermissionName)
  , Playwright
  , ReducedMotion (ReducedMotionReduce)
  , TimezoneId (TimezoneId)
  , UrlMatch (UrlMatchGlob)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , acceptDialog
  , armDialog
  , browserVersion
  , chromium
  , click
  , closeBrowser
  , closeContext
  , contextBrowser
  , contextPages
  , contextsEqual
  , defaultGetByRoleOptions
  , defaultGotoOptions
  , defaultLaunchOptions
  , defaultNewContextOptions
  , evaluate
  , evaluateBool
  , evaluateNumber
  , evaluateText
  , getByRole
  , goto
  , launchPersistentContext
  , latitudeFromInt
  , longitudeFromInt
  , newBrowserPage
  , newPage
  , pageInnerHtml
  , registerSelector
  , reload
  , requestHeaders
  , runPlaywright
  , selectorFromText
  , setContent
  , title
  , url
  , urlFromText
  , urlToText
  , waitForContextClose
  , armRequest
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , assertProtocolContains
  , withAssetsPrefix
  , withPersistentContext
  , withPersistentContextWith
  , withPersistentUserDataWith
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( wait
  , withAsync
  )
import UnliftIO.Directory
  ( listDirectory
  , removePathForcibly
  )
import UnliftIO.Temporary
  ( withSystemTempDirectory
  )

tests :: TestTree
tests =
  testGroup
    "defaultbrowsercontext-2"
    [ testCase "should support hasTouch option" $
        withAssetsPrefix $ \prefix ->
          withPersistentContextWith
            defaultNewContextOptions {newContextHasTouch = Just True}
            $ \_ page -> do
              _ <- goto page (urlFromText (prefix <> "/mobile.html")) defaultGotoOptions
              assertEqual "touch" True =<< evaluateBool page "() => 'ontouchstart' in window"
    , testCase "should work in persistent context" $
        withAssetsPrefix $ \prefix ->
          withPersistentContextWith
            defaultNewContextOptions
              { newContextViewport = Just (ViewportSize (ViewportWidth 320) (ViewportHeight 480))
              , newContextIsMobile = Just True
              }
            $ \_ page -> do
              _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
              assertEqual "innerWidth" (980 :: Double) =<< evaluateNumber page "() => window.innerWidth"
    , testCase "should support colorScheme option" $
        withPersistentContextWith
          defaultNewContextOptions {newContextColorScheme = Just ColorSchemeDark}
          $ \_ page -> do
            assertEqual "light" False
              =<< evaluateBool page "() => matchMedia('(prefers-color-scheme: light)').matches"
            assertEqual "dark" True
              =<< evaluateBool page "() => matchMedia('(prefers-color-scheme: dark)').matches"
    , testCase "should support reducedMotion option" $
        withPersistentContextWith
          defaultNewContextOptions {newContextReducedMotion = Just ReducedMotionReduce}
          $ \_ page -> do
            assertEqual "reduce" True
              =<< evaluateBool page "() => matchMedia('(prefers-reduced-motion: reduce)').matches"
            assertEqual "no preference" False
              =<< evaluateBool page "() => matchMedia('(prefers-reduced-motion: no-preference)').matches"
    , testCase "should support forcedColors option" $
        withPersistentContextWith
          defaultNewContextOptions {newContextForcedColors = Just ForcedColorsActive}
          $ \_ page -> do
            assertEqual "active" True
              =<< evaluateBool page "() => matchMedia('(forced-colors: active)').matches"
            assertEqual "none" False
              =<< evaluateBool page "() => matchMedia('(forced-colors: none)').matches"
    , testCase "should support contrast option" $
        withPersistentContextWith
          defaultNewContextOptions {newContextContrast = Just ContrastMore}
          $ \_ page -> do
            assertEqual "more" True
              =<< evaluateBool page "() => matchMedia('(prefers-contrast: more)').matches"
            assertEqual "no preference" False
              =<< evaluateBool page "() => matchMedia('(prefers-contrast: no-preference)').matches"
    , testCase "should support timezoneId option" $
        withPersistentContextWith
          defaultNewContextOptions
            { newContextLocale = Just (Locale "en-US")
            , newContextTimezoneId = Just (TimezoneId "America/Jamaica")
            }
          $ \_ page -> do
            rendered <- evaluateText page "() => new Date(1479579154987).toString()"
            assertBool "jamaica" ("Sat Nov 19 2016 13:12:34 GMT-0500" `isInfixOf` rendered)
    , testCase "should support locale option" $
        withPersistentContextWith
          defaultNewContextOptions {newContextLocale = Just (Locale "fr-FR")}
          $ \_ page ->
            assertEqual "language" "fr-FR" =<< evaluateText page "() => navigator.language"
    , testCase "should support geolocation and permissions options" $
        withAssetsPrefix $ \prefix ->
          withPersistentContextWith
            defaultNewContextOptions
              { newContextGeolocation =
                  Just
                    Geolocation
                      { geolocationAccuracy = Nothing
                      , geolocationLatitude = latitudeFromInt 10
                      , geolocationLongitude = longitudeFromInt 10
                      }
              , newContextPermissions = Just [PermissionName "geolocation"]
              }
            $ \_ page -> do
              _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
              assertEqual "latitude" (10 :: Double)
                =<< evaluateNumber
                  page
                  "() => new Promise(resolve => navigator.geolocation.getCurrentPosition(position => resolve(position.coords.latitude)))"
              assertEqual "longitude" (10 :: Double)
                =<< evaluateNumber
                  page
                  "() => new Promise(resolve => navigator.geolocation.getCurrentPosition(position => resolve(position.coords.longitude)))"
    , testCase "should support extraHTTPHeaders option" $
        withAssetsPrefix $ \prefix ->
          withPersistentContextWith
            defaultNewContextOptions
              { newContextExtraHTTPHeaders =
                  Just [HttpHeader (HeaderName "foo") (HeaderValue "bar")]
              }
            $ \_ page ->
              armRequest page (UrlMatchGlob "**/empty.html") >>= \pending -> do
                _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
                request <- pending
                headers <- requestHeaders request
                assertEqual "foo" (Just "bar") (lookupHeader "foo" headers)
    , testCase "should accept userDataDir" $
        acceptUserDataDirTest
    , testCase "should restore state from userDataDir" $
        restoreUserDataDirTest
    , testCase "should create userDataDir if it does not exist" $
        createUserDataDirTest
    , testCase "should goto about:blank on relaunched persistent context" $
        relaunchAboutBlankTest
    , testCase "should have default URL when launching browser" $
        withPersistentContext $ \ctx _page -> do
          pages <- contextPages ctx
          urls <- traverse url pages
          assertEqual "urls" ["about:blank"] (fmap urlToText urls)
    , testCase "should fire close event for a persistent context" $
        withPersistentContext $ \ctx _page ->
          withAsync (waitForContextClose ctx) $ \handle -> do
            closeContext ctx
            closed <- wait handle
            assertEqual "same" True (contextsEqual closed ctx)
    , testCase "should respect selectors" $
        respectSelectorsTest
    , testCase "dialog.accept should work" $
        withPersistentContext $ \ctx page -> do
          _ <-
            goto
              page
              (urlFromText "data:text/html,<html><title>Title</title><button onclick=\"alert('Alert')\">Button</button></html>")
              defaultGotoOptions
          takeDialog <- armDialog page
          withAsync (takeDialog >>= acceptDialog) $ \_ ->
            click
              ( getByRole
                  page
                  (AriaRole "button")
                  defaultGetByRoleOptions {getByRoleName = Just "Button"}
              )
          closeContext ctx
    , testCase "exposes browser" $
        withPersistentContext $ \ctx _page -> do
          browser <- contextBrowser ctx
          version <- browserVersion browser
          assertBool "version" (version /= "")
          page <- newBrowserPage browser
          _ <-
            goto
              page
              (urlFromText "data:text/html,<html><title>Title</title></html>")
              defaultGotoOptions
          assertEqual "title" "Title" =<< title page
          closeBrowser browser
          remaining <- contextPages ctx
          assertEqual "closed pages" 0 (length remaining)
          closeContext ctx
    , testCase "should accept relative userDataDir" $
        relativeUserDataDirTest
    , testCase "should throw if page argument is passed" $
        passedPageArgumentTest
    , testCase "CacheStorage entry should survive page.reload()" $
        withAssetsPrefix $ \prefix ->
          withPersistentContext $ \_ctx page -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            _ <-
              evaluate
                page
                "async () => { const cache = await caches.open('repro-cache'); await cache.put('/meta', new Response('payload')); }"
            _ <- reload page
            after <-
              evaluateText
                page
                "async () => { const cache = await caches.open('repro-cache'); const resp = await cache.match('/meta'); return resp ? await resp.text() : 'missing'; }"
            assertEqual "after reload" "payload" after
    , testCase "should support storage.getDirectory()" $
        withAssetsPrefix $ \prefix ->
          withPersistentContext $ \_ctx page -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            name <-
              evaluateText
                page
                "async () => (await navigator.storage.getDirectory()).name"
            assertEqual "name" "" name
    ]

relativeUserDataDirTest :: IO ()
relativeUserDataDirTest = do
  let
    userDataDir :: FilePath
    userDataDir = "pw-relative-user-data"
  removePathForcibly userDataDir
  runPlaywright $ do
    browserType <- chromium
    ctx <-
      launchPersistentContext browserType userDataDir defaultLaunchOptions defaultNewContextOptions
    closeContext ctx
  entries <- listDirectory userDataDir
  removePathForcibly userDataDir
  assertBool "populated" (not (null entries))

passedPageArgumentTest :: IO ()
passedPageArgumentTest =
  withSystemTempDirectory "pw-user-data" $ \userDataDir ->
    runPlaywright $ do
      browserType <- chromium
      assertProtocolContains
        "can not specify page"
        ( launchPersistentContext
            browserType
            userDataDir
            defaultLaunchOptions {launchOptionsArgs = Just ["http://127.0.0.1/empty.html"]}
            defaultNewContextOptions
        )

acceptUserDataDirTest :: IO ()
acceptUserDataDirTest =
  withSystemTempDirectory "pw-user-data" $ \userDataDir ->
    runPlaywright $ do
      withPersistentUserDataWith userDataDir defaultNewContextOptions $ \_ctx _page -> do
        entries <- liftIO (listDirectory userDataDir)
        assertBool "populated" (not (null entries))
      after <- liftIO (listDirectory userDataDir)
      assertBool "kept" (not (null after))

restoreUserDataDirTest :: IO ()
restoreUserDataDirTest =
  withAssetsPrefix $ \prefix ->
    withSystemTempDirectory "pw-user-data" $ \userDataDir ->
      withSystemTempDirectory "pw-user-data-other" $ \otherDir ->
        runPlaywright $ do
          let
            empty = urlFromText (prefix <> "/empty.html")
          browserType <- chromium
          ctx1 <-
            launchPersistentContext browserType userDataDir defaultLaunchOptions defaultNewContextOptions
          page1 <- newPage ctx1
          _ <- goto page1 empty defaultGotoOptions
          _ <- evaluate page1 "() => { localStorage.hey = 'hello'; }"
          _ <- goto page1 empty defaultGotoOptions
          closeContext ctx1
          ctx2 <-
            launchPersistentContext browserType userDataDir defaultLaunchOptions defaultNewContextOptions
          page2 <- newPage ctx2
          _ <- goto page2 empty defaultGotoOptions
          assertEqual "restored" "hello" =<< evaluateText page2 "() => localStorage.hey"
          closeContext ctx2
          ctx3 <-
            launchPersistentContext browserType otherDir defaultLaunchOptions defaultNewContextOptions
          page3 <- newPage ctx3
          _ <- goto page3 empty defaultGotoOptions
          assertEqual "fresh" True . (/= "hello") =<< evaluateText page3 "() => String(localStorage.hey)"
          closeContext ctx3

createUserDataDirTest :: IO ()
createUserDataDirTest =
  withSystemTempDirectory "pw-user-data" $ \parent -> do
    let
      userDataDir = parent <> "/nonexisting"
    runPlaywright $ do
      browserType <- chromium
      ctx <-
        launchPersistentContext browserType userDataDir defaultLaunchOptions defaultNewContextOptions
      closeContext ctx
    entries <- listDirectory userDataDir
    assertBool "created" (not (null entries))

relaunchAboutBlankTest :: IO ()
relaunchAboutBlankTest =
  withSystemTempDirectory "pw-user-data" $ \userDataDir ->
    runPlaywright $ do
      browserType <- chromium
      ctx1 <-
        launchPersistentContext browserType userDataDir defaultLaunchOptions defaultNewContextOptions
      page1 <- requireFirstPage ctx1
      _ <- goto page1 (urlFromText "about:blank") defaultGotoOptions
      closeContext ctx1
      ctx2 <-
        launchPersistentContext browserType userDataDir defaultLaunchOptions defaultNewContextOptions
      page2 <- requireFirstPage ctx2
      _ <- goto page2 (urlFromText "about:blank") defaultGotoOptions
      assertEqual "url" "about:blank" . urlToText =<< url page2
      closeContext ctx2

respectSelectorsTest :: IO ()
respectSelectorsTest =
  runPlaywright $ do
    registerSelector "defaultContextCSS" tagSelectorEngine
    withSystemTempDirectory "pw-user-data" $ \userDataDir ->
      withPersistentUserDataWith userDataDir defaultNewContextOptions $ \_ page -> do
        setContent page "<div>hello</div>"
        assertEqual "css" "hello" =<< pageInnerHtml page (selectorFromText "css=div")
        assertEqual "custom" "hello" =<< pageInnerHtml page (selectorFromText "defaultContextCSS=div")

requireFirstPage :: BrowserContext -> Playwright Page
requireFirstPage ctx = do
  pages <- contextPages ctx
  case pages of
    page : _ ->
      pure page
    [] ->
      assertFailure "expected the persistent context default page"

lookupHeader :: Text -> [HttpHeader] -> Maybe Text
lookupHeader name headers =
  fmap (headerValueToText . httpHeaderValue)
    ( find
        (\header -> toLower (headerNameToText (httpHeaderName header)) == toLower name)
        headers
    )

tagSelectorEngine :: Text
tagSelectorEngine =
  "(() => ({\n\
  \  query(root, selector) {\n\
  \    return root.querySelector(selector);\n\
  \  },\n\
  \  queryAll(root, selector) {\n\
  \    return Array.from(root.querySelectorAll(selector));\n\
  \  }\n\
  \}))()"

module Playwright.BrowserContextTimezoneSpec
  ( tests
  ) where

import Data.Text
  ( Text
  , isInfixOf
  )
import Playwright
  ( ConsoleMessage (consoleMessageText)
  , Locale (Locale)
  , Page
  , Playwright
  , newContext
  , NewContextOptions (newContextLocale, newContextTimezoneId)
  , TimezoneId (TimezoneId)
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluateText
  , evaluateWith
  , goto
  , newContextWith
  , newPage
  , textArgument
  , urlFromText
  , waitForContextConsole
  , waitForPopup
  )
import Playwright.Harness
  ( assertProtocolContains
  , withAssetsContextWith
  , withChromiumBrowser
  , withChromiumContextWith
  , assertBool
  , assertEqual
  )
import UnliftIO.Async
  ( wait
  , withAsync
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
    "browsercontext-timezone-id"
    [ testCase "should work" $ do
        assertTimezone (TimezoneId "America/Jamaica") "Sat Nov 19 2016 13:12:34 GMT-0500"
        assertTimezone (TimezoneId "Pacific/Honolulu") "Sat Nov 19 2016 08:12:34 GMT-1000"
        assertTimezone (TimezoneId "America/Buenos_Aires") "Sat Nov 19 2016 15:12:34 GMT-0300"
        assertTimezone (TimezoneId "Europe/Berlin") "Sat Nov 19 2016 19:12:34 GMT+0100"
    , testCase "should throw for invalid timezone IDs when creating pages" $
        withChromiumBrowser $ \browser ->
          mapM_
            ( \zone ->
                assertProtocolContains
                  ("Invalid timezone ID: " <> zone)
                  ( do
                      ctx <-
                        newContextWith
                          browser
                          defaultNewContextOptions {newContextTimezoneId = Just (TimezoneId zone)}
                      _ <- newPage ctx
                      pure ()
                  )
            )
            ["Foo/Bar", "Baz/Qux"]
    , testCase "should work for multiple pages sharing same process" $
        withAssetsContextWith
          defaultNewContextOptions {newContextTimezoneId = Just (TimezoneId "Europe/Moscow")}
          $ \prefix _ page -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            popup <- openPopup page (prefix <> "/empty.html")
            _ <- openPopup popup (prefix <> "/empty.html")
            pure ()
    , testCase "should affect Intl.DateTimeFormat().resolvedOptions().timeZone" $
        withChromiumContextWith
          defaultNewContextOptions {newContextTimezoneId = Just (TimezoneId "America/Jamaica")}
          $ \_ page ->
            assertEqual "zone" "America/Jamaica"
              =<< evaluateText page "() => (new Intl.DateTimeFormat()).resolvedOptions().timeZone"
    , testCase "should not change default timezone in another context" $
        withChromiumBrowser $ \browser -> do
          defaultContext <- newContext browser
          defaultPage <- newPage defaultContext
          defaultZone <- evaluateText defaultPage "() => Intl.DateTimeFormat().resolvedOptions().timeZone"
          let
            override = overrideZone defaultZone
          overridden <-
            newContextWith
              browser
              defaultNewContextOptions {newContextTimezoneId = Just (TimezoneId override)}
          overriddenPage <- newPage overridden
          assertEqual "override" override
            =<< evaluateText overriddenPage "() => Intl.DateTimeFormat().resolvedOptions().timeZone"
          again <- newContext browser
          againPage <- newPage again
          assertEqual "default again" defaultZone
            =<< evaluateText againPage "() => Intl.DateTimeFormat().resolvedOptions().timeZone"
    , testCase "should propagate timezone to workers" $
        withAssetsContextWith
          defaultNewContextOptions {newContextTimezoneId = Just (TimezoneId "America/Jamaica")}
          $ \prefix ctx page -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            withAsync (waitForContextConsole ctx) $ \pending -> do
              _ <-
                evaluateText
                  page
                  "() => { new Worker(URL.createObjectURL(new Blob(['console.log(Intl.DateTimeFormat().resolvedOptions().timeZone)'], { type: 'application/javascript' }))); return 'ok'; }"
              message <- wait pending
              assertEqual "worker timezone" "America/Jamaica" (consoleMessageText message)
    ]

assertTimezone :: TimezoneId -> Text -> IO ()
assertTimezone zone needle =
  withChromiumContextWith
    defaultNewContextOptions
      { newContextLocale = Just (Locale "en-US")
      , newContextTimezoneId = Just zone
      }
    $ \_ page -> do
      rendered <- evaluateText page "() => new Date(1479579154987).toString()"
      assertBool (show needle) (needle `isInfixOf` rendered)

-- | Open a popup from the page by calling window.open(url), waiting for the popup event.
openPopup :: Page -> Text -> Playwright Page
openPopup page url =
  withAsync (waitForPopup page) $ \handle -> do
    _ <- evaluateWith page "url => { window.open(url); }" (textArgument url)
    wait handle

-- | A timezone other than the given default one.
overrideZone :: Text -> Text
overrideZone zone =
  if zone == "Europe/Moscow" then "America/Los_Angeles" else "Europe/Moscow"

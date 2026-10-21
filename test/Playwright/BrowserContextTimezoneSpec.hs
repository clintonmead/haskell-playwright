module Playwright.BrowserContextTimezoneSpec
  ( tests
  ) where

import Data.Text
  ( Text
  , isInfixOf
  )
import Playwright
  ( Locale (Locale)
  , newContext
  , NewContextOptions (newContextLocale, newContextTimezoneId)
  , TimezoneId (TimezoneId)
  , defaultNewContextOptions
  , evaluateText
  , newContextWith
  , newPage
  )
import Playwright.Harness
  ( assertProtocolContains
  , withChromiumBrowser
  , withChromiumContextWith
  , assertBool
  , assertEqual
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
          assertProtocolContains
            "Invalid timezone ID: Foo/Bar"
            ( do
                ctx <-
                  newContextWith
                    browser
                    defaultNewContextOptions {newContextTimezoneId = Just (TimezoneId "Foo/Bar")}
                _ <- newPage ctx
                pure ()
            )
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

-- | A timezone other than the given default one.
overrideZone :: Text -> Text
overrideZone zone =
  if zone == "Europe/Moscow" then "America/Los_Angeles" else "Europe/Moscow"

module Playwright.BrowserContextLocaleSpec
  ( tests
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe
  ( fromMaybe
  )
import UnliftIO.IORef
  ( newIORef
  , readIORef
  )
import Data.Text
  ( Text
  , pack
  )
import Data.Text qualified as Text
import Network.HTTP.Types
  ( hAcceptLanguage
  , hContentType
  , status200
  )
import Network.Wai
  ( Application
  , responseLBS
  )
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( Locale (Locale)
  , NewContextOptions (newContextLocale, newContextTimezoneId)
  , TimezoneId (TimezoneId)
  , newContext
  , newContextWith
  , newPage
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluateText
  , goto
  , urlFromText
  )
import Playwright.Harness
  ( Recorded (recordedHeaders, recordedPath)
  , assertBool
  , withChromiumBrowser
  , recordingPageApp
  , withChromiumContextWith
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
    "browsercontext-locale"
    [ testCase "should affect accept-language header" $
        withApplication (pure echoAcceptLanguage) $ \port ->
          withChromiumContextWith
            defaultNewContextOptions {newContextLocale = Just (Locale "fr-CH")}
            $ \_ page -> do
              _ <-
                goto
                  page
                  (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
                  defaultGotoOptions
              header <- evaluateText page "() => document.body.innerText"
              assertEqual "prefix" "fr-CH" (Text.take 5 header)
    , testCase "should affect navigator.language" $
        withChromiumContextWith
          defaultNewContextOptions {newContextLocale = Just (Locale "fr-FR")}
          $ \_ page ->
            assertEqual "language" "fr-FR" =<< evaluateText page "() => navigator.language"
    , testCase "should format number" $
        withChromiumContextWith
          defaultNewContextOptions {newContextLocale = Just (Locale "en-US")}
          $ \_ page ->
            assertEqual "en-US" "1,000,000.5"
              =<< evaluateText page "() => (1000000.50).toLocaleString()"
    , testCase "should format date" $
        withChromiumBrowser $ \browser -> do
          english <-
            newContextWith
              browser
              defaultNewContextOptions
                { newContextLocale = Just (Locale "en-US")
                , newContextTimezoneId = Just (TimezoneId "America/Los_Angeles")
                }
          englishPage <- newPage english
          assertEqual "en-US" "Sat Nov 19 2016 10:12:34 GMT-0800 (Pacific Standard Time)"
            =<< evaluateText englishPage "() => new Date(1479579154987).toString()"
          german <-
            newContextWith
              browser
              defaultNewContextOptions
                { newContextLocale = Just (Locale "de-DE")
                , newContextTimezoneId = Just (TimezoneId "Europe/Berlin")
                }
          germanPage <- newPage german
          assertEqual "de-DE" "Sat Nov 19 2016 19:12:34 GMT+0100 (Mitteleuropäische Normalzeit)"
            =<< evaluateText germanPage "() => new Date(1479579154987).toString()"
    , testCase "should not change default locale in another context" $
        withChromiumBrowser $ \browser -> do
          defaultContext <- newContext browser
          defaultPage <- newPage defaultContext
          defaultLocale <- evaluateText defaultPage "() => new Intl.NumberFormat().resolvedOptions().locale"
          let
            override = if defaultLocale == ("es-MX" :: Text) then "de-DE" else "es-MX" :: Text
          overridden <-
            newContextWith
              browser
              defaultNewContextOptions {newContextLocale = Just (Locale override)}
          overriddenPage <- newPage overridden
          assertEqual "override" override
            =<< evaluateText overriddenPage "() => new Intl.NumberFormat().resolvedOptions().locale"
    , testCase "should be isolated between contexts" $
        withChromiumBrowser $ \browser -> do
          first <-
            newContextWith browser defaultNewContextOptions {newContextLocale = Just (Locale "en-US")}
          firstPage <- newPage first
          second <-
            newContextWith browser defaultNewContextOptions {newContextLocale = Just (Locale "ru-RU")}
          secondPage <- newPage second
          assertEqual "first" "1,000,000.5" =<< evaluateText firstPage "() => (1000000.50).toLocaleString()"
          assertBool "second differs" . (/= "1,000,000.5") =<< evaluateText secondPage "() => (1000000.50).toLocaleString()"
    , testCase "should send user Accept-Language header" $ do
        requests <- newIORef []
        withApplication (pure (recordingPageApp requests)) $ \port ->
          withChromiumContextWith
            defaultNewContextOptions {newContextLocale = Just (Locale "en-GB")}
            $ \_ page -> do
              let
                base = "http://127.0.0.1:" <> pack (show port)
              _ <- goto page (urlFromText (base <> "/empty.html")) defaultGotoOptions
              _ <-
                evaluateText
                  page
                  ( "async () => { await fetch('" <> base <> "/empty.html', {headers: {'Content-Type': 'application/json', 'Accept-Language': 'de'}}); return 'done'; }"
                  )
              seenOverride <- readIORef requests
              assertEqual "override" (Just "de") (lookup "accept-language" . recordedHeaders =<< latestRecorded seenOverride)
              _ <-
                evaluateText
                  page
                  ( "async () => { await fetch('" <> base <> "/empty.html', {headers: {'Content-Type': 'application/json'}}); return 'done'; }"
                  )
              seenDefault <- readIORef requests
              let
                defaultHeader = lookup "accept-language" . recordedHeaders =<< latestRecorded seenDefault
              assertBool "context locale" (maybe False ("en-GB" `BS8.isInfixOf`) defaultHeader)
    ]

echoAcceptLanguage :: Application
echoAcceptLanguage req respond =
  respond
    ( responseLBS
        status200
        [(hContentType, BS8.pack "text/plain")]
        (LBS8.fromStrict (fromMaybe BS8.empty (lookup hAcceptLanguage (Wai.requestHeaders req))))
    )

-- | The most recent request to /empty.html, if any.
latestRecorded :: [Recorded] -> Maybe Recorded
latestRecorded seen =
  case filter ((== "/empty.html") . recordedPath) seen of
    latest : _ -> Just latest
    [] -> Nothing

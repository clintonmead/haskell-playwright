module Playwright.BrowserContextLocaleSpec
  ( tests
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe
  ( fromMaybe
  )
import Data.Text
  ( pack
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
  , NewContextOptions (newContextLocale)
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluateText
  , goto
  , urlFromText
  )
import Playwright.Harness
  ( withChromiumContextWith
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , testCase
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
    ]

echoAcceptLanguage :: Application
echoAcceptLanguage req respond =
  respond
    ( responseLBS
        status200
        [(hContentType, BS8.pack "text/plain")]
        (LBS8.fromStrict (fromMaybe BS8.empty (lookup hAcceptLanguage (Wai.requestHeaders req))))
    )

module Playwright.PageRequestContinueSpec
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
import Network.HTTP.Types
  ( hContentType
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
  ( ContinueOptions (continueHeaders)
  , HeaderName (HeaderName)
  , HeaderValue (HeaderValue)
  , HttpHeader (HttpHeader)
  , UrlGlob (UrlGlob)
  , continueRoute
  , continueRouteWith
  , defaultContinueOptions
  , defaultGotoOptions
  , evaluateText
  , goto
  , requestHeaders
  , responseOk
  , route
  , routeRequest
  , urlFromText
  )
import Playwright.Harness
  ( withAssets
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
    "page-request-continue"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          route page (UrlGlob "**/*") continueRoute
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "ok" True =<< responseOk value
    , testCase "should amend HTTP headers" $
        withApplication (pure echoFooHeader) $ \port ->
          withChromiumPage $ \page -> do
            route page (UrlGlob "**/*") $ \handled -> do
              headers <- requestHeaders =<< routeRequest handled
              continueRouteWith
                handled
                defaultContinueOptions
                  { continueHeaders =
                      Just (headers <> [HttpHeader (HeaderName "foo") (HeaderValue "bar")])
                  }
            _ <-
              goto
                page
                (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
                defaultGotoOptions
            assertEqual "foo" "bar" =<< evaluateText page "() => document.body.innerText"
    ]

echoFooHeader :: Application
echoFooHeader req respond =
  respond
    ( responseLBS
        status200
        [(hContentType, BS8.pack "text/plain")]
        (LBS8.fromStrict (fromMaybe BS8.empty (lookup "foo" (Wai.requestHeaders req))))
    )

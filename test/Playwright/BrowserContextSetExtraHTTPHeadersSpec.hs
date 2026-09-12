module Playwright.BrowserContextSetExtraHTTPHeadersSpec
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
  ( HeaderName (HeaderName)
  , HeaderValue (HeaderValue)
  , HttpHeader (HttpHeader)
  , NewContextOptions (newContextExtraHTTPHeaders)
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluateText
  , goto
  , setExtraHTTPHeaders
  , urlFromText
  )
import Playwright.Harness
  ( withChromiumContextWith
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
    "browsercontext-set-extra-http-headers"
    [ testCase "should override extra headers from browser context" $
        withApplication (pure echoFooBar) $ \port ->
          withChromiumContextWith
            defaultNewContextOptions
              { newContextExtraHTTPHeaders =
                  Just
                    [ HttpHeader (HeaderName "fOo") (HeaderValue "bAr")
                    , HttpHeader (HeaderName "baR") (HeaderValue "foO")
                    ]
              }
            $ \_ page -> do
              setExtraHTTPHeaders
                page
                [HttpHeader (HeaderName "Foo") (HeaderValue "Bar")]
              _ <-
                goto
                  page
                  (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
                  defaultGotoOptions
              assertEqual "headers" "Bar foO" =<< evaluateText page "() => document.body.innerText"
    ]

echoFooBar :: Application
echoFooBar req respond =
  let
    headers = Wai.requestHeaders req
    foo = fromMaybe BS8.empty (lookup "foo" headers)
    bar = fromMaybe BS8.empty (lookup "bar" headers)
  in
    respond
      ( responseLBS
          status200
          [(hContentType, BS8.pack "text/plain")]
          (LBS8.fromStrict (foo <> BS8.pack " " <> bar))
      )

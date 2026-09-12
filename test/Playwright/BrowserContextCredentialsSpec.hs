module Playwright.BrowserContextCredentialsSpec
  ( tests
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text
  ( pack
  )
import Network.HTTP.Types
  ( hAuthorization
  , hContentType
  , status200
  , status401
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
  ( HttpCredentials (HttpCredentials, httpCredentialsOrigin, httpCredentialsPassword, httpCredentialsSend, httpCredentialsUsername)
  , HttpPassword (HttpPassword)
  , HttpStatus (httpStatusToInt)
  , HttpUsername (HttpUsername)
  , NewContextOptions (newContextHttpCredentials)
  , Response
  , defaultGotoOptions
  , defaultNewContextOptions
  , goto
  , reload
  , responseStatus
  , setHTTPCredentials
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
  , assertFailure
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "browsercontext-credentials"
    [ testCase "should fail without credentials" $
        withAuthServer $ \port ->
          withChromiumContextWith defaultNewContextOptions $ \_ page -> do
            response <-
              goto
                page
                (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
                defaultGotoOptions
            assertEqual "status" 401 =<< statusCode response
    , testCase "should work with setHTTPCredentials" $
        withAuthServer $ \port ->
          withChromiumContextWith defaultNewContextOptions $ \ctx page -> do
            let
              target = urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html")
            first <- goto page target defaultGotoOptions
            assertEqual "unauthorized" 401 =<< statusCode first
            setHTTPCredentials
              ctx
              HttpCredentials
                { httpCredentialsUsername = HttpUsername "user"
                , httpCredentialsPassword = HttpPassword "pass"
                , httpCredentialsOrigin = Nothing
                , httpCredentialsSend = Nothing
                }
            second <- reload page
            assertEqual "authorized" 200 =<< statusCode second
    , testCase "should work with correct credentials" $
        withAuthServer $ \port ->
          withChromiumContextWith
            defaultNewContextOptions {newContextHttpCredentials = Just validCredentials}
            $ \_ page -> do
              response <-
                goto
                  page
                  (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
                  defaultGotoOptions
              assertEqual "status" 200 =<< statusCode response
    , testCase "should fail with wrong credentials" $
        withAuthServer $ \port ->
          withChromiumContextWith
            defaultNewContextOptions
              { newContextHttpCredentials =
                  Just
                    HttpCredentials
                      { httpCredentialsUsername = HttpUsername "foo"
                      , httpCredentialsPassword = HttpPassword "bar"
                      , httpCredentialsOrigin = Nothing
                      , httpCredentialsSend = Nothing
                      }
              }
            $ \_ page -> do
              response <-
                goto
                  page
                  (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
                  defaultGotoOptions
              assertEqual "status" 401 =<< statusCode response
    ]

validCredentials :: HttpCredentials
validCredentials =
  HttpCredentials
    { httpCredentialsUsername = HttpUsername "user"
    , httpCredentialsPassword = HttpPassword "pass"
    , httpCredentialsOrigin = Nothing
    , httpCredentialsSend = Nothing
    }

statusCode :: Maybe Response -> IO Int
statusCode response =
  case response of
    Nothing ->
      assertFailure "missing response"
    Just value ->
      httpStatusToInt <$> responseStatus value

withAuthServer :: (Int -> IO a) -> IO a
withAuthServer =
  withApplication (pure (basicAuthApp "Basic dXNlcjpwYXNz"))

basicAuthApp :: BS8.ByteString -> Application
basicAuthApp expected req respond =
  case lookup hAuthorization (Wai.requestHeaders req) of
    Just value
      | value == expected ->
          respond
            ( responseLBS
                status200
                [(hContentType, BS8.pack "text/html")]
                (LBS8.pack "<html><title>ok</title></html>")
            )
    _ ->
      respond
            ( responseLBS
                status401
                [("WWW-Authenticate", BS8.pack "Basic realm=\"test\"")]
                LBS8.empty
            )

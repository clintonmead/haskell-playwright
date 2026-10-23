module Playwright.BrowserContextCredentialsSpec
  ( tests
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe
  ( fromMaybe
  )
import Data.Text
  ( Text
  , pack
  )
import Network.HTTP.Types
  ( hAuthorization
  , hContentType
  , status200
  , status401
  )
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , staticApp
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
  , evaluateText
  , goto
  , reload
  , responseStatus
  , setHTTPCredentials
  , urlFromText
  , Playwright
  )
import Playwright.Harness
  ( testAssetsDir
  , withChromiumContextWith
  , assertEqual
  , assertFailure
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
    , testCase "should return resource body" $ do
        assets <- testAssetsDir
        withApplication (pure (authAssetsApp assets "Basic dXNlcjpwYXNz")) $ \port ->
          withChromiumContextWith
            defaultNewContextOptions {newContextHttpCredentials = Just validCredentials}
            $ \_ page -> do
              response <-
                goto
                  page
                  (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/playground.html"))
                  defaultGotoOptions
              assertEqual "status" 200 =<< statusCode response
              assertEqual "title" "Playground" =<< evaluateText page "() => document.title"
    , testCase "should fail with correct credentials and mismatching scheme" $
        withAuthServer $ \port ->
          withChromiumContextWith
            defaultNewContextOptions
              {newContextHttpCredentials = Just (credentialsWithOrigin ("https://127.0.0.1:" <> pack (show port)))}
            $ \_ page -> do
              response <-
                goto page (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html")) defaultGotoOptions
              assertEqual "status" 401 =<< statusCode response
    , testCase "should fail with correct credentials and mismatching hostname" $
        withAuthServer $ \port ->
          withChromiumContextWith
            defaultNewContextOptions
              {newContextHttpCredentials = Just (credentialsWithOrigin ("http://mismatching-hostname:" <> pack (show port)))}
            $ \_ page -> do
              response <-
                goto page (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html")) defaultGotoOptions
              assertEqual "status" 401 =<< statusCode response
    , testCase "should fail with correct credentials and mismatching port" $
        withAuthServer $ \port ->
          withChromiumContextWith
            defaultNewContextOptions
              {newContextHttpCredentials = Just (credentialsWithOrigin ("http://127.0.0.1:" <> pack (show (port + 1))))}
            $ \_ page -> do
              response <-
                goto page (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html")) defaultGotoOptions
              assertEqual "status" 401 =<< statusCode response
    , testCase "should not override Authorization header set by the page" $
        withApplication (pure (echoAuthApp "Basic dXNlcjpwYXNz")) $ \port ->
          withChromiumContextWith
            defaultNewContextOptions {newContextHttpCredentials = Just validCredentials}
            $ \_ page -> do
              response <-
                goto
                  page
                  (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
                  defaultGotoOptions
              assertEqual "status" 200 =<< statusCode response
              assertEqual "page header" "Bearer my-own-app-token"
                =<< evaluateText
                  page
                  "async () => { const response = await fetch('/echo-auth', { headers: { 'Authorization': 'Bearer my-own-app-token' } }); return await response.text(); }"
    , testCase "should work with correct credentials and matching origin" $
        withAuthServer $ \port ->
          withChromiumContextWith
            defaultNewContextOptions
              {newContextHttpCredentials = Just (credentialsWithOrigin ("http://127.0.0.1:" <> pack (show port)))}
            $ \_ page -> do
              response <-
                goto page (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html")) defaultGotoOptions
              assertEqual "status" 200 =<< statusCode response
    , testCase "should work with correct credentials and matching origin case insensitive" $
        withAuthServer $ \port ->
          withChromiumContextWith
            defaultNewContextOptions
              {newContextHttpCredentials = Just (credentialsWithOrigin ("HTTP://127.0.0.1:" <> pack (show port)))}
            $ \_ page -> do
              response <-
                goto page (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html")) defaultGotoOptions
              assertEqual "status" 200 =<< statusCode response
    ]

-- | Answers /echo-auth with the Authorization header the request sent, and
-- applies the basic check everywhere else.
echoAuthApp :: BS8.ByteString -> Application
echoAuthApp expected req respond =
  case Wai.pathInfo req of
    ["echo-auth"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, BS8.pack "text/plain")]
            (LBS8.fromStrict (fromMaybe (BS8.pack "<none>") (lookup hAuthorization (Wai.requestHeaders req))))
        )
    _ ->
      basicAuthApp expected req respond

validCredentials :: HttpCredentials
validCredentials =
  HttpCredentials
    { httpCredentialsUsername = HttpUsername "user"
    , httpCredentialsPassword = HttpPassword "pass"
    , httpCredentialsOrigin = Nothing
    , httpCredentialsSend = Nothing
    }

statusCode :: Maybe Response -> Playwright Int
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

-- | Checks the Authorization header, then serves the file from the assets
-- directory, or answers 401 without the expected credentials.
authAssetsApp :: FilePath -> BS8.ByteString -> Application
authAssetsApp assets expected req respond =
  case lookup hAuthorization (Wai.requestHeaders req) of
    Just value
      | value == expected ->
          staticApp (defaultFileServerSettings assets) req respond
    _ ->
      respond
        ( responseLBS
            status401
            [("WWW-Authenticate", BS8.pack "Basic realm=\"test\"")]
            LBS8.empty
        )

-- | The valid credentials, limited to the origin given.
credentialsWithOrigin :: Text -> HttpCredentials
credentialsWithOrigin origin =
  validCredentials {httpCredentialsOrigin = Just (urlFromText origin)}

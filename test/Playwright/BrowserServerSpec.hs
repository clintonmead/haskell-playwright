module Playwright.BrowserServerSpec
  ( tests
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Aeson
  ( Value (Object, String)
  , eitherDecodeFileStrict
  )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as Text
import Playwright
  ( Playwright
  , BrowserServer (browserServerEndpoint)
  , BrowserServerOptions (browserServerHost, browserServerMetadata, browserServerPort, browserServerWorkspaceDir, browserServerWsPath)
  , bindBrowserWith
  , defaultBrowserServerOptions
  , unbindBrowser
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , withChromiumBrowser
  )
import System.Directory
  ( doesPathExist
  , listDirectory
  )
import System.Environment
  ( setEnv
  , unsetEnv
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Exception
  ( finally
  )
import UnliftIO.Temporary
  ( withSystemTempDirectory
  )

-- Skipped upstream cases (tests/library/browser-server.spec.ts):
--   "should start and stop pipe server": needs BrowserType.connect, which is
--     client-side in upstream (LocalUtils.connect with a second Connection).
--     Haskell has no connect yet.
--   "should start ws server with host/port and produce well-formed endpoint":
--     only the bind and endpoint-shape half is ported below. The
--     browserType.connect half is skipped for the same reason as above.

tests :: TestTree
tests =
  testGroup
    "browser-server"
    [ testCase "should write descriptor on start and remove on stop" $
        withSystemTempDirectory "pw-registry" $ \registryDir -> do
          setEnv "PWTEST_SERVER_REGISTRY" registryDir
          withChromiumBrowser
            ( \browser -> do
                serverInfo <-
                  bindBrowserWith
                    browser
                    "my-title"
                    defaultBrowserServerOptions {browserServerWsPath = Just "test"}
                let
                  endpoint = browserServerEndpoint serverInfo
                files <- liftIO (listDirectory registryDir)
                assertEqual "one descriptor file" 1 (length files)
                descriptor <- liftIO (readDescriptor (registryDir <> "/" <> take1 files))
                assertField "title" (String "my-title") descriptor
                assertHasField "playwrightVersion" descriptor
                assertHasField "playwrightLib" descriptor
                assertHasField "browser" descriptor
                assertField "endpoint" (String endpoint) descriptor
                assertEndpointIsPath endpoint
                unbindBrowser browser
                remaining <- liftIO (listDirectory registryDir)
                assertEqual "descriptor removed after unbind" [] remaining
                endpointGone <- liftIO (doesPathExist (Text.unpack endpoint))
                assertBool "pipe endpoint removed after unbind" (not endpointGone)
            )
            `finally` unsetEnv "PWTEST_SERVER_REGISTRY"
    , testCase "should start ws server with host/port and produce well-formed endpoint" $
        withChromiumBrowser $ \browser -> do
          serverInfo <-
            bindBrowserWith
              browser
              "default"
              defaultBrowserServerOptions
                { browserServerHost = Just "localhost"
                , browserServerPort = Just 0
                , browserServerWsPath = Nothing
                , browserServerWorkspaceDir = Nothing
                , browserServerMetadata = Nothing
                }
          assertWsEndpointShape (browserServerEndpoint serverInfo)
          unbindBrowser browser
    ]
  where
    take1 :: [FilePath] -> FilePath
    take1 (f : _) = f
    take1 [] = ""

    readDescriptor :: FilePath -> IO Value
    readDescriptor path = do
      decoded <- eitherDecodeFileStrict path
      either (\err -> fail ("bad descriptor json: " <> err)) pure decoded

    assertField :: String -> Value -> Value -> Playwright ()
    assertField key expected descriptor =
      assertEqual ("descriptor " <> key) (Just expected) (lookupField key descriptor)

    assertHasField :: String -> Value -> Playwright ()
    assertHasField key descriptor =
      assertBool ("descriptor has " <> key) (lookupField key descriptor /= Nothing)

    assertEndpointIsPath :: Text.Text -> Playwright ()
    assertEndpointIsPath endpoint =
      assertBool "pipe endpoint is a path" (not (Text.isPrefixOf "ws://" endpoint))

    assertWsEndpointShape :: Text.Text -> Playwright ()
    assertWsEndpointShape endpoint = do
      let
        afterScheme = maybe "" id (Text.stripPrefix "ws://" endpoint)
        (hostPart, afterHost) = Text.breakOn "/" afterScheme
        guidPart = Text.drop 1 afterHost
        hostOk =
          any
            (\h -> hostPart == h || Text.isPrefixOf (h <> ":") hostPart)
            ["127.0.0.1", "[::1]"]
      assertBool
        ("ws endpoint shape: " <> Text.unpack endpoint)
        (hostOk && not (Text.null guidPart) && Text.all isHexDigit guidPart)

    isHexDigit :: Char -> Bool
    isHexDigit c = c `elem` ("0123456789abcdef" :: String)

    lookupField :: String -> Value -> Maybe Value
    lookupField key (Object o) = KeyMap.lookup (Key.fromString key) o
    lookupField _ _ = Nothing

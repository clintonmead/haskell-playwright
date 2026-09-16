module Playwright.DisableWebSecuritySpec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Network.HTTP.Types
  ( hContentType
  , status200
  , status404
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
import Playwright
  ( AriaRole (AriaRole)
  , BrowserContext
  , GetByRoleOptions (getByRoleName)
  , LaunchOptions (launchOptionsArgs)
  , Page
  , Playwright
  , UrlGlob (UrlGlob)
  , addContextInitScript
  , click
  , defaultGetByRoleOptions
  , defaultGotoOptions
  , defaultLaunchOptions
  , defaultNewContextOptions
  , evaluateNumber
  , getByRole
  , goto
  , urlFromText
  , waitForPopup
  , waitForURL
  )
import Playwright.Harness
  ( withChromiumContextLaunchWith
  , withLocalApp
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
    "disable-web-security"
    [ testCase "test utility world in popup w/ --disable-web-security" $
        withDisabledWebSecurity $ \_ctx page ->
          withLocalApp popupApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/main.html")) defaultGotoOptions
            withAsync (waitForPopup page) $ \pending -> do
              click
                ( getByRole
                    page
                    (AriaRole "link")
                    defaultGetByRoleOptions {getByRoleName = Just "Click me"}
                )
              popup <- wait pending
              waitForURL popup (UrlGlob "**/target.html")
    , testCase "test init script w/ --disable-web-security" $
        withDisabledWebSecurity $ \ctx page ->
          withLocalApp popupApp $ \prefix -> do
            _ <- addContextInitScript ctx "window.injected = 123"
            _ <- goto page (urlFromText (prefix <> "/main.html")) defaultGotoOptions
            withAsync (waitForPopup page) $ \pending -> do
              click
                ( getByRole
                    page
                    (AriaRole "link")
                    defaultGetByRoleOptions {getByRoleName = Just "Click me"}
                )
              popup <- wait pending
              waitForURL popup (UrlGlob "**/target.html")
              assertEqual "injected" (123 :: Double) =<< evaluateNumber popup "() => window.injected"
    ]

withDisabledWebSecurity :: (BrowserContext -> Page -> Playwright a) -> IO a
withDisabledWebSecurity =
  withChromiumContextLaunchWith
    defaultLaunchOptions {launchOptionsArgs = Just ["--disable-web-security"]}
    defaultNewContextOptions

popupApp :: Application
popupApp request respond =
  case pathInfo request of
    ["main.html"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/html")]
            "<a href=\"/target.html\" target=\"_blank\">Click me</a>"
        )
    ["target.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] "<html></html>")
    _ ->
      respond (responseLBS status404 [] LBS8.empty)

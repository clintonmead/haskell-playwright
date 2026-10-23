module Playwright.PermissionsSpec
  ( tests
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text
  ( Text
  , pack
  , replace
  , unpack
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , pathInfo
  , requestHeaders
  , responseLBS
  )
import Playwright
  ( Browser
  , NewContextOptions (newContextPermissions)
  , Page
  , PermissionName (PermissionName)
  , Playwright
  , chromium
  , clearPermissions
  , contextBrowser
  , defaultGotoOptions
  , defaultLaunchOptions
  , defaultNewContextOptions
  , evaluateOnFrameText
  , evaluateText
  , evaluateTextWith
  , frames
  , goto
  , grantPermissions
  , grantPermissionsFor
  , newContext
  , newPage
  , pageContext
  , runPlaywright
  , textArgument
  , urlFromText
  , withBrowser
  )
import Playwright.Harness
  ( assertEqual
  , assertProtocolContains
  , withAssets
  , withAssetsPrefix
  , withAssetsWith
  , withChromiumContext
  , withLocalApp
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
    "permissions"
    [ testCase "should be prompt by default" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "state" "prompt" =<< permissionState page "geolocation"
    , testCase "should deny permission when not listed" $
        withAssets $ \prefix page -> do
          let
            empty = urlFromText (prefix <> "/empty.html")
          _ <- goto page empty defaultGotoOptions
          grantPermissionsFor (pageContext page) [] empty
          assertEqual "state" "denied" =<< permissionState page "geolocation"
    , testCase "should fail when bad permission is given" $
        withAssets $ \prefix page -> do
          let
            empty = urlFromText (prefix <> "/empty.html")
          _ <- goto page empty defaultGotoOptions
          assertProtocolContains
            "Unknown permission: foo"
            (grantPermissionsFor (pageContext page) [PermissionName "foo"] empty)
    , testCase "should grant geolocation permission when origin is listed" $
        withAssets $ \prefix page -> do
          let
            empty = urlFromText (prefix <> "/empty.html")
          _ <- goto page empty defaultGotoOptions
          grantPermissionsFor (pageContext page) [PermissionName "geolocation"] empty
          assertEqual "state" "granted" =<< permissionState page "geolocation"
    , testCase "should prompt for geolocation permission when origin is not listed" $
        withAssets $ \prefix page -> do
          let
            empty = urlFromText (prefix <> "/empty.html")
          _ <- goto page empty defaultGotoOptions
          grantPermissionsFor (pageContext page) [PermissionName "geolocation"] empty
          _ <- goto page (urlFromText (otherOrigin prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "state" "prompt" =<< permissionState page "geolocation"
    , testCase "should grant geolocation permission when listed" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          grantPermissions (pageContext page) [PermissionName "geolocation"]
          assertEqual "state" "granted" =<< permissionState page "geolocation"
    , testCase "should accumulate when adding" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          grantPermissions (pageContext page) [PermissionName "geolocation"]
          grantPermissions (pageContext page) [PermissionName "notifications"]
          assertEqual "geo" "granted" =<< permissionState page "geolocation"
          assertEqual "notify" "granted" =<< permissionState page "notifications"
    , testCase "should clear permissions" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          grantPermissions (pageContext page) [PermissionName "geolocation"]
          clearPermissions (pageContext page)
          grantPermissions (pageContext page) [PermissionName "notifications"]
          geo <- permissionState page "geolocation"
          assertEqual "geo not granted" False (geo == "granted")
          assertEqual "notify" "granted" =<< permissionState page "notifications"
    , testCase "should reset permissions" $
        withAssets $ \prefix page -> do
          let
            empty = urlFromText (prefix <> "/empty.html")
          _ <- goto page empty defaultGotoOptions
          grantPermissionsFor (pageContext page) [PermissionName "geolocation"] empty
          assertEqual "granted" "granted" =<< permissionState page "geolocation"
          clearPermissions (pageContext page)
          assertEqual "prompt" "prompt" =<< permissionState page "geolocation"
    , testCase "should trigger permission onchange" $
        withAssets $ \prefix page -> do
          let
            empty = urlFromText (prefix <> "/empty.html")
          _ <- goto page empty defaultGotoOptions
          _ <- evaluateText page onchangeSetupScript
          assertEqual "prompt" "[\"prompt\"]" =<< recordedEvents page
          grantPermissionsFor (pageContext page) [] empty
          assertEqual "denied" "[\"prompt\",\"denied\"]" =<< recordedEvents page
          grantPermissionsFor (pageContext page) [PermissionName "geolocation"] empty
          assertEqual "granted" "[\"prompt\",\"denied\",\"granted\"]" =<< recordedEvents page
          clearPermissions (pageContext page)
          assertEqual "cleared" "[\"prompt\",\"denied\",\"granted\",\"prompt\"]" =<< recordedEvents page
    , testCase "should support clipboard read" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "prompt" "prompt" =<< permissionState page "clipboard-read"
          grantPermissions (pageContext page) [PermissionName "clipboard-read"]
          assertEqual "granted" "granted" =<< permissionState page "clipboard-read"
          grantPermissions (pageContext page) [PermissionName "clipboard-write"]
          _ <- evaluateText page "() => navigator.clipboard.writeText('test content').then(() => 'ok')"
          assertEqual "read back" "test content" =<< evaluateText page "() => navigator.clipboard.readText()"
    , testCase "should isolate the headless clipboard from the operating system" $
        withAssetsPrefix $ \prefix -> runPlaywright $ do
          browserType <- chromium
          withBrowser browserType defaultLaunchOptions $ \browser1 ->
            withBrowser browserType defaultLaunchOptions $ \browser2 -> do
              page1 <- clipboardPage browser1 prefix
              page2 <- clipboardPage browser2 prefix
              _ <- evaluateText page1 "() => navigator.clipboard.writeText('first').then(() => 'ok')"
              assertEqual "first" "first" =<< evaluateText page1 "() => navigator.clipboard.readText()"
              _ <- evaluateText page2 "() => navigator.clipboard.writeText('second').then(() => 'ok')"
              assertEqual "second" "second" =<< evaluateText page2 "() => navigator.clipboard.readText()"
              assertEqual "first again" "first" =<< evaluateText page1 "() => navigator.clipboard.readText()"
    , testCase "should be able to use the local-fonts API" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "prompt" "prompt" =<< permissionState page "local-fonts"
          grantPermissions (pageContext page) [PermissionName "local-fonts"]
          assertEqual "granted" "granted" =<< permissionState page "local-fonts"
          assertEqual "fonts" "true" =<< evaluateText page "() => window.queryLocalFonts().then(fonts => String(fonts.length > 0))"
    , testCase "storage access" $
        withChromiumContext $ \ctx page ->
          withLocalApp storageAccessApp $ \prefix -> do
            grantPermissions ctx [PermissionName "storage-access"]
            _ <- goto page (urlFromText (otherOrigin prefix <> "/set-cookie.html")) defaultGotoOptions
            assertEqual "granted" "granted" =<< permissionState page "storage-access"
            _ <- goto page (urlFromText (prefix <> "/my-frame.html")) defaultGotoOptions
            frameList <- frames page
            case frameList of
              _ : frame : _ -> do
                assertEqual "frame state" "granted" =<< evaluateOnFrameText frame permissionQueryScript
                assertEqual "request access" "true" =<< evaluateOnFrameText frame "() => document.requestStorageAccess().then(() => 'true', () => 'false')"
                assertEqual "has access" "true" =<< evaluateOnFrameText frame "() => document.hasStorageAccess().then(String)"
              _ -> error "expected an iframe"
    , testCase "can request screen-wake-lock" $
        withChromiumContext $ \ctx page ->
          withLocalApp (responseApp "<div>Hello there!</div>") $ \prefix -> do
            grantPermissions ctx [PermissionName "screen-wake-lock"]
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            assertEqual "wake lock" "ok" =<< evaluateText page "() => navigator.wakeLock.request('screen').then(() => 'ok', () => 'failed')"
    , testCase "should grant permission when specified on context" $
        withAssetsWith
          defaultNewContextOptions {newContextPermissions = Just [PermissionName "geolocation"]}
          $ \prefix page -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            assertEqual "state" "granted" =<< permissionState page "geolocation"
    , testCase "should grant permission when creating context" $
        withAssetsWith
          defaultNewContextOptions {newContextPermissions = Just [PermissionName "geolocation"]}
          $ \prefix page -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            assertEqual "geolocation" "granted" =<< permissionState page "geolocation"
    , testCase "should grant notifications permission when listed" $
        withAssets $ \prefix page -> do
          let
            empty = urlFromText (prefix <> "/empty.html")
          _ <- goto page empty defaultGotoOptions
          grantPermissionsFor (pageContext page) [PermissionName "notifications"] empty
          assertEqual "state" "granted" =<< permissionState page "notifications"
    , testCase "should grant permission when listed for all domains" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          grantPermissions (pageContext page) [PermissionName "geolocation"]
          assertEqual "state" "granted" =<< permissionState page "geolocation"
    , testCase "should isolate permissions between browser contexts" $
        withAssets $ \prefix page -> do
          let
            empty = urlFromText (prefix <> "/empty.html")
          _ <- goto page empty defaultGotoOptions
          browser <- contextBrowser (pageContext page)
          other <- newContext browser
          otherPage <- newPage other
          _ <- goto otherPage empty defaultGotoOptions
          assertEqual "first prompt" "prompt" =<< permissionState page "geolocation"
          assertEqual "other prompt" "prompt" =<< permissionState otherPage "geolocation"
          grantPermissionsFor (pageContext page) [] empty
          assertEqual "first denied" "denied" =<< permissionState page "geolocation"
          grantPermissionsFor other [PermissionName "geolocation"] empty
          assertEqual "other granted" "granted" =<< permissionState otherPage "geolocation"
    ]

permissionState :: Page -> Text -> Playwright Text
permissionState page name =
  evaluateTextWith
    page
    "name => navigator.permissions.query({ name }).then(result => result.state)"
    (textArgument name)

responseApp :: Text -> Application
responseApp body _ respond =
  respond (responseLBS status200 [(hContentType, BS8.pack "text/html")] (LBS8.pack (unpack body)))

permissionQueryScript :: Text
permissionQueryScript =
  "() => navigator.permissions.query({ name: 'storage-access' }).then(result => result.state)"

onchangeSetupScript :: Text
onchangeSetupScript =
  "() => navigator.permissions.query({ name: 'geolocation' }).then(result => {"
    <> " window.events = [result.state];"
    <> " result.onchange = () => window.events.push(result.state);"
    <> " return 'ok'; })"

recordedEvents :: Page -> Playwright Text
recordedEvents page =
  evaluateText page "() => JSON.stringify(window.events)"

-- The same test server reached through a different host name is a different origin.
otherOrigin :: Text -> Text
otherOrigin =
  replace "127.0.0.1" "localhost"

clipboardPage :: Browser -> Text -> Playwright Page
clipboardPage browser prefix = do
  ctx <- newContext browser
  grantPermissions ctx [PermissionName "clipboard-read", PermissionName "clipboard-write"]
  page <- newPage ctx
  _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
  pure page

-- Serves the pages used by the storage-access case. The cross-site frame is
-- addressed through the "localhost" host name, which is a different site from
-- the "127.0.0.1" host the page is loaded from.
storageAccessApp :: Application
storageAccessApp request respond =
  case pathInfo request of
    ["set-cookie.html"] ->
      respond
        ( responseLBS
            status200
            [("Set-Cookie", BS8.pack "name=value; Path=/; SameSite=Strict; Secure")]
            LBS8.empty
        )
    ["my-frame.html"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, BS8.pack "text/html")]
            (LBS8.pack ("<iframe src=\"http://" <> unpack crossHost <> "/empty.html\"></iframe>"))
        )
    _ ->
      respond (responseLBS status200 [] LBS8.empty)
  where
    crossHost =
      replace "127.0.0.1" "localhost" (maybe "" (pack . BS8.unpack) (lookup "host" (requestHeaders request)))

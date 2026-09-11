module Playwright.PermissionsSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Playwright
  ( NewContextOptions (newContextPermissions)
  , Page
  , PermissionName (PermissionName)
  , clearPermissions
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluateTextWith
  , goto
  , grantPermissions
  , grantPermissionsFor
  , pageContext
  , textArgument
  , urlFromText
  )
import Playwright.Harness
  ( assertProtocolContains
  , withAssets
  , withAssetsWith
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
    , testCase "should grant permission when specified on context" $
        withAssetsWith
          defaultNewContextOptions {newContextPermissions = Just [PermissionName "geolocation"]}
          $ \prefix page -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            assertEqual "state" "granted" =<< permissionState page "geolocation"
    ]

permissionState :: Page -> Text -> IO Text
permissionState page name =
  evaluateTextWith
    page
    "name => navigator.permissions.query({ name }).then(result => result.state)"
    (textArgument name)

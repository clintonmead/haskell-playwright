module Playwright.GeolocationSpec
  ( tests
  ) where

import Playwright
  ( Geolocation (Geolocation, geolocationAccuracy, geolocationLatitude, geolocationLongitude)
  , PermissionName (PermissionName)
  , defaultGotoOptions
  , evaluateNumber
  , goto
  , grantPermissions
  , latitudeFromInt
  , longitudeFromInt
  , pageContext
  , setGeolocation
  , urlFromText
  )
import Playwright.Harness
  ( assertProtocolContains
  , withAssets
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
    "geolocation"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          let
            ctx = pageContext page
          grantPermissions ctx [PermissionName "geolocation"]
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setGeolocation
            ctx
            Geolocation
              { geolocationAccuracy = Nothing
              , geolocationLatitude = latitudeFromInt 10
              , geolocationLongitude = longitudeFromInt 10
              }
          assertEqual "latitude" (10 :: Double)
            =<< evaluateNumber
              page
              "() => new Promise(resolve => navigator.geolocation.getCurrentPosition(position => resolve(position.coords.latitude)))"
          assertEqual "longitude" (10 :: Double)
            =<< evaluateNumber
              page
              "() => new Promise(resolve => navigator.geolocation.getCurrentPosition(position => resolve(position.coords.longitude)))"
    , testCase "should throw when invalid longitude" $
        withAssets $ \_ page ->
          assertProtocolContains
            "geolocation.longitude: precondition -180 <= LONGITUDE <= 180 failed."
            ( setGeolocation
                (pageContext page)
                Geolocation
                  { geolocationAccuracy = Nothing
                  , geolocationLatitude = latitudeFromInt 10
                  , geolocationLongitude = longitudeFromInt 200
                  }
            )
    ]

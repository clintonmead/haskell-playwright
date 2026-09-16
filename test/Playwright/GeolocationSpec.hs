module Playwright.GeolocationSpec
  ( tests
  ) where

import Data.Text
  ( isInfixOf
  )
import Playwright
  ( ConsoleMessage (consoleMessageText)
  , Geolocation (Geolocation, geolocationAccuracy, geolocationLatitude, geolocationLongitude)
  , LoadState (LoadStateLoad)
  , NewContextOptions (newContextGeolocation, newContextPermissions)
  , PermissionName (PermissionName)
  , armPopup
  , closeContext
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluateNumber
  , evaluateWith
  , goto
  , grantPermissions
  , latitudeFromInt
  , longitudeFromInt
  , newContextWith
  , newPage
  , pageContext
  , setGeolocation
  , textArgument
  , urlFromText
  , waitForConsoleWhere
  , waitForLoadState
  )
import Playwright.Harness
  ( assertProtocolContains
  , withAssets
  , withAssetsPrefix
  , withChromiumBrowser
  , assertEqual
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( wait
  , withAsync
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
    , testCase "should isolate contexts" $
        isolateContextsTest
    , testCase "should use context options" $
        withAssetsPrefix $ \prefix ->
          withChromiumBrowser $ \browser -> do
            ctx <-
              newContextWith
                browser
                defaultNewContextOptions
                  { newContextGeolocation =
                      Just
                        Geolocation
                          { geolocationAccuracy = Nothing
                          , geolocationLatitude = latitudeFromInt 10
                          , geolocationLongitude = longitudeFromInt 10
                          }
                  , newContextPermissions = Just [PermissionName "geolocation"]
                  }
            page <- newPage ctx
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            assertEqual "latitude" (10 :: Double)
              =<< evaluateNumber
                page
                "() => new Promise(resolve => navigator.geolocation.getCurrentPosition(position => resolve(position.coords.latitude)))"
            assertEqual "longitude" (10 :: Double)
              =<< evaluateNumber
                page
                "() => new Promise(resolve => navigator.geolocation.getCurrentPosition(position => resolve(position.coords.longitude)))"
            closeContext ctx
    , testCase "watchPosition should be notified" $
        withAssets $ \prefix page -> do
          let
            ctx = pageContext page
          grantPermissions ctx [PermissionName "geolocation"]
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setGeolocation
            ctx
            Geolocation
              { geolocationAccuracy = Nothing
              , geolocationLatitude = latitudeFromInt 0
              , geolocationLongitude = longitudeFromInt 0
              }
          _ <-
            evaluateNumber
              page
              "() => { navigator.geolocation.watchPosition(pos => { console.log(`lat=${pos.coords.latitude} lng=${pos.coords.longitude}`); }, err => {}); return 0; }"
          withAsync
            (waitForConsoleWhere page (\message -> "lat=0 lng=10" `isInfixOf` consoleMessageText message))
            $ \pending -> do
              setGeolocation
                ctx
                Geolocation
                  { geolocationAccuracy = Nothing
                  , geolocationLatitude = latitudeFromInt 0
                  , geolocationLongitude = longitudeFromInt 10
                  }
              _ <- wait pending
              pure ()
          withAsync
            (waitForConsoleWhere page (\message -> "lat=20 lng=30" `isInfixOf` consoleMessageText message))
            $ \pending -> do
              setGeolocation
                ctx
                Geolocation
                  { geolocationAccuracy = Nothing
                  , geolocationLatitude = latitudeFromInt 20
                  , geolocationLongitude = longitudeFromInt 30
                  }
              _ <- wait pending
              pure ()
          withAsync
            (waitForConsoleWhere page (\message -> "lat=40 lng=50" `isInfixOf` consoleMessageText message))
            $ \pending -> do
              setGeolocation
                ctx
                Geolocation
                  { geolocationAccuracy = Nothing
                  , geolocationLatitude = latitudeFromInt 40
                  , geolocationLongitude = longitudeFromInt 50
                  }
              _ <- wait pending
              pure ()
    , testCase "should use context options for popup" $
        withAssets $ \prefix page -> do
          let
            ctx = pageContext page
          grantPermissions ctx [PermissionName "geolocation"]
          setGeolocation
            ctx
            Geolocation
              { geolocationAccuracy = Nothing
              , geolocationLatitude = latitudeFromInt 10
              , geolocationLongitude = longitudeFromInt 10
              }
          takePopup <- armPopup page
          withAsync takePopup $ \pending -> do
            _ <-
              evaluateWith
                page
                "url => { window._popup = window.open(url); }"
                (textArgument (prefix <> "/geolocation.html"))
            popup <- wait pending
            waitForLoadState popup LoadStateLoad
            assertEqual "latitude" (10 :: Double)
              =<< evaluateNumber popup "async () => { const g = await window.geolocationPromise; return g.latitude; }"
            assertEqual "longitude" (10 :: Double)
              =<< evaluateNumber popup "async () => { const g = await window.geolocationPromise; return g.longitude; }"
    ]

isolateContextsTest :: IO ()
isolateContextsTest =
  withAssetsPrefix $ \prefix ->
    withChromiumBrowser $ \browser -> do
      ctx <- newContextWith browser defaultNewContextOptions
      page <- newPage ctx
      grantPermissions ctx [PermissionName "geolocation"]
      setGeolocation
        ctx
        Geolocation
          { geolocationAccuracy = Nothing
          , geolocationLatitude = latitudeFromInt 10
          , geolocationLongitude = longitudeFromInt 10
          }
      _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
      ctx2 <-
        newContextWith
          browser
          defaultNewContextOptions
            { newContextGeolocation =
                Just
                  Geolocation
                    { geolocationAccuracy = Nothing
                    , geolocationLatitude = latitudeFromInt 20
                    , geolocationLongitude = longitudeFromInt 20
                    }
            , newContextPermissions = Just [PermissionName "geolocation"]
            }
      page2 <- newPage ctx2
      _ <- goto page2 (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
      assertEqual "first lat" (10 :: Double)
        =<< evaluateNumber
          page
          "() => new Promise(resolve => navigator.geolocation.getCurrentPosition(position => resolve(position.coords.latitude)))"
      assertEqual "first lng" (10 :: Double)
        =<< evaluateNumber
          page
          "() => new Promise(resolve => navigator.geolocation.getCurrentPosition(position => resolve(position.coords.longitude)))"
      assertEqual "second lat" (20 :: Double)
        =<< evaluateNumber
          page2
          "() => new Promise(resolve => navigator.geolocation.getCurrentPosition(position => resolve(position.coords.latitude)))"
      assertEqual "second lng" (20 :: Double)
        =<< evaluateNumber
          page2
          "() => new Promise(resolve => navigator.geolocation.getCurrentPosition(position => resolve(position.coords.longitude)))"
      closeContext ctx2
      closeContext ctx

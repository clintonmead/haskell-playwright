module Playwright.GeolocationSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Text
  ( Text
  , isInfixOf
  )
import UnliftIO.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
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
  , onConsole
  , Playwright
  , pageContext
  , setGeolocation
  , textArgument
  , urlFromText
  , waitForLoadState
  )
import Playwright.Harness
  ( assertProtocolContains
  , withAssets
  , withAssetsPrefix
  , withChromiumBrowser
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
    -- Skipped: "should throw with missing latitude". Geolocation requires a latitude, so the call does not type-check.
    -- "should throw with missing latitude"
    , testCase "should not modify passed default options object" $
        withChromiumBrowser $ \browser -> do
          let
            options = defaultNewContextOptions {newContextGeolocation = Just (geolocationAt 10 10)}
          ctx <- newContextWith browser options
          setGeolocation ctx (geolocationAt 20 20)
          assertEqual "options" (Just (geolocationAt 10 10)) (newContextGeolocation options)
          closeContext ctx
    -- Skipped: "should throw with missing longitude in default options". Geolocation requires a longitude, so the options do not type-check.
    -- "should throw with missing longitude in default options"
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
          -- Record every console message from the start, so an update that
          -- arrives before a waiter is registered is not missed.
          messages <- newIORef []
          onConsole page (\message -> modifyIORef' messages (<> [consoleMessageText message]))
          setGeolocation ctx (geolocationAt 0 0)
          _ <-
            evaluateNumber
              page
              "() => { navigator.geolocation.watchPosition(pos => { console.log(`lat=${pos.coords.latitude} lng=${pos.coords.longitude}`); }, err => {}); return 0; }"
          setGeolocation ctx (geolocationAt 0 10)
          awaitLogged messages "lat=0 lng=10"
          setGeolocation ctx (geolocationAt 20 30)
          awaitLogged messages "lat=20 lng=30"
          setGeolocation ctx (geolocationAt 40 50)
          awaitLogged messages "lat=40 lng=50"
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

-- | A geolocation fix at the given latitude and longitude.
geolocationAt :: Int -> Int -> Geolocation
geolocationAt latitude longitude =
  Geolocation
    { geolocationAccuracy = Nothing
    , geolocationLatitude = latitudeFromInt latitude
    , geolocationLongitude = longitudeFromInt longitude
    }

-- | Polls the console messages seen so far until one contains the text.
awaitLogged :: IORef [Text] -> Text -> Playwright ()
awaitLogged messages expected = go (200 :: Int)
  where
    go :: Int -> Playwright ()
    go attempts = do
      current <- readIORef messages
      if any (expected `isInfixOf`) current
        then pure ()
        else
          if attempts <= 0
            then assertFailure ("no console message containing " <> show expected)
            else do
              liftIO (threadDelay 50000)
              go (attempts - 1)

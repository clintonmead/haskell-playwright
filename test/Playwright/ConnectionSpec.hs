module Playwright.ConnectionSpec
  ( tests
  ) where

import UnliftIO.Async
  ( async
  , cancel
  )
import Data.Aeson
  ( Value
  , encode
  , object
  , (.=)
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString.Lazy
  ( toStrict
  )
import Data.Text
  ( Text
  )
import Playwright.BrowserType
  ( BrowserType (browserTypeName)
  )
import Playwright
  ( chromium
  )
import Playwright.Connection
  ( withConnection
  )
import Playwright.Instance
  ( initializePlaywright
  )
import Playwright.Monad
  ( runPlaywrightWith
  )
import Playwright.Transport
  ( Transport (closeTransport, recvPayload, sendPayload)
  )
import Playwright.Transport.Memory
  ( newConnectedPair
  )
import Playwright.Harness
  ( assertEqual
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
    "connection"
    [ testCase "initialize against a fake driver" $ do
        (client, server) <- newConnectedPair
        worker <- async (fakeDriver server)
        withConnection client $ \conn ->
          runPlaywrightWith conn $ do
            initializePlaywright
            browserType <- chromium
            assertEqual "chromium browser type" "chromium" (browserTypeName browserType)
        cancel worker
    ]

fakeDriver :: Transport -> IO ()
fakeDriver transport = do
  incoming <- recvPayload transport
  case incoming of
    Nothing ->
      closeTransport transport
    Just _ -> do
      sendPayload transport (encodeCreate "" "Playwright" "playwright" playwrightInit)
      sendPayload transport (encodeCreate "playwright" "BrowserType" "chromium" browserTypeInit)
      sendPayload transport (encodeCreate "playwright" "BrowserType" "firefox" browserTypeInit)
      sendPayload transport (encodeCreate "playwright" "BrowserType" "webkit" browserTypeInit)
      sendPayload transport (encodeCreate "playwright" "Android" "android" emptyObject)
      sendPayload transport (encodeCreate "playwright" "Electron" "electron" emptyObject)
      sendPayload transport (encodeResult 1)
      fakeDriver transport

playwrightInit :: Value
playwrightInit =
  object
    [ "chromium" .= object ["guid" .= ("chromium" :: Text)]
    , "firefox" .= object ["guid" .= ("firefox" :: Text)]
    , "webkit" .= object ["guid" .= ("webkit" :: Text)]
    , "android" .= object ["guid" .= ("android" :: Text)]
    , "electron" .= object ["guid" .= ("electron" :: Text)]
    ]

browserTypeInit :: Value
browserTypeInit =
  object
    [ "executablePath" .= ("" :: Text)
    , "name" .= ("chromium" :: Text)
    ]

emptyObject :: Value
emptyObject = object []

encodeCreate :: Text -> Text -> Text -> Value -> ByteString
encodeCreate parent typeName guid initializer =
  toStrict $
    encode $
      object
        [ "guid" .= parent
        , "method" .= ("__create__" :: Text)
        , "params"
            .= object
              [ "type" .= typeName
              , "guid" .= guid
              , "initializer" .= initializer
              ]
        ]

encodeResult :: Int -> ByteString
encodeResult reqId =
  toStrict $
    encode $
      object
        [ "id" .= reqId
        , "result" .= object ["playwright" .= object ["guid" .= ("playwright" :: Text)]]
        ]

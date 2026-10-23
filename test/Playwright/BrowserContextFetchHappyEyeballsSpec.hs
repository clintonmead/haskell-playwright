module Playwright.BrowserContextFetchHappyEyeballsSpec
  ( tests
  ) where

import Control.Concurrent
  ( forkIO
  , killThread
  , newEmptyMVar
  , putMVar
  , takeMVar
  )
import Control.Exception
  ( bracket
  )
import Data.String
  ( fromString
  )
import Data.Text
  ( pack
  )
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setBeforeMainLoop
  , setHost
  , setPort
  , withApplication
  )
import Playwright
  ( apiResponseOk
  , apiResponseUrl
  , disposeAPIRequestContext
  , fetchGet
  , newRequestContext
  , runPlaywright
  , urlFromText
  , urlToText
  )
import Playwright.GlobalFetchSupport
  ( simpleJsonApp
  )
import Playwright.Harness
  ( assertEqual
  )
import Test.Tasty
  ( localOption
  , mkTimeout
  , testGroup
  , TestTree
  )
import Test.Tasty.HUnit
  ( testCase
  )

tests :: TestTree
tests =
  localOption (mkTimeout 30000000) $
    testGroup
      "browsercontext-fetch-happy-eyeballs"
      [ testCase "should work with ip6 and port as the host" $
          ipv6PortHostTest
      ]

-- Upstream cases not ported here:
--   get should work: stubs DNS lookup through the in-process __testHookLookup option, a JS
--     function that runs inside the upstream Node driver. The Haskell client cannot send it.
--   get should work on request fixture: same __testHookLookup reason as above.
--   https post should work with ignoreHTTPSErrors option: same __testHookLookup reason, and
--     the harness has no TLS fixture for httpsServer.

-- The upstream case also skips when INSIDE_DOCKER is set; this suite does not read that variable.
ipv6PortHostTest :: IO ()
ipv6PortHostTest = do
  port <- withApplication (pure simpleJsonApp) pure
  ready <- newEmptyMVar
  let settings =
        setBeforeMainLoop (putMVar ready ()) $
          setHost (fromString "::1") $
            setPort port defaultSettings
  bracket (forkIO (runSettings settings simpleJsonApp)) killThread $ \_ -> do
    takeMVar ready
    runPlaywright $ do
      request <- newRequestContext
      let url = "http://[::1]:" <> pack (show port) <> "/simple.json"
      response <- fetchGet request (urlFromText url)
      assertEqual "url" url (urlToText (apiResponseUrl response))
      assertEqual "ok" True (apiResponseOk response)
      disposeAPIRequestContext request

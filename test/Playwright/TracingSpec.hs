module Playwright.TracingSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad
  ( void
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , responseLBS
  )
import UnliftIO.Async
  ( concurrently
  )
import UnliftIO.Exception
  ( SomeException
  , try
  )
import UnliftIO.Temporary
  ( withSystemTempDirectory
  )
import Playwright
  ( ClickOptions (clickOptionsTimeout)
  , Playwright
  , TracingStartOptions (TracingStartOptions)
  , acceptDialog
  , clickWith
  , defaultClickOptions
  , defaultGotoOptions
  , dismissDialog
  , evaluateBool
  , goto
  , milliseconds
  , onDialog
  , setContent
  , tracingStart
  , tracingStartWith
  , tracingStop
  , tracingStopTo
  , urlFromText
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertProtocolContains
  , locate
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
    "Tracing"
    [ testCase "should throw when starting with different options" $
        withChromiumContext $ \ctx _ -> do
          tracingStartWith ctx (TracingStartOptions Nothing (Just True) (Just True) Nothing)
          assertProtocolContains "Tracing has been already started" $
            tracingStartWith ctx (TracingStartOptions Nothing (Just False) (Just False) Nothing)
    , testCase "should throw when stopping without start" $
        withSystemTempDirectory "tracing" $ \dir ->
          withChromiumContext $ \ctx _ ->
            assertProtocolContains "Must start tracing before stopping" $
              tracingStopTo ctx (dir <> "/trace.zip")
    , testCase "should not throw when stopping without start but not exporting" $
        withChromiumContext $ \ctx _ ->
          tracingStop ctx
    , testCase "should not stall on dialogs" $
        withChromiumContext $ \ctx page -> do
          tracingStartWith ctx (TracingStartOptions Nothing (Just True) (Just True) Nothing)
          void (goto page (urlFromText "about:blank") defaultGotoOptions)
          onDialog page acceptDialog
          assertEqual "confirm accepted" True =<< evaluateBool page "() => confirm('are you sure')"
          tracingStop ctx
    , testCase "should not hang for clicks that open dialogs" $
        withChromiumContext $ \ctx page -> do
          tracingStartWith ctx (TracingStartOptions Nothing (Just True) (Just True) Nothing)
          onDialog page dismissDialog
          setContent page "<div onclick='window.alert(123)'>Click me</div>"
          _ <- try (clickWith (locate page "div") defaultClickOptions {clickOptionsTimeout = milliseconds 3500}) :: Playwright (Either SomeException ())
          tracingStop ctx
    , testCase "should export trace concurrently to second navigation" $
        withSystemTempDirectory "tracing" $ \dir ->
          withChromiumContext $ \ctx page ->
            withLocalApp gridApp $ \prefix -> do
              let
                url = urlFromText (prefix <> "/grid.html")
                path = dir <> "/trace.zip"
              mapM_
                ( \timeoutMs -> do
                    tracingStartWith ctx (TracingStartOptions Nothing (Just True) (Just True) Nothing)
                    void (goto page url defaultGotoOptions)
                    void $
                      concurrently
                        (goto page url defaultGotoOptions)
                        ( do
                            liftIO (threadDelay (timeoutMs * 1000))
                            tracingStopTo ctx path
                        )
                )
                [0, 20 .. 180 :: Int]
    , testCase "should overwrite existing file" $
        withSystemTempDirectory "tracing" $ \dir ->
          withChromiumContext $ \ctx page -> do
            let
              path = dir <> "/trace1.zip"
              options = TracingStartOptions Nothing (Just True) (Just True) Nothing
            tracingStartWith ctx options
            void (goto page (urlFromText "about:blank") defaultGotoOptions)
            setContent page "<button>Click</button>"
            clickWith (locate page "\"Click\"") defaultClickOptions
            tracingStopTo ctx path
            first <- liftIO (zipEntryNames <$> BS.readFile path)
            assertBool "first archive has action screenshots" (anyPrefix "screenshots/" first)
            tracingStartWith ctx options
            tracingStopTo ctx path
            second <- liftIO (zipEntryNames <$> BS.readFile path)
            assertBool "second archive has no action screenshots" (not (anyPrefix "screenshots/" second))
    , testCase "should recover tracing after a failed stop" $
        withSystemTempDirectory "tracing" $ \dir ->
          withChromiumContext $ \ctx page -> do
            let
              blocker = dir <> "/blocker"
            liftIO (BS.writeFile blocker BS.empty)
            tracingStart ctx
            _ <- try (tracingStopTo ctx (blocker <> "/trace1.zip")) :: Playwright (Either SomeException ())
            tracingStart ctx
            void (goto page (urlFromText "about:blank") defaultGotoOptions)
            tracingStopTo ctx (dir <> "/trace2.zip")
            entries <- liftIO (zipEntryNames <$> BS.readFile (dir <> "/trace2.zip"))
            assertBool "trace archive has a .trace entry" (any (BS.isSuffixOf ".trace") entries)
    ]

anyPrefix :: ByteString -> [ByteString] -> Bool
anyPrefix prefix = any (BS.isPrefixOf prefix)

-- | Names of the entries listed in a zip archive's central directory.
-- Entry names are stored uncompressed there, so no inflation is needed.
zipEntryNames :: ByteString -> [ByteString]
zipEntryNames = centralEntries

centralSignature :: ByteString
centralSignature = BS.pack [0x50, 0x4b, 0x01, 0x02]

centralEntries :: ByteString -> [ByteString]
centralEntries rest =
  let
    (_, found) = BS.breakSubstring centralSignature rest
  in
    if BS.length found < 46
      then []
      else
        let
          nameLen = le16 found 28
          extraLen = le16 found 30
          commentLen = le16 found 32
          name = BS.take nameLen (BS.drop 46 found)
        in
          name : centralEntries (BS.drop (46 + nameLen + extraLen + commentLen) found)

le16 :: ByteString -> Int -> Int
le16 b i =
  fromIntegral (BS.index b i) + 256 * fromIntegral (BS.index b (i + 1))

gridApp :: Application
gridApp _ respond =
  respond (responseLBS status200 [(hContentType, "text/html")] (LBS.fromStrict "<html><body>grid</body></html>"))

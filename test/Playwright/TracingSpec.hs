module Playwright.TracingSpec
  ( tests
  ) where

import Codec.Compression.Zlib.Raw qualified as Raw
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
  , Point (Point)
  , TracingStartOptions (TracingStartOptions)
  , acceptDialog
  , clickWith
  , closePage
  , cssPixelFromInt
  , defaultClickOptions
  , defaultGotoOptions
  , defaultScreenshotOptions
  , dismissDialog
  , evaluateBool
  , goto
  , keyboardInsertText
  , milliseconds
  , mouseDblclick
  , mouseMove
  , onDialog
  , screenshot
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
    , testCase "should not collect snapshots by default" $
        withSystemTempDirectory "tracing" $ \dir ->
          withChromiumContext $ \ctx page -> do
            let
              path = dir <> "/trace.zip"
            tracingStart ctx
            void (goto page (urlFromText "about:blank") defaultGotoOptions)
            setContent page "<button>Click</button>"
            clickWith (locate page "\"Click\"") defaultClickOptions
            closePage page
            tracingStopTo ctx path
            archive <- liftIO (BS.readFile path)
            assertBool "no frame snapshots" (not (traceContains archive (eventType "frame-snapshot")))
            assertBool "no resource snapshots" (not (traceContains archive (eventType "resource-snapshot")))
    , testCase "should not collect action screenshots and aria snapshots by default" $
        withSystemTempDirectory "tracing" $ \dir ->
          withChromiumContext $ \ctx page -> do
            let
              path = dir <> "/trace.zip"
            tracingStartWith ctx (TracingStartOptions Nothing Nothing (Just True) Nothing)
            void (goto page (urlFromText "about:blank") defaultGotoOptions)
            setContent page "<button>Click</button>"
            clickWith (locate page "\"Click\"") defaultClickOptions
            tracingStopTo ctx path
            archive <- liftIO (BS.readFile path)
            assertBool "no screenshot events" (not (traceContains archive (eventType "screenshot")))
            assertBool "no aria snapshot events" (not (traceContains archive (eventType "aria-snapshot")))
    , testCase "should collect action screenshots" $
        withSystemTempDirectory "tracing" $ \dir ->
          withChromiumContext $ \ctx page -> do
            let
              path = dir <> "/trace.zip"
            tracingStartWith ctx (TracingStartOptions Nothing (Just True) Nothing Nothing)
            void (goto page (urlFromText "about:blank") defaultGotoOptions)
            setContent page "<button>Click</button>"
            clickWith (locate page "\"Click\"") defaultClickOptions
            tracingStopTo ctx path
            archive <- liftIO (BS.readFile path)
            let names = zipEntryNames archive
            assertBool "has screenshot events" (traceContains archive (eventType "screenshot"))
            assertBool "has before screenshot" (any (BS.isSuffixOf "-before.png") names)
            assertBool "has action screenshot" (any (BS.isSuffixOf "-action.png") names)
            assertBool "has after screenshot" (any (BS.isSuffixOf "-after.png") names)
    , testCase "should not include buffers in the trace" $
        withSystemTempDirectory "tracing" $ \dir ->
          withChromiumContext $ \ctx page -> do
            let
              path = dir <> "/trace.zip"
            tracingStartWith ctx (TracingStartOptions Nothing Nothing (Just True) Nothing)
            void (goto page (urlFromText "about:blank") defaultGotoOptions)
            void (screenshot page defaultScreenshotOptions)
            tracingStopTo ctx path
            archive <- liftIO (BS.readFile path)
            assertBool "screenshot result is a buffer placeholder" (traceContains archive "\"binary\":\"<Buffer>\"")
    , testCase "should collect trace with resources, but no js" $
        withSystemTempDirectory "tracing" $ \dir ->
          withChromiumContext $ \ctx page -> do
            let
              path = dir <> "/trace.zip"
            tracingStartWith ctx (TracingStartOptions Nothing (Just True) (Just True) Nothing)
            void (goto page (urlFromText "about:blank") defaultGotoOptions)
            setContent page "<button>Click</button>"
            clickWith (locate page "\"Click\"") defaultClickOptions
            mouseMove page (Point (cssPixelFromInt 20) (cssPixelFromInt 20))
            mouseDblclick page (Point (cssPixelFromInt 30) (cssPixelFromInt 30))
            keyboardInsertText page "abc"
            liftIO (threadDelay 2000000)
            closePage page
            tracingStopTo ctx path
            archive <- liftIO (BS.readFile path)
            assertBool "has frame snapshots" (traceContains archive (eventType "frame-snapshot"))
            assertBool "has screencast frames" (traceContains archive (eventType "screencast-frame"))
    ]

eventType :: ByteString -> ByteString
eventType name = "\"type\":\"" <> name <> "\""

-- | Whether any trace entry (a @.trace@ file of JSON lines) of the archive contains the text.
traceContains :: ByteString -> ByteString -> Bool
traceContains archive needle =
  any
    (\(name, body) -> ".trace" `BS.isSuffixOf` name && needle `BS.isInfixOf` body)
    (zipContents archive)

anyPrefix :: ByteString -> [ByteString] -> Bool
anyPrefix prefix = any (BS.isPrefixOf prefix)

-- | Names of the entries listed in a zip archive's central directory.
zipEntryNames :: ByteString -> [ByteString]
zipEntryNames archive = map zipEntryName (zipEntries archive)

-- | Uncompressed contents of every entry of a zip archive.
zipContents :: ByteString -> [(ByteString, ByteString)]
zipContents archive = [(zipEntryName entry, entryContents archive entry) | entry <- zipEntries archive]

-- | One record of a zip archive's central directory.
data ZipEntry = ZipEntry
  { zipEntryName :: ByteString
  , zipEntryMethod :: Int
  , zipEntryCompressedSize :: Int
  , zipEntryOffset :: Int
  , zipEntryRecordLength :: Int
  }

eocdSignature :: ByteString
eocdSignature = BS.pack [0x50, 0x4b, 0x05, 0x06]

centralSignature :: ByteString
centralSignature = BS.pack [0x50, 0x4b, 0x01, 0x02]

-- | Central directory records, found through the end-of-central-directory record.
zipEntries :: ByteString -> [ZipEntry]
zipEntries archive =
  let
    (_, eocd) = BS.breakSubstring eocdSignature archive
  in
    if BS.length eocd < 22
      then []
      else readRecords (BS.drop (le32 eocd 16) archive)

readRecords :: ByteString -> [ZipEntry]
readRecords rest
  | BS.take 4 rest /= centralSignature = []
  | BS.length rest < 46 = []
  | otherwise =
      let
        nameLen = le16 rest 28
        extraLen = le16 rest 30
        commentLen = le16 rest 32
        recordLen = 46 + nameLen + extraLen + commentLen
        entry =
          ZipEntry
            { zipEntryName = BS.take nameLen (BS.drop 46 rest)
            , zipEntryMethod = le16 rest 10
            , zipEntryCompressedSize = le32 rest 20
            , zipEntryOffset = le32 rest 42
            , zipEntryRecordLength = recordLen
            }
      in
        entry : readRecords (BS.drop recordLen rest)

-- | Stored (method 0) or deflated (method 8) contents, located through the local file header.
entryContents :: ByteString -> ZipEntry -> ByteString
entryContents archive entry =
  let
    local = BS.drop (zipEntryOffset entry) archive
    start = 30 + le16 local 26 + le16 local 28
    raw = BS.take (zipEntryCompressedSize entry) (BS.drop start local)
  in
    case zipEntryMethod entry of
      8 -> LBS.toStrict (Raw.decompress (LBS.fromStrict raw))
      _ -> raw

le16 :: ByteString -> Int -> Int
le16 b i =
  fromIntegral (BS.index b i) + 256 * fromIntegral (BS.index b (i + 1))

le32 :: ByteString -> Int -> Int
le32 b i =
  le16 b i + 65536 * le16 b (i + 2)

gridApp :: Application
gridApp _ respond =
  respond (responseLBS status200 [(hContentType, "text/html")] (LBS.fromStrict "<html><body>grid</body></html>"))

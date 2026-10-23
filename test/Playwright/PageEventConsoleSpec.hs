module Playwright.PageEventConsoleSpec
  ( tests
  ) where

import Data.Text
  ( Text
  , isInfixOf
  , isPrefixOf
  , isSuffixOf
  )
import Playwright
  ( ConsoleLocation (consoleLocationLineNumber, consoleLocationUrl)
  , ConsoleMessage (consoleMessageArgs, consoleMessageLocation, consoleMessageText, consoleMessageTimestamp, consoleMessageType)
  , ConsoleType (ConsoleDebug, ConsoleDir, ConsoleError, ConsoleInfo, ConsoleLog, ConsoleTimeEnd, ConsoleTrace, ConsoleWarning)
  , Page
  , Playwright
  , armConsole
  , evaluate
  , defaultGotoOptions
  , evaluateNumber
  , evaluateText
  , evaluateWith
  , goto
  , urlFromText
  , jsonValueNumber
  , jsonValueText
  , onConsole
  , textArgument
  , waitForPopup
  )
import Playwright.Harness
  ( withChromiumPage
  , assertBool
  , assertEqual
  , assertFailure
  , withAssets
  )
import UnliftIO.Async
  ( wait
  , withAsync
  )
import UnliftIO.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
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
    "page-event-console"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          takeMessage <- armConsole page
          _ <- evaluate page "() => console.log('hello', 5, { foo: 'bar' })"
          message <- takeMessage
          assertEqual "type" ConsoleLog (consoleMessageType message)
          assertEqual "text" "hello 5 {foo: bar}" (consoleMessageText message)
          case consoleMessageArgs message of
            firstArg : secondArg : _ -> do
              assertEqual "arg0" "hello" =<< jsonValueText firstArg
              assertEqual "arg1" (5 :: Double) =<< jsonValueNumber secondArg
            _ ->
              assertFailure "expected console args"
    , testCase "should emit same log twice" $
        withChromiumPage $ \page -> do
          messages <- collectConsole page
          _ <- evaluate page "() => { for (let i = 0; i < 2; ++i) console.log('hello'); }"
          assertEqual "texts" ["hello", "hello"] . map consoleMessageText =<< readMessages messages
    , testCase "should use text() for inspection" $
        withChromiumPage $ \page -> do
          messages <- collectConsole page
          _ <- evaluate page "() => console.log('Hello world')"
          assertEqual "text" ["Hello world"] . map consoleMessageText =<< readMessages messages
    , testCase "should work for different console API calls" $
        withChromiumPage $ \page -> do
          messages <- collectConsole page
          _ <-
            evaluate
              page
              "() => { console.time('calling console.time'); console.timeEnd('calling console.time'); console.trace('calling console.trace'); console.dir('calling console.dir'); console.warn('calling console.warn'); console.error('calling console.error'); console.info('calling console.info'); console.debug('calling console.debug'); console.log(Promise.resolve('should not wait until resolved!')); }"
          seen <- readMessages messages
          assertEqual
            "types"
            [ConsoleTimeEnd, ConsoleTrace, ConsoleDir, ConsoleWarning, ConsoleError, ConsoleInfo, ConsoleDebug, ConsoleLog]
            (map consoleMessageType seen)
          case seen of
            first : rest -> do
              assertBool "time" ("calling console.time" `isInfixOf` consoleMessageText first)
              assertEqual
                "texts"
                [ "calling console.trace"
                , "calling console.dir"
                , "calling console.warn"
                , "calling console.error"
                , "calling console.info"
                , "calling console.debug"
                , "Promise"
                ]
                (map consoleMessageText rest)
            [] ->
              assertFailure "expected console messages"
    , testCase "should not fail for window object" $
        withChromiumPage $ \page -> do
          messages <- collectConsole page
          _ <- evaluate page "() => console.error(window)"
          assertEqual "text" ["Window"] . map consoleMessageText =<< readMessages messages
    , testCase "should not fail for object that can't be coerced to a primitive" $
        withChromiumPage $ \page -> do
          messages <- collectConsole page
          _ <-
            evaluate
              page
              "() => console.log({ toString() { return {}; }, valueOf() { return {}; } })"
          texts <- map consoleMessageText <$> readMessages messages
          assertBool "object text" (any (\text -> "toString:" `isInfixOf` text && "valueOf:" `isInfixOf` text) texts)
    , testCase "should use object previews for arrays and objects" $
        withChromiumPage $ \page -> do
          messages <- collectConsole page
          _ <- evaluate page "() => console.log([1, 2, 3], { a: 1 }, window)"
          assertEqual "text" ["[1, 2, 3] {a: 1} Window"] . map consoleMessageText =<< readMessages messages
    , testCase "should use object previews for errors" $
        withChromiumPage $ \page -> do
          messages <- collectConsole page
          _ <- evaluate page "() => console.log(new Error('Exception'))"
          texts <- map consoleMessageText <$> readMessages messages
          assertBool "error text" (any (".evaluate" `isInfixOf`) texts)
    , testCase "should format the message correctly with time/timeLog/timeEnd" $
        withChromiumPage $ \page -> do
          messages <- collectConsole page
          _ <-
            evaluate
              page
              "async () => { console.time('foo time'); await new Promise(x => setTimeout(x, 100)); console.timeLog('foo time'); await new Promise(x => setTimeout(x, 100)); console.timeEnd('foo time'); }"
          seen <- readMessages messages
          assertEqual "count" 2 (length seen)
          assertEqual "last type" [ConsoleTimeEnd] (map consoleMessageType (drop 1 seen))
          mapM_ (assertTimeText . consoleMessageText) seen
    , testCase "should trigger correct Log" $
        withAssets $ \prefix page -> do
          messages <- collectConsole page
          _ <-
            evaluateWith
              page
              "async url => fetch(url).catch(e => {})"
              (textArgument (prefix <> "/empty.html"))
          seen <- readMessages messages
          case seen of
            message : _ -> do
              assertBool "text" (any (`isInfixOf` consoleMessageText message) ["Access-Control-Allow-Origin", "CORS"])
              assertEqual "type" ConsoleError (consoleMessageType message)
            [] ->
              assertFailure "expected a console message"
    , testCase "should have timestamp" $
        withChromiumPage $ \page -> do
          -- Generous slack: the browser's clock and Date.now() are read on the same machine.
          before <- evaluateNumber page "() => Date.now()"
          takeMessage <- armConsole page
          _ <- evaluate page "() => console.log('timestamp test')"
          message <- takeMessage
          after <- evaluateNumber page "() => Date.now()"
          assertBool "timestamp in range" (consoleMessageTimestamp message >= before - 100 && consoleMessageTimestamp message <= after + 100)
    , testCase "should have location for console API calls" $
        withAssets $ \prefix page -> do
          messages <- collectConsole page
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- goto page (urlFromText (prefix <> "/consolelog.html")) defaultGotoOptions
          seen <- readMessages messages
          case filter (("here:" `isPrefixOf`) . consoleMessageText) seen of
            message : _ -> do
              assertEqual "type" ConsoleLog (consoleMessageType message)
              let location = consoleMessageLocation message
              assertEqual "url" (prefix <> "/consolelog.html") (consoleLocationUrl location)
              assertEqual "line" 7 (consoleLocationLineNumber location)
            [] ->
              assertFailure "expected a console message starting with here:"
    , testCase "should have increasing timestamps" $
        withChromiumPage $ \page -> do
          messages <- collectConsole page
          _ <-
            evaluate
              page
              "() => { console.log('first'); console.log('second'); console.log('third'); }"
          seen <- readMessages messages
          assertEqual "count" 3 (length seen)
          let stamps = map consoleMessageTimestamp seen
          assertBool "non-decreasing" (and (zipWith (<=) stamps (drop 1 stamps)))
    , testCase "should not throw when there are console messages in detached iframes" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync (waitForPopup page) $ \popupPending -> do
            _ <- evaluate page (detachedIframeScript prefix)
            popup <- wait popupPending
            assertEqual "popup" "2" =<< evaluateText popup "() => String(1 + 1)"
    ]

-- | Opens a popup, logs from an iframe inside it, then removes the iframe.
detachedIframeScript :: Text -> Text
detachedIframeScript prefix =
  "async () => { const win = window.open(''); window['_popup'] = win; "
    <> "if (window.document.readyState !== 'complete') await new Promise(f => window.addEventListener('load', f)); "
    <> "win.document.body.innerHTML = `<iframe src='"
    <> prefix
    <> "/consolelog.html'></iframe>`; "
    <> "const frame = win.document.querySelector('iframe'); "
    <> "if (!frame.contentDocument || frame.contentDocument.readyState !== 'complete') "
    <> "await new Promise(f => frame.addEventListener('load', f)); "
    <> "frame.remove(); }"

-- | Collects every console message the page emits, in order. The handler runs
-- before the command that caused the messages returns, so one evaluate is
-- enough to read them back.
collectConsole :: Page -> Playwright (IORef [ConsoleMessage])
collectConsole page = do
  messages <- newIORef []
  onConsole page (\message -> modifyIORef' messages (<> [message]))
  pure messages

readMessages :: IORef [ConsoleMessage] -> Playwright [ConsoleMessage]
readMessages messages =
  readIORef messages

-- | "foo time: 102ms" style text from console.time and console.timeEnd.
assertTimeText :: Text -> Playwright ()
assertTimeText text =
  assertBool ("time text: " <> show text)
    ("foo time: " `isPrefixOf` text && "ms" `isSuffixOf` text)

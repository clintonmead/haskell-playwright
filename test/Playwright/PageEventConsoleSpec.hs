module Playwright.PageEventConsoleSpec
  ( tests
  ) where

import Data.Text
  ( Text
  , isInfixOf
  , isPrefixOf
  , isSuffixOf
  , pack
  )
import Playwright
  ( ConsoleFilter (ConsoleFilterAll)
  , ConsoleLocation (consoleLocationLineNumber, consoleLocationUrl)
  , ConsoleMessage (consoleMessageArgs, consoleMessageLocation, consoleMessagePage, consoleMessageText, consoleMessageTimestamp, consoleMessageType)
  , ConsoleType (ConsoleDebug, ConsoleDir, ConsoleError, ConsoleInfo, ConsoleLog, ConsoleTimeEnd, ConsoleTrace, ConsoleWarning)
  , PageError (pageErrorMessage)
  , Page
  , Playwright
  , armConsole
  , clearConsoleMessages
  , consoleMessages
  , consoleMessagesWith
  , pageErrors
  , pageErrorsWith
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
  , withLocalApp
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Maybe
  ( isJust
  )
import Data.Time.Clock.POSIX
  ( getPOSIXTime
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , rawPathInfo
  , responseLBS
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
    , testCase "do not update console count on unhandled rejections" $
        withChromiumPage $ \page -> do
          messages <- collectConsole page
          _ <-
            evaluate
              page
              "() => { const fail = async () => Promise.reject(new Error('error')); console.log('begin'); void fail(); void fail(); fail().catch(() => { console.log('end'); }); }"
          assertEqual "texts" ["begin", "end"] . map consoleMessageText =<< readMessages messages
    , testCase "should have timestamp in consoleMessages" $
        withChromiumPage $ \page -> do
          before <- subtract 100 <$> nowMs
          _ <- evaluate page "() => console.log('stored message')"
          after <- (+ 100) <$> nowMs
          messages <- consoleMessages page
          assertBool "at least one message" (not (null messages))
          lastMessage <- lastMessageOf messages
          assertEqual "text" "stored message" (consoleMessageText lastMessage)
          let stamp = consoleMessageTimestamp lastMessage
          assertBool ("timestamp " <> show stamp <> " in range") (stamp >= before && stamp <= after)
    , testCase "consoleMessages should work" $
        withChromiumPage $ \page -> do
          _ <-
            evaluate
              page
              "() => { for (let i = 0; i < 301; i++) console.log('message' + i); }"
          messages <- consoleMessages page
          let
            lastOnes = drop (length messages - 100) messages
            expectedTexts = [pack ("message" <> show i) | i <- [201 .. 300 :: Int]]
          assertBool "at least 100 messages" (length messages >= 100)
          assertEqual "last messages" expectedTexts (map consoleMessageText lastOnes)
          assertBool "all log" (all ((== ConsoleLog) . consoleMessageType) lastOnes)
          assertBool "page set" (all (isJust . consoleMessagePage) lastOnes)
    , testCase "clearConsoleMessages should work" $
        withChromiumPage $ \page -> do
          _ <- evaluate page "() => { console.log('message1'); console.log('message2'); }"
          texts <- map consoleMessageText <$> consoleMessages page
          assertBool "contains message1" ("message1" `elem` texts)
          assertBool "contains message2" ("message2" `elem` texts)
          clearConsoleMessages page
          assertEqual "cleared" [] . map consoleMessageText =<< consoleMessages page
          _ <- evaluate page "() => console.log('message3')"
          assertEqual "after clear" ["message3"] . map consoleMessageText =<< consoleMessages page
    , testCase "consoleMessages since-navigation filter should work" $
        withAssets $ \prefix page -> do
          _ <- evaluate page "() => console.log('before navigation')"
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- evaluate page "() => console.log('after navigation')"
          allTexts <- map consoleMessageText <$> consoleMessagesWith page ConsoleFilterAll
          assertBool "all has before" ("before navigation" `elem` allTexts)
          assertBool "all has after" ("after navigation" `elem` allTexts)
          -- since-navigation is the default
          sinceTexts <- map consoleMessageText <$> consoleMessages page
          assertBool "default excludes before" ("before navigation" `notElem` sinceTexts)
          assertBool "default has after" ("after navigation" `elem` sinceTexts)
    , testCase "pageErrors since-navigation filter should work" $
        withChromiumPage $ \page ->
          withLocalApp pageErrorApp $ \base -> do
            _ <- goto page (urlFromText (base <> "/page1")) defaultGotoOptions
            _ <- goto page (urlFromText (base <> "/page2")) defaultGotoOptions
            allMessages <- map pageErrorMessage <$> pageErrorsWith page ConsoleFilterAll
            assertBool "all has page1" ("page1 error" `elem` allMessages)
            assertBool "all has page2" ("page2 error" `elem` allMessages)
            -- since-navigation is the default
            sinceMessages <- map pageErrorMessage <$> pageErrors page
            assertBool "default excludes page1" ("page1 error" `notElem` sinceMessages)
            assertBool "default has page2" ("page2 error" `elem` sinceMessages)
    ]

-- | Serves a page that throws on load, naming the error after the request path.
pageErrorApp :: Application
pageErrorApp request respond =
  respond
    ( responseLBS
        status200
        [(hContentType, "text/html")]
        ( if rawPathInfo request == "/page1"
            then "<script>throw new Error('page1 error');</script>"
            else "<script>throw new Error('page2 error');</script>"
        )
    )

nowMs :: Playwright Double
nowMs = do
  now <- liftIO getPOSIXTime
  pure (realToFrac now * 1000)

lastMessageOf :: [ConsoleMessage] -> Playwright ConsoleMessage
lastMessageOf messages =
  case reverse messages of
    lastMessage : _ ->
      pure lastMessage
    [] ->
      assertFailure "expected a console message"

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

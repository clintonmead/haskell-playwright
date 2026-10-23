module Playwright.HarRecordSpec
  ( tests
  ) where

import Control.Monad
  ( void
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Aeson
  ( Value (Array, Number, Object, String)
  , eitherDecodeFileStrict'
  )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable
  ( toList
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Playwright
  ( Browser
  , NewContextOptions (newContextRecordHar)
  , Page
  , Playwright
  , RecordHar (RecordHar)
  , browserVersion
  , pageClickSelector
  , selectorFromText
  , closeContext
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluate
  , goto
  , newContextWith
  , newPage
  , setContent
  , urlFromText
  , LoadState (LoadStateLoad)
  , waitForLoadState
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , withAssetsPrefix
  , withChromiumBrowser
  )
import System.Directory
  ( createDirectoryIfMissing
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removePathForcibly
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Exception
  ( bracket
  )

tests :: TestTree
tests =
  testGroup
    "har-record"
    [ testCase "should have version and creator" $
        withTempDir "version" $ \dir ->
          withAssetsPrefix $ \prefix ->
            withChromiumBrowser $ \browser -> do
              log' <- recordLog browser dir (\page -> gotoUrl page (prefix <> "/empty.html"))
              assertEqual "version" (String "1.2") (field "version" log')
              let creator = field "creator" log'
              assertEqual "creator name" (String "Playwright") (field "name" creator)
              assertBool "creator version is set" (isNonEmptyString (field "version" creator))
    , testCase "should have browser" $
        withTempDir "browser" $ \dir ->
          withAssetsPrefix $ \prefix ->
            withChromiumBrowser $ \browser -> do
              log' <- recordLog browser dir (\page -> gotoUrl page (prefix <> "/empty.html"))
              let browserField = field "browser" log'
              assertEqual "browser name" (String "chromium") (field "name" browserField)
              expected <- browserVersion browser
              assertEqual "browser version" (String expected) (field "version" browserField)
    , testCase "should have pages" $
        withTempDir "pages" $ \dir ->
          withChromiumBrowser $ \browser -> do
            log' <- recordLog browser dir $ \page -> do
              gotoUrl page "data:text/html,<title>Hello</title>"
              waitForLoadState page LoadStateLoad
            let pageEntry = nth 0 (field "pages" log')
                timings = field "pageTimings" pageEntry
            assertEqual "page count" 1 (length (arrayItems (field "pages" log')))
            assertBool "page id is set" (isNonEmptyString (field "id" pageEntry))
            assertEqual "page title" (String "Hello") (field "title" pageEntry)
            assertBool "onContentLoad positive" (positiveNumber (field "onContentLoad" timings))
            assertBool "onLoad positive" (positiveNumber (field "onLoad" timings))
    , testCase "should include request" $
        withTempDir "request" $ \dir ->
          withAssetsPrefix $ \prefix ->
            withChromiumBrowser $ \browser -> do
              log' <- recordLog browser dir (\page -> gotoUrl page (prefix <> "/empty.html"))
              let entry = nth 0 (field "entries" log')
                  request = field "request" entry
              assertEqual "entry count" 1 (length (arrayItems (field "entries" log')))
              assertEqual "pageref" (field "id" (nth 0 (field "pages" log'))) (field "pageref" entry)
              assertEqual "url" (String (prefix <> "/empty.html")) (field "url" request)
              assertEqual "method" (String "GET") (field "method" request)
              assertEqual "httpVersion" (String "HTTP/1.1") (field "httpVersion" request)
              assertBool "headers" (length (arrayItems (field "headers" request)) > 1)
              assertBool "user-agent header" (hasHeader "user-agent" (field "headers" request))
              assertEqual "bodySize" (intValue 0) (field "bodySize" request)
    , testCase "should include response" $
        withTempDir "response" $ \dir ->
          withAssetsPrefix $ \prefix ->
            withChromiumBrowser $ \browser -> do
              log' <- recordLog browser dir (\page -> gotoUrl page (prefix <> "/empty.html"))
              let response = field "response" (nth 0 (field "entries" log'))
              assertEqual "status" (intValue 200) (field "status" response)
              assertEqual "statusText" (String "OK") (field "statusText" response)
              assertEqual "httpVersion" (String "HTTP/1.1") (field "httpVersion" response)
              assertBool "headers" (length (arrayItems (field "headers" response)) > 1)
              assertBool "content-type is html" (headerContains "content-type" "text/html" (field "headers" response))
    , testCase "should include query params" $
        withTempDir "query" $ \dir ->
          withAssetsPrefix $ \prefix ->
            withChromiumBrowser $ \browser -> do
              log' <- recordLog browser dir (\page -> gotoUrl page (prefix <> "/har.html?name=value"))
              let request = field "request" (nth 0 (field "entries" log'))
              assertEqual "queryString" [nameValue "name" "value"] (arrayItems (field "queryString" request))
    , testCase "should include postData" $
        withTempDir "postdata" $ \dir ->
          withAssetsPrefix $ \prefix ->
            withChromiumBrowser $ \browser -> do
              log' <- recordLog browser dir $ \page -> do
                gotoUrl page (prefix <> "/empty.html")
                void (evaluate page "() => fetch('./post', { method: 'POST', body: 'Hello' })")
              assertPostData "text/plain;charset=UTF-8" [] "Hello" (postDataOf log' 1)
    , testCase "should include form params" $
        withTempDir "form" $ \dir ->
          withAssetsPrefix $ \prefix ->
            withChromiumBrowser $ \browser -> do
              log' <- recordLog browser dir $ \page -> do
                gotoUrl page (prefix <> "/empty.html")
                setContent page "<form method='POST' action='/post'><input type='text' name='foo' value='bar'><input type='number' name='baz' value='123'><input type='submit'></form>"
                pageClickSelector page (selectorFromText "input[type=submit]")
              assertPostData
                "application/x-www-form-urlencoded"
                [nameValue "foo" "bar", nameValue "baz" "123"]
                "foo=bar&baz=123"
                (postDataOf log' 1)
    ]

-- | Records a HAR for the work done by the action, closes the context so the
-- HAR is written, and returns the parsed @log@ object.
recordLog :: Browser -> FilePath -> (Page -> Playwright ()) -> Playwright Value
recordLog browser dir action = do
  let harPath = dir <> "/test.har"
  ctx <- newContextWith browser defaultNewContextOptions {newContextRecordHar = Just (RecordHar harPath Nothing Nothing Nothing)}
  page <- newPage ctx
  action page
  closeContext ctx
  liftIO (readLog harPath)

readLog :: FilePath -> IO Value
readLog harPath = do
  outcome <- eitherDecodeFileStrict' harPath
  case outcome of
    Left err -> fail ("could not parse HAR: " <> err)
    Right root -> pure (field "log" root)

gotoUrl :: Page -> Text -> Playwright ()
gotoUrl page url =
  void (goto page (urlFromText url) defaultGotoOptions)

-- | Looks up a key in a JSON object. A missing key is a failed lookup.
field :: String -> Value -> Value
field key (Object o) =
  case KeyMap.lookup (Key.fromString key) o of
    Just found -> found
    Nothing -> error ("missing HAR field " <> key)
field key _ = error ("HAR field " <> key <> " looked up on a non-object")

arrayItems :: Value -> [Value]
arrayItems (Array xs) = toList xs
arrayItems _ = []

-- | The nth element (zero based) of a JSON array.
nth :: Int -> Value -> Value
nth n v = case drop n (arrayItems v) of
  (x : _) -> x
  [] -> error ("no HAR array element " <> show n)

-- | The post data of the nth entry (zero based).
postDataOf :: Value -> Int -> Value
postDataOf log' n = field "postData" (field "request" (nth n (field "entries" log')))

-- | Asserts the mime type, params and text of a request's post data.
assertPostData :: Text -> [Value] -> Text -> Value -> Playwright ()
assertPostData mime params text postData = do
  assertEqual "postData mimeType" (String mime) (field "mimeType" postData)
  assertEqual "postData params" params (arrayItems (field "params" postData))
  assertEqual "postData text" (String text) (field "text" postData)

isNonEmptyString :: Value -> Bool
isNonEmptyString (String s) = not (Text.null s)
isNonEmptyString _ = False

intValue :: Int -> Value
intValue = Number . fromIntegral

positiveNumber :: Value -> Bool
positiveNumber (Number n) = n > 0
positiveNumber _ = False

hasHeader :: Text -> Value -> Bool
hasHeader name headers =
  any (\h -> headerName h == name) (arrayItems headers)

headerName :: Value -> Text
headerName h = case field "name" h of
  String s -> Text.toLower s
  _ -> ""

headerContains :: Text -> Text -> Value -> Bool
headerContains name needle headers =
  any matches (arrayItems headers)
  where
    matches h = headerName h == name && needle `Text.isInfixOf` valueText h
    valueText h = case field "value" h of
      String s -> s
      _ -> ""

nameValue :: Text -> Text -> Value
nameValue name value =
  Object (KeyMap.fromList [(Key.fromString "name", String name), (Key.fromString "value", String value)])

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir name action = do
  root <- getTemporaryDirectory
  let dir = root <> "/pw-har-record-" <> name
  bracket
    (removePathForcibly dir >> createDirectoryIfMissing True dir >> pure dir)
    removeDirectoryRecursive
    action

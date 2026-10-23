module Playwright.BrowserContextCookiesThirdPartySpec
  ( tests
  ) where

import Control.Monad
  ( filterM
  , void
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List
  ( sort
  )
import Data.Maybe
  ( fromMaybe
  , isJust
  )
import Data.Text
  ( Text
  , pack
  )
import Network.HTTP.Types
  ( hContentType
  , hCookie
  , hReferer
  , status200
  , status404
  )
import Network.Wai
  ( Application
  , Request
  , rawPathInfo
  , requestHeaders
  , responseLBS
  , Response
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( BrowserContext
  , Browser
  , Cookie (cookieDomain, cookieExpires, cookieHttpOnly, cookieName, cookiePath, cookieSameSite, cookieSecure, cookieValue)
  , CookieDomain (CookieDomain)
  , CookieHttpOnly (CookieHttpOnly)
  , CookieName (CookieName)
  , CookiePath (CookiePath)
  , CookieSameSite (CookieSameSiteNone)
  , CookieSecure (CookieSecure)
  , CookieToSet (CookieToSet, cookieToSetDomain, cookieToSetExpires, cookieToSetHttpOnly, cookieToSetName, cookieToSetPath, cookieToSetSameSite, cookieToSetSecure, cookieToSetUrl, cookieToSetValue)
  , CookieValue (CookieValue)
  , Frame
  , NewContextOptions (newContextStorageState)
  , Page
  , Playwright
  , StorageState
  , addCookies
  , childFrames
  , closeContext
  , isDetached
  , contextBrowser
  , cookiesFor
  , cookies
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluateOnFrameText
  , evaluateText
  , goto
  , mainFrame
  , newContextWith
  , newPage
  , setContent
  , storageState
  , urlFromText
  )
import Playwright.Harness
  ( Recorded (recordedHeaders, recordedPath)
  , assertEqual
  , assertFailure
  , attachFrame
  , recordRequest
  , withChromiumContext
  , withPersistentContext
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Concurrent
  ( threadDelay
  )
import UnliftIO.Exception
  ( bracket
  , throwIO
  , tryAny
  )
import UnliftIO.IORef
  ( IORef
  , newIORef
  , readIORef
  )

-- | The two origins the tests use. The first is 'localhost', the second is
-- '127.0.0.1', so the two are cross-site. Both are served by one Warp server,
-- which is why they share a port.
data Origins = Origins
  { origin1 :: Text
  , origin2 :: Text
  }

-- | How '/set-cookie.html' answers.
data SetMode
  = -- | One cookie, not partitioned: 'top-level=value' or 'frame=value'.
    SetPlain
  | -- | 'top-level-partitioned' with the 'Partitioned' attribute, plus
    -- 'top-level-non-partitioned'.
    SetPartitioned
  | -- | 'top-level' with the 'Partitioned' attribute, plus
    -- 'top-level-non-partitioned'.
    SetNamedPartitioned

tests :: TestTree
tests =
  testGroup
    "browsercontext-cookies-third-party"
    [ testCase "third party non-partitioned cookies" $
        withServer SetPlain $ \_ origins ->
          withChromiumContext $ \_ page ->
            runNonPartitioned origins page
    , testCase "save/load third party non-partitioned cookies" $
        withServer SetPlain $ \_ origins ->
          withChromiumContext $ \ctx page -> do
            runNonPartitioned origins page
            checkNonPartitioned origins page
            browser <- contextBrowser ctx
            exportViaCookies browser ctx $ \page2 ->
              checkNonPartitioned origins page2
            exportViaStorageState browser ctx $ \page3 ->
              checkNonPartitioned origins page3
    , testCase "should(not) block third party cookies - persistent context" $
        withServer SetPlain $ \_ origins ->
          withPersistentContext $ \ctx page ->
            checkThirdPartyCookie origins ctx page
    , testCase "should(not) block third party cookies - ephemeral context" $
        withServer SetPlain $ \_ origins ->
          withChromiumContext $ \ctx page ->
            checkThirdPartyCookie origins ctx page
    , testCase "should not block third party SameSite=None cookies" $
        withServer SetPlain $ \requests origins ->
          withChromiumContext $ \_ page -> do
            gotoUrl page (origin1 origins <> "/with-samesite-frame.html")
            seen <- awaitRequest requests "/json"
            assertEqual "cookie header sent to /json" (Just "a=b") (headerOf seen "cookie")
    , testCase "should be able to send third party cookies via an iframe" $
        withServer SetPlain $ \requests origins ->
          withChromiumContext $ \ctx page -> do
            gotoUrl page (origin1 origins <> "/empty.html")
            addCookies
              ctx
              [ CookieToSet
                  { cookieToSetName = CookieName "cookie1"
                  , cookieToSetValue = CookieValue "yes"
                  , cookieToSetUrl = Nothing
                  , cookieToSetDomain = Just (CookieDomain "127.0.0.1")
                  , cookieToSetPath = Just (CookiePath "/")
                  , cookieToSetExpires = Nothing
                  , cookieToSetHttpOnly = Just (CookieHttpOnly True)
                  , cookieToSetSecure = Just (CookieSecure True)
                  , cookieToSetSameSite = Just CookieSameSiteNone
                  }
              ]
            setContent page ("<iframe src=\"" <> origin2 origins <> "/grid.html\"></iframe>")
            seen <- awaitRequest requests "/grid.html"
            assertEqual "cookie header sent to the iframe" (Just "cookie1=yes") (headerOf seen "cookie")
    , testCase "third party 'Partitioned;' cookies" $
        withServer SetPartitioned $ \_ origins ->
          withChromiumContext $ \_ page -> do
            gotoUrl page (origin1 origins <> "/set-cookie.html")
            gotoUrl page (origin1 origins <> "/read-cookie.html")
            topBody page >>= assertEqual "top-level read" "Received cookie: top-level-non-partitioned=value; top-level-partitioned=value"
            gotoUrl page (origin2 origins <> "/frame-read-cookie.html")
            expectText (childText page) "Received cookie: top-level-non-partitioned=value"
            gotoUrl page (origin2 origins <> "/frame-set-cookie.html")
            gotoUrl page (origin2 origins <> "/frame-read-cookie.html")
            expectText (childText page) "Received cookie: frame-non-partitioned=value; frame-partitioned=value; top-level-non-partitioned=value"
    , testCase "same origin third party 'Partitioned;' cookie with different origin intermediate iframe" $
        withServer SetPartitioned $ \_ origins ->
          withChromiumContext $ \ctx page -> do
            -- main frame: origin1 -> iframe1: origin2 -> iframe2: origin1
            -- The cookie in iframe2 is a third-party partitioned cookie, even
            -- though it has the same origin as the main frame.
            gotoUrl page (origin1 origins <> "/nested-frame-set-cookie.html")
            let checkCookies p = do
                  gotoUrl p (origin1 origins <> "/nested-frame-read-cookie.html")
                  expectText (nestedText p) "Received cookie: frame-non-partitioned=value; frame-partitioned=value"
            checkCookies page
            browser <- contextBrowser ctx
            exportViaCookies browser ctx checkCookies
            exportViaStorageState browser ctx checkCookies
    , testCase "top level 'Partitioned;' cookie and same origin iframe" $
        withServer SetNamedPartitioned $ \_ origins ->
          withChromiumContext $ \_ page -> do
            -- Same origin iframe cookies are partitioned the same way as
            -- top-level cookies. The export steps are not ported, because
            -- Cookie and CookieToSet have no partitionKey to carry the
            -- partition across a context.
            gotoUrl page (origin1 origins <> "/set-cookie.html")
            gotoUrl page (origin1 origins <> "/read-cookie.html")
            topBody page >>= assertEqual "top-level read" "Received cookie: top-level-non-partitioned=value; top-level=value"
            gotoUrl page (origin1 origins <> "/frame-read-cookie.html")
            expectText (childText page) "Received cookie: top-level-non-partitioned=value; top-level=value"
            gotoUrl page (origin1 origins <> "/nested-frame-read-cookie.html")
            expectText (nestedText page) "Received cookie: top-level-non-partitioned=value"
    ]

-- | The 'Partitioned;' save/load case asserts the partitionKey of each stored
-- cookie, and the 'add Partitioned cookie via API' case passes a partitionKey
-- to addCookies. The Haskell Cookie and CookieToSet records have no
-- partitionKey, so those two cases are not ported.

-- | Runs the non-partitioned flow, which the save/load case reuses.
runNonPartitioned :: Origins -> Page -> Playwright ()
runNonPartitioned origins page = do
  gotoUrl page (origin1 origins <> "/set-cookie.html")
  gotoUrl page (origin1 origins <> "/read-cookie.html")
  topBody page >>= assertEqual "top-level read after set" "Received cookie: top-level=value"
  gotoUrl page (origin2 origins <> "/frame-read-cookie.html")
  expectText (childText page) "Received cookie: top-level=value"
  gotoUrl page (origin2 origins <> "/frame-set-cookie.html")
  gotoUrl page (origin2 origins <> "/frame-read-cookie.html")
  expectText (childText page) "Received cookie: frame=value; top-level=value"
  gotoUrl page (origin1 origins <> "/read-cookie.html")
  topBody page >>= assertEqual "top-level read after third-party set" "Received cookie: frame=value; top-level=value"

-- | Checks the cookies left behind by 'runNonPartitioned'.
checkNonPartitioned :: Origins -> Page -> Playwright ()
checkNonPartitioned origins page = do
  gotoUrl page (origin1 origins <> "/read-cookie.html")
  topBody page >>= assertEqual "top-level read" "Received cookie: frame=value; top-level=value"
  gotoUrl page (origin2 origins <> "/frame-read-cookie.html")
  expectText (childText page) "Received cookie: frame=value; top-level=value"

-- | Opens a page in a new context that receives the cookies of the source
-- context through 'addCookies'.
exportViaCookies :: Browser -> BrowserContext -> (Page -> Playwright ()) -> Playwright ()
exportViaCookies browser source check = do
  cs <- cookies source
  withContextOf browser defaultNewContextOptions $ \ctx2 -> do
    addCookies ctx2 (map cookieToSetFrom cs)
    page2 <- newPage ctx2
    check page2

-- | Opens a page in a new context created from the source context's
-- storageState.
exportViaStorageState :: Browser -> BrowserContext -> (Page -> Playwright ()) -> Playwright ()
exportViaStorageState browser source check = do
  state <- storageState source
  withContextOf browser (defaultNewContextOptions {newContextStorageState = Just (state :: StorageState)}) $ \ctx3 -> do
    page3 <- newPage ctx3
    check page3

withContextOf :: Browser -> NewContextOptions -> (BrowserContext -> Playwright a) -> Playwright a
withContextOf browser opts =
  bracket (newContextWith browser opts) closeContext

-- | Mirrors the upstream helper that copies a cookie into a CookieToSet.
cookieToSetFrom :: Cookie -> CookieToSet
cookieToSetFrom cookie =
  CookieToSet
    { cookieToSetName = cookieName cookie
    , cookieToSetValue = cookieValue cookie
    , cookieToSetUrl = Nothing
    , cookieToSetDomain = Just (cookieDomain cookie)
    , cookieToSetPath = Just (cookiePath cookie)
    , cookieToSetExpires = Just (cookieExpires cookie)
    , cookieToSetHttpOnly = Just (cookieHttpOnly cookie)
    , cookieToSetSecure = Just (cookieSecure cookie)
    , cookieToSetSameSite = Just (cookieSameSite cookie)
    }

-- | The third-party write in an iframe. Chromium blocks third-party cookies
-- here, as the upstream test expects for every browser but Firefox, so the
-- frame's document.cookie stays empty and the context has no cookies for the
-- third-party url.
checkThirdPartyCookie :: Origins -> BrowserContext -> Page -> Playwright ()
checkThirdPartyCookie origins ctx page = do
  gotoUrl page (origin1 origins <> "/empty.html")
  frame <- attachFrame page "third-party" (origin2 origins <> "/grid.html")
  documentCookie <-
    evaluateOnFrameText
      frame
      "() => { document.cookie = 'username=John Doe'; return document.cookie; }"
  liftIO (threadDelay 2000000)
  assertEqual "document.cookie in the third-party frame" "" documentCookie
  cs <- cookiesFor ctx [urlFromText (origin2 origins <> "/grid.html")]
  assertEqual "cookies for the third-party url" (0 :: Int) (length cs)

gotoUrl :: Page -> Text -> Playwright ()
gotoUrl page url =
  void (goto page (urlFromText url) defaultGotoOptions)

topBody :: Page -> Playwright Text
topBody page =
  evaluateText page "() => document.body.textContent"

frameBodyText :: Frame -> Playwright Text
frameBodyText frame =
  evaluateOnFrameText frame "() => document.body.textContent"

-- | The body text of the first child frame, looked up again on every poll so
-- a frame replaced by a navigation is not read through a stale handle.
childText :: Page -> Playwright Text
childText page =
  firstChild (mainFrame page) >>= frameBodyText

-- | The body text of the nested grandchild frame, looked up on every poll.
nestedText :: Page -> Playwright Text
nestedText page =
  nestedGrandChild page >>= frameBodyText

firstChild :: Frame -> Playwright Frame
firstChild frame = do
  kids <- childFrames frame
  live <- filterM (fmap not . isDetached) kids
  case live of
    kid : _ -> pure kid
    [] -> assertFailure "expected a child frame"

-- | The frame nested in the first child of the page's main frame.
nestedGrandChild :: Page -> Playwright Frame
nestedGrandChild page =
  firstChild (mainFrame page) >>= firstChild

-- | Polls the frame text for a short while, as the upstream 'toHaveText'
-- waits do. A read that throws, such as a child frame that is not attached
-- yet or was detached by a navigation, counts as not matching and is retried.
expectText :: Playwright Text -> Text -> Playwright ()
expectText readText expected =
  go (30 :: Int)
  where
    go :: Int -> Playwright ()
    go n = do
      result <- tryAny readText
      let matched = case result of
            Right actual -> actual == expected
            Left _ -> False
      if matched || n <= 0
        then case result of
          Right actual -> assertEqual "frame body text" expected actual
          Left err -> throwIO err
        else liftIO (threadDelay 100000) >> go (n - 1)

-- | Waits for the server to record a request for the path.
awaitRequest :: IORef [Recorded] -> BS8.ByteString -> Playwright Recorded
awaitRequest requests path =
  go (100 :: Int)
  where
    go :: Int -> Playwright Recorded
    go n = do
      seen <- readIORef requests
      case filter ((== path) . recordedPath) seen of
        latest : _ -> pure latest
        []
          | n > 0 -> liftIO (threadDelay 100000) >> go (n - 1)
          | otherwise -> assertFailure ("the server saw no request for " <> show path)

headerOf :: Recorded -> BS8.ByteString -> Maybe BS8.ByteString
headerOf recorded name =
  lookup name (recordedHeaders recorded)

-- | Starts one server for both origins, then runs the action with its
-- request log and origins.
withServer :: SetMode -> (IORef [Recorded] -> Origins -> IO a) -> IO a
withServer mode action = do
  requests <- newIORef []
  withApplication (pure (app requests mode)) $ \port ->
    action requests (originsFor port)

originsFor :: Int -> Origins
originsFor port =
  Origins
    { origin1 = "http://localhost:" <> pack (show port)
    , origin2 = "http://127.0.0.1:" <> pack (show port)
    }

-- | Serves the upstream routes on both origins. Each origin is taken from the
-- Host header, and the port is the same for both.
app :: IORef [Recorded] -> SetMode -> Application
app requests mode request respond = do
  recordRequest requests request
  respond (route request mode)

route :: Request -> SetMode -> Response
route request mode =
  case rawPathInfo request of
    "/read-cookie.html" ->
      html ("Received cookie: " <> readCookieHeader (lookup hCookie headers))
    "/frame-set-cookie.html" ->
      html (iframe (o1 <> "/set-cookie.html"))
    "/frame-read-cookie.html" ->
      html (iframe (o1 <> "/read-cookie.html"))
    "/nested-frame-set-cookie.html" ->
      html (iframe (o2 <> "/frame-set-cookie.html"))
    "/nested-frame-read-cookie.html" ->
      html (iframe (o2 <> "/frame-read-cookie.html"))
    "/set-cookie.html" ->
      responseLBS status200 ((hContentType, "text/html") : map (\value -> ("Set-Cookie", value)) (setCookies mode)) ""
    "/grid.html" ->
      html "Hello world"
    "/with-samesite-frame.html" ->
      html (iframe (o2 <> "/grid-samesite.html"))
    "/grid-samesite.html" ->
      responseLBS
        status200
        [ (hContentType, "text/html")
        , ("Set-Cookie", "a=b; Path=/; Max-Age=3600; SameSite=None; Secure")
        ]
        "Hello world <script>setTimeout(() => fetch('/json'), 1000);</script>"
    "/json" ->
      responseLBS status200 [] ""
    "/empty.html" ->
      html ""
    _ ->
      responseLBS status404 [(hContentType, "text/plain")] "not found"
  where
    headers = requestHeaders request
    port = BS8.drop 1 (BS8.dropWhile (/= ':') (fromMaybe "" (lookup "Host" headers)))
    o1 = "http://localhost:" <> port
    o2 = "http://127.0.0.1:" <> port
    iframe :: BS8.ByteString -> BS8.ByteString
    iframe src = "<iframe src='" <> src <> "'></iframe>"
    html :: BS8.ByteString -> Response
    html body = responseLBS status200 [(hContentType, "text/html")] (LBS8.fromStrict body)
    setCookies current =
      let prefix :: BS8.ByteString
          prefix = if isJust (lookup hReferer headers) then "frame" else "top-level"
       in case current of
            SetPlain ->
              [prefix <> "=value; SameSite=None; Path=/; Secure;"]
            SetPartitioned ->
              [ prefix <> "-partitioned=value; SameSite=None; Path=/; Secure; Partitioned;"
              , prefix <> "-non-partitioned=value; SameSite=None; Path=/; Secure;"
              ]
            SetNamedPartitioned ->
              [ prefix <> "=value; SameSite=None; Path=/; Secure; Partitioned;"
              , prefix <> "-non-partitioned=value; SameSite=None; Path=/; Secure;"
              ]

-- | The sorted cookie names and values, joined with '; '. It is 'undefined'
-- when the request had no Cookie header, as the upstream server answers.
readCookieHeader :: Maybe BS8.ByteString -> BS8.ByteString
readCookieHeader Nothing = "undefined"
readCookieHeader (Just header) =
  BS8.intercalate "; " (sort (map BS8.strip (BS8.split ';' header)))

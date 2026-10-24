module Playwright.PageGotoSupport
  ( requireMs
  , assetFileUrl
  , crossProcess
  , withHangServer
  , withHttpsFixture
  , withGotoServer
  , hangApp
  , empty204App
  , redirectApp
  , coopHtml
  , coopApp
  , coopRedirectApp
  , listenNetwork
  , addUnique
  , coopTest
  , coopRedirectTest
  , captureIframeNavigation
  , subframe204
  , refererNavigation
  , waitForHit
  , pathKey
  , headerReferer
  , firstReferer
  , canceledNavigation
  , commitNavigation
  , commitApp
  , rtcApp
  , loadEventOrder
  , failBeforeCommit
  , windowStopGoto
  , gotoInterruptedBySecond
  , iframeAttachDetach
  , replacedByAnotherNavigation
  , jsRedirectUrlBar
  , urlBarDuringPending
  ) where

import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )
import UnliftIO
  ( withRunInIO
  )
import UnliftIO.Concurrent
  ( threadDelay
  )
import UnliftIO.Async
  ( async
  , wait
  , withAsync
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , putMVar
  , takeMVar
  )
import UnliftIO.Exception
  ( SomeException
  , bracket
  , try
  )
import Control.Monad
  ( forever
  , void
  )
import Data.ByteString.Lazy
  ( ByteString
  )
import Data.ByteString.Lazy qualified as LBS
import UnliftIO.IORef
  ( IORef
  , newIORef
  , atomicModifyIORef'
  , modifyIORef'
  , readIORef
  , writeIORef
  )
import Data.List
  ( find
  )
import Data.Text
  ( Text
  , pack
  , replace
  )
import Data.Text.Encoding
  ( decodeUtf8
  )
import Data.Text qualified as Text
import Network.HTTP.Types
  ( hContentType
  , hLocation
  , hReferer
  , status200
  , status204
  , status302
  , status404
  , status500
  )
import Network.Wai
  ( Application
  , pathInfo
  , requestHeaders
  , responseLBS
  , responseRaw
  )
import Network.Wai qualified as Wai
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , staticApp
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import System.IO
  ( hGetLine
  )
import System.Process
  ( CreateProcess (std_out)
  , StdStream (CreatePipe)
  , cleanupProcess
  , createProcess
  , proc
  )
import Playwright
  ( Frame
  , GotoOptions (gotoOptionsReferer, gotoOptionsWaitUntil)
  , HttpStatus (httpStatusToInt)
  , LoadState (LoadStateCommit, LoadStateLoad)
  , Page
  , Request
  , Response
  , Timeout
  , UrlMatch (UrlMatchGlob)
  , armFrameDetached
  , continueRoute
  , defaultGotoOptions
  , evaluateTexts
  , frames
  , framesEqual
  , goto
  , mainFrame
  , milliseconds
  , onRequest
  , onRequestFailed
  , onRequestFinished
  , onResponse
  , querySelector
  , requestFailure
  , requestFrame
  , requestRedirectedFrom
  , requestUrl
  , requestsEqual
  , responseFinished
  , responseFrame
  , responseRequest
  , responseStatus
  , responseUrl
  , route
  , selectorFromText
  , title
  , url
  , urlFromText
  , urlToText
  , waitForTimeout
  , Playwright
  )
import Playwright.Harness
  ( readTestAsset
  , testAssetsDir
  , withAssets
  , withChromiumPage
  , withLocalApp
  , assertEqual
  , assertFailure
  )

requireMs :: (MonadIO m) => Double -> m Timeout
requireMs n =
  case milliseconds n of
    Nothing ->
      assertFailure ("timeout " <> show n)
    Just timeout ->
      pure timeout

assetFileUrl :: (MonadIO m) => FilePath -> m Text
assetFileUrl relative = do
  dir <- testAssetsDir
  pure ("file://" <> pack dir <> "/" <> pack relative)

crossProcess :: Text -> Text
crossProcess =
  replace "127.0.0.1" "localhost"

withHangServer :: (Text -> Playwright a) -> Playwright a
withHangServer =
  withLocalApp hangApp

withGotoServer :: Application -> (Text -> Page -> Playwright a) -> IO a
withGotoServer app action =
  withChromiumPage $ \page ->
    withLocalApp app $ \prefix ->
      action prefix page

-- | Starts a Node HTTPS server with the untrusted test server certificate, so
-- Chromium rejects it unless the context ignores HTTPS errors. It serves
-- /redirect/1.html -> /redirect/2.html -> /empty.html like the upstream
-- httpsServer fixture.
withHttpsFixture :: (Text -> Playwright a) -> Playwright a
withHttpsFixture action = do
  root <- testAssetsDir
  withRunInIO $ \run ->
    bracket
      (createProcess (proc "node" ["-e", httpsNodeScript, root <> "/client-certificates/server"]) {std_out = CreatePipe})
      cleanupProcess
      ( \(_, stdoutHandle, _, _) -> do
          line <- maybe (fail "https fixture has no stdout") hGetLine stdoutHandle
          case words line of
            ["PORT", port] ->
              run (action (pack ("https://127.0.0.1:" <> port)))
            _ ->
              fail ("unexpected https fixture output: " <> line)
      )

httpsNodeScript :: String
httpsNodeScript =
  unlines
    [ "const fs = require('fs');"
    , "const https = require('https');"
    , "const dir = process.argv[1];"
    , "const redirects = { '/redirect/1.html': '/redirect/2.html', '/redirect/2.html': '/empty.html' };"
    , "const options = { key: fs.readFileSync(dir + '/server_key.pem'), cert: fs.readFileSync(dir + '/server_cert.pem') };"
    , "const server = https.createServer(options, (req, res) => {"
    , "  if (redirects[req.url]) { res.writeHead(302, { Location: redirects[req.url] }); res.end(); return; }"
    , "  res.writeHead(200, { 'Content-Type': 'text/html' });"
    , "  res.end('<div>ok</div>');"
    , "});"
    , "server.listen(0, '127.0.0.1', () => console.log(`PORT ${server.address().port}`));"
    ]

hangApp :: Application
hangApp _request _respond =
  forever (threadDelay 1000000)

empty204App :: Application
empty204App _request respond =
  respond (responseLBS status204 [] "")

redirectApp :: Application
redirectApp request respond =
  case pathInfo request of
    ["redirect", "1.html"] ->
      respond (responseLBS status302 [(hLocation, "/redirect/2.html")] "")
    ["redirect", "2.html"] ->
      respond (responseLBS status302 [(hLocation, "/empty.html")] "")
    ["empty.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] "")
    _ ->
      respond (responseLBS status404 [] "")

coopHtml :: ByteString
coopHtml =
  "<div>Hello there!</div><script>window.onload = () => console.log('onload')</script>"

coopApp :: Application
coopApp request respond =
  case pathInfo request of
    ["empty.html"] ->
      respond
        ( responseLBS
            status200
            [ (hContentType, "text/html")
            , ("Cross-Origin-Opener-Policy", "same-origin")
            ]
            coopHtml
        )
    _ ->
      respond (responseLBS status404 [] "")

coopRedirectApp :: Application
coopRedirectApp request respond =
  case pathInfo request of
    ["redirect"] ->
      respond (responseLBS status302 [(hLocation, "/empty.html")] "")
    ["empty.html"] ->
      respond
        ( responseLBS
            status200
            [ (hContentType, "text/html")
            , ("Cross-Origin-Opener-Policy", "same-origin")
            ]
            coopHtml
        )
    _ ->
      respond (responseLBS status404 [] "")

listenNetwork :: Page -> Playwright (IORef [Text], IORef [Request])
listenNetwork page = do
  events <- newIORef ([] :: [Text])
  seen <- newIORef ([] :: [Request])
  let
    remember :: Text -> Request -> Playwright ()
    remember label request = do
      modifyIORef' events (<> [label])
      modifyIORef' seen (addUnique request)
  onRequest page (remember "request")
  onRequestFailed page (remember "requestfailed")
  onRequestFinished page (remember "requestfinished")
  onResponse page $ \response -> do
    request <- responseRequest response
    remember "response" request
  pure (events, seen)

addUnique :: Request -> [Request] -> [Request]
addUnique request seen =
  if any (requestsEqual request) seen
    then seen
    else seen <> [request]

coopTest :: Bool -> IO ()
coopTest intercept =
  withGotoServer coopApp $ \prefix page -> do
    (events, seen) <- listenNetwork page
    if intercept
      then
        route page (UrlMatchGlob "**/*") $ \handled -> do
          threadDelay 100000
          continueRoute handled
      else
        pure ()
    response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
    assertEqual "url" (prefix <> "/empty.html") . urlToText =<< url page
    case response of
      Nothing ->
        assertFailure "missing response"
      Just value -> do
        responseFinished value
        assertEqual "events" ["request", "response", "requestfinished"] =<< readIORef events
        assertEqual "unique" 1 . length =<< readIORef seen
        failure <- requestFailure =<< responseRequest value
        case failure of
          Nothing ->
            pure ()
          Just _ ->
            assertFailure "expected no failure"

coopRedirectTest :: IO ()
coopRedirectTest =
  withGotoServer coopRedirectApp $ \prefix page -> do
    (events, seen) <- listenNetwork page
    response <- goto page (urlFromText (prefix <> "/redirect")) defaultGotoOptions
    assertEqual "url" (prefix <> "/empty.html") . urlToText =<< url page
    case response of
      Nothing ->
        assertFailure "missing response"
      Just value -> do
        responseFinished value
        assertEqual
          "events"
          ["request", "response", "requestfinished", "request", "response", "requestfinished"]
          =<< readIORef events
        assertEqual "unique" 2 . length =<< readIORef seen
        failure <- requestFailure =<< responseRequest value
        case failure of
          Nothing ->
            pure ()
          Just _ ->
            assertFailure "expected no failure"
        redirected <- requestRedirectedFrom =<< responseRequest value
        case redirected of
          Nothing ->
            assertFailure "expected redirectedFrom"
          Just first ->
            assertEqual "redirect url" (prefix <> "/redirect") . urlToText =<< requestUrl first

captureIframeNavigation :: Bool -> IO ()
captureIframeNavigation cross =
  withAssets $ \prefix page -> do
    let
      empty :: Text
      empty = prefix <> "/empty.html"
      origin :: Text
      origin =
        if cross
          then crossProcess prefix
          else prefix
      frameUrl :: Text
      frameUrl = origin <> "/frames/frame.html"
      pageUrl :: Text
      pageUrl = origin <> "/frames/one-frame.html"
    _ <- goto page (urlFromText empty) defaultGotoOptions
    seen <- newIORef (Nothing :: Maybe Frame)
    onRequest page $ \request -> do
      location <- urlToText <$> requestUrl request
      if location == frameUrl
        then writeIORef seen =<< requestFrame request
        else pure ()
    response <- goto page (urlFromText pageUrl) defaultGotoOptions
    assertEqual "url" pageUrl . urlToText =<< url page
    case response of
      Nothing ->
        assertFailure "missing response"
      Just value -> do
        frame <- responseFrame value
        case frame of
          Nothing ->
            assertFailure "missing response frame"
          Just actual ->
            assertEqual "response frame" True (framesEqual actual (mainFrame page))
        assertEqual "response url" pageUrl . urlToText =<< responseUrl value
    allFrames <- frames page
    assertEqual "frames" 2 (length allFrames)
    requestFrameValue <- readIORef seen
    case (requestFrameValue, allFrames) of
      (Just actual, [_, iframe]) ->
        assertEqual "iframe" True (framesEqual actual iframe)
      (Nothing, _) ->
        assertFailure "missing iframe request"
      _ ->
        assertFailure "expected two frames"

subframe204 :: Maybe LoadState -> IO ()
subframe204 waitUntil = do
  assets <- testAssetsDir
  let
    static = staticApp (defaultFileServerSettings assets)
    app request respond =
      if pathInfo request == ["frames", "frame.html"]
        then respond (responseLBS status204 [] "")
        else static request respond
  withGotoServer app $ \prefix page -> do
    _ <-
      goto
        page
        (urlFromText (prefix <> "/frames/one-frame.html"))
        defaultGotoOptions {gotoOptionsWaitUntil = waitUntil}
    pure ()

refererNavigation :: Text -> Bool -> IO ()
refererNavigation referer inheritPng = do
  hits <- newIORef ([] :: [(Text, Maybe Text)])
  assets <- testAssetsDir
  grid <- readTestAsset "grid.html"
  let
    static = staticApp (defaultFileServerSettings assets)
    app request respond = do
      modifyIORef' hits (<> [(pathKey request, headerReferer request)])
      if pathInfo request == ["grid.html"] && not inheritPng
        then
          respond
            ( responseLBS
                status200
                [ (hContentType, "text/html")
                , ("Referrer-Policy", "no-referrer")
                ]
                (LBS.fromStrict grid)
            )
        else
          static request respond
  withGotoServer app $ \prefix page -> do
    let gridUrl = prefix <> "/grid.html"
    _ <-
      goto
        page
        (urlFromText gridUrl)
        defaultGotoOptions {gotoOptionsReferer = Just (urlFromText referer)}
    recorded <- liftIO (waitForHit hits "digits/1.png")
    case firstReferer "grid.html" recorded of
      Nothing ->
        assertFailure "missing grid request"
      Just actual ->
        assertEqual "grid" (Just referer) actual
    case firstReferer "digits/1.png" recorded of
      Nothing ->
        assertFailure "missing png request"
      Just actual ->
        if inheritPng
          then assertEqual "png" (Just gridUrl) actual
          else assertEqual "png" Nothing actual
    assertEqual "url" gridUrl . urlToText =<< url page

-- | Reads the hit list once the given path has been requested, polling for up to five
-- seconds. The image request can still be in flight when @goto@ returns.
waitForHit :: IORef [(Text, Maybe Text)] -> Text -> IO [(Text, Maybe Text)]
waitForHit hits path = go (50 :: Int)
  where
    go :: Int -> IO [(Text, Maybe Text)]
    go remaining = do
      recorded <- readIORef hits
      case firstReferer path recorded of
        Just _ -> pure recorded
        Nothing
          | remaining <= 0 -> pure recorded
          | otherwise -> threadDelay 100000 >> go (remaining - 1)

pathKey :: Wai.Request -> Text
pathKey request =
  Text.intercalate "/" (pathInfo request)

headerReferer :: Wai.Request -> Maybe Text
headerReferer request =
  fmap decodeUtf8 (lookup hReferer (requestHeaders request))

firstReferer :: Text -> [(Text, Maybe Text)] -> Maybe (Maybe Text)
firstReferer path hits =
  fmap snd (find (\(name, _) -> name == path) hits)

canceledNavigation :: IO ()
canceledNavigation = do
  started <- newEmptyMVar
  let
    app :: Application
    app request respond =
      case pathInfo request of
        ["one-style.html"] -> do
          putMVar started ()
          forever (threadDelay 1000000)
        ["empty.html"] ->
          respond (responseLBS status200 [(hContentType, "text/html")] "")
        _ ->
          respond (responseLBS status404 [] "")
  withGotoServer app $ \prefix page ->
    withAsync
      ( try (goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions) ::
          Playwright (Either SomeException (Maybe Response))
      )
      $ \pending -> do
        takeMVar started
        _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
        result <- wait pending
        case result of
          Left _ ->
            pure ()
          Right _ ->
            assertFailure "expected canceled navigation to fail"

commitNavigation :: IO ()
commitNavigation =
  withGotoServer commitApp $ \prefix page -> do
    response <-
      goto
        page
        (urlFromText (prefix <> "/empty.html"))
        defaultGotoOptions {gotoOptionsWaitUntil = Just LoadStateCommit}
    case response of
      Nothing ->
        assertFailure "missing response"
      Just value ->
        assertEqual "status" 200 . httpStatusToInt =<< responseStatus value
    assertEqual "title" "Hello" =<< title page

commitApp :: Application
commitApp request respond =
  case pathInfo request of
    ["script.js"] ->
      forever (threadDelay 1000000)
    ["empty.html"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/html")]
            "<title>Hello</title><script src=\"script.js\"></script>"
        )
    _ ->
      respond (responseLBS status404 [] "")

rtcApp :: Application
rtcApp request respond =
  case pathInfo request of
    ["rtc.html"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/html")]
            "<!DOCTYPE html><html><body><script>window.RTCPeerConnection && new window.RTCPeerConnection({ iceServers: [] });</script></body></html>"
        )
    _ ->
      respond (responseLBS status404 [] "")

loadEventOrder :: IO ()
loadEventOrder = do
  assets <- testAssetsDir
  let
    static = staticApp (defaultFileServerSettings assets)
    app request respond =
      case pathInfo request of
        ["slow.js"] -> do
          threadDelay 100000
          respond
            ( responseLBS
                status200
                [(hContentType, "application/javascript")]
                "window.results.push('slow module');export const foo = 'slow';"
            )
        _ ->
          static request respond
  withGotoServer app $ \prefix page -> do
    _ <- goto page (urlFromText (prefix <> "/load-event/load-event.html")) defaultGotoOptions
    results <- evaluateTexts page "() => window.results"
    assertEqual
      "order"
      [ "script tag after after module"
      , "slow module"
      , "module"
      , "DOMContentLoaded"
      , "load"
      ]
      results

failBeforeCommit :: IO ()
failBeforeCommit = do
  assets <- testAssetsDir
  let
    static = staticApp (defaultFileServerSettings assets)
    app :: Application
    app request respond =
      case pathInfo request of
        ["empty.html"] ->
          respond (responseRaw (\_ _ -> pure ()) (responseLBS status500 [] ""))
        _ ->
          static request respond
  withGotoServer app $ \prefix page -> do
    response <-
      goto page (urlFromText (crossProcess prefix <> "/title.html")) defaultGotoOptions
    case response of
      Nothing ->
        assertFailure "missing response"
      Just value ->
        responseFinished value
    result <-
      try (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions) ::
        Playwright (Either SomeException (Maybe Response))
    case result of
      Left _ ->
        pure ()
      Right _ ->
        assertFailure "expected navigation error"

windowStopGoto :: IO ()
windowStopGoto = do
  assets <- testAssetsDir
  started <- newEmptyMVar
  release <- newEmptyMVar
  done <- newIORef False
  let
    static = staticApp (defaultFileServerSettings assets)
    app :: Application
    app request respond =
      case pathInfo request of
        ["module.js"] -> do
          putMVar started ()
          takeMVar release
          respond
            ( responseLBS
                status200
                [(hContentType, "application/javascript")]
                ""
            )
        _ ->
          static request respond
  withGotoServer app $ \prefix page -> do
    timeout <- requireMs 1000
    withAsync
      ( do
          result <-
            try (goto page (urlFromText (prefix <> "/window-stop.html")) defaultGotoOptions) ::
              Playwright (Either SomeException (Maybe Response))
          case result of
            Left _ ->
              pure ()
            Right _ ->
              writeIORef done True
      )
      $ \_ -> do
        takeMVar started
        assertEqual "before end" False =<< readIORef done
        waitForTimeout page timeout
        putMVar release ()
        waitForTimeout page timeout
        assertEqual "after end" False =<< readIORef done

gotoInterruptedBySecond :: IO ()
gotoInterruptedBySecond = do
  assets <- testAssetsDir
  finished <- newIORef False
  let
    static = staticApp (defaultFileServerSettings assets)
    app :: Application
    app request respond =
      case pathInfo request of
        ["slow.js"] ->
          forever (threadDelay 1000000)
        _ ->
          static request respond
  withGotoServer app $ \prefix page -> do
    withAsync
      ( do
          response <-
            goto
              page
              (urlFromText (prefix <> "/load-event/load-event.html"))
              defaultGotoOptions
          writeIORef finished True
          pure response
      )
      $ \pending -> do
        threadDelay 500000
        assertEqual "still pending" False =<< readIORef finished
        _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
        first <- wait pending
        case first of
          Nothing ->
            assertFailure "missing first navigation response"
          Just value ->
            assertEqual "status" 200 . httpStatusToInt =<< responseStatus value

iframeAttachDetach :: IO ()
iframeAttachDetach = do
  let
    app :: Application
    app request respond =
      case pathInfo request of
        ["empty.html"] ->
          respond
            ( responseLBS
                status200
                [(hContentType, "text/html")]
                "<body><script>const iframe = document.createElement('iframe'); iframe.src = './iframe.html'; document.body.appendChild(iframe); setTimeout(() => iframe.remove(), 1000);</script></body>"
            )
        ["iframe.html"] ->
          respond
            ( responseLBS
                status200
                [(hContentType, "text/html")]
                "<link rel=\"stylesheet\" href=\"./style2.css\">"
            )
        ["style2.css"] ->
          forever (threadDelay 1000000)
        _ ->
          respond (responseLBS status404 [] "")
  withGotoServer app $ \prefix page -> do
    awaitDetached <- armFrameDetached page
    withAsync
      ( goto
          page
          (urlFromText (prefix <> "/empty.html"))
          defaultGotoOptions {gotoOptionsWaitUntil = Just LoadStateLoad}
      )
      $ \done -> do
        _ <- awaitDetached
        _ <- wait done
        iframe <- querySelector page (selectorFromText "iframe")
        case iframe of
          Nothing ->
            pure ()
          Just _ ->
            assertFailure "expected iframe to be gone"

replacedByAnotherNavigation :: IO ()
replacedByAnotherNavigation = do
  assets <- testAssetsDir
  prefixRef <- newIORef ("" :: Text)
  pageRef <- newIORef (Nothing :: Maybe Page)
  gotoIO <- newIORef (Nothing :: Maybe (Page -> Text -> IO (Maybe Response)))
  another <- newEmptyMVar
  let
    static = staticApp (defaultFileServerSettings assets)
    app :: Application
    app request respond =
      case pathInfo request of
        ["empty.html"] -> do
          prefix <- readIORef prefixRef
          maybePage <- readIORef pageRef
          maybeGoto <- readIORef gotoIO
          case (maybePage, maybeGoto) of
            (Just page, Just runGoto) -> do
              void $
                async $
                  putMVar another
                    =<< runGoto page (prefix <> "/one-style.html")
              forever (threadDelay 1000000)
            _ ->
              respond (responseLBS status500 [] "")
        _ ->
          static request respond
  withChromiumPage $ \page ->
    withRunInIO $ \run -> do
      writeIORef
        gotoIO
        (Just (\targetPage target -> run (goto targetPage (urlFromText target) defaultGotoOptions)))
      writeIORef pageRef (Just page)
      withApplication (pure app) $ \port ->
        run $ do
          let prefix = "http://127.0.0.1:" <> pack (show port)
          writeIORef prefixRef prefix
          result <-
            try (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions) ::
              Playwright (Either SomeException (Maybe Response))
          _ <- takeMVar another
          case result of
            Left _ ->
              pure ()
            Right _ ->
              assertFailure "expected replaced navigation to fail"

jsRedirectUrlBar :: IO ()
jsRedirectUrlBar = do
  events <- newIORef ([] :: [Text])
  let
    record :: Text -> IO ()
    record label =
      atomicModifyIORef' events (\xs -> (xs <> [label], ()))
    app :: Application
    app request respond =
      case pathInfo request of
        ["a"] ->
          respond
            ( responseLBS
                status200
                [(hContentType, "text/html")]
                "<body><script>setTimeout(() => { window.location.pathname = '/c'; }, 1000);</script></body>"
            )
        ["b"] -> do
          record "started b"
          threadDelay 2000000
          received <- respond (responseLBS status200 [(hContentType, "text/html")] "BBB")
          record "finished b"
          pure received
        ["c"] -> do
          record "started c"
          threadDelay 2000000
          received <- respond (responseLBS status200 [(hContentType, "text/html")] "CCC")
          record "finished c"
          pure received
        _ ->
          respond (responseLBS status404 [] "")
  withGotoServer app $ \prefix page -> do
    _ <- goto page (urlFromText (prefix <> "/a")) defaultGotoOptions
    result <-
      try (goto page (urlFromText (prefix <> "/b")) defaultGotoOptions) ::
        Playwright (Either SomeException (Maybe Response))
    recorded <- readIORef events
    assertEqual "events" ["started b", "finished b"] recorded
    case result of
      Left _ ->
        assertFailure "chromium should keep url-bar navigation"
      Right _ ->
        assertEqual "url" (prefix <> "/b") . urlToText =<< url page

urlBarDuringPending :: IO ()
urlBarDuringPending = do
  events <- newIORef ([] :: [Text])
  let
    record :: Text -> IO ()
    record label =
      atomicModifyIORef' events (\xs -> (xs <> [label], ()))
    app :: Application
    app request respond =
      case pathInfo request of
        ["a"] ->
          respond
            ( responseLBS
                status200
                [(hContentType, "text/html")]
                "<body><script>setTimeout(() => { window.location.pathname = '/c'; }, 10);</script></body>"
            )
        ["b"] -> do
          record "started b"
          threadDelay 2000000
          received <- respond (responseLBS status200 [(hContentType, "text/html")] "BBB")
          record "finished b"
          pure received
        ["c"] -> do
          record "started c"
          threadDelay 2000000
          received <- respond (responseLBS status200 [(hContentType, "text/html")] "CCC")
          record "finished c"
          pure received
        _ ->
          respond (responseLBS status404 [] "")
  withGotoServer app $ \prefix page -> do
    timeout <- requireMs 1000
    _ <- goto page (urlFromText (prefix <> "/a")) defaultGotoOptions
    waitForTimeout page timeout
    result <-
      try (goto page (urlFromText (prefix <> "/b")) defaultGotoOptions) ::
        Playwright (Either SomeException (Maybe Response))
    recorded <- readIORef events
    assertEqual "events" ["started c", "started b", "finished c", "finished b"] recorded
    case result of
      Left _ ->
        assertFailure "expected url-bar navigation to succeed"
      Right _ ->
        assertEqual "url" (prefix <> "/b") . urlToText =<< url page

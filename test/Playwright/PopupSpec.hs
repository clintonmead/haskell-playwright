module Playwright.PopupSpec
  ( tests
  ) where

import Control.Concurrent
  ( Chan
  , newChan
  , readChan
  , writeChan
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text
  ( Text
  , intercalate
  , pack
  , replace
  )
import Network.HTTP.Types
  ( RequestHeaders
  , hAuthorization
  , hContentType
  , hUserAgent
  , status200
  , status401
  )
import Network.HTTP.Types qualified as HTTP
import Network.Wai
  ( Application
  , pathInfo
  , requestHeaders
  , responseLBS
  )
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , staticApp
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( AriaRole (AriaRole)
  , BindingName (BindingName)
  , HeaderName (HeaderName)
  , HeaderValue (HeaderValue)
  , HttpCredentials (HttpCredentials, httpCredentialsOrigin, httpCredentialsPassword, httpCredentialsSend, httpCredentialsUsername)
  , HttpHeader (HttpHeader)
  , HttpPassword (HttpPassword)
  , HttpUsername (HttpUsername)
  , LoadState (LoadStateDomContentLoaded, LoadStateLoad)
  , NewContextOptions (newContextExtraHTTPHeaders, newContextHasTouch, newContextHttpCredentials, newContextUserAgent, newContextViewport)
  , Page
  , Playwright
  , UrlMatch (UrlMatchGlob)
  , UserAgent (UserAgent)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , addContextInitScript
  , click
  , contextExposeFunction
  , contextRoute
  , continueRoute
  , defaultGetByRoleOptions
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluate
  , evaluateBool
  , evaluateBoolWith
  , evaluateNumber
  , evaluateNumberWith
  , evaluateNumbersWith
  , evaluateText
  , evaluateWith
  , getByRole
  , goto
  , numberArgument
  , onContextPage
  , pageContext
  , reload
  , setContent
  , setOffline
  , setViewportSize
  , textArgument
  , title
  , undefinedArgument
  , urlFromText
  , armContextPage
  , waitForLoadState
  , waitForPopup
  )
import Playwright.Error
  ( PlaywrightError (EvaluateUnexpected)
  , ignoreTargetClosed
  )
import Playwright.Harness
  ( locate
  , testAssetsDir
  , withAssets
  , withAssetsContextWith
  , withChromiumContextWith
  , assertEqual
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadNumber)
  , SerializedValue (SerializedNode)
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
import UnliftIO.Exception
  ( throwIO
  )
import UnliftIO.IORef
  ( modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )

tests :: TestTree
tests =
  testGroup
    "popup"
    [ testCase "should inherit user agent from browser context" inheritUserAgent
    , testCase "should respect routes from browser context" contextRoutesOnClick
    , testCase "should inherit extra headers from browser context" inheritExtraHeaders
    , testCase "should inherit offline from browser context" inheritOffline
    , testCase "should inherit http credentials from browser context" inheritHttpCredentials
    , testCase "should inherit touch support from browser context" inheritTouch
    , testCase "should inherit viewport size from browser context" inheritViewport
    , testCase "should use viewport size from window features" windowFeaturesViewport
    , testCase "should respect routes from browser context when using window.open" contextRoutesOnWindowOpen
    , testCase "BrowserContext.addInitScript should apply to an in-process popup" initScriptInProcess
    , testCase "BrowserContext.addInitScript should apply to a cross-process popup" initScriptCrossProcess
    , testCase "should expose function from browser context" exposeFunctionOnPopup
    , testCase "should not throttle rAF in the opener page" rafNotThrottled
    , testCase "should not throw when click closes popup" clickClosesPopup
    -- Skipped: "should not dispatch binding on a closed page". It needs `exposeBinding` to pass a
    -- source page and check `source.page.isClosed()`. Bindings do not carry a page yet.
    -- "should not dispatch binding on a closed page"
    ]

inheritUserAgent :: IO ()
inheritUserAgent = do
  captured <- newChan
  assets <- testAssetsDir
  withApplication (pure (capturePathApp captured ["popup", "popup.html"] assets)) $ \port ->
    withChromiumContextWith
      defaultNewContextOptions {newContextUserAgent = Just (UserAgent "hey")}
      $ \ctx page -> do
        let
          prefix = "http://127.0.0.1:" <> pack (show port)
        _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
        setContent page "<a target=_blank rel=noopener href=\"/popup/popup.html\">link</a>"
        withAsync (liftIO (readChan captured)) $ \headerPending ->
          armContextPage ctx >>= \pagePending -> do
            click (locate page "a")
            popup <- pagePending
            waitForLoadState popup LoadStateDomContentLoaded
            ua <- evaluateText popup "() => window['initialUserAgent']"
            headers <- wait headerPending
            assertEqual "ua" "hey" ua
            assertEqual "header" (Just "hey") (lookupHeader hUserAgent headers)

contextRoutesOnClick :: IO ()
contextRoutesOnClick =
  withAssets $ \prefix page -> do
    let
      ctx = pageContext page
    _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
    setContent page "<a target=_blank rel=noopener href=\"empty.html\">link</a>"
    intercepted <- newIORef False
    contextRoute ctx (UrlMatchGlob "**/empty.html") $ \handled -> do
      continueRoute handled
      writeIORef intercepted True
    armContextPage ctx >>= \pending -> do
      click (locate page "a")
      _ <- pending
      pure ()
    assertEqual "intercepted" True =<< readIORef intercepted

inheritExtraHeaders :: IO ()
inheritExtraHeaders = do
  captured <- newChan
  withApplication (pure (captureDummyApp captured)) $ \port ->
    withChromiumContextWith
      defaultNewContextOptions
        { newContextExtraHTTPHeaders =
            Just [HttpHeader (HeaderName "foo") (HeaderValue "bar")]
        }
      $ \_ page -> do
        let
          prefix = "http://127.0.0.1:" <> pack (show port)
        _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
        withAsync (liftIO (readChan captured)) $ \pending -> do
          openPopup page (prefix <> "/dummy.html")
          headers <- wait pending
          assertEqual "foo" (Just "bar") (lookupHeader "foo" headers)

inheritOffline :: IO ()
inheritOffline =
  withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
    _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
    setOffline ctx True
    online <-
      evaluateBoolWith
        page
        "url => { const win = window.open(url); return win.navigator.onLine; }"
        (textArgument (prefix <> "/dummy.html"))
    assertEqual "offline" False online

inheritHttpCredentials :: IO ()
inheritHttpCredentials =
  withApplication (pure titleAuthApp) $ \port ->
    withChromiumContextWith
      defaultNewContextOptions
        { newContextHttpCredentials =
            Just
              HttpCredentials
                { httpCredentialsUsername = HttpUsername "user"
                , httpCredentialsPassword = HttpPassword "pass"
                , httpCredentialsOrigin = Nothing
                , httpCredentialsSend = Nothing
                }
        }
      $ \_ page -> do
        let
          prefix = "http://127.0.0.1:" <> pack (show port)
        _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
        withAsync (waitForPopup page) $ \pending -> do
          openPopup page (prefix <> "/title.html")
          popup <- wait pending
          waitForLoadState popup LoadStateDomContentLoaded
          assertEqual "title" "Woof-Woof" =<< title popup

inheritTouch :: IO ()
inheritTouch =
  withChromiumContextWith
    defaultNewContextOptions
      { newContextViewport = Just (ViewportSize (ViewportWidth 400) (ViewportHeight 500))
      , newContextHasTouch = Just True
      }
    $ \_ page -> do
      hasTouch <-
        evaluateBool
          page
          "() => { const win = window.open(''); return 'ontouchstart' in win; }"
      assertEqual "touch" True hasTouch

inheritViewport :: IO ()
inheritViewport =
  withChromiumContextWith
    defaultNewContextOptions
      { newContextViewport = Just (ViewportSize (ViewportWidth 400) (ViewportHeight 500))
      }
    $ \_ page -> do
      size <-
        evaluateNumbersWith
          page
          "() => { const win = window.open('about:blank'); return [win.innerWidth, win.innerHeight]; }"
          undefinedArgument
      assertEqual "size" [400, 500] size

windowFeaturesViewport :: IO ()
windowFeaturesViewport =
  withAssetsContextWith
    defaultNewContextOptions
      { newContextViewport = Just (ViewportSize (ViewportWidth 700) (ViewportHeight 700))
      }
    $ \prefix _ page -> do
      _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
      withAsync (waitForPopup page) $ \pending -> do
        size <-
          evaluateNumbersWith
            page
            "async () => {\
            \  const win = window.open(window.location.href, 'Title', 'toolbar=no,location=no,directories=no,status=no,menubar=no,scrollbars=yes,resizable=yes,width=600,height=300,top=0,left=0');\
            \  await new Promise(resolve => {\
            \    const interval = window.setInterval(() => {\
            \      if (win.innerWidth === 600 && win.innerHeight === 300) {\
            \        window.clearInterval(interval);\
            \        resolve(undefined);\
            \      }\
            \    }, 10);\
            \  });\
            \  return [win.innerWidth, win.innerHeight];\
            \}"
            undefinedArgument
        popup <- wait pending
        setViewportSize popup (ViewportSize (ViewportWidth 500) (ViewportHeight 400))
        waitForLoadState popup LoadStateLoad
        resized <-
          evaluateNumbersWith
            popup
            "() => [window.innerWidth, window.innerHeight]"
            undefinedArgument
        assertEqual "features" [600, 300] size
        assertEqual "resized" [500, 400] resized

contextRoutesOnWindowOpen :: IO ()
contextRoutesOnWindowOpen =
  withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
    _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
    intercepted <- newIORef False
    contextRoute ctx (UrlMatchGlob "**/empty.html") $ \handled -> do
      continueRoute handled
      writeIORef intercepted True
    withAsync (waitForPopup page) $ \pending -> do
      openPopup page (prefix <> "/empty.html")
      _ <- wait pending
      pure ()
    assertEqual "intercepted" True =<< readIORef intercepted

initScriptInProcess :: IO ()
initScriptInProcess =
  withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
    _ <- addContextInitScript ctx "window['injected'] = 123"
    _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
    injected <-
      evaluateNumber
        page
        "() => { const win = window.open('about:blank'); return win['injected']; }"
    assertEqual "injected" (123 :: Double) injected

initScriptCrossProcess :: IO ()
initScriptCrossProcess =
  withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
    _ <- addContextInitScript ctx "window['injected'] = 123"
    _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
    withAsync (waitForPopup page) $ \pending -> do
      openPopup page (replace "127.0.0.1" "localhost" prefix <> "/title.html")
      popup <- wait pending
      assertEqual "injected" (123 :: Double)
        =<< evaluateNumber popup "() => window['injected']"
      _ <- reload popup
      assertEqual "after reload" (123 :: Double)
        =<< evaluateNumber popup "() => window['injected']"

exposeFunctionOnPopup :: IO ()
exposeFunctionOnPopup =
  withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
    messages <- newIORef ([] :: [Text])
    _ <-
      contextExposeFunction ctx (BindingName "add") $ \args -> do
        modifyIORef' messages (<> ["binding"])
        add args
    onContextPage ctx $ \_ ->
      modifyIORef' messages (<> ["page"])
    _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
    added <-
      evaluateNumber
        page
        "async () => { const win = window.open('about:blank'); return win['add'](9, 4); }"
    assertEqual "sum" (13 :: Double) added
    assertEqual "order" "page|binding" . intercalate "|" =<< readIORef messages

rafNotThrottled :: IO ()
rafNotThrottled =
  withAssets $ \prefix page -> do
    _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
    withAsync (waitForPopup page) $ \pending -> do
      _ <- evaluate page "() => { window.open('about:blank'); }"
      popup <- wait pending
      withAsync (waitForRafs page 30) $ \openerRafs ->
        withAsync (waitForRafs popup 30) $ \popupRafs -> do
          wait openerRafs
          wait popupRafs

clickClosesPopup :: IO ()
clickClosesPopup =
  withAssets $ \prefix page -> do
    _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
    withAsync (waitForPopup page) $ \pending -> do
      _ <-
        evaluate
          page
          "() => { const w = window.open('about:blank'); w.document.body.innerHTML = '<button onclick=\"window.close()\">close</button>'; }"
      popup <- wait pending
      ignoreTargetClosed (click (getByRole popup (AriaRole "button") defaultGetByRoleOptions))

openPopup :: Page -> Text -> Playwright ()
openPopup page url = do
  _ <-
    evaluateWith
      page
      "url => { window['_popup'] = window.open(url); }"
      (textArgument url)
  pure ()

waitForRafs :: Page -> Double -> Playwright ()
waitForRafs page count = do
  _ <-
    evaluateNumberWith
      page
      "count => new Promise(resolve => {\
      \  const onRaf = () => {\
      \    count -= 1;\
      \    if (!count) resolve(0);\
      \    else requestAnimationFrame(onRaf);\
      \  };\
      \  requestAnimationFrame(onRaf);\
      \})"
      (numberArgument count)
  pure ()

add :: [SerializedValue] -> Playwright SerializedArgument
add args =
  case args of
    [SerializedNode (PayloadNumber left) _, SerializedNode (PayloadNumber right) _] ->
      pure (numberArgument (left + right))
    _ ->
      throwIO (EvaluateUnexpected (SerializedNode (PayloadNumber 0) Nothing))

capturePathApp :: Chan RequestHeaders -> [Text] -> FilePath -> Application
capturePathApp captured path assets request respond = do
  if pathInfo request == path
    then writeChan captured (requestHeaders request)
    else pure ()
  staticApp (defaultFileServerSettings assets) request respond

captureDummyApp :: Chan RequestHeaders -> Application
captureDummyApp captured request respond = do
  if pathInfo request == ["dummy.html"]
    then writeChan captured (requestHeaders request)
    else pure ()
  respond (responseLBS status200 [(hContentType, "text/html")] "<html></html>")

titleAuthApp :: Application
titleAuthApp request respond =
  case pathInfo request of
    ["title.html"] ->
      case lookup hAuthorization (requestHeaders request) of
        Just value
          | value == BS8.pack "Basic dXNlcjpwYXNz" ->
              respond
                ( responseLBS
                    status200
                    [(hContentType, "text/html")]
                    (LBS8.pack "<html><title>Woof-Woof</title></html>")
                )
        _ ->
          respond
            ( responseLBS
                status401
                [("WWW-Authenticate", BS8.pack "Basic realm=\"test\"")]
                LBS8.empty
            )
    _ ->
      respond (responseLBS status200 [(hContentType, "text/html")] "<html></html>")

lookupHeader :: HTTP.HeaderName -> RequestHeaders -> Maybe ByteString
lookupHeader =
  lookup

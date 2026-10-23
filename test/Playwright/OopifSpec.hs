module Playwright.OopifSpec
  ( tests
  ) where

import Control.Monad
  ( void
  )
import UnliftIO.Async
  ( concurrently_
  , wait
  , withAsync
  )
import UnliftIO.Exception
  ( throwIO
  )
import UnliftIO.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Playwright
  ( BindingName (BindingName)
  , BoundingBox (BoundingBox)
  , ColorScheme (ColorSchemeDark)
  , ConsoleMessage (consoleMessageText)
  , ElementHandle
  , EmulateMedia (emulateMediaColorScheme)
  , Frame
  , HeaderName (headerNameToText)
  , HeaderValue (headerValueToText)
  , HttpHeader (httpHeaderName, httpHeaderValue)
  , LaunchOptions (launchOptionsArgs)
  , LoadState (LoadStateDomContentLoaded, LoadStateLoad)
  , Locale (Locale)
  , NewContextOptions (newContextDeviceScaleFactor, newContextHasTouch, newContextIsMobile, newContextLocale, newContextTimezoneId, newContextUserAgent, newContextViewport)
  , Page
  , Playwright
  , Request
  , TimezoneId (TimezoneId)
  , UrlMatch (UrlMatchGlob)
  , UserAgent (UserAgent)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , addContextInitScript
  , addInitScript
  , boundingBox
  , childFrames
  , childLocator
  , click
  , clickHandle
  , contentFrameHandle
  , contextExposeFunction
  , continueRoute
  , cssPixelFromInt
  , currentUrl
  , defaultEmulateMedia
  , defaultGotoOptions
  , defaultLaunchOptions
  , defaultNewContextOptions
  , deviceScaleFactorFromInt
  , elementHandle
  , emulateMedia
  , evalOnSelector
  , evaluate
  , evaluateBool
  , evaluateNumber
  , evaluateOnElement
  , evaluateOnElementBoolWith
  , evaluateOnFrameBool
  , evaluateOnFrameNumber
  , evaluateOnFrameText
  , evaluateOnFrameWith
  , evaluateWith
  , exposeFunction
  , frameLocator
  , frames
  , framesEqual
  , goto
  , gotoFrame
  , locatorCount
  , locatorOnFrame
  , mainFrame
  , numberArgument
  , onRequest
  , onRequestFinished
  , pageClickSelector
  , pageContext
  , querySelector
  , requestFrame
  , requestHeaders
  , responseText
  , route
  , selectorFromText
  , setContentOnFrame
  , setOffline
  , textArgument
  , undefinedArgument
  , urlFromText
  , waitForConsoleWhere
  , waitForFileChooser
  , waitForFrameDetached
  , waitForLoadStateFrame
  , waitForNavigatedFrame
  , armRequest
  , waitForResponse
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadNumber)
  , SerializedValue (SerializedNode)
  )
import Playwright.Error
  ( PlaywrightError (EvaluateUnexpected)
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , requireHandle
  , withAssetsLaunchWith
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
    "oopif"
    [ testCase "should report oopif frames" $
        withOopif $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/dynamic-oopif.html")) defaultGotoOptions
          allFrames <- frames page
          assertEqual "frames" 2 (length allFrames)
          oopif <- requireOopif page
          assertEqual "href" (crossProcessPrefix prefix <> "/grid.html")
            =<< evaluateOnFrameText oopif "() => '' + location.href"
    , testCase "should handle oopif detach" $
        withOopif $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/dynamic-oopif.html")) defaultGotoOptions
          assertEqual "frames" 2 . length =<< frames page
          oopif <- requireOopif page
          assertEqual "href" (crossProcessPrefix prefix <> "/grid.html")
            =<< evaluateOnFrameText oopif "() => '' + location.href"
          withAsync (waitForFrameDetached page) $ \pending -> do
            _ <- evaluate page "() => document.querySelector('iframe').remove()"
            detached <- wait pending
            assertEqual "detached" True (framesEqual detached oopif)
    , testCase "should handle remote -> local -> remote transitions" $
        withOopif $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/dynamic-oopif.html")) defaultGotoOptions
          oopif <- requireOopif page
          assertEqual "remote" (crossProcessPrefix prefix <> "/grid.html")
            =<< evaluateOnFrameText oopif "() => '' + location.href"
          navigateOopif page oopif "() => goLocal()"
          assertEqual "local" (prefix <> "/grid.html")
            =<< evaluateOnFrameText oopif "() => '' + location.href"
          navigateOopif page oopif "() => goRemote()"
          assertEqual "remote again" (crossProcessPrefix prefix <> "/grid.html")
            =<< evaluateOnFrameText oopif "() => '' + location.href"
    , testCase "should expose function" $
        withOopif $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/dynamic-oopif.html")) defaultGotoOptions
          oopif <- requireOopif page
          _ <- exposeFunction page (BindingName "mul") multiply
          assertEqual "product" (36 :: Double)
            =<< evaluateOnFrameNumber oopif "async function() { return await window['mul'](9, 4); }"
    , testCase "should emulate media" $
        withOopif $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/dynamic-oopif.html")) defaultGotoOptions
          oopif <- requireOopif page
          assertEqual "light" False
            =<< evaluateOnFrameBool oopif "() => matchMedia('(prefers-color-scheme: dark)').matches"
          emulateMedia page defaultEmulateMedia {emulateMediaColorScheme = Just ColorSchemeDark}
          assertEqual "dark" True
            =<< evaluateOnFrameBool oopif "() => matchMedia('(prefers-color-scheme: dark)').matches"
    , testCase "should emulate offline" $
        withOopif $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/dynamic-oopif.html")) defaultGotoOptions
          oopif <- requireOopif page
          assertEqual "online" True =<< evaluateOnFrameBool oopif "() => navigator.onLine"
          setOffline (pageContext page) True
          assertEqual "offline" False =<< evaluateOnFrameBool oopif "() => navigator.onLine"
    , testCase "should support context options" $
        withAssetsLaunchWith sitePerProcess iPhoneOopif $ \prefix _ctx page -> do
          armRequest page (UrlMatchGlob "**/grid.html") >>= \pending -> do
            _ <- goto page (urlFromText (prefix <> "/dynamic-oopif.html")) defaultGotoOptions
            request <- pending
            oopif <- requireOopif page
            assertEqual "touch" True =<< evaluateOnFrameBool oopif "() => 'ontouchstart' in window"
            assertEqual
              "date"
              "Sat Nov 19 2016 13:12:34 GMT-0500 (heure normale de l’Est nord-américain)"
              =<< evaluateOnFrameText oopif "() => new Date(1479579154987).toString()"
            assertEqual "lang" "fr-CH" =<< evaluateOnFrameText oopif "() => navigator.language"
            assertEqual "ua" "UA" =<< evaluateOnFrameText oopif "() => navigator.userAgent"
            headers <- requestHeaders request
            let
              ua =
                [ headerValueToText (httpHeaderValue header)
                | header <- headers
                , Text.toLower (headerNameToText (httpHeaderName header)) == "user-agent"
                ]
            assertEqual "request ua" ["UA"] ua
    , testCase "should respect route" $
        withOopif $ \prefix page -> do
          intercepted <- newIORef False
          route page (UrlMatchGlob "**/digits/0.png") $ \handled -> do
            modifyIORef' intercepted (const True)
            continueRoute handled
          _ <- goto page (urlFromText (prefix <> "/dynamic-oopif.html")) defaultGotoOptions
          assertEqual "frames" 2 . length =<< frames page
          assertEqual "intercepted" True =<< readIORef intercepted
    , testCase "should load oopif iframes with subresources and route" $
        withOopif $ \prefix page -> do
          route page (UrlMatchGlob "**/*") continueRoute
          _ <- goto page (urlFromText (prefix <> "/dynamic-oopif.html")) defaultGotoOptions
          assertEqual "frames" 2 . length =<< frames page
    , testCase "should report main requests" $
        withOopif $ \prefix page -> do
          requestFrames <- newIORef ([] :: [Frame])
          finishedFrames <- newIORef ([] :: [Frame])
          onRequest page (recordFrame requestFrames)
          onRequestFinished page (recordFrame finishedFrames)
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let
            main = mainFrame page
          appendIframe page (crossProcessPrefix prefix <> "/empty.html")
          assertEqual "child frames" 2 . length =<< frames page
          children <- childFrames main
          child <- requireChild children
          waitForLoadStateFrame child LoadStateDomContentLoaded
          appendIframeOnFrame child (prefix <> "/empty.html")
          assertEqual "grandchild frames" 3 . length =<< frames page
          grandChildren <- childFrames child
          grandChild <- requireChild grandChildren
          waitForLoadStateFrame grandChild LoadStateDomContentLoaded
          assertRequestFrames "request" requestFrames main child grandChild
          assertRequestFrames "finished" finishedFrames main child grandChild
    , testCase "should support exposeFunction" $
        withOopif $ \prefix page -> do
          _ <- contextExposeFunction (pageContext page) (BindingName "dec") decrement
          _ <- exposeFunction page (BindingName "inc") increment
          _ <- goto page (urlFromText (prefix <> "/dynamic-oopif.html")) defaultGotoOptions
          allFrames <- frames page
          case allFrames of
            (main : oopif : _) -> do
              assertEqual "inc main" (4 :: Double) =<< evaluateOnFrameNumber main "() => window['inc'](3)"
              assertEqual "inc oopif" (5 :: Double) =<< evaluateOnFrameNumber oopif "() => window['inc'](4)"
              assertEqual "dec main" (2 :: Double) =<< evaluateOnFrameNumber main "() => window['dec'](3)"
              assertEqual "dec oopif" (3 :: Double) =<< evaluateOnFrameNumber oopif "() => window['dec'](4)"
            _ ->
              assertFailure "expected main and oopif frames"
    , testCase "should support addInitScript" $
        withOopif $ \prefix page -> do
          _ <- addContextInitScript (pageContext page) "window['bar'] = 17"
          addInitScript page "window['foo'] = 42"
          _ <- goto page (urlFromText (prefix <> "/dynamic-oopif.html")) defaultGotoOptions
          allFrames <- frames page
          case allFrames of
            (main : oopif : _) -> do
              assertEqual "foo main" (42 :: Double) =<< evaluateOnFrameNumber main "() => window['foo']"
              assertEqual "foo oopif" (42 :: Double) =<< evaluateOnFrameNumber oopif "() => window['foo']"
              assertEqual "bar main" (17 :: Double) =<< evaluateOnFrameNumber main "() => window['bar']"
              assertEqual "bar oopif" (17 :: Double) =<< evaluateOnFrameNumber oopif "() => window['bar']"
            _ ->
              assertFailure "expected main and oopif frames"
    , testCase "should click a button when it overlays oopif" $
        withOopif $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/button-overlay-oopif.html")) defaultGotoOptions
          pageClickSelector page (selectorFromText "button")
          assertEqual "clicked" True =<< evaluateBool page "() => window['BUTTON_CLICKED']"
    , testCase "ElementHandle.boundingBox() should work" $
        withOopif $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/dynamic-oopif.html")) defaultGotoOptions
          styleIframe page "520px" "520px" "42px" "17px"
          oopif <- requireOopif page
          oopifUrl <- currentUrl oopif
          _ <- gotoFrame oopif oopifUrl defaultGotoOptions
          handle1 <-
            requireHandle
              =<< querySelectorOn oopif page ".box:nth-of-type(13)"
          box1 <- boundingBox handle1
          assertEqual
            "remote box"
            ( Just
                ( BoundingBox
                    (cssPixelFromInt 142)
                    (cssPixelFromInt 67)
                    (cssPixelFromInt 50)
                    (cssPixelFromInt 50)
                )
            )
            box1
          navigateOopif page oopif "() => goLocal()"
          handle2 <-
            requireHandle
              =<< querySelectorOn oopif page ".box:nth-of-type(13)"
          box2 <- boundingBox handle2
          assertEqual
            "local box"
            ( Just
                ( BoundingBox
                    (cssPixelFromInt 142)
                    (cssPixelFromInt 67)
                    (cssPixelFromInt 50)
                    (cssPixelFromInt 50)
                )
            )
            box2
    , testCase "should click" $
        withOopif $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/dynamic-oopif.html")) defaultGotoOptions
          styleIframe page "500px" "500px" "102px" "117px"
          oopif <- requireOopif page
          oopifUrl <- currentUrl oopif
          _ <- gotoFrame oopif oopifUrl defaultGotoOptions
          handle <-
            requireHandle
              =<< querySelectorOn oopif page ".box:nth-of-type(13)"
          _ <-
            evaluateOnElement
              handle
              "div => div.addEventListener('click', () => window['_clicked'] = true, false)"
          clickHandle handle
          assertEqual "clicked" True
            =<< evaluateOnElementBoolWith handle "() => window['_clicked']" undefinedArgument
    , testCase "contentFrame should work" $
        withOopif $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/dynamic-oopif.html")) defaultGotoOptions
          assertEqual "frames" 2 . length =<< frames page
          assertEqual "locator count" 200
            =<< locatorCount (childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "div"))
          iframe <- requireHandle =<< querySelector page (selectorFromText "iframe")
          content <- contentFrameHandle iframe
          case content of
            Nothing ->
              assertFailure "expected content frame"
            Just frame ->
              assertEqual "handle count" 200
                =<< locatorCount (locatorOnFrame page frame (selectorFromText "div"))
    , testCase "should emit filechooser event for iframe" $
        withOopif $ \prefix page -> do
          withAsync (waitForFileChooser page) $ \pending -> do
            _ <- goto page (urlFromText (prefix <> "/dynamic-oopif.html")) defaultGotoOptions
            oopif <- requireOopif page
            setContentOnFrame oopif "<input type=file>"
            click (locatorOnFrame page oopif (selectorFromText "input"))
            _ <- wait pending
            pure ()
    , testCase "should be able to click in iframe" $
        withOopif $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/dynamic-oopif.html")) defaultGotoOptions
          oopif <- requireOopif page
          setContentOnFrame oopif "<button onclick=\"console.log('clicked')\">OK</button>"
          withAsync (fmap consoleMessageText (waitForConsoleWhere page (const True))) $ \pending -> do
            click (locatorOnFrame page oopif (selectorFromText "button"))
            assertEqual "clicked" "clicked" =<< wait pending
    , testCase "should not throw on exposeFunction when oopif detaches" $
        withOopif $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/dynamic-oopif.html")) defaultGotoOptions
          concurrently_
            (void (exposeFunction page (BindingName "myFunc") (\_ -> pure (numberArgument 2022))))
            (void (evaluate page "() => document.querySelector('iframe').remove()"))
          assertEqual "result" (2022 :: Double) =<< evaluateNumber page "() => window.myFunc()"
    , testCase "should intercept response body from oopif" $
        withOopif $ \prefix page -> do
          withAsync (waitForResponse page (UrlMatchGlob "**/grid.html")) $ \pending -> do
            _ <- goto page (urlFromText (prefix <> "/dynamic-oopif.html")) defaultGotoOptions
            body <- responseText =<< wait pending
            assertBool "body" (not (Text.null body))
    ]

sitePerProcess :: LaunchOptions
sitePerProcess =
  defaultLaunchOptions {launchOptionsArgs = Just ["--site-per-process"]}

withOopif :: (Text -> Page -> Playwright a) -> IO a
withOopif action =
  withAssetsLaunchWith sitePerProcess defaultNewContextOptions $ \prefix _ctx page ->
    action prefix page

crossProcessPrefix :: Text -> Text
crossProcessPrefix =
  Text.replace "127.0.0.1" "localhost"

requireOopif :: Page -> Playwright Frame
requireOopif page = do
  allFrames <- frames page
  case allFrames of
    (_ : frame : _) ->
      pure frame
    _ ->
      assertFailure ("expected an oopif frame, got " <> show (length allFrames))

requireChild :: [Frame] -> Playwright Frame
requireChild children =
  case children of
    frame : _ ->
      pure frame
    [] ->
      assertFailure "expected a child frame"

navigateOopif :: Page -> Frame -> Text -> Playwright ()
navigateOopif page frame script =
  withAsync
    ( do
        waitForNavigatedFrame page frame
        waitForLoadStateFrame frame LoadStateLoad
    )
    $ \pending -> do
      _ <- evaluate page script
      wait pending

styleIframe :: Page -> Text -> Text -> Text -> Text -> Playwright ()
styleIframe page width height marginLeft marginTop = do
  _ <-
    evalOnSelector
      page
      (selectorFromText "iframe")
      ( "iframe => { iframe.style.width = '"
          <> width
          <> "'; iframe.style.height = '"
          <> height
          <> "'; iframe.style.marginLeft = '"
          <> marginLeft
          <> "'; iframe.style.marginTop = '"
          <> marginTop
          <> "'; }"
      )
  pure ()

querySelectorOn :: Frame -> Page -> Text -> Playwright (Maybe ElementHandle)
querySelectorOn frame page selector =
  elementHandle (locatorOnFrame page frame (selectorFromText selector))

appendIframe :: Page -> Text -> Playwright ()
appendIframe page src = do
  _ <-
    evaluateWith
      page
      "url => { const iframe = document.createElement('iframe'); iframe.src = url; document.body.appendChild(iframe); return new Promise(f => iframe.onload = f); }"
      (textArgument src)
  pure ()

appendIframeOnFrame :: Frame -> Text -> Playwright ()
appendIframeOnFrame frame src = do
  _ <-
    evaluateOnFrameWith
      frame
      "url => { const iframe = document.createElement('iframe'); iframe.src = url; document.body.appendChild(iframe); return new Promise(f => iframe.onload = f); }"
      (textArgument src)
  pure ()

recordFrame :: IORef [Frame] -> Request -> Playwright ()
recordFrame found request = do
  maybeFrame <- requestFrame request
  case maybeFrame of
    Nothing ->
      pure ()
    Just frame ->
      modifyIORef' found (<> [frame])

assertRequestFrames :: Text -> IORef [Frame] -> Frame -> Frame -> Frame -> Playwright ()
assertRequestFrames label found main child grandChild = do
  got <- readIORef found
  case got of
    (firstFrame : secondFrame : thirdFrame : _) -> do
      assertEqual (show label <> " main") True (framesEqual firstFrame main)
      assertEqual (show label <> " child") True (framesEqual secondFrame child)
      assertEqual (show label <> " grandchild") True (framesEqual thirdFrame grandChild)
    _ ->
      assertFailure (show label <> " expected three frames, got " <> show (length got))

iPhoneOopif :: NewContextOptions
iPhoneOopif =
  defaultNewContextOptions
    { newContextUserAgent = Just (UserAgent "UA")
    , newContextViewport = Just (ViewportSize (ViewportWidth 375) (ViewportHeight 667))
    , newContextDeviceScaleFactor = Just (deviceScaleFactorFromInt 2)
    , newContextIsMobile = Just True
    , newContextHasTouch = Just True
    , newContextTimezoneId = Just (TimezoneId "America/Jamaica")
    , newContextLocale = Just (Locale "fr-CH")
    }

multiply :: [SerializedValue] -> Playwright SerializedArgument
multiply args =
  case args of
    [SerializedNode (PayloadNumber left) _, SerializedNode (PayloadNumber right) _] ->
      pure (numberArgument (left * right))
    _ ->
      throwIO (EvaluateUnexpected (SerializedNode (PayloadNumber 0) Nothing))

increment :: [SerializedValue] -> Playwright SerializedArgument
increment args =
  case args of
    [SerializedNode (PayloadNumber value) _] ->
      pure (numberArgument (value + 1))
    _ ->
      throwIO (EvaluateUnexpected (SerializedNode (PayloadNumber 0) Nothing))

decrement :: [SerializedValue] -> Playwright SerializedArgument
decrement args =
  case args of
    [SerializedNode (PayloadNumber value) _] ->
      pure (numberArgument (value - 1))
    _ ->
      throwIO (EvaluateUnexpected (SerializedNode (PayloadNumber 0) Nothing))

module Playwright.PageBasicSpec
  ( tests
  ) where

import Data.Maybe
  ( isNothing
  )
import Data.Text
  ( Text
  , isInfixOf
  , isPrefixOf
  )
import Playwright
  ( Frame
  , Key (Key)
  , Page
  , Playwright
  , armConsole
  , armDomContentLoadedEvent
  , armLoadEvent
  , abortRoute
  , armPopup
  , consoleMessageText
  , defaultGotoOptions
  , evaluate
  , evaluateBool
  , evaluateOnFrameText
  , evaluateText
  , frameByName
  , frameByUrl
  , UrlMatch (UrlMatchGlob, UrlMatchRegex)
  , frames
  , currentUrl
  , framesEqual
  , goto
  , locatorOnFrame
  , onDomContentLoadedEvent
  , onLoadEvent
  , onPageClose
  , opener
  , pagesEqual
  , press
  , route
  , selectorFromText
  , setContent
  , title
  , url
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , assertProtocolContainsAll
  , locate
  , withAssetPage
  , withAssets
  , withChromiumPage
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
import UnliftIO.MVar
  ( newEmptyMVar
  , putMVar
  , takeMVar
  , tryPutMVar
  )

tests :: TestTree
tests =
  testGroup
    "page-basic"
    [ testCase "async stacks should work" $
        withAssets $ \prefix page -> do
          -- Upstream drops the socket and checks the JS Error.stack names the test file. A
          -- Haskell exception has no JS stack, so this aborts the request instead and checks
          -- the driver error names the network failure and the navigation call log.
          route page (UrlMatchGlob (prefix <> "/empty.html")) abortRoute
          assertProtocolContainsAll
            ["net::ERR_FAILED", "navigating to"]
            (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
    , testCase "page.press should work" $
        withAssetPage "input/textarea.html" $ \page -> do
          press (locate page "textarea") (Key "a")
          assertEqual "value" "a" =<< evaluateText page "() => document.querySelector('textarea').value"
    , testCase "page.press should work for Enter" $
        withChromiumPage $ \page -> do
          setContent page "<input onkeypress=\"console.log('press')\"></input>"
          takeMessage <- armConsole page
          press (locate page "input") (Key "Enter")
          message <- takeMessage
          assertEqual "console text" "press" (consoleMessageText message)
    , testCase "frame.press should work" $
        withAssets $ \prefix page -> do
          setContent
            page
            ("<iframe name=inner src=\"" <> prefix <> "/input/textarea.html\"></iframe>")
          frame <- requireNamedFrame page "inner"
          press (locatorOnFrame page frame (selectorFromText "textarea")) (Key "a")
          assertEqual "value" "a" =<< evaluateOnFrameText frame "() => document.querySelector('textarea').value"
    , testCase "has navigator.webdriver set to true" $
        withChromiumPage $ \page -> do
          assertEqual "webdriver" True =<< evaluateBool page "() => navigator.webdriver"
    , testCase "should have sane user agent" $
        withChromiumPage $ \page -> do
          userAgent <- evaluateText page "() => navigator.userAgent"
          assertBool "Mozilla" ("Mozilla/5.0" `isPrefixOf` userAgent)
          assertBool "AppleWebKit" ("AppleWebKit/" `isInfixOf` userAgent)
          assertBool "Chrome" ("Chrome/" `isInfixOf` userAgent)
          assertBool "Safari" ("Safari/" `isInfixOf` userAgent)
    , testCase "should fire load when expected" $
        withChromiumPage $ \page -> do
          takeLoad <- armLoadEvent page
          _ <- goto page (urlFromText "about:blank") defaultGotoOptions
          takeLoad
    , testCase "should fire domcontentloaded when expected" $
        withChromiumPage $ \page -> do
          takeDomContentLoaded <- armDomContentLoadedEvent page
          _ <- goto page (urlFromText "about:blank") defaultGotoOptions
          takeDomContentLoaded
    , testCase "should pass self as argument to domcontentloaded event" $
        withChromiumPage $ \page -> do
          received <- newEmptyMVar
          onDomContentLoadedEvent page (\eventPage -> () <$ tryPutMVar received eventPage)
          _ <- goto page (urlFromText "about:blank") defaultGotoOptions
          eventPage <- takeMVar received
          assertEqual "event page is the page" True (pagesEqual eventPage page)
    , testCase "should pass self as argument to load event" $
        withChromiumPage $ \page -> do
          received <- newEmptyMVar
          onLoadEvent page (\eventPage -> () <$ tryPutMVar received eventPage)
          _ <- goto page (urlFromText "about:blank") defaultGotoOptions
          eventPage <- takeMVar received
          assertEqual "event page is the page" True (pagesEqual eventPage page)
    , testCase "should provide access to the opener page" $
        withChromiumPage $ \page -> do
          popup <- openPopup page "() => { window.open('about:blank'); }"
          matched <- opener popup
          case matched of
            Nothing ->
              assertFailure "expected an opener"
            Just parent ->
              assertEqual "opener identity" True (pagesEqual parent page)
    , testCase "page.url should work" $
        withAssets $ \prefix page -> do
          assertEqual "start" "about:blank" . urlToText =<< url page
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "after goto" (prefix <> "/empty.html") . urlToText =<< url page
    , testCase "page.url should include hashes" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html#hash")) defaultGotoOptions
          assertEqual "hash" (prefix <> "/empty.html#hash") . urlToText =<< url page
          _ <- evaluate page "() => { window.location.hash = 'dynamic'; }"
          assertEqual "dynamic hash" (prefix <> "/empty.html#dynamic") . urlToText =<< url page
    , testCase "page.title should return the page title" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/title.html")) defaultGotoOptions
          assertEqual "title" "Woof-Woof" =<< title page
    , testCase "page.title should not throw during navigation" $
        withAssets $ \prefix page -> do
          setContent page "<title>hello</title>"
          withAsync (goto page (urlFromText (prefix <> "/title.html")) defaultGotoOptions) $ \navigation -> do
            during <- title page
            assertBool "during navigation" (titleDuringNavigation during)
            _ <- wait navigation
            assertEqual "after" "Woof-Woof" =<< title page
    , testCase "page.close should work with window.close" $
        withChromiumPage $ \page -> do
          popup <- openPopup page "() => { window.newPage = window.open('about:blank'); }"
          closed <- newEmptyMVar
          onPageClose popup (putMVar closed ())
          _ <- evaluate page "() => { window.newPage.close(); }"
          takeMVar closed
    , testCase "page.frame should respect name" $
        withChromiumPage $ \page -> do
          setContent page "<iframe name=target></iframe>"
          missing <- frameByName page "bogus"
          assertEqual "bogus" True (isNothing missing)
          found <- frameByName page "target"
          allFrames <- frames page
          case (found, drop 1 allFrames) of
            (Just frame, child : _) ->
              assertEqual "same frame" True (framesEqual frame child)
            _ ->
              assertFailure "expected the named frame"
    , testCase "page.frame should respect url" $
        withChromiumPage $ \page -> do
          setContent page "<iframe src=\"about:blank\"></iframe>"
          missing <- frameByUrl page (UrlMatchRegex "bogus" "")
          assertEqual "bogus" True (isNothing missing)
          found <- frameByUrl page (UrlMatchRegex "blank" "")
          assertEqual "blank" (Just "about:blank") . fmap urlToText =<< traverse currentUrl found
    ]

-- | Opens a popup with the script and returns it once it exists.
openPopup :: Page -> Text -> Playwright Page
openPopup page script = do
  takePopup <- armPopup page
  _ <- evaluate page script
  takePopup

-- | Looks up the iframe by name, failing the test if it is missing.
requireNamedFrame :: Page -> Text -> Playwright Frame
requireNamedFrame page name = do
  found <- frameByName page name
  case found of
    Nothing ->
      assertFailure ("expected a frame named " <> show name)
    Just frame ->
      pure frame

-- | While a navigation is in flight, the title is the old one, a loading
-- placeholder, or the new one. It never errors.
titleDuringNavigation :: Text -> Bool
titleDuringNavigation current =
  current `elem` ["hello", "Woof-Woof", ""]
    || ("Loading http" `isPrefixOf` current && "title.html" `isInfixOf` current)

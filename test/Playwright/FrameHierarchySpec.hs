module Playwright.FrameHierarchySpec
  ( tests
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import Data.IORef
  ( IORef
  , modifyIORef
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.List
  ( sortBy
  )
import Data.Ord
  ( comparing
  )
import Data.Text
  ( Text
  , isInfixOf
  , replace
  , toLower
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , responseLBS
  )
import Playwright
  ( ConsoleMessage (consoleMessageText)
  , Frame
  , Playwright
  , armConsole
  , childFrames
  , currentName
  , currentUrl
  , defaultGotoOptions
  , evaluate
  , frameByName
  , framePage
  , frames
  , framesEqual
  , goto
  , isDetached
  , mainFrame
  , onFrameAttached
  , onFrameDetached
  , onFrameNavigated
  , pagesEqual
  , parentFrame
  , setContent
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( attachFrame
  , withAssets
  , withChromiumPage
  , withLocalApp
  , assertBool
  , assertEqual
  , assertFailure
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
    "frame-hierarchy"
    [ testCase "should handle nested frames" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/nested-frames.html")) defaultGotoOptions
          dumped <- dumpFrames (mainFrame page) ""
          assertEqual
            "tree"
            [ prefix <> "/frames/nested-frames.html"
            , "    " <> prefix <> "/frames/frame.html (aframe)"
            , "    " <> prefix <> "/frames/two-frames.html (2frames)"
            , "        " <> prefix <> "/frames/frame.html (dos)"
            , "        " <> prefix <> "/frames/frame.html (uno)"
            ]
            dumped
    , testCase "page.frame should respect name" $
        withChromiumPage $ \page -> do
          setContent page "<iframe name=target></iframe>"
          missing <- frameByName page "bogus"
          found <- frameByName page "target"
          children <- childFrames (mainFrame page)
          assertEqual "missing" True (maybe True (const False) missing)
          assertEqual "found" True (maybe False (const True) found)
          assertEqual "one child" 1 (length children)
    , testCase "page.frame should respect url" $
        withAssets $ \prefix page -> do
          setContent page ("<iframe src=\"" <> prefix <> "/empty.html\"></iframe>")
          allFrames <- frames page
          urls <- traverse (fmap urlToText . currentUrl) allFrames
          let matching = filter ("empty.html" `isInfixOf`) urls
          assertEqual "iframe url" [prefix <> "/empty.html"] matching
    , testCase "should report frame.name()" $
        withAssets $ \prefix page -> do
          _ <- attachFrame page "theFrameId" (prefix <> "/empty.html")
          _ <-
            evaluate
              page
              ( "() => new Promise(x => { const frame = document.createElement('iframe'); frame.name = 'theFrameName'; frame.src = '"
                  <> prefix
                  <> "/empty.html'; document.body.appendChild(frame); frame.onload = () => x(true); })"
              )
          -- frames lists children in no fixed order, so compare sorted names.
          names <- traverse currentName =<< frames page
          assertEqual "names" ["", "theFrameId", "theFrameName"] (sortBy (comparing id) names)
    , testCase "should report frame.parent()" $
        withAssets $ \prefix page -> do
          _ <- attachFrame page "frame1" (prefix <> "/empty.html")
          _ <- attachFrame page "frame2" (prefix <> "/empty.html")
          parents <- traverse parentFrame =<< frames page
          case parents of
            [Nothing, Just first, Just second] -> do
              assertEqual "frame1 parent" True (framesEqual first (mainFrame page))
              assertEqual "frame2 parent" True (framesEqual second (mainFrame page))
            _ ->
              assertFailure ("unexpected parents: " <> show (length parents))
    , testCase "should send events when frames are manipulated dynamically" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          attached <- liftIO (newIORef [])
          onFrameAttached page (recordFrame attached)
          _ <- attachFrame page "frame1" (prefix <> "/frames/frame.html")
          attachedFrames <- liftIO (readIORef attached)
          attachedUrls <- traverse (fmap urlToText . currentUrl) attachedFrames
          assertEqual "attached" [prefix <> "/frames/frame.html"] attachedUrls

          navigated <- liftIO (newIORef [])
          onFrameNavigated page (recordFrame navigated)
          _ <-
            evaluate
              page
              "() => new Promise(x => { const frame = document.getElementById('frame1'); frame.src = './empty.html'; frame.onload = x; })"
          navigatedFrames <- liftIO (readIORef navigated)
          navigatedUrls <- traverse (fmap urlToText . currentUrl) navigatedFrames
          assertEqual "navigated" [prefix <> "/empty.html"] navigatedUrls

          detached <- liftIO (newIORef [])
          onFrameDetached page (recordFrame detached)
          _ <- evaluate page "() => document.getElementById('frame1').remove()"
          detachedFrames <- liftIO (readIORef detached)
          assertEqual "detached count" 1 (length detachedFrames)
          states <- traverse isDetached detachedFrames
          assertEqual "isDetached" [True] states
    , testCase "should not send attach/detach events for main frame" $
        withAssets $ \prefix page -> do
          attached <- liftIO (newIORef [])
          detached <- liftIO (newIORef [])
          onFrameAttached page (recordFrame attached)
          onFrameDetached page (recordFrame detached)
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          attachedCount <- countFrames attached
          detachedCount <- countFrames detached
          assertEqual "attached" 0 attachedCount
          assertEqual "detached" 0 detachedCount
    , testCase "should detach child frames on navigation" $
        withAssets $ \prefix page -> do
          attached <- liftIO (newIORef [])
          detached <- liftIO (newIORef [])
          navigated <- liftIO (newIORef [])
          onFrameAttached page (recordFrame attached)
          onFrameDetached page (recordFrame detached)
          onFrameNavigated page (recordFrame navigated)
          _ <- goto page (urlFromText (prefix <> "/frames/nested-frames.html")) defaultGotoOptions
          assertCounts "nested-frames load" (4, 0, 5) attached detached navigated
          clearFrames attached
          clearFrames detached
          clearFrames navigated
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertCounts "empty page load" (0, 4, 1) attached detached navigated
    , testCase "should support framesets" $
        withAssets $ \prefix page -> do
          attached <- liftIO (newIORef [])
          detached <- liftIO (newIORef [])
          navigated <- liftIO (newIORef [])
          onFrameAttached page (recordFrame attached)
          onFrameDetached page (recordFrame detached)
          onFrameNavigated page (recordFrame navigated)
          _ <- goto page (urlFromText (prefix <> "/frames/frameset.html")) defaultGotoOptions
          assertCounts "frameset load" (4, 0, 5) attached detached navigated
          clearFrames attached
          clearFrames detached
          clearFrames navigated
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertCounts "empty page load" (0, 4, 1) attached detached navigated
    , testCase "should persist mainFrame on cross-process navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let
            before = mainFrame page
            cross = replace "127.0.0.1" "localhost" prefix
          _ <- goto page (urlFromText (cross <> "/empty.html")) defaultGotoOptions
          assertEqual "same main frame" True (framesEqual (mainFrame page) before)
    , testCase "should report frame from-inside shadow DOM" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/shadow.html")) defaultGotoOptions
          _ <-
            evaluate
              page
              ( "() => new Promise(x => { const frame = document.createElement('iframe'); frame.src = '"
                  <> prefix
                  <> "/empty.html'; document.body.shadowRoot.appendChild(frame); frame.onload = () => x(true); })"
              )
          allFrames <- frames page
          assertEqual "frame count" 2 (length allFrames)
          url <- fmap urlToText (currentUrl (allFrames !! 1))
          assertEqual "shadow frame url" (prefix <> "/empty.html") url
    , testCase "should report different frame instance when frame re-attaches" $
        withAssets $ \prefix page -> do
          frame1 <- attachFrame page "frame1" (prefix <> "/empty.html")
          _ <-
            evaluate
              page
              "() => { window['frame'] = document.querySelector('#frame1'); window['frame'].remove(); }"
          detachedBefore <- isDetached frame1
          assertEqual "detached after remove" True detachedBefore
          attached <- liftIO (newIORef [])
          onFrameAttached page (recordFrame attached)
          -- The load event resolves after the attach, so the attach is recorded before this returns.
          _ <-
            evaluate
              page
              "() => new Promise(x => { const frame = window['frame']; frame.onload = () => x(true); document.body.appendChild(frame); })"
          frame2s <- liftIO (readIORef attached)
          case frame2s of
            [frame2] -> do
              detachedAfter <- isDetached frame2
              assertEqual "detached after reattach" False detachedAfter
              assertEqual "different instance" False (framesEqual frame1 frame2)
            _ ->
              assertFailure ("unexpected attach count: " <> show (length frame2s))
    , testCase "should refuse to display x-frame-options:deny iframe" $
        withAssets $ \prefix page -> withLocalApp denyFrameApp $ \denyPrefix -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          -- Armed before setContent so the refusal message cannot arrive unseen.
          refusal <- armConsole page
          setContent
            page
            ("<iframe src=\"" <> replace "127.0.0.1" "localhost" denyPrefix <> "/x-frame-options-deny.html\"></iframe>")
          message <- refusal
          let text = toLower (consoleMessageText message)
          assertBool "refusal text" ("refused to display" `isInfixOf` text)
          assertBool "refusal reason" ("'x-frame-options' to 'deny'" `isInfixOf` text)
    , testCase "should return frame.page()" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/one-frame.html")) defaultGotoOptions
          mainPage <- framePage (mainFrame page)
          assertEqual "main frame page" True (pagesEqual mainPage page)
          children <- childFrames (mainFrame page)
          assertEqual "child frame count" (1 :: Int) (length children)
          case children of
            [child] -> do
              childPage <- framePage child
              assertEqual "child frame page" True (pagesEqual childPage page)
            _ -> assertFailure "expected one child frame"
    ]

-- | Serves the page with X-Frame-Options: DENY, so a frame cannot display it.
denyFrameApp :: Application
denyFrameApp _ respond =
  respond
    ( responseLBS
        status200
        [ (hContentType, "text/html")
        , ("X-Frame-Options", "DENY")
        ]
        "<!DOCTYPE html><html><head><title>login</title></head><body style=\"background-color: red;\"><p>dangerous login page</p></body></html>"
    )

recordFrame :: IORef [Frame] -> Frame -> Playwright ()
recordFrame ref frame = liftIO (modifyIORef ref (<> [frame]))

countFrames :: IORef [Frame] -> Playwright Int
countFrames ref = length <$> liftIO (readIORef ref)

clearFrames :: IORef [Frame] -> Playwright ()
clearFrames ref = liftIO (writeIORef ref [])

-- | Checks the attached, detached and navigated counts for one load, as the
-- upstream spec does with three arrays.
assertCounts :: String -> (Int, Int, Int) -> IORef [Frame] -> IORef [Frame] -> IORef [Frame] -> Playwright ()
assertCounts label (attachedWant, detachedWant, navigatedWant) attached detached navigated = do
  attachedCount <- countFrames attached
  detachedCount <- countFrames detached
  navigatedCount <- countFrames navigated
  assertEqual (label <> " attached") attachedWant attachedCount
  assertEqual (label <> " detached") detachedWant detachedCount
  assertEqual (label <> " navigated") navigatedWant navigatedCount

dumpFrames :: Frame -> Text -> Playwright [Text]
dumpFrames frame indent = do
  urlText <- fmap urlToText (currentUrl frame)
  name <- currentName frame
  let description =
        if name == ""
          then indent <> urlText
          else indent <> urlText <> " (" <> name <> ")"
  children <- childFrames frame
  sorted <- sortChildren children
  nested <- traverse (\child -> dumpFrames child ("    " <> indent)) sorted
  pure (description : concat nested)

sortChildren :: [Frame] -> Playwright [Frame]
sortChildren children = do
  keyed <- traverse keyFrame children
  pure (fmap (\(_, _, child) -> child) (sortBy (comparing (\(urlText, name, _) -> (urlText, name))) keyed))

keyFrame :: Frame -> Playwright (Text, Text, Frame)
keyFrame frame = do
  urlText <- fmap urlToText (currentUrl frame)
  name <- currentName frame
  pure (urlText, name, frame)

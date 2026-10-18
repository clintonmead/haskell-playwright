module Playwright.PageSetContentSpec
  ( tests
  ) where

import Control.Monad
  ( forM_
  )
import Data.List
  ( isInfixOf
  )
import Data.Text
  ( Text
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  , status404
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
import Playwright
  ( Frame
  , LoadState (LoadStateCommit, LoadStateDomContentLoaded)
  , Page
  , Playwright
  , SetContentOptions (setContentOptionsTimeout, setContentOptionsWaitUntil)
  , content
  , frameContent
  , defaultGotoOptions
  , defaultSetContentOptions
  , evaluateText
  , frames
  , goto
  , setContent
  , setContentWith
  , setDefaultNavigationTimeout
  , urlFromText
  , waitForTimeout
  )
import Playwright.Error
  ( PlaywrightError
  )
import Playwright.Harness
  ( assertEqual
  , assertFailure
  , assertProtocolContains
  , assertStillPending
  , requireTimeout
  , withAssets
  , withChromiumPage
  , withLocalApp
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
  ( try
  )
import UnliftIO.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , takeMVar
  )

tests :: TestTree
tests =
  testGroup
    "page-set-content"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          setContent page "<div>hello</div>"
          result <- content page
          assertEqual "content" expectedOutput result
    , testCase "should work with domcontentloaded" $
        withChromiumPage $ \page -> do
          setContentWith
            page
            "<div>hello</div>"
            defaultSetContentOptions {setContentOptionsWaitUntil = Just LoadStateDomContentLoaded}
          assertEqual "content" expectedOutput =<< content page
    , testCase "should work with commit" $
        withChromiumPage $ \page -> do
          setContentWith
            page
            "<div>hello</div>"
            defaultSetContentOptions {setContentOptionsWaitUntil = Just LoadStateCommit}
          assertEqual "content" expectedOutput =<< content page
    , testCase "should work with doctype" $
        withChromiumPage $ \page -> do
          let
            doctype :: Text
            doctype = "<!DOCTYPE html>"
          setContent page (doctype <> "<div>hello</div>")
          result <- content page
          assertEqual "content" (doctype <> expectedOutput) result
    , testCase "should work with HTML 4 doctype" $
        withChromiumPage $ \page -> do
          let
            doctype :: Text
            doctype =
              "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"
          setContent page (doctype <> "<div>hello</div>")
          result <- content page
          assertEqual "content" (doctype <> expectedOutput) result
    , testCase "should respect timeout" $ do
        release <- newEmptyMVar
        withChromiumPage $ \page ->
          withLocalApp (stalledImageApp release) $ \prefix -> do
            timeout <- requireTimeout 1
            assertProtocolContains
              "Timeout 1ms exceeded"
              ( setContentWith
                  page
                  (imageHtml prefix)
                  defaultSetContentOptions {setContentOptionsTimeout = Just timeout}
              )
            putMVar release ()
    , testCase "should respect default navigation timeout" $ do
        release <- newEmptyMVar
        withChromiumPage $ \page ->
          withLocalApp (stalledImageApp release) $ \prefix -> do
            timeout <- requireTimeout 1
            setDefaultNavigationTimeout page timeout
            assertProtocolContains
              "Timeout 1ms exceeded"
              (setContent page (imageHtml prefix))
            putMVar release ()
    , testCase "should await resources to load" $ do
        arrived <- newEmptyMVar
        release <- newEmptyMVar
        withChromiumPage $ \page ->
          withLocalApp (signalingImageApp arrived release) $ \prefix ->
            withAsync (setContent page (imageHtml prefix)) $ \pending -> do
              takeMVar arrived
              assertStillPending pending
              putMVar release ()
              wait pending
    , testCase "should work fast enough" $
        withChromiumPage $ \page ->
          forM_ [1 .. 20 :: Int] $ \_ ->
            setContent page "<div>yo</div>"
    , testCase "should work with tricky content" $
        withChromiumPage $ \page -> do
          setContent page "<div>hello world</div>\x7F"
          assertEqual "text" "hello world" =<< textOfFirst page "div"
    , testCase "should work with accents" $
        withChromiumPage $ \page -> do
          setContent page "<div>aberración</div>"
          assertEqual "text" "aberración" =<< textOfFirst page "div"
    , testCase "should work with emojis" $
        withChromiumPage $ \page -> do
          setContent page "<div>🐥</div>"
          assertEqual "text" "🐥" =<< textOfFirst page "div"
    , testCase "should work with newline" $
        withChromiumPage $ \page -> do
          setContent page "<div>\n</div>"
          assertEqual "text" "\n" =<< textOfFirst page "div"
    , testCase "content() should throw nice error during navigation" $
        withAssets $ \prefix page -> do
          let
            emptyOutput :: Text
            emptyOutput = "<html><head></head><body></body></html>"
          forM_ [0, 20 .. 180 :: Int] $ \ms -> do
            setContent page "<div>hello</div>"
            timeout <- requireTimeout (fromIntegral ms)
            withAsync (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions) $ \navigation -> do
              waitForTimeout page timeout
              outcome <- try (content page)
              _ <- wait navigation
              case outcome of
                Right html ->
                  assertEqual "content" True (html == expectedOutput || html == emptyOutput)
                Left err ->
                  assertEqual "error" True
                    ("Unable to retrieve content because the page is navigating and changing the content."
                       `isInfixOf` show (err :: PlaywrightError))
    , testCase "should return empty content there is no iframe src" $
        withChromiumPage $ \page -> do
          setContent page "<iframe src=\"javascript:console.log(1)\"></iframe>"
          allFrames <- frames page
          assertEqual "frame count" 2 (length allFrames)
          child <- requireChild allFrames
          assertEqual "frame content" "<html><head></head><body></body></html>" =<< frameContent child
    ]

expectedOutput :: Text
expectedOutput = "<html><head></head><body><div>hello</div></body></html>"

-- | Reads the text of the first element matching the selector in the main frame.
textOfFirst :: Page -> Text -> Playwright Text
textOfFirst page selector =
  evaluateText page ("() => document.querySelector('" <> selector <> "').textContent")

requireChild :: [Frame] -> Playwright Frame
requireChild allFrames =
  case drop 1 allFrames of
    child : _ ->
      pure child
    [] ->
      assertFailure "expected a child frame"

-- | A page whose only resource is an image served by the local app.
imageHtml :: Text -> Text
imageHtml prefix =
  "<img src=\"" <> prefix <> "/img.png\"></img>"

-- | Serves the image only after the test releases the MVar, so a load that is
-- still waiting on it can be timed out.
stalledImageApp :: MVar () -> Application
stalledImageApp release request respond =
  case pathInfo request of
    ["img.png"] -> do
      takeMVar release
      respond (responseLBS status200 [(hContentType, "image/png")] "")
    _ ->
      respond (responseLBS status404 [] "")

-- | Like 'stalledImageApp', but first tells the test the request arrived.
signalingImageApp :: MVar () -> MVar () -> Application
signalingImageApp arrived release request respond =
  case pathInfo request of
    ["img.png"] -> do
      putMVar arrived ()
      takeMVar release
      respond (responseLBS status200 [(hContentType, "image/png")] "")
    _ ->
      respond (responseLBS status404 [] "")

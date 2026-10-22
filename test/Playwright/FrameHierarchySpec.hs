module Playwright.FrameHierarchySpec
  ( tests
  ) where

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
  )
import Playwright
  ( Frame
  , Playwright
  , childFrames
  , currentName
  , currentUrl
  , defaultGotoOptions
  , evaluate
  , frameByName
  , frames
  , framesEqual
  , goto
  , mainFrame
  , parentFrame
  , setContent
  , urlFromText
  , urlToText
  , Playwright
  )
import Playwright.Harness
  ( attachFrame
  , withAssets
  , withChromiumPage
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
    , testCase "should persist mainFrame on cross-process navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let
            before = mainFrame page
            cross = replace "127.0.0.1" "localhost" prefix
          _ <- goto page (urlFromText (cross <> "/empty.html")) defaultGotoOptions
          assertEqual "same main frame" True (framesEqual (mainFrame page) before)
    ]

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

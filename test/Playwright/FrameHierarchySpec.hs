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
  )
import Playwright
  ( Frame
  , childFrames
  , currentName
  , currentUrl
  , defaultGotoOptions
  , frameByName
  , frames
  , goto
  , mainFrame
  , setContent
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( withAssets
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , testCase
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
    ]

dumpFrames :: Frame -> Text -> IO [Text]
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

sortChildren :: [Frame] -> IO [Frame]
sortChildren children = do
  keyed <- traverse keyFrame children
  pure (fmap (\(_, _, child) -> child) (sortBy (comparing (\(urlText, name, _) -> (urlText, name))) keyed))

keyFrame :: Frame -> IO (Text, Text, Frame)
keyFrame frame = do
  urlText <- fmap urlToText (currentUrl frame)
  name <- currentName frame
  pure (urlText, name, frame)

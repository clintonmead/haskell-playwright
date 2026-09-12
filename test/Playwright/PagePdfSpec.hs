module Playwright.PagePdfSpec
  ( tests
  ) where

import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as BS
import Playwright
  ( PdfOptions (pdfOptionsOutline, pdfOptionsTagged)
  , defaultGotoOptions
  , defaultPdfOptions
  , goto
  , pdf
  , pdfWith
  , urlFromText
  )
import Playwright.Harness
  ( withAssets
  , withChromiumPage
  , assertBool
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
    "pdf"
    [ testCase "should be able to save file" $
        withChromiumPage $ \page -> do
          bytes <- pdf page
          assertBool "pdf magic" (pdfMagic `BS.isPrefixOf` bytes)
          assertBool "non-empty" (BS.length bytes > 0)
    , testCase "should be able to generate outline" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/headings.html")) defaultGotoOptions
          withoutOutline <- pdf page
          withOutline <-
            pdfWith
              page
              defaultPdfOptions
                { pdfOptionsOutline = Just True
                , pdfOptionsTagged = Just True
                }
          assertBool "outline larger" (BS.length withOutline > BS.length withoutOutline)
    ]

pdfMagic :: ByteString
pdfMagic =
  "%PDF"

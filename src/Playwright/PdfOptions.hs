module Playwright.PdfOptions
  ( PdfOptions (..)
  , defaultPdfOptions
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , object
  , optionalField'
  , (.=)
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )

data PdfOptions = PdfOptions
  { pdfOptionsOutline :: Maybe Bool
  , pdfOptionsTagged :: Maybe Bool
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec PdfOptions)

defaultPdfOptions :: PdfOptions
defaultPdfOptions =
  PdfOptions
    { pdfOptionsOutline = Nothing
    , pdfOptionsTagged = Nothing
    }

instance HasCodec PdfOptions where
  codec =
    object "PdfOptions" $
      PdfOptions
        <$> optionalField' "outline" .= pdfOptionsOutline
        <*> optionalField' "tagged" .= pdfOptionsTagged

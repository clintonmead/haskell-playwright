module Playwright.ScreenshotOptions
  ( ScreenshotOptions (..)
  , ScreenshotType (..)
  , defaultScreenshotOptions
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , JSONCodec
  , bimapCodec
  , object
  , optionalField'
  , optionalFieldWith'
  , requiredField'
  , requiredFieldWith'
  , (.=)
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Data.Scientific
  ( Scientific
  )
import Data.Text
  ( Text
  )
import Playwright.BoundingBox
  ( BoundingBox (BoundingBox, boundingBoxHeight, boundingBoxWidth, boundingBoxX, boundingBoxY)
  )
import Playwright.CssPixel
  ( CssPixel (CssPixel)
  , cssPixelToScientific
  )
import Playwright.Timeout
  ( Timeout
  , defaultTimeout
  )

data ScreenshotOptions = ScreenshotOptions
  { screenshotOptionsTimeout :: Timeout
  , screenshotOptionsFullPage :: Maybe Bool
  , screenshotOptionsType :: Maybe ScreenshotType
  , screenshotOptionsQuality :: Maybe Int
  , screenshotOptionsOmitBackground :: Maybe Bool
  , screenshotOptionsClip :: Maybe BoundingBox
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec ScreenshotOptions)

data ScreenshotType
  = ScreenshotPng
  | ScreenshotJpeg
  | ScreenshotWebp
  deriving stock (Eq, Show)

defaultScreenshotOptions :: ScreenshotOptions
defaultScreenshotOptions =
  ScreenshotOptions
    { screenshotOptionsTimeout = defaultTimeout
    , screenshotOptionsFullPage = Nothing
    , screenshotOptionsType = Nothing
    , screenshotOptionsQuality = Nothing
    , screenshotOptionsOmitBackground = Nothing
    , screenshotOptionsClip = Nothing
    }

instance HasCodec ScreenshotOptions where
  codec =
    object "ScreenshotOptions" $
      ScreenshotOptions
        <$> requiredField' "timeout" .= screenshotOptionsTimeout
        <*> optionalField' "fullPage" .= screenshotOptionsFullPage
        <*> optionalField' "type" .= screenshotOptionsType
        <*> optionalField' "quality" .= screenshotOptionsQuality
        <*> optionalField' "omitBackground" .= screenshotOptionsOmitBackground
        <*> optionalFieldWith' "clip" boundingBoxCodec .= screenshotOptionsClip

boundingBoxCodec :: JSONCodec BoundingBox
boundingBoxCodec =
  object "ScreenshotClip" $
    BoundingBox
      <$> requiredFieldWith' "x" cssPixelCodec .= boundingBoxX
      <*> requiredFieldWith' "y" cssPixelCodec .= boundingBoxY
      <*> requiredFieldWith' "width" cssPixelCodec .= boundingBoxWidth
      <*> requiredFieldWith' "height" cssPixelCodec .= boundingBoxHeight

cssPixelCodec :: JSONCodec CssPixel
cssPixelCodec =
  bimapCodec (Right . CssPixel) cssPixelToScientific (codec :: JSONCodec Scientific)

instance HasCodec ScreenshotType where
  codec =
    bimapCodec screenshotTypeFromText screenshotTypeToText (codec :: JSONCodec Text)

screenshotTypeFromText :: Text -> Either String ScreenshotType
screenshotTypeFromText name =
  case name of
    "png" ->
      Right ScreenshotPng
    "jpeg" ->
      Right ScreenshotJpeg
    "webp" ->
      Right ScreenshotWebp
    _ ->
      Left ("unknown screenshot type: " <> show name)

screenshotTypeToText :: ScreenshotType -> Text
screenshotTypeToText screenshotType =
  case screenshotType of
    ScreenshotPng ->
      "png"
    ScreenshotJpeg ->
      "jpeg"
    ScreenshotWebp ->
      "webp"

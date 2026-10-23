module Playwright.ScreenshotOptions
  ( ScreenshotOptions (..)
  , ScreenshotType (..)
  , ScreenshotCaret (..)
  , ScreenshotAnimations (..)
  , ScreenshotScale (..)
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
import Data.Text qualified as Text
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
  , screenshotOptionsPath :: Maybe FilePath
  , screenshotOptionsCaret :: Maybe ScreenshotCaret
  , screenshotOptionsAnimations :: Maybe ScreenshotAnimations
  , screenshotOptionsScale :: Maybe ScreenshotScale
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec ScreenshotOptions)

data ScreenshotType
  = ScreenshotPng
  | ScreenshotJpeg
  | ScreenshotWebp
  deriving stock (Eq, Show)

data ScreenshotCaret
  = ScreenshotCaretHide
  | ScreenshotCaretInitial
  deriving stock (Eq, Show)

data ScreenshotAnimations
  = ScreenshotAnimationsDisabled
  | ScreenshotAnimationsAllow
  deriving stock (Eq, Show)

data ScreenshotScale
  = ScreenshotScaleCss
  | ScreenshotScaleDevice
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
    , screenshotOptionsPath = Nothing
    , screenshotOptionsCaret = Nothing
    , screenshotOptionsAnimations = Nothing
    , screenshotOptionsScale = Nothing
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
        <*> optionalFieldWith' "path" filePathCodec .= screenshotOptionsPath
        <*> optionalField' "caret" .= screenshotOptionsCaret
        <*> optionalField' "animations" .= screenshotOptionsAnimations
        <*> optionalField' "scale" .= screenshotOptionsScale

filePathCodec :: JSONCodec FilePath
filePathCodec =
  bimapCodec (Right . Text.unpack) Text.pack (codec :: JSONCodec Text)

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

instance HasCodec ScreenshotCaret where
  codec =
    bimapCodec screenshotCaretFromText screenshotCaretToText (codec :: JSONCodec Text)

screenshotCaretFromText :: Text -> Either String ScreenshotCaret
screenshotCaretFromText name =
  case name of
    "hide" ->
      Right ScreenshotCaretHide
    "initial" ->
      Right ScreenshotCaretInitial
    _ ->
      Left ("unknown screenshot caret: " <> show name)

screenshotCaretToText :: ScreenshotCaret -> Text
screenshotCaretToText screenshotCaret =
  case screenshotCaret of
    ScreenshotCaretHide ->
      "hide"
    ScreenshotCaretInitial ->
      "initial"

instance HasCodec ScreenshotAnimations where
  codec =
    bimapCodec screenshotAnimationsFromText screenshotAnimationsToText (codec :: JSONCodec Text)

screenshotAnimationsFromText :: Text -> Either String ScreenshotAnimations
screenshotAnimationsFromText name =
  case name of
    "disabled" ->
      Right ScreenshotAnimationsDisabled
    "allow" ->
      Right ScreenshotAnimationsAllow
    _ ->
      Left ("unknown screenshot animations: " <> show name)

screenshotAnimationsToText :: ScreenshotAnimations -> Text
screenshotAnimationsToText screenshotAnimations =
  case screenshotAnimations of
    ScreenshotAnimationsDisabled ->
      "disabled"
    ScreenshotAnimationsAllow ->
      "allow"

instance HasCodec ScreenshotScale where
  codec =
    bimapCodec screenshotScaleFromText screenshotScaleToText (codec :: JSONCodec Text)

screenshotScaleFromText :: Text -> Either String ScreenshotScale
screenshotScaleFromText name =
  case name of
    "css" ->
      Right ScreenshotScaleCss
    "device" ->
      Right ScreenshotScaleDevice
    _ ->
      Left ("unknown screenshot scale: " <> show name)

screenshotScaleToText :: ScreenshotScale -> Text
screenshotScaleToText screenshotScale =
  case screenshotScale of
    ScreenshotScaleCss ->
      "css"
    ScreenshotScaleDevice ->
      "device"

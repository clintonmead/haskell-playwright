module Playwright.ScreenshotOptions
  ( ScreenshotOptions (..)
  , ScreenshotType (..)
  , ScreenshotCaret (..)
  , ScreenshotAnimations (..)
  , ScreenshotScale (..)
  , defaultScreenshotOptions
  , resolveScreenshotType
  , writeScreenshotFile
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
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Maybe (fromMaybe)
import Data.Scientific
  ( Scientific
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
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
  , screenshotOptionsMaskColor :: Maybe Text
  , screenshotOptionsStyle :: Maybe Text
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
    , screenshotOptionsMaskColor = Nothing
    , screenshotOptionsStyle = Nothing
    }

-- | The type to capture as: the explicit 'screenshotOptionsType', or else the type implied by the path's extension.
resolveScreenshotType :: ScreenshotOptions -> Either Text (Maybe ScreenshotType)
resolveScreenshotType options =
  case (screenshotOptionsType options, screenshotOptionsPath options) of
    (Nothing, Just filePath) ->
      typeFromPath filePath
    (screenshotType, _) ->
      Right screenshotType

typeFromPath :: FilePath -> Either Text (Maybe ScreenshotType)
typeFromPath filePath =
  case fromMaybe "null" (mimeTypeForPath filePath) of
    "image/png" ->
      Right (Just ScreenshotPng)
    "image/jpeg" ->
      Right (Just ScreenshotJpeg)
    "image/webp" ->
      Right (Just ScreenshotWebp)
    mimeType ->
      Left ("path: unsupported mime type \"" <> mimeType <> "\"")

mimeTypeForPath :: FilePath -> Maybe Text
mimeTypeForPath filePath =
  case break (== '.') (reverse filePath) of
    (reversedExtension, '.' : _) ->
      lookup (Text.pack (reverse reversedExtension)) mimeTypes
    _ ->
      Nothing

mimeTypes :: [(Text, Text)]
mimeTypes =
  [ ("png", "image/png")
  , ("jpg", "image/jpeg")
  , ("jpeg", "image/jpeg")
  , ("jpe", "image/jpeg")
  , ("webp", "image/webp")
  , ("txt", "text/plain")
  ]

writeScreenshotFile :: FilePath -> ByteString -> IO ()
writeScreenshotFile filePath bytes = do
  createDirectoryIfMissing True (takeDirectory filePath)
  ByteString.writeFile filePath bytes

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
        <*> optionalField' "maskColor" .= screenshotOptionsMaskColor
        <*> optionalField' "style" .= screenshotOptionsStyle

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

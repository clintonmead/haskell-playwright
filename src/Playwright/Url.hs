module Playwright.Url
  ( Url
  , resolveAgainstBase
  , urlFromText
  , urlToText
  ) where

import Autodocodec
  ( HasCodec
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as Text

newtype Url = Url Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, HasCodec)

urlFromText :: Text -> Url
urlFromText = Url

urlToText :: Url -> Text
urlToText (Url text) = text

resolveAgainstBase :: Maybe Url -> Url -> Url
resolveAgainstBase base url
  | isAbsoluteUrl url =
      url
  | otherwise =
      case base of
        Nothing ->
          url
        Just root ->
          joinAgainst root url

isAbsoluteUrl :: Url -> Bool
isAbsoluteUrl url =
  let
    text = urlToText url
  in
    Text.isInfixOf "://" text
      || Text.isPrefixOf "data:" text
      || Text.isPrefixOf "about:" text
      || Text.isPrefixOf "blob:" text

joinAgainst :: Url -> Url -> Url
joinAgainst root url =
  let
    target = urlToText url
    baseText = urlToText root
  in
    if Text.isPrefixOf "/" target
      then urlFromText (urlOrigin baseText <> target)
      else
        let
          relative = stripDotSlash target
        in
          if relative == "." || Text.null relative
            then urlFromText (directoryOf baseText)
            else urlFromText (directoryOf baseText <> relative)

stripDotSlash :: Text -> Text
stripDotSlash text =
  if Text.isPrefixOf "./" text
    then Text.drop 2 text
    else text

urlOrigin :: Text -> Text
urlOrigin text =
  case Text.breakOn "://" text of
    (scheme, rest)
      | Text.isPrefixOf "://" rest ->
          scheme <> "://" <> Text.takeWhile (/= '/') (Text.drop 3 rest)
      | otherwise ->
          text

directoryOf :: Text -> Text
directoryOf text =
  let
    origin = urlOrigin text
    afterOrigin = Text.drop (Text.length origin) text
  in
    case afterOrigin of
      "" ->
        origin <> "/"
      "/" ->
        origin <> "/"
      path ->
        case Text.breakOnEnd "/" path of
          (dir, _) ->
            origin <> dir

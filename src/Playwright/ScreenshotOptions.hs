module Playwright.ScreenshotOptions
  ( ScreenshotOptions (..)
  , defaultScreenshotOptions
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , object
  , optionalField'
  , requiredField'
  , (.=)
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Playwright.Timeout
  ( Timeout
  , defaultTimeout
  )

data ScreenshotOptions = ScreenshotOptions
  { screenshotOptionsTimeout :: Timeout
  , screenshotOptionsFullPage :: Maybe Bool
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec ScreenshotOptions)

defaultScreenshotOptions :: ScreenshotOptions
defaultScreenshotOptions =
  ScreenshotOptions
    { screenshotOptionsTimeout = defaultTimeout
    , screenshotOptionsFullPage = Nothing
    }

instance HasCodec ScreenshotOptions where
  codec =
    object "ScreenshotOptions" $
      ScreenshotOptions
        <$> requiredField' "timeout" .= screenshotOptionsTimeout
        <*> optionalField' "fullPage" .= screenshotOptionsFullPage

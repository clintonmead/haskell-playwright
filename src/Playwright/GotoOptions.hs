module Playwright.GotoOptions
  ( GotoOptions (..)
  , defaultGotoOptions
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
import Playwright.LoadState
  ( LoadState
  )
import Playwright.Timeout
  ( Timeout
  )
import Playwright.Url
  ( Url
  )

data GotoOptions = GotoOptions
  { gotoOptionsTimeout :: Maybe Timeout
  , gotoOptionsWaitUntil :: Maybe LoadState
  , gotoOptionsReferer :: Maybe Url
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec GotoOptions)

defaultGotoOptions :: GotoOptions
defaultGotoOptions =
  GotoOptions
    { gotoOptionsTimeout = Nothing
    , gotoOptionsWaitUntil = Nothing
    , gotoOptionsReferer = Nothing
    }

instance HasCodec GotoOptions where
  codec =
    object "GotoOptions" $
      GotoOptions
        <$> optionalField' "timeout" .= gotoOptionsTimeout
        <*> optionalField' "waitUntil" .= gotoOptionsWaitUntil
        <*> optionalField' "referer" .= gotoOptionsReferer

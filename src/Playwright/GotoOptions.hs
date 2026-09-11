module Playwright.GotoOptions
  ( GotoOptions (..)
  , defaultGotoOptions
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
import Playwright.Protocol
  ( LifecycleEvent
  )
import Playwright.Timeout
  ( Timeout
  , defaultTimeout
  )
import Playwright.Url
  ( Url
  )

data GotoOptions = GotoOptions
  { gotoOptionsTimeout :: Timeout
  , gotoOptionsWaitUntil :: Maybe LifecycleEvent
  , gotoOptionsReferer :: Maybe Url
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec GotoOptions)

defaultGotoOptions :: GotoOptions
defaultGotoOptions =
  GotoOptions
    { gotoOptionsTimeout = defaultTimeout
    , gotoOptionsWaitUntil = Nothing
    , gotoOptionsReferer = Nothing
    }

instance HasCodec GotoOptions where
  codec =
    object "GotoOptions" $
      GotoOptions
        <$> requiredField' "timeout" .= gotoOptionsTimeout
        <*> optionalField' "waitUntil" .= gotoOptionsWaitUntil
        <*> optionalField' "referer" .= gotoOptionsReferer

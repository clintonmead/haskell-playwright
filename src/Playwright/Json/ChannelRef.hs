module Playwright.Json.ChannelRef
  ( ChannelRef (..)
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , object
  , requiredField'
  , (.=)
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Playwright.Guid
  ( Guid
  )

data ChannelRef = ChannelRef
  { channelRefGuid :: Guid
  }
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec ChannelRef)

instance HasCodec ChannelRef where
  codec =
    object "ChannelRef" $
      ChannelRef <$> requiredField' "guid" .= channelRefGuid

module Playwright.JsHandle
  ( JsHandle (..)
  , jsHandleFromRef
  ) where

import Playwright.Channel
  ( Channel (Channel)
  , JsHandleTag
  )
import Playwright.Connection
  ( Connection
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )

newtype JsHandle = JsHandle
  { jsHandleChannel :: Channel JsHandleTag
  }

jsHandleFromRef :: Connection -> ChannelRef -> JsHandle
jsHandleFromRef conn ref =
  JsHandle (Channel conn (channelRefGuid ref))

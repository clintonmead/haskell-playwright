module Playwright.Response
  ( Response (..)
  ) where

import Playwright.Channel
  ( Channel
  , ResponseTag
  )

newtype Response = Response
  { responseChannel :: Channel ResponseTag
  }

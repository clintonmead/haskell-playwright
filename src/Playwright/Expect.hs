module Playwright.Expect
  ( ExpectOptions (..)
  , defaultExpectOptions
  , expectLocator
  , expectLocatorWith
  , expectLocatorCount
  ) where

import Data.Aeson
  ( Value
  )
import Data.Text
  ( Text
  )
import Playwright.Frame
  ( Frame
  , frameSend_
  , frameSendTimed_
  )
import Playwright.Locator
  ( Locator (locatorFrame, locatorSelector)
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.Protocol
  ( FrameExpectParams
  , frameExpectParamsExpectedNumber
  , frameExpectParamsSelector
  , mkFrameExpectParams
  )
import Playwright.Timeout
  ( Timeout
  )

-- | Options shared by every expect matcher.
--
-- 'expectIsNot' inverts the matcher, and 'expectTimeout' bounds the
-- driver-side polling. 'Nothing' leaves the timeout to the driver default.
data ExpectOptions = ExpectOptions
  { expectIsNot :: Bool
  , expectTimeout :: Maybe Timeout
  }
  deriving stock (Eq, Show)

defaultExpectOptions :: ExpectOptions
defaultExpectOptions =
  ExpectOptions
    { expectIsNot = False
    , expectTimeout = Nothing
    }

-- | Run a boolean locator matcher such as @to.be.visible@ through the
-- driver's server-side polling. The call throws the driver's error when the
-- assertion does not hold within the timeout.
expectLocator :: Locator -> Text -> Playwright ()
expectLocator = expectLocatorWith defaultExpectOptions

expectLocatorWith :: ExpectOptions -> Locator -> Text -> Playwright ()
expectLocatorWith options locator expression =
  sendExpect
    options
    (locatorFrame locator)
    (mkFrameExpectParams expression (expectIsNot options))
      { frameExpectParamsSelector = Just (locatorSelector locator)
      }

-- | @to.have.count@ with a numeric expectation.
expectLocatorCount :: ExpectOptions -> Locator -> Int -> Playwright ()
expectLocatorCount options locator count =
  sendExpect
    options
    (locatorFrame locator)
    (mkFrameExpectParams "to.have.count" (expectIsNot options))
      { frameExpectParamsSelector = Just (locatorSelector locator)
      , frameExpectParamsExpectedNumber = Just (fromIntegral count)
      }

sendExpect :: ExpectOptions -> Frame -> FrameExpectParams -> Playwright ()
sendExpect options frame params =
  case expectTimeout options of
    Nothing ->
      frameSend_ @Value frame "expect" params
    Just timeout ->
      frameSendTimed_ @Value frame "expect" timeout params

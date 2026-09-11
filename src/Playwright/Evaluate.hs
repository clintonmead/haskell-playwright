module Playwright.Evaluate
  ( evaluate
  , evaluateText
  ) where

import Control.Exception
  ( throwIO
  )
import Data.Text
  ( Text
  )
import Playwright.Error
  ( PlaywrightError (EvaluateUnexpected)
  )
import Playwright.Frame
  ( evaluateFrame
  )
import Playwright.Page
  ( Page (pageMainFrame)
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadString)
  , SerializedValue (SerializedNode)
  )

evaluate :: Page -> Text -> IO SerializedValue
evaluate page = evaluateFrame (pageMainFrame page)

evaluateText :: Page -> Text -> IO Text
evaluateText page expression = do
  value <- evaluate page expression
  case value of
    SerializedNode (PayloadString text) _ ->
      pure text
    other ->
      throwIO (EvaluateUnexpected other)

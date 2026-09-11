module Playwright.InputFile
  ( InputFile (..)
  ) where

import Data.ByteString
  ( ByteString
  )
import Playwright.InputFileName
  ( InputFileName
  )

data InputFile = InputFile
  { inputFileName :: InputFileName
  , inputFileBytes :: ByteString
  }
  deriving stock (Eq, Show)

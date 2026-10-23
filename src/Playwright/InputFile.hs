module Playwright.InputFile
  ( InputFile (..)
  , resolveInputFilePaths
  ) where

import Control.Monad
  ( forM
  )
import Data.ByteString
  ( ByteString
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (ProtocolFailure, protocolFailureName, protocolFailureMessage, protocolFailureStack)
  )
import Playwright.InputFileName
  ( InputFileName
  )
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , makeAbsolute
  )
import UnliftIO.Exception
  ( throwIO
  )

data InputFile = InputFile
  { inputFileName :: InputFileName
  , inputFileBytes :: ByteString
  }
  deriving stock (Eq, Show)

-- | Resolves host paths the way the upstream client does before it sends a
-- setInputFiles request. Each path is stat'ed: directories are allowed only
-- as the single item, and mixing files with a directory is rejected. Returns
-- the absolute local files and the absolute local directory, if any.
resolveInputFilePaths :: [FilePath] -> IO ([Text], Maybe Text)
resolveInputFilePaths items = do
  entries <- forM items $ \item -> do
    isDir <- doesDirectoryExist item
    isFile <- doesFileExist item
    if isDir || isFile
      then do
        absolute <- makeAbsolute item
        pure (absolute, isDir)
      else
        throwIO
          ( ProtocolRpc
              ProtocolFailure
                { protocolFailureName = "Error"
                , protocolFailureMessage =
                    Text.pack ("ENOENT: no such file or directory, stat '" <> item <> "'")
                , protocolFailureStack = Nothing
                }
          )
  let directories = [path | (path, True) <- entries]
      files = [Text.pack path | (path, False) <- entries]
  case directories of
    [] -> pure (files, Nothing)
    [directory]
      | null files -> pure ([], Just (Text.pack directory))
      | otherwise -> clientError "File paths must be all files or a single directory"
    _ -> clientError "Multiple directories are not supported"
  where
    clientError :: Text -> IO a
    clientError message =
      throwIO
        ( ProtocolRpc
            ProtocolFailure
              { protocolFailureName = "Error"
              , protocolFailureMessage = message
              , protocolFailureStack = Nothing
              }
        )

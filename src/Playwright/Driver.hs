module Playwright.Driver
  ( DriverPaths (..)
  , findDriver
  ) where

import Control.Exception
  ( throwIO
  )
import Playwright.Error
  ( PlaywrightError (DriverEnvMissing, DriverNotFound)
  )
import System.Directory
  ( doesFileExist
  )
import System.Environment
  ( lookupEnv
  )
import System.FilePath
  ( (</>)
  )

data DriverPaths = DriverPaths
  { driverNode :: FilePath
  , driverCli :: FilePath
  }
  deriving stock (Eq, Show)

findDriver :: IO DriverPaths
findDriver = do
  node <- lookupEnv "PLAYWRIGHT_NODEJS_PATH"
  core <- lookupEnv "PLAYWRIGHT_CORE_DIR"
  case (node, core) of
    (Nothing, _) ->
      throwIO (DriverEnvMissing "PLAYWRIGHT_NODEJS_PATH")
    (_, Nothing) ->
      throwIO (DriverEnvMissing "PLAYWRIGHT_CORE_DIR")
    (Just nodePath, Just coreDir) -> do
      let
        cli :: FilePath
        cli = coreDir </> "cli.js"
      exists <- doesFileExist cli
      if exists
        then pure DriverPaths {driverNode = nodePath, driverCli = cli}
        else throwIO (DriverNotFound cli)

module Main
  ( main
  ) where

import Data.Text.IO qualified as TextIO
import Data.Version
  ( makeVersion
  )
import OptEnvConf
  ( HasParser (settingsParser)
  , Parser
  , env
  , help
  , long
  , metavar
  , option
  , reader
  , runSettingsParser
  , setting
  , str
  )
import Playwright.Codegen.Emit
  ( emitModule
  )
import Playwright.Codegen.Spec
  ( loadSpecDir
  )
import System.Environment
  ( getArgs
  , lookupEnv
  )
import System.Exit
  ( die
  )
import System.FilePath
  ( takeExtension
  )

data CodegenSettings = CodegenSettings
  { codegenSpecDir :: FilePath
  , codegenOutFile :: FilePath
  }

instance HasParser CodegenSettings where
  settingsParser = codegenSettingsParser

codegenSettingsParser :: Parser CodegenSettings
codegenSettingsParser =
  CodegenSettings
    <$> setting
      [ help "Directory containing protocol YAML spec files"
      , reader str
      , option
      , long "spec-dir"
      , env "PLAYWRIGHT_PROTOCOL_DIR"
      , metavar "DIR"
      ]
    <*> setting
      [ help "Haskell module to write"
      , reader str
      , option
      , long "out-file"
      , metavar "FILE"
      ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [orig, _input, output]
      | takeExtension orig == ".hs" ->
          writeGenerated output
    _ -> do
      settings <-
        runSettingsParser
          (makeVersion [0, 1, 0, 0])
          "Generate Playwright.Protocol from protocol YAML"
      writeFrom (codegenSpecDir settings) (codegenOutFile settings)

writeGenerated :: FilePath -> IO ()
writeGenerated output = do
  specDir <- requireSpecDir
  writeFrom specDir output

requireSpecDir :: IO FilePath
requireSpecDir = do
  found <- lookupEnv "PLAYWRIGHT_PROTOCOL_DIR"
  case found of
    Nothing ->
      die "PLAYWRIGHT_PROTOCOL_DIR is not set"
    Just dir ->
      pure dir

writeFrom :: FilePath -> FilePath -> IO ()
writeFrom specDir output = do
  spec <- loadSpecDir specDir
  TextIO.writeFile output (emitModule spec)

module Playwright.LaunchEnv
  ( applyEnvLaunchOverrides
  ) where

import Control.Exception
  ( throwIO
  )
import Data.Char
  ( toLower
  )
import Data.Text
  ( Text
  , pack
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  )
import Playwright.LaunchOptions
  ( LaunchOptions (launchOptionsArgs, launchOptionsHeadless, launchOptionsSlowMo)
  )
import Playwright.Timeout
  ( Timeout
  , milliseconds
  )
import System.Environment
  ( lookupEnv
  )
import Text.Read
  ( readMaybe
  )

applyEnvLaunchOverrides :: LaunchOptions -> IO LaunchOptions
applyEnvLaunchOverrides opts = do
  headedEnv <- lookupEnv "PLAYWRIGHT_HEADED"
  slowMoEnv <- lookupEnv "PLAYWRIGHT_SLOWMO"
  gpuEnv <- lookupEnv "PLAYWRIGHT_HEADED_GPU"
  display <- lookupEnv "DISPLAY"
  wayland <- lookupEnv "WAYLAND_DISPLAY"
  let
    headed :: Bool
    headed = envFlag headedEnv
  slowMo <- resolveSlowMo headed slowMoEnv (launchOptionsSlowMo opts)
  pure
    opts
      { launchOptionsHeadless = headedHeadless headed (launchOptionsHeadless opts)
      , launchOptionsSlowMo = slowMo
      , launchOptionsArgs = headedArgs headed (envFlag gpuEnv) display wayland (launchOptionsArgs opts)
      }

headedHeadless :: Bool -> Maybe Bool -> Maybe Bool
headedHeadless headed current =
  if headed
    then Just False
    else current

headedArgs :: Bool -> Bool -> Maybe String -> Maybe String -> Maybe [Text] -> Maybe [Text]
headedArgs headed allowGpu display wayland current =
  if headed
    then appendLaunchArgs (gpuArgs allowGpu <> ozoneArgs display wayland) current
    else current

gpuArgs :: Bool -> [Text]
gpuArgs allowGpu =
  if allowGpu
    then []
    else [pack "--disable-gpu"]

ozoneArgs :: Maybe String -> Maybe String -> [Text]
ozoneArgs display wayland =
  if nonEmptyEnv display
    then [pack "--ozone-platform=x11"]
    else
      if nonEmptyEnv wayland
        then [pack "--ozone-platform=wayland", pack "--enable-features=UseOzonePlatform"]
        else []

appendLaunchArgs :: [Text] -> Maybe [Text] -> Maybe [Text]
appendLaunchArgs extra current =
  foldl' (flip appendLaunchArg) current extra

appendLaunchArg :: Text -> Maybe [Text] -> Maybe [Text]
appendLaunchArg arg current =
  case current of
    Nothing ->
      Just [arg]
    Just args ->
      if elem arg args
        then Just args
        else Just (args <> [arg])

resolveSlowMo :: Bool -> Maybe String -> Maybe Timeout -> IO (Maybe Timeout)
resolveSlowMo headed raw existing =
  case raw of
    Just text
      | not (null text) ->
          parseSlowMo text
    _ ->
      pure (headedSlowMo headed existing)

parseSlowMo :: String -> IO (Maybe Timeout)
parseSlowMo text =
  case readMaybe text of
    Just n ->
      case milliseconds n of
        Just timeout ->
          pure (Just timeout)
        Nothing ->
          invalidSlowMo
    Nothing ->
      invalidSlowMo

invalidSlowMo :: IO (Maybe Timeout)
invalidSlowMo =
  throwIO (DecodeFailure (pack "PLAYWRIGHT_SLOWMO must be a non-negative number of milliseconds"))

headedSlowMo :: Bool -> Maybe Timeout -> Maybe Timeout
headedSlowMo headed existing =
  case existing of
    Just timeout ->
      Just timeout
    Nothing ->
      if headed
        then milliseconds 250
        else Nothing

envFlag :: Maybe String -> Bool
envFlag value =
  case fmap (fmap toLower) value of
    Just "1" ->
      True
    Just "true" ->
      True
    Just "yes" ->
      True
    Just "on" ->
      True
    _ ->
      False

nonEmptyEnv :: Maybe String -> Bool
nonEmptyEnv value =
  case value of
    Just raw | not (null raw) ->
      True
    _ ->
      False

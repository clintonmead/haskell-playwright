module Playwright.Selectors
  ( registerSelector
  , registerSelectorWith
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( forM_
  , unless
  , when
  )
import Data.Char
  ( isAlphaNum
  , isAscii
  )
import Data.Set qualified as Set
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Playwright.Channel
  ( Channel (channelConnection)
  )
import Playwright.Connection
  ( addSelectorEngine
  , closedGuids
  , guidsOfType
  , lookupSelectorEngines
  , sendCommand
  )
import Playwright.Error
  ( PlaywrightError (SelectorsRegister)
  )
import Playwright.Instance
  ( Playwright (playwrightChannel)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( BrowserContextRegisterSelectorEngineResult
  , ChannelType (ChannelTypeBrowserContext)
  , SelectorEngine (selectorEngineContentScript, selectorEngineName)
  , mkBrowserContextRegisterSelectorEngineParams
  , mkSelectorEngine
  )
import Playwright.RegisterSelectorOptions
  ( RegisterSelectorOptions (registerSelectorContentScript)
  , defaultRegisterSelectorOptions
  )

registerSelector :: Playwright -> Text -> Text -> IO ()
registerSelector pw name source =
  registerSelectorWith pw name source defaultRegisterSelectorOptions

registerSelectorWith :: Playwright -> Text -> Text -> RegisterSelectorOptions -> IO ()
registerSelectorWith pw name source opts = do
  let
    conn = channelConnection (playwrightChannel pw)
    engine =
      (mkSelectorEngine name source)
        { selectorEngineContentScript = registerSelectorContentScript opts
        }
  unless (validEngineName name) $
    throwIO (SelectorsRegister "selectors.register: Selector engine name may only contain [a-zA-Z0-9_] characters")
  when (predefinedEngine name) $
    throwIO (SelectorsRegister ("selectors.register: \"" <> name <> "\" is a predefined selector engine"))
  engines <- lookupSelectorEngines conn
  when (any ((== name) . selectorEngineName) engines) $
    throwIO (SelectorsRegister ("selectors.register: \"" <> name <> "\" selector engine has been already registered"))
  guids <- guidsOfType conn ChannelTypeBrowserContext
  closed <- closedGuids conn
  forM_ guids $ \guid ->
    unless (Set.member guid closed) $ do
      _ :: BrowserContextRegisterSelectorEngineResult <-
        sendCommand
          conn
          guid
          (MethodName "registerSelectorEngine")
          (mkBrowserContextRegisterSelectorEngineParams engine)
      pure ()
  addSelectorEngine conn engine

validEngineName :: Text -> Bool
validEngineName name =
  not (Text.null name) && Text.all validNameChar name

validNameChar :: Char -> Bool
validNameChar c =
  isAscii c && (isAlphaNum c || c == '_' || c == '-')

predefinedEngine :: Text -> Bool
predefinedEngine name =
  name == "zs" || name == "zs:light" || name `elem` builtinEngines

builtinEngines :: [Text]
builtinEngines =
  [ "css"
  , "css:light"
  , "xpath"
  , "xpath:light"
  , "_react"
  , "_vue"
  , "text"
  , "text:light"
  , "id"
  , "id:light"
  , "data-testid"
  , "data-testid:light"
  , "data-test-id"
  , "data-test-id:light"
  , "data-test"
  , "data-test:light"
  , "nth"
  , "visible"
  , "internal:control"
  , "internal:has"
  , "internal:has-not"
  , "internal:has-text"
  , "internal:has-not-text"
  , "internal:and"
  , "internal:or"
  , "internal:chain"
  , "role"
  , "internal:attr"
  , "internal:label"
  , "internal:text"
  , "internal:role"
  , "internal:testid"
  , "internal:describe"
  , "aria-ref"
  ]

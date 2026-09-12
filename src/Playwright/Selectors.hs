module Playwright.Selectors
  ( registerSelector
  , registerSelectorWith
  ) where

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
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Monad
  ( Playwright
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
import UnliftIO.Exception
  ( throwIO
  )

registerSelector :: Text -> Text -> Playwright ()
registerSelector name source =
  registerSelectorWith name source defaultRegisterSelectorOptions

registerSelectorWith :: Text -> Text -> RegisterSelectorOptions -> Playwright ()
registerSelectorWith name source opts = do
  let
    engine =
      (mkSelectorEngine name source)
        { selectorEngineContentScript = registerSelectorContentScript opts
        }
  unless (validEngineName name) $
    throwIO (SelectorsRegister "selectors.register: Selector engine name may only contain [a-zA-Z0-9_] characters")
  when (predefinedEngine name) $
    throwIO (SelectorsRegister ("selectors.register: \"" <> name <> "\" is a predefined selector engine"))
  engines <- lookupSelectorEngines
  when (any ((== name) . selectorEngineName) engines) $
    throwIO (SelectorsRegister ("selectors.register: \"" <> name <> "\" selector engine has been already registered"))
  guids <- guidsOfType ChannelTypeBrowserContext
  closed <- closedGuids
  forM_ guids $ \guid ->
    unless (Set.member guid closed) $ do
      _ :: BrowserContextRegisterSelectorEngineResult <-
        sendCommand
          guid
          (MethodName "registerSelectorEngine")
          (mkBrowserContextRegisterSelectorEngineParams engine)
      pure ()
  addSelectorEngine engine

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

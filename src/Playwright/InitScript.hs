module Playwright.InitScript
  ( addContextInitScriptExposing
  , addPageInitScriptExposing
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Aeson.Text
  ( encodeToLazyText
  )
import Data.Text
  ( Text
  , intercalate
  , pack
  )
import Data.Text.Lazy
  ( toStrict
  )
import Data.Unique
  ( hashUnique
  , newUnique
  )
import Playwright.BindingName
  ( BindingName (BindingName)
  )
import Playwright.BrowserContext
  ( BrowserContext
  , addContextInitScript
  )
import Playwright.Channel
  ( send
  )
import Playwright.Disposable
  ( Disposable
  , disposableFromRef
  )
import Playwright.Expose
  ( contextExposeFunction
  , exposeFunction
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.Page
  ( Page (pageChannel)
  )
import Playwright.Protocol
  ( PageAddInitScriptResult (pageAddInitScriptResultDisposable)
  , mkPageAddInitScriptParams
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  )
import Playwright.SerializedValue
  ( SerializedValue
  )

-- | Adds an init script whose source receives an object of functions, one per
-- key. Each function is exposed as a context binding under a unique name, and
-- the script reads it from @window@ when the page loads. The returned handle
-- removes only the init script; the bindings stay registered until the context
-- closes.
addContextInitScriptExposing ::
  BrowserContext ->
  Text ->
  [(Text, [SerializedValue] -> Playwright SerializedArgument)] ->
  Playwright Disposable
addContextInitScriptExposing ctx source functions = do
  bound <- traverse (bindFunction (contextExposeFunction ctx)) functions
  addContextInitScript ctx (initScript source bound)

-- | Page-level counterpart of 'addContextInitScriptExposing'. The functions are
-- exposed on the page, so they stay registered until the page closes, and the
-- returned handle removes only the init script.
addPageInitScriptExposing ::
  Page ->
  Text ->
  [(Text, [SerializedValue] -> Playwright SerializedArgument)] ->
  Playwright Disposable
addPageInitScriptExposing page source functions = do
  bound <- traverse (bindFunction (exposeFunction page)) functions
  result <-
    send (pageChannel page) (MethodName "addInitScript") (mkPageAddInitScriptParams (initScript source bound))
  pure (disposableFromRef (pageAddInitScriptResultDisposable result))

bindFunction ::
  (BindingName -> ([SerializedValue] -> Playwright SerializedArgument) -> Playwright Disposable) ->
  (Text, [SerializedValue] -> Playwright SerializedArgument) ->
  Playwright (Text, Text)
bindFunction expose (key, handler) = do
  unique <- liftIO newUnique
  let name = "__pwInitScriptFunction" <> pack (show (hashUnique unique))
  _ <- expose (BindingName name) handler
  pure (key, name)

-- | Each binding is read off @window@ and then deleted, so the exposed function
-- is reachable only through the argument the source receives, not as a global.
initScript :: Text -> [(Text, Text)] -> Text
initScript source bound =
  "(" <> source <> ")({" <> intercalate ", " (map entry bound) <> "})"
  where
    entry (key, name) =
      quoteJs key <> ": (() => { const f = window[" <> quoteJs name <> "]; delete window[" <> quoteJs name <> "]; return f; })()"

quoteJs :: Text -> Text
quoteJs = toStrict . encodeToLazyText

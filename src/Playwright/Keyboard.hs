module Playwright.Keyboard
  ( keyboardDown
  , keyboardInsertText
  , keyboardPress
  , keyboardType
  , keyboardUp
  ) where

import Data.Text
  ( Text
  )
import Playwright.Channel
  ( send
  )
import Playwright.Key
  ( Key (keyToText)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Page
  ( Page (pageChannel)
  )
import Playwright.Protocol
  ( PageKeyboardDownResult
  , PageKeyboardInsertTextResult
  , PageKeyboardPressResult
  , PageKeyboardTypeResult
  , PageKeyboardUpResult
  , mkPageKeyboardDownParams
  , mkPageKeyboardInsertTextParams
  , mkPageKeyboardPressParams
  , mkPageKeyboardTypeParams
  , mkPageKeyboardUpParams
  )

keyboardType :: Page -> Text -> IO ()
keyboardType page text = do
  _ :: PageKeyboardTypeResult <-
    send (pageChannel page) (MethodName "keyboardType") (mkPageKeyboardTypeParams text)
  pure ()

keyboardPress :: Page -> Key -> IO ()
keyboardPress page key = do
  _ :: PageKeyboardPressResult <-
    send (pageChannel page) (MethodName "keyboardPress") (mkPageKeyboardPressParams (keyToText key))
  pure ()

keyboardDown :: Page -> Key -> IO ()
keyboardDown page key = do
  _ :: PageKeyboardDownResult <-
    send (pageChannel page) (MethodName "keyboardDown") (mkPageKeyboardDownParams (keyToText key))
  pure ()

keyboardUp :: Page -> Key -> IO ()
keyboardUp page key = do
  _ :: PageKeyboardUpResult <-
    send (pageChannel page) (MethodName "keyboardUp") (mkPageKeyboardUpParams (keyToText key))
  pure ()

keyboardInsertText :: Page -> Text -> IO ()
keyboardInsertText page text = do
  _ :: PageKeyboardInsertTextResult <-
    send (pageChannel page) (MethodName "keyboardInsertText") (mkPageKeyboardInsertTextParams text)
  pure ()

module Playwright.ElementHandle.Action
  ( checkHandle
  , disposeHandle
  , fillHandle
  , focusHandle
  , hoverHandle
  , selectOptionHandle
  , setCheckedHandle
  , uncheckHandle
  ) where

import Playwright.Monad
  ( Playwright
  )
import UnliftIO.Exception
  ( throwIO
  , try
  )
import Data.Text
  ( Text
  , isInfixOf
  )
import Playwright.Channel
  ( send
  )
import Playwright.ElementHandle
  ( ElementHandle (elementHandleChannel)
  )
import Playwright.Error
  ( PlaywrightError (MissingObject, ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage, protocolFailureName)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( ElementHandleCheckResult
  , ElementHandleFillResult
  , ElementHandleFocusResult
  , ElementHandleHoverResult
  , ElementHandleSelectOptionParams (elementHandleSelectOptionParamsOptions)
  , ElementHandleSelectOptionParamsOptions (elementHandleSelectOptionParamsOptionsIndex, elementHandleSelectOptionParamsOptionsLabel, elementHandleSelectOptionParamsOptionsValue, elementHandleSelectOptionParamsOptionsValueOrLabel)
  , ElementHandleSelectOptionResult (elementHandleSelectOptionResultValues)
  , ElementHandleUncheckResult
  , JSHandleDisposeResult
  , mkElementHandleCheckParams
  , mkElementHandleFillParams
  , mkElementHandleFocusParams
  , mkElementHandleHoverParams
  , mkElementHandleSelectOptionParams
  , mkElementHandleSelectOptionParamsOptions
  , mkElementHandleUncheckParams
  , mkJSHandleDisposeParams
  )
import Playwright.SelectOption
  ( SelectAttributes (selectAttributesIndex, selectAttributesLabel, selectAttributesValue)
  , SelectOption (SelectIndex, SelectLabel, SelectMatching, SelectValue, SelectValueOrLabel)
  )
import Playwright.Timeout
  ( defaultTimeout
  )

hoverHandle :: ElementHandle -> Playwright ()
hoverHandle handle = do
  _ :: ElementHandleHoverResult <-
    send (elementHandleChannel handle) (MethodName "hover") (mkElementHandleHoverParams defaultTimeout)
  pure ()

fillHandle :: ElementHandle -> Text -> Playwright ()
fillHandle handle value = do
  _ :: ElementHandleFillResult <-
    send (elementHandleChannel handle) (MethodName "fill") (mkElementHandleFillParams defaultTimeout value)
  pure ()

checkHandle :: ElementHandle -> Playwright ()
checkHandle handle = do
  _ :: ElementHandleCheckResult <-
    send (elementHandleChannel handle) (MethodName "check") (mkElementHandleCheckParams defaultTimeout)
  pure ()

uncheckHandle :: ElementHandle -> Playwright ()
uncheckHandle handle = do
  _ :: ElementHandleUncheckResult <-
    send (elementHandleChannel handle) (MethodName "uncheck") (mkElementHandleUncheckParams defaultTimeout)
  pure ()

setCheckedHandle :: ElementHandle -> Bool -> Playwright ()
setCheckedHandle handle wanted =
  if wanted
    then checkHandle handle
    else uncheckHandle handle

focusHandle :: ElementHandle -> Playwright ()
focusHandle handle = do
  _ :: ElementHandleFocusResult <-
    send (elementHandleChannel handle) (MethodName "focus") mkElementHandleFocusParams
  pure ()

selectOptionHandle :: ElementHandle -> [SelectOption] -> Playwright [Text]
selectOptionHandle handle options = do
  result <-
    send
      (elementHandleChannel handle)
      (MethodName "selectOption")
      (mkElementHandleSelectOptionParams defaultTimeout)
        { elementHandleSelectOptionParamsOptions = Just (fmap toProtocolOption options)
        }
  pure (elementHandleSelectOptionResultValues result)

toProtocolOption :: SelectOption -> ElementHandleSelectOptionParamsOptions
toProtocolOption option =
  case option of
    SelectValueOrLabel text ->
      mkElementHandleSelectOptionParamsOptions {elementHandleSelectOptionParamsOptionsValueOrLabel = Just text}
    SelectValue text ->
      mkElementHandleSelectOptionParamsOptions {elementHandleSelectOptionParamsOptionsValue = Just text}
    SelectLabel text ->
      mkElementHandleSelectOptionParamsOptions {elementHandleSelectOptionParamsOptionsLabel = Just text}
    SelectIndex index ->
      mkElementHandleSelectOptionParamsOptions {elementHandleSelectOptionParamsOptionsIndex = Just index}
    SelectMatching attributes ->
      mkElementHandleSelectOptionParamsOptions
        { elementHandleSelectOptionParamsOptionsValue = selectAttributesValue attributes
        , elementHandleSelectOptionParamsOptionsLabel = selectAttributesLabel attributes
        , elementHandleSelectOptionParamsOptionsIndex = selectAttributesIndex attributes
        }

disposeHandle :: ElementHandle -> Playwright ()
disposeHandle handle = do
  result <-
    try
      ( do
          _ :: JSHandleDisposeResult <-
            send (elementHandleChannel handle) (MethodName "dispose") mkJSHandleDisposeParams
          pure ()
      )
  case result of
    Left err ->
      if isBenignDisposeError err
        then pure ()
        else throwIO err
    Right () ->
      pure ()

isBenignDisposeError :: PlaywrightError -> Bool
isBenignDisposeError err =
  case err of
    MissingObject _ ->
      True
    ProtocolRpc failure ->
      protocolFailureName failure == "TargetClosedError"
        || "disposed" `isInfixOf` protocolFailureMessage failure
    _ ->
      False

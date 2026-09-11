module Playwright.Frame.Inspect
  ( getAttributeFrame
  , innerHtmlFrame
  , innerTextFrame
  , inputValueFrame
  , isCheckedFrame
  , isDisabledFrame
  , isEditableFrame
  , isEnabledFrame
  , isHiddenFrame
  , isVisibleFrame
  , queryCountFrame
  , textContentFrame
  ) where

import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Data.Text
  ( Text
  )
import Playwright.Frame
  ( Frame
  , frameSend
  )
import Playwright.Protocol
  ( FrameGetAttributeParams (frameGetAttributeParamsStrict)
  , FrameGetAttributeResult (frameGetAttributeResultValue)
  , FrameInnerHTMLParams (frameInnerHTMLParamsStrict)
  , FrameInnerHTMLResult (frameInnerHTMLResultValue)
  , FrameInnerTextParams (frameInnerTextParamsStrict)
  , FrameInnerTextResult (frameInnerTextResultValue)
  , FrameInputValueParams (frameInputValueParamsStrict)
  , FrameInputValueResult (frameInputValueResultValue)
  , FrameIsCheckedParams (frameIsCheckedParamsStrict)
  , FrameIsCheckedResult (frameIsCheckedResultValue)
  , FrameIsDisabledParams (frameIsDisabledParamsStrict)
  , FrameIsDisabledResult (frameIsDisabledResultValue)
  , FrameIsEditableParams (frameIsEditableParamsStrict)
  , FrameIsEditableResult (frameIsEditableResultValue)
  , FrameIsEnabledParams (frameIsEnabledParamsStrict)
  , FrameIsEnabledResult (frameIsEnabledResultValue)
  , FrameIsHiddenParams (frameIsHiddenParamsStrict)
  , FrameIsHiddenResult (frameIsHiddenResultValue)
  , FrameIsVisibleParams (frameIsVisibleParamsStrict)
  , FrameIsVisibleResult (frameIsVisibleResultValue)
  , FrameQueryCountResult (frameQueryCountResultValue)
  , FrameTextContentParams (frameTextContentParamsStrict)
  , FrameTextContentResult (frameTextContentResultValue)
  , mkFrameGetAttributeParams
  , mkFrameInnerHTMLParams
  , mkFrameInnerTextParams
  , mkFrameInputValueParams
  , mkFrameIsCheckedParams
  , mkFrameIsDisabledParams
  , mkFrameIsEditableParams
  , mkFrameIsEnabledParams
  , mkFrameIsHiddenParams
  , mkFrameIsVisibleParams
  , mkFrameQueryCountParams
  , mkFrameTextContentParams
  )
import Playwright.Selector
  ( Selector
  )
import Playwright.Timeout
  ( Timeout
  )

queryWithTimeout ::
  (ToJSON params, FromJSON result) =>
  Text ->
  (Selector -> Timeout -> params) ->
  (params -> params) ->
  (result -> a) ->
  Frame ->
  Selector ->
  Timeout ->
  IO a
queryWithTimeout method mk setStrict project frame selector timeout =
  project <$> frameSend frame method (setStrict (mk selector timeout))

innerTextFrame :: Frame -> Selector -> Timeout -> IO Text
innerTextFrame =
  queryWithTimeout
    "innerText"
    mkFrameInnerTextParams
    (\params -> params {frameInnerTextParamsStrict = Just True})
    frameInnerTextResultValue

innerHtmlFrame :: Frame -> Selector -> Timeout -> IO Text
innerHtmlFrame =
  queryWithTimeout
    "innerHTML"
    mkFrameInnerHTMLParams
    (\params -> params {frameInnerHTMLParamsStrict = Just True})
    frameInnerHTMLResultValue

textContentFrame :: Frame -> Selector -> Timeout -> IO (Maybe Text)
textContentFrame =
  queryWithTimeout
    "textContent"
    mkFrameTextContentParams
    (\params -> params {frameTextContentParamsStrict = Just True})
    frameTextContentResultValue

inputValueFrame :: Frame -> Selector -> Timeout -> IO Text
inputValueFrame =
  queryWithTimeout
    "inputValue"
    mkFrameInputValueParams
    (\params -> params {frameInputValueParamsStrict = Just True})
    frameInputValueResultValue

isCheckedFrame :: Frame -> Selector -> Timeout -> IO Bool
isCheckedFrame =
  queryWithTimeout
    "isChecked"
    mkFrameIsCheckedParams
    (\params -> params {frameIsCheckedParamsStrict = Just True})
    frameIsCheckedResultValue

isEnabledFrame :: Frame -> Selector -> Timeout -> IO Bool
isEnabledFrame =
  queryWithTimeout
    "isEnabled"
    mkFrameIsEnabledParams
    (\params -> params {frameIsEnabledParamsStrict = Just True})
    frameIsEnabledResultValue

isDisabledFrame :: Frame -> Selector -> Timeout -> IO Bool
isDisabledFrame =
  queryWithTimeout
    "isDisabled"
    mkFrameIsDisabledParams
    (\params -> params {frameIsDisabledParamsStrict = Just True})
    frameIsDisabledResultValue

isEditableFrame :: Frame -> Selector -> Timeout -> IO Bool
isEditableFrame =
  queryWithTimeout
    "isEditable"
    mkFrameIsEditableParams
    (\params -> params {frameIsEditableParamsStrict = Just True})
    frameIsEditableResultValue

getAttributeFrame :: Frame -> Selector -> Text -> Timeout -> IO (Maybe Text)
getAttributeFrame frame selector name timeout =
  queryWithTimeout
    "getAttribute"
    (\sel t -> mkFrameGetAttributeParams name sel t)
    (\params -> params {frameGetAttributeParamsStrict = Just True})
    frameGetAttributeResultValue
    frame
    selector
    timeout

isVisibleFrame :: Frame -> Selector -> IO Bool
isVisibleFrame frame selector = do
  result <-
    frameSend
      frame
      "isVisible"
      (mkFrameIsVisibleParams selector)
        { frameIsVisibleParamsStrict = Just True
        }
  pure (frameIsVisibleResultValue result)

isHiddenFrame :: Frame -> Selector -> IO Bool
isHiddenFrame frame selector = do
  result <-
    frameSend
      frame
      "isHidden"
      (mkFrameIsHiddenParams selector)
        { frameIsHiddenParamsStrict = Just True
        }
  pure (frameIsHiddenResultValue result)

queryCountFrame :: Frame -> Selector -> IO Int
queryCountFrame frame selector = do
  result <- frameSend frame "queryCount" (mkFrameQueryCountParams selector)
  pure (frameQueryCountResultValue result)

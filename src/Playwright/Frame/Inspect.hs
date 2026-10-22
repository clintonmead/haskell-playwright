module Playwright.Frame.Inspect
  ( frameElement
  , getAttributeFrame
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
  , querySelectorAllFrame
  , querySelectorFrame
  , textContentFrame
  , textContentFrameStrict
  ) where

import Playwright.Monad
  ( Playwright
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Data.Text
  ( Text
  )
import Playwright.ElementHandle
  ( ElementHandle
  , elementHandleFromRef
  )
import Playwright.Frame
  ( Frame
  , frameSend
  , frameSendWithTimeout
  )
import Playwright.Protocol
  ( FrameFrameElementResult (frameFrameElementResultElement)
  , FrameGetAttributeParams (frameGetAttributeParamsStrict)
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
  , FrameQuerySelectorAllResult (frameQuerySelectorAllResultElements)
  , FrameQuerySelectorParams (frameQuerySelectorParamsStrict)
  , FrameQuerySelectorResult (frameQuerySelectorResultElement)
  , FrameTextContentParams (frameTextContentParamsStrict)
  , FrameTextContentResult (frameTextContentResultValue)
  , mkFrameFrameElementParams
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
  , mkFrameQuerySelectorAllParams
  , mkFrameQuerySelectorParams
  , mkFrameTextContentParams
  )
import Playwright.Selector
  ( Selector
  )
import Playwright.Timeout
  ( Timeout
  )

frameElement :: Frame -> Playwright ElementHandle
frameElement frame = do
  result <- frameSend frame "frameElement" mkFrameFrameElementParams
  pure
    ( elementHandleFromRef
        (frameFrameElementResultElement result)
    )

queryWithTimeout ::
  (ToJSON params, FromJSON result) =>
  Text ->
  (Selector -> params) ->
  (params -> params) ->
  (result -> a) ->
  Frame ->
  Selector ->
  Timeout ->
  Playwright a
queryWithTimeout method mk setStrict project frame selector timeout =
  project <$> frameSendWithTimeout frame method timeout (setStrict (mk selector))

innerTextFrame :: Frame -> Selector -> Timeout -> Playwright Text
innerTextFrame =
  queryWithTimeout
    "innerText"
    mkFrameInnerTextParams
    (\params -> params {frameInnerTextParamsStrict = Just True})
    frameInnerTextResultValue

innerHtmlFrame :: Frame -> Selector -> Timeout -> Playwright Text
innerHtmlFrame =
  queryWithTimeout
    "innerHTML"
    mkFrameInnerHTMLParams
    (\params -> params {frameInnerHTMLParamsStrict = Just True})
    frameInnerHTMLResultValue

textContentFrame :: Frame -> Selector -> Timeout -> Playwright (Maybe Text)
textContentFrame frame selector timeout =
  textContentFrameStrict frame selector timeout (Just True)

textContentFrameStrict :: Frame -> Selector -> Timeout -> Maybe Bool -> Playwright (Maybe Text)
textContentFrameStrict frame selector timeout strict =
  queryWithTimeout
    "textContent"
    mkFrameTextContentParams
    (\params -> params {frameTextContentParamsStrict = strict})
    frameTextContentResultValue
    frame
    selector
    timeout

inputValueFrame :: Frame -> Selector -> Timeout -> Playwright Text
inputValueFrame =
  queryWithTimeout
    "inputValue"
    mkFrameInputValueParams
    (\params -> params {frameInputValueParamsStrict = Just True})
    frameInputValueResultValue

isCheckedFrame :: Frame -> Selector -> Timeout -> Playwright Bool
isCheckedFrame =
  queryWithTimeout
    "isChecked"
    mkFrameIsCheckedParams
    (\params -> params {frameIsCheckedParamsStrict = Just True})
    frameIsCheckedResultValue

isEnabledFrame :: Frame -> Selector -> Timeout -> Playwright Bool
isEnabledFrame =
  queryWithTimeout
    "isEnabled"
    mkFrameIsEnabledParams
    (\params -> params {frameIsEnabledParamsStrict = Just True})
    frameIsEnabledResultValue

isDisabledFrame :: Frame -> Selector -> Timeout -> Playwright Bool
isDisabledFrame =
  queryWithTimeout
    "isDisabled"
    mkFrameIsDisabledParams
    (\params -> params {frameIsDisabledParamsStrict = Just True})
    frameIsDisabledResultValue

isEditableFrame :: Frame -> Selector -> Timeout -> Playwright Bool
isEditableFrame =
  queryWithTimeout
    "isEditable"
    mkFrameIsEditableParams
    (\params -> params {frameIsEditableParamsStrict = Just True})
    frameIsEditableResultValue

getAttributeFrame :: Frame -> Selector -> Text -> Timeout -> Playwright (Maybe Text)
getAttributeFrame frame selector name timeout =
  queryWithTimeout
    "getAttribute"
    (\sel -> mkFrameGetAttributeParams name sel)
    (\params -> params {frameGetAttributeParamsStrict = Just True})
    frameGetAttributeResultValue
    frame
    selector
    timeout

isVisibleFrame :: Frame -> Selector -> Playwright Bool
isVisibleFrame frame selector = do
  result <-
    frameSend
      frame
      "isVisible"
      (mkFrameIsVisibleParams selector)
        { frameIsVisibleParamsStrict = Just True
        }
  pure (frameIsVisibleResultValue result)

isHiddenFrame :: Frame -> Selector -> Playwright Bool
isHiddenFrame frame selector = do
  result <-
    frameSend
      frame
      "isHidden"
      (mkFrameIsHiddenParams selector)
        { frameIsHiddenParamsStrict = Just True
        }
  pure (frameIsHiddenResultValue result)

queryCountFrame :: Frame -> Selector -> Playwright Int
queryCountFrame frame selector = do
  result <- frameSend frame "queryCount" (mkFrameQueryCountParams selector)
  pure (frameQueryCountResultValue result)

querySelectorAllFrame :: Frame -> Selector -> Playwright [ElementHandle]
querySelectorAllFrame frame selector = do
  result <- frameSend frame "querySelectorAll" (mkFrameQuerySelectorAllParams selector)
  pure
    ( fmap
        (elementHandleFromRef)
        (frameQuerySelectorAllResultElements result)
    )

querySelectorFrame :: Frame -> Selector -> Maybe Bool -> Playwright (Maybe ElementHandle)
querySelectorFrame frame selector strict = do
  result <-
    frameSend
      frame
      "querySelector"
      (mkFrameQuerySelectorParams selector)
        { frameQuerySelectorParamsStrict = strict
        }
  pure
    ( fmap
        (elementHandleFromRef)
        (frameQuerySelectorResultElement result)
    )

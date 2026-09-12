module Playwright.Evaluate
  ( allInnerTexts
  , allTextContents
  , evaluate
  , evaluateBool
  , evaluateBoolWith
  , evaluateNumber
  , evaluateNumberWith
  , evaluateNumbersWith
  , evaluateHandle
  , evaluateHandleExpression
  , evaluateHandleOnElement
  , evaluateHandleOnFrame
  , evaluateHandleOnLocator
  , evaluateHandleWith
  , evaluateOnElement
  , evaluateOnElementBoolWith
  , evaluateOnElementNumber
  , evaluateOnElementText
  , evaluateOnElementWith
  , evaluateOnFrame
  , evaluateOnFrameBool
  , evaluateOnFrameText
  , evaluateOnHandle
  , evaluateOnHandleExpression
  , evaluateOnHandleExpressionNumbers
  , evaluateOnHandleNumbers
  , evaluateOnLocator
  , evaluateOnLocatorAllTexts
  , evaluateOnLocatorBool
  , evaluateOnLocatorNumber
  , evaluateOnLocatorText
  , evaluateOnLocatorWith
  , evaluateSpecial
  , evaluateSpecialWith
  , evaluateText
  , evaluateTextWith
  , evaluateTexts
  , evaluateWith
  , evalOnSelector
  , evalOnSelectorAll
  , evalOnSelectorAllBool
  , evalOnSelectorAllHandle
  , evalOnSelectorAllHandleNumber
  , evalOnSelectorAllHandleTexts
  , evalOnSelectorAllNumber
  , evalOnSelectorAllTexts
  , evalOnSelectorAllWith
  , evalOnSelectorBool
  , evalOnSelectorHandle
  , evalOnSelectorHandleText
  , evalOnSelectorText
  , evalOnSelectorTextWith
  , evalOnSelectorWith
  ) where

import Control.Exception
  ( finally
  , throwIO
  )
import Data.Text
  ( Text
  )
import Playwright.Channel
  ( channelConnection
  , send
  )
import Playwright.ElementHandle
  ( ElementHandle (elementHandleChannel)
  )
import Playwright.ElementHandle.Action
  ( disposeHandle
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure, EvaluateUnexpected)
  )
import Playwright.Frame
  ( Frame (frameChannel)
  , evalOnSelectorAllFrame
  , evalOnSelectorFrame
  , evalOnSelectorFrameStrict
  , evaluateFrame
  , evaluateFrameWith
  , frameSend
  )
import Playwright.JsHandle
  ( JsHandle (jsHandleChannel)
  , jsHandleFromRef
  )
import Playwright.Locator
  ( Locator (locatorFrame, locatorSelector)
  , elementHandle
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Page
  ( Page (pageMainFrame)
  )
import Playwright.Protocol
  ( FrameEvaluateExpressionHandleParams (frameEvaluateExpressionHandleParamsIsFunction)
  , FrameEvaluateExpressionHandleResult (frameEvaluateExpressionHandleResultHandle)
  , JSHandleEvaluateExpressionHandleParams (jSHandleEvaluateExpressionHandleParamsIsFunction)
  , JSHandleEvaluateExpressionHandleResult (jSHandleEvaluateExpressionHandleResultHandle)
  , JSHandleEvaluateExpressionParams (jSHandleEvaluateExpressionParamsIsFunction)
  , JSHandleEvaluateExpressionResult (jSHandleEvaluateExpressionResultValue)
  , ElementHandleEvalOnSelectorAllParams (elementHandleEvalOnSelectorAllParamsIsFunction)
  , ElementHandleEvalOnSelectorAllResult (elementHandleEvalOnSelectorAllResultValue)
  , ElementHandleEvalOnSelectorParams (elementHandleEvalOnSelectorParamsIsFunction)
  , ElementHandleEvalOnSelectorResult (elementHandleEvalOnSelectorResultValue)
  , mkElementHandleEvalOnSelectorAllParams
  , mkElementHandleEvalOnSelectorParams
  , mkFrameEvaluateExpressionHandleParams
  , mkJSHandleEvaluateExpressionHandleParams
  , mkJSHandleEvaluateExpressionParams
  )
import Playwright.Selector
  ( Selector
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  , undefinedArgument
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadArray, PayloadBool, PayloadNumber, PayloadSpecial, PayloadString)
  , SerializedValue (SerializedNode)
  )
import Playwright.SpecialJsValue
  ( SpecialJsValue
  )

evaluate :: Page -> Text -> IO SerializedValue
evaluate page = evaluateFrame (pageMainFrame page)

evaluateWith :: Page -> Text -> SerializedArgument -> IO SerializedValue
evaluateWith page =
  evaluateFrameWith (pageMainFrame page)

evaluateHandle :: Page -> Text -> IO JsHandle
evaluateHandle page =
  evaluateHandleOnFrame (pageMainFrame page)

evaluateHandleExpression :: Page -> Text -> IO JsHandle
evaluateHandleExpression page expression =
  evaluateHandleOnFrameIsFunction (pageMainFrame page) expression undefinedArgument False

evaluateHandleWith :: Page -> Text -> SerializedArgument -> IO JsHandle
evaluateHandleWith page =
  evaluateHandleOnFrameWith (pageMainFrame page)

evaluateHandleOnFrame :: Frame -> Text -> IO JsHandle
evaluateHandleOnFrame frame expression =
  evaluateHandleOnFrameWith frame expression undefinedArgument

evaluateHandleOnFrameWith :: Frame -> Text -> SerializedArgument -> IO JsHandle
evaluateHandleOnFrameWith frame expression arg =
  evaluateHandleOnFrameIsFunction frame expression arg True

evaluateHandleOnFrameIsFunction :: Frame -> Text -> SerializedArgument -> Bool -> IO JsHandle
evaluateHandleOnFrameIsFunction frame expression arg isFunction = do
  result <-
    frameSend
      frame
      "evaluateExpressionHandle"
      (mkFrameEvaluateExpressionHandleParams arg expression)
        { frameEvaluateExpressionHandleParamsIsFunction = Just isFunction
        }
  pure
    ( jsHandleFromRef
        (channelConnection (frameChannel frame))
        (frameEvaluateExpressionHandleResultHandle result)
    )

evaluateHandleOnElement :: ElementHandle -> Text -> IO JsHandle
evaluateHandleOnElement handle expression = do
  result <-
    send
      (elementHandleChannel handle)
      (MethodName "evaluateExpressionHandle")
      (mkJSHandleEvaluateExpressionHandleParams undefinedArgument expression)
        { jSHandleEvaluateExpressionHandleParamsIsFunction = Just True
        }
  pure
    ( jsHandleFromRef
        (channelConnection (elementHandleChannel handle))
        (jSHandleEvaluateExpressionHandleResultHandle result)
    )

evaluateOnFrame :: Frame -> Text -> IO SerializedValue
evaluateOnFrame =
  evaluateFrame

evaluateOnFrameText :: Frame -> Text -> IO Text
evaluateOnFrameText frame expression =
  requireText =<< evaluateOnFrame frame expression

evaluateOnFrameBool :: Frame -> Text -> IO Bool
evaluateOnFrameBool frame expression =
  requireBool =<< evaluateOnFrame frame expression

evaluateOnElement :: ElementHandle -> Text -> IO SerializedValue
evaluateOnElement handle expression =
  evaluateOnElementWith handle expression undefinedArgument

evaluateOnElementWith :: ElementHandle -> Text -> SerializedArgument -> IO SerializedValue
evaluateOnElementWith handle expression arg = do
  result <-
    send
      (elementHandleChannel handle)
      (MethodName "evaluateExpression")
      (mkJSHandleEvaluateExpressionParams arg expression)
        { jSHandleEvaluateExpressionParamsIsFunction = Just True
        }
  pure (jSHandleEvaluateExpressionResultValue result)

evaluateOnElementBoolWith :: ElementHandle -> Text -> SerializedArgument -> IO Bool
evaluateOnElementBoolWith handle expression arg =
  requireBool =<< evaluateOnElementWith handle expression arg

evaluateOnElementText :: ElementHandle -> Text -> IO Text
evaluateOnElementText handle expression =
  requireText =<< evaluateOnElement handle expression

evaluateOnElementNumber :: ElementHandle -> Text -> IO Double
evaluateOnElementNumber handle expression =
  requireNumber =<< evaluateOnElement handle expression

evaluateOnHandle :: JsHandle -> Text -> IO SerializedValue
evaluateOnHandle handle expression =
  evaluateOnHandleIsFunction handle expression True

evaluateOnHandleExpression :: JsHandle -> Text -> IO SerializedValue
evaluateOnHandleExpression handle expression =
  evaluateOnHandleIsFunction handle expression False

evaluateOnHandleNumbers :: JsHandle -> Text -> IO [Double]
evaluateOnHandleNumbers handle expression =
  requireNumbers =<< evaluateOnHandle handle expression

evaluateOnHandleExpressionNumbers :: JsHandle -> Text -> IO [Double]
evaluateOnHandleExpressionNumbers handle expression =
  requireNumbers =<< evaluateOnHandleExpression handle expression

evaluateOnHandleIsFunction :: JsHandle -> Text -> Bool -> IO SerializedValue
evaluateOnHandleIsFunction handle expression isFunction = do
  result <-
    send
      (jsHandleChannel handle)
      (MethodName "evaluateExpression")
      (mkJSHandleEvaluateExpressionParams undefinedArgument expression)
        { jSHandleEvaluateExpressionParamsIsFunction = Just isFunction
        }
  pure (jSHandleEvaluateExpressionResultValue result)

evaluateOnLocator :: Locator -> Text -> IO SerializedValue
evaluateOnLocator loc expression =
  evaluateOnLocatorWith loc expression undefinedArgument

evaluateOnLocatorWith :: Locator -> Text -> SerializedArgument -> IO SerializedValue
evaluateOnLocatorWith loc expression arg =
  evalOnSelectorFrameStrict (locatorFrame loc) (locatorSelector loc) expression arg

evaluateHandleOnLocator :: Locator -> Text -> IO JsHandle
evaluateHandleOnLocator loc expression = do
  found <- elementHandle loc
  case found of
    Nothing ->
      throwIO (DecodeFailure "Could not resolve locator to DOM Element")
    Just handle ->
      evaluateHandleOnElement handle expression `finally` disposeHandle handle

allInnerTexts :: Locator -> IO [Text]
allInnerTexts loc =
  evaluateOnLocatorAllTexts loc "ee => ee.map(e => e.innerText)"

allTextContents :: Locator -> IO [Text]
allTextContents loc =
  evaluateOnLocatorAllTexts loc "ee => ee.map(e => e.textContent || '')"

evalOnSelectorHandle :: ElementHandle -> Selector -> Text -> IO SerializedValue
evalOnSelectorHandle handle selector expression = do
  result <-
    send
      (elementHandleChannel handle)
      (MethodName "evalOnSelector")
      (mkElementHandleEvalOnSelectorParams undefinedArgument expression selector)
        { elementHandleEvalOnSelectorParamsIsFunction = Just True
        }
  pure (elementHandleEvalOnSelectorResultValue result)

evalOnSelectorHandleText :: ElementHandle -> Selector -> Text -> IO Text
evalOnSelectorHandleText handle selector expression =
  requireText =<< evalOnSelectorHandle handle selector expression

evalOnSelectorAllHandle :: ElementHandle -> Selector -> Text -> IO SerializedValue
evalOnSelectorAllHandle handle selector expression = do
  result <-
    send
      (elementHandleChannel handle)
      (MethodName "evalOnSelectorAll")
      (mkElementHandleEvalOnSelectorAllParams undefinedArgument expression selector)
        { elementHandleEvalOnSelectorAllParamsIsFunction = Just True
        }
  pure (elementHandleEvalOnSelectorAllResultValue result)

evalOnSelectorAllHandleTexts :: ElementHandle -> Selector -> Text -> IO [Text]
evalOnSelectorAllHandleTexts handle selector expression =
  requireTexts =<< evalOnSelectorAllHandle handle selector expression

evalOnSelectorAllHandleNumber :: ElementHandle -> Selector -> Text -> IO Double
evalOnSelectorAllHandleNumber handle selector expression =
  requireNumber =<< evalOnSelectorAllHandle handle selector expression

evalOnSelector :: Page -> Selector -> Text -> IO SerializedValue
evalOnSelector page selector expression =
  evalOnSelectorWith page selector expression undefinedArgument

evalOnSelectorWith :: Page -> Selector -> Text -> SerializedArgument -> IO SerializedValue
evalOnSelectorWith page selector expression arg =
  evalOnSelectorFrame (pageMainFrame page) selector expression arg

evalOnSelectorText :: Page -> Selector -> Text -> IO Text
evalOnSelectorText page selector expression =
  requireText =<< evalOnSelector page selector expression

evalOnSelectorTextWith :: Page -> Selector -> Text -> SerializedArgument -> IO Text
evalOnSelectorTextWith page selector expression arg =
  requireText =<< evalOnSelectorWith page selector expression arg

evalOnSelectorBool :: Page -> Selector -> Text -> IO Bool
evalOnSelectorBool page selector expression =
  requireBool =<< evalOnSelector page selector expression

evalOnSelectorAll :: Page -> Selector -> Text -> IO SerializedValue
evalOnSelectorAll page selector expression =
  evalOnSelectorAllWith page selector expression undefinedArgument

evalOnSelectorAllWith :: Page -> Selector -> Text -> SerializedArgument -> IO SerializedValue
evalOnSelectorAllWith page selector expression arg =
  evalOnSelectorAllFrame (pageMainFrame page) selector expression arg

evalOnSelectorAllBool :: Page -> Selector -> Text -> IO Bool
evalOnSelectorAllBool page selector expression =
  requireBool =<< evalOnSelectorAll page selector expression

evalOnSelectorAllNumber :: Page -> Selector -> Text -> IO Double
evalOnSelectorAllNumber page selector expression =
  requireNumber =<< evalOnSelectorAll page selector expression

evalOnSelectorAllTexts :: Page -> Selector -> Text -> IO [Text]
evalOnSelectorAllTexts page selector expression =
  requireTexts =<< evalOnSelectorAll page selector expression

evaluateOnLocatorAllTexts :: Locator -> Text -> IO [Text]
evaluateOnLocatorAllTexts loc expression =
  requireTexts
    =<< evalOnSelectorAllFrame (locatorFrame loc) (locatorSelector loc) expression undefinedArgument

evaluateText :: Page -> Text -> IO Text
evaluateText page expression =
  requireText =<< evaluate page expression

evaluateTextWith :: Page -> Text -> SerializedArgument -> IO Text
evaluateTextWith page expression arg =
  requireText =<< evaluateWith page expression arg

evaluateBool :: Page -> Text -> IO Bool
evaluateBool page expression =
  requireBool =<< evaluate page expression

evaluateBoolWith :: Page -> Text -> SerializedArgument -> IO Bool
evaluateBoolWith page expression arg =
  requireBool =<< evaluateWith page expression arg

evaluateOnLocatorText :: Locator -> Text -> IO Text
evaluateOnLocatorText loc expression =
  requireText =<< evaluateOnLocator loc expression

evaluateOnLocatorBool :: Locator -> Text -> IO Bool
evaluateOnLocatorBool loc expression =
  requireBool =<< evaluateOnLocator loc expression

evaluateOnLocatorNumber :: Locator -> Text -> IO Double
evaluateOnLocatorNumber loc expression =
  requireNumber =<< evaluateOnLocator loc expression

evaluateNumber :: Page -> Text -> IO Double
evaluateNumber page expression =
  requireNumber =<< evaluate page expression

evaluateNumberWith :: Page -> Text -> SerializedArgument -> IO Double
evaluateNumberWith page expression arg =
  requireNumber =<< evaluateWith page expression arg

evaluateNumbersWith :: Page -> Text -> SerializedArgument -> IO [Double]
evaluateNumbersWith page expression arg =
  requireNumbers =<< evaluateWith page expression arg

evaluateSpecial :: Page -> Text -> IO SpecialJsValue
evaluateSpecial page expression =
  requireSpecial =<< evaluate page expression

evaluateSpecialWith :: Page -> Text -> SerializedArgument -> IO SpecialJsValue
evaluateSpecialWith page expression arg =
  requireSpecial =<< evaluateWith page expression arg

evaluateTexts :: Page -> Text -> IO [Text]
evaluateTexts page expression =
  requireTexts =<< evaluate page expression

requireText :: SerializedValue -> IO Text
requireText value =
  case value of
    SerializedNode (PayloadString text) _ ->
      pure text
    _ ->
      throwIO (EvaluateUnexpected value)

requireBool :: SerializedValue -> IO Bool
requireBool value =
  case value of
    SerializedNode (PayloadBool flag) _ ->
      pure flag
    _ ->
      throwIO (EvaluateUnexpected value)

requireNumber :: SerializedValue -> IO Double
requireNumber value =
  case value of
    SerializedNode (PayloadNumber number) _ ->
      pure number
    _ ->
      throwIO (EvaluateUnexpected value)

requireNumbers :: SerializedValue -> IO [Double]
requireNumbers value =
  case value of
    SerializedNode (PayloadArray items) _ ->
      traverse requireNumber items
    _ ->
      throwIO (EvaluateUnexpected value)

requireSpecial :: SerializedValue -> IO SpecialJsValue
requireSpecial value =
  case value of
    SerializedNode (PayloadSpecial special) _ ->
      pure special
    _ ->
      throwIO (EvaluateUnexpected value)

requireTexts :: SerializedValue -> IO [Text]
requireTexts value =
  case value of
    SerializedNode (PayloadArray items) _ ->
      traverse requireText items
    _ ->
      throwIO (EvaluateUnexpected value)

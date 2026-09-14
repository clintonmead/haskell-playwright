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
  , evaluateOnHandleText
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

import Playwright.Monad
  ( Playwright
  )
import UnliftIO.Exception
  ( finally
  , throwIO
  )
import Data.Text
  ( Text
  )
import Playwright.Channel
  ( send
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
  ( Frame
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

evaluate :: Page -> Text -> Playwright SerializedValue
evaluate page = evaluateFrame (pageMainFrame page)

evaluateWith :: Page -> Text -> SerializedArgument -> Playwright SerializedValue
evaluateWith page =
  evaluateFrameWith (pageMainFrame page)

evaluateHandle :: Page -> Text -> Playwright JsHandle
evaluateHandle page =
  evaluateHandleOnFrame (pageMainFrame page)

evaluateHandleExpression :: Page -> Text -> Playwright JsHandle
evaluateHandleExpression page expression =
  evaluateHandleOnFrameIsFunction (pageMainFrame page) expression undefinedArgument False

evaluateHandleWith :: Page -> Text -> SerializedArgument -> Playwright JsHandle
evaluateHandleWith page =
  evaluateHandleOnFrameWith (pageMainFrame page)

evaluateHandleOnFrame :: Frame -> Text -> Playwright JsHandle
evaluateHandleOnFrame frame expression =
  evaluateHandleOnFrameWith frame expression undefinedArgument

evaluateHandleOnFrameWith :: Frame -> Text -> SerializedArgument -> Playwright JsHandle
evaluateHandleOnFrameWith frame expression arg =
  evaluateHandleOnFrameIsFunction frame expression arg True

evaluateHandleOnFrameIsFunction :: Frame -> Text -> SerializedArgument -> Bool -> Playwright JsHandle
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
        (frameEvaluateExpressionHandleResultHandle result)
    )

evaluateHandleOnElement :: ElementHandle -> Text -> Playwright JsHandle
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
        (jSHandleEvaluateExpressionHandleResultHandle result)
    )

evaluateOnFrame :: Frame -> Text -> Playwright SerializedValue
evaluateOnFrame =
  evaluateFrame

evaluateOnFrameText :: Frame -> Text -> Playwright Text
evaluateOnFrameText frame expression =
  requireText =<< evaluateOnFrame frame expression

evaluateOnFrameBool :: Frame -> Text -> Playwright Bool
evaluateOnFrameBool frame expression =
  requireBool =<< evaluateOnFrame frame expression

evaluateOnElement :: ElementHandle -> Text -> Playwright SerializedValue
evaluateOnElement handle expression =
  evaluateOnElementWith handle expression undefinedArgument

evaluateOnElementWith :: ElementHandle -> Text -> SerializedArgument -> Playwright SerializedValue
evaluateOnElementWith handle expression arg = do
  result <-
    send
      (elementHandleChannel handle)
      (MethodName "evaluateExpression")
      (mkJSHandleEvaluateExpressionParams arg expression)
        { jSHandleEvaluateExpressionParamsIsFunction = Just True
        }
  pure (jSHandleEvaluateExpressionResultValue result)

evaluateOnElementBoolWith :: ElementHandle -> Text -> SerializedArgument -> Playwright Bool
evaluateOnElementBoolWith handle expression arg =
  requireBool =<< evaluateOnElementWith handle expression arg

evaluateOnElementText :: ElementHandle -> Text -> Playwright Text
evaluateOnElementText handle expression =
  requireText =<< evaluateOnElement handle expression

evaluateOnElementNumber :: ElementHandle -> Text -> Playwright Double
evaluateOnElementNumber handle expression =
  requireNumber =<< evaluateOnElement handle expression

evaluateOnHandle :: JsHandle -> Text -> Playwright SerializedValue
evaluateOnHandle handle expression =
  evaluateOnHandleIsFunction handle expression True

evaluateOnHandleExpression :: JsHandle -> Text -> Playwright SerializedValue
evaluateOnHandleExpression handle expression =
  evaluateOnHandleIsFunction handle expression False

evaluateOnHandleNumbers :: JsHandle -> Text -> Playwright [Double]
evaluateOnHandleNumbers handle expression =
  requireNumbers =<< evaluateOnHandle handle expression

evaluateOnHandleText :: JsHandle -> Text -> Playwright Text
evaluateOnHandleText handle expression =
  requireText =<< evaluateOnHandle handle expression

evaluateOnHandleExpressionNumbers :: JsHandle -> Text -> Playwright [Double]
evaluateOnHandleExpressionNumbers handle expression =
  requireNumbers =<< evaluateOnHandleExpression handle expression

evaluateOnHandleIsFunction :: JsHandle -> Text -> Bool -> Playwright SerializedValue
evaluateOnHandleIsFunction handle expression isFunction = do
  result <-
    send
      (jsHandleChannel handle)
      (MethodName "evaluateExpression")
      (mkJSHandleEvaluateExpressionParams undefinedArgument expression)
        { jSHandleEvaluateExpressionParamsIsFunction = Just isFunction
        }
  pure (jSHandleEvaluateExpressionResultValue result)

evaluateOnLocator :: Locator -> Text -> Playwright SerializedValue
evaluateOnLocator loc expression =
  evaluateOnLocatorWith loc expression undefinedArgument

evaluateOnLocatorWith :: Locator -> Text -> SerializedArgument -> Playwright SerializedValue
evaluateOnLocatorWith loc expression arg =
  evalOnSelectorFrameStrict (locatorFrame loc) (locatorSelector loc) expression arg

evaluateHandleOnLocator :: Locator -> Text -> Playwright JsHandle
evaluateHandleOnLocator loc expression = do
  found <- elementHandle loc
  case found of
    Nothing ->
      throwIO (DecodeFailure "Could not resolve locator to DOM Element")
    Just handle ->
      evaluateHandleOnElement handle expression `finally` disposeHandle handle

allInnerTexts :: Locator -> Playwright [Text]
allInnerTexts loc =
  evaluateOnLocatorAllTexts loc "ee => ee.map(e => e.innerText)"

allTextContents :: Locator -> Playwright [Text]
allTextContents loc =
  evaluateOnLocatorAllTexts loc "ee => ee.map(e => e.textContent || '')"

evalOnSelectorHandle :: ElementHandle -> Selector -> Text -> Playwright SerializedValue
evalOnSelectorHandle handle selector expression = do
  result <-
    send
      (elementHandleChannel handle)
      (MethodName "evalOnSelector")
      (mkElementHandleEvalOnSelectorParams undefinedArgument expression selector)
        { elementHandleEvalOnSelectorParamsIsFunction = Just True
        }
  pure (elementHandleEvalOnSelectorResultValue result)

evalOnSelectorHandleText :: ElementHandle -> Selector -> Text -> Playwright Text
evalOnSelectorHandleText handle selector expression =
  requireText =<< evalOnSelectorHandle handle selector expression

evalOnSelectorAllHandle :: ElementHandle -> Selector -> Text -> Playwright SerializedValue
evalOnSelectorAllHandle handle selector expression = do
  result <-
    send
      (elementHandleChannel handle)
      (MethodName "evalOnSelectorAll")
      (mkElementHandleEvalOnSelectorAllParams undefinedArgument expression selector)
        { elementHandleEvalOnSelectorAllParamsIsFunction = Just True
        }
  pure (elementHandleEvalOnSelectorAllResultValue result)

evalOnSelectorAllHandleTexts :: ElementHandle -> Selector -> Text -> Playwright [Text]
evalOnSelectorAllHandleTexts handle selector expression =
  requireTexts =<< evalOnSelectorAllHandle handle selector expression

evalOnSelectorAllHandleNumber :: ElementHandle -> Selector -> Text -> Playwright Double
evalOnSelectorAllHandleNumber handle selector expression =
  requireNumber =<< evalOnSelectorAllHandle handle selector expression

evalOnSelector :: Page -> Selector -> Text -> Playwright SerializedValue
evalOnSelector page selector expression =
  evalOnSelectorWith page selector expression undefinedArgument

evalOnSelectorWith :: Page -> Selector -> Text -> SerializedArgument -> Playwright SerializedValue
evalOnSelectorWith page selector expression arg =
  evalOnSelectorFrame (pageMainFrame page) selector expression arg

evalOnSelectorText :: Page -> Selector -> Text -> Playwright Text
evalOnSelectorText page selector expression =
  requireText =<< evalOnSelector page selector expression

evalOnSelectorTextWith :: Page -> Selector -> Text -> SerializedArgument -> Playwright Text
evalOnSelectorTextWith page selector expression arg =
  requireText =<< evalOnSelectorWith page selector expression arg

evalOnSelectorBool :: Page -> Selector -> Text -> Playwright Bool
evalOnSelectorBool page selector expression =
  requireBool =<< evalOnSelector page selector expression

evalOnSelectorAll :: Page -> Selector -> Text -> Playwright SerializedValue
evalOnSelectorAll page selector expression =
  evalOnSelectorAllWith page selector expression undefinedArgument

evalOnSelectorAllWith :: Page -> Selector -> Text -> SerializedArgument -> Playwright SerializedValue
evalOnSelectorAllWith page selector expression arg =
  evalOnSelectorAllFrame (pageMainFrame page) selector expression arg

evalOnSelectorAllBool :: Page -> Selector -> Text -> Playwright Bool
evalOnSelectorAllBool page selector expression =
  requireBool =<< evalOnSelectorAll page selector expression

evalOnSelectorAllNumber :: Page -> Selector -> Text -> Playwright Double
evalOnSelectorAllNumber page selector expression =
  requireNumber =<< evalOnSelectorAll page selector expression

evalOnSelectorAllTexts :: Page -> Selector -> Text -> Playwright [Text]
evalOnSelectorAllTexts page selector expression =
  requireTexts =<< evalOnSelectorAll page selector expression

evaluateOnLocatorAllTexts :: Locator -> Text -> Playwright [Text]
evaluateOnLocatorAllTexts loc expression =
  requireTexts
    =<< evalOnSelectorAllFrame (locatorFrame loc) (locatorSelector loc) expression undefinedArgument

evaluateText :: Page -> Text -> Playwright Text
evaluateText page expression =
  requireText =<< evaluate page expression

evaluateTextWith :: Page -> Text -> SerializedArgument -> Playwright Text
evaluateTextWith page expression arg =
  requireText =<< evaluateWith page expression arg

evaluateBool :: Page -> Text -> Playwright Bool
evaluateBool page expression =
  requireBool =<< evaluate page expression

evaluateBoolWith :: Page -> Text -> SerializedArgument -> Playwright Bool
evaluateBoolWith page expression arg =
  requireBool =<< evaluateWith page expression arg

evaluateOnLocatorText :: Locator -> Text -> Playwright Text
evaluateOnLocatorText loc expression =
  requireText =<< evaluateOnLocator loc expression

evaluateOnLocatorBool :: Locator -> Text -> Playwright Bool
evaluateOnLocatorBool loc expression =
  requireBool =<< evaluateOnLocator loc expression

evaluateOnLocatorNumber :: Locator -> Text -> Playwright Double
evaluateOnLocatorNumber loc expression =
  requireNumber =<< evaluateOnLocator loc expression

evaluateNumber :: Page -> Text -> Playwright Double
evaluateNumber page expression =
  requireNumber =<< evaluate page expression

evaluateNumberWith :: Page -> Text -> SerializedArgument -> Playwright Double
evaluateNumberWith page expression arg =
  requireNumber =<< evaluateWith page expression arg

evaluateNumbersWith :: Page -> Text -> SerializedArgument -> Playwright [Double]
evaluateNumbersWith page expression arg =
  requireNumbers =<< evaluateWith page expression arg

evaluateSpecial :: Page -> Text -> Playwright SpecialJsValue
evaluateSpecial page expression =
  requireSpecial =<< evaluate page expression

evaluateSpecialWith :: Page -> Text -> SerializedArgument -> Playwright SpecialJsValue
evaluateSpecialWith page expression arg =
  requireSpecial =<< evaluateWith page expression arg

evaluateTexts :: Page -> Text -> Playwright [Text]
evaluateTexts page expression =
  requireTexts =<< evaluate page expression

requireText :: SerializedValue -> Playwright Text
requireText value =
  case value of
    SerializedNode (PayloadString text) _ ->
      pure text
    _ ->
      throwIO (EvaluateUnexpected value)

requireBool :: SerializedValue -> Playwright Bool
requireBool value =
  case value of
    SerializedNode (PayloadBool flag) _ ->
      pure flag
    _ ->
      throwIO (EvaluateUnexpected value)

requireNumber :: SerializedValue -> Playwright Double
requireNumber value =
  case value of
    SerializedNode (PayloadNumber number) _ ->
      pure number
    _ ->
      throwIO (EvaluateUnexpected value)

requireNumbers :: SerializedValue -> Playwright [Double]
requireNumbers value =
  case value of
    SerializedNode (PayloadArray items) _ ->
      traverse requireNumber items
    _ ->
      throwIO (EvaluateUnexpected value)

requireSpecial :: SerializedValue -> Playwright SpecialJsValue
requireSpecial value =
  case value of
    SerializedNode (PayloadSpecial special) _ ->
      pure special
    _ ->
      throwIO (EvaluateUnexpected value)

requireTexts :: SerializedValue -> Playwright [Text]
requireTexts value =
  case value of
    SerializedNode (PayloadArray items) _ ->
      traverse requireText items
    _ ->
      throwIO (EvaluateUnexpected value)

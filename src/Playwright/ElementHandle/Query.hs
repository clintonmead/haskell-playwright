module Playwright.ElementHandle.Query
  ( querySelectorAllHandle
  , querySelectorHandle
  , waitForSelectorOnHandle
  , waitForSelectorOnHandleWith
  ) where

import Playwright.Monad
  ( Playwright
  )
import Playwright.Channel
  ( send
  , sendWithTimeout
  )
import Playwright.ElementHandle
  ( ElementHandle (elementHandleChannel)
  , elementHandleFromRef
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( ElementHandleQuerySelectorAllResult (elementHandleQuerySelectorAllResultElements)
  , ElementHandleQuerySelectorParams (elementHandleQuerySelectorParamsStrict)
  , ElementHandleQuerySelectorResult (elementHandleQuerySelectorResultElement)
  , ElementHandleWaitForSelectorParams (elementHandleWaitForSelectorParamsState)
  , ElementHandleWaitForSelectorParamsState (ElementHandleWaitForSelectorParamsStateAttached, ElementHandleWaitForSelectorParamsStateDetached, ElementHandleWaitForSelectorParamsStateHidden, ElementHandleWaitForSelectorParamsStateVisible)
  , ElementHandleWaitForSelectorResult (elementHandleWaitForSelectorResultElement)
  , mkElementHandleQuerySelectorAllParams
  , mkElementHandleQuerySelectorParams
  , mkElementHandleWaitForSelectorParams
  )
import Playwright.Selector
  ( Selector
  )
import Playwright.Timeout
  ( Timeout
  , defaultTimeout
  )
import Playwright.WaitForState
  ( WaitForState (WaitForAttached, WaitForDetached, WaitForHidden, WaitForVisible)
  )

querySelectorHandle :: ElementHandle -> Selector -> Playwright (Maybe ElementHandle)
querySelectorHandle handle selector = do
  result <-
    send
      (elementHandleChannel handle)
      (MethodName "querySelector")
      (mkElementHandleQuerySelectorParams selector)
        { elementHandleQuerySelectorParamsStrict = Nothing
        }
  pure
    ( fmap
        (elementHandleFromRef)
        (elementHandleQuerySelectorResultElement result)
    )

querySelectorAllHandle :: ElementHandle -> Selector -> Playwright [ElementHandle]
querySelectorAllHandle handle selector = do
  result <-
    send
      (elementHandleChannel handle)
      (MethodName "querySelectorAll")
      (mkElementHandleQuerySelectorAllParams selector)
  pure
    ( fmap
        (elementHandleFromRef)
        (elementHandleQuerySelectorAllResultElements result)
    )

waitForSelectorOnHandle :: ElementHandle -> Selector -> WaitForState -> Playwright (Maybe ElementHandle)
waitForSelectorOnHandle handle selector state =
  waitForSelectorOnHandleWith handle selector state defaultTimeout

waitForSelectorOnHandleWith :: ElementHandle -> Selector -> WaitForState -> Timeout -> Playwright (Maybe ElementHandle)
waitForSelectorOnHandleWith handle selector state timeout = do
  result <-
    sendWithTimeout
      (elementHandleChannel handle)
      (MethodName "waitForSelector")
      timeout
      (mkElementHandleWaitForSelectorParams selector)
        { elementHandleWaitForSelectorParamsState = Just (toProtocolState state)
        }
  pure
    ( fmap
        (elementHandleFromRef)
        (elementHandleWaitForSelectorResultElement result)
    )

toProtocolState :: WaitForState -> ElementHandleWaitForSelectorParamsState
toProtocolState state =
  case state of
    WaitForAttached ->
      ElementHandleWaitForSelectorParamsStateAttached
    WaitForDetached ->
      ElementHandleWaitForSelectorParamsStateDetached
    WaitForVisible ->
      ElementHandleWaitForSelectorParamsStateVisible
    WaitForHidden ->
      ElementHandleWaitForSelectorParamsStateHidden

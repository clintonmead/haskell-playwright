module Playwright.ElementHandle.Query
  ( querySelectorAllHandle
  , querySelectorHandle
  , waitForSelectorOnHandle
  , waitForSelectorOnHandleWith
  ) where

import Playwright.Channel
  ( Channel (channelConnection)
  , send
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

querySelectorHandle :: ElementHandle -> Selector -> IO (Maybe ElementHandle)
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
        (elementHandleFromRef (channelConnection (elementHandleChannel handle)))
        (elementHandleQuerySelectorResultElement result)
    )

querySelectorAllHandle :: ElementHandle -> Selector -> IO [ElementHandle]
querySelectorAllHandle handle selector = do
  result <-
    send
      (elementHandleChannel handle)
      (MethodName "querySelectorAll")
      (mkElementHandleQuerySelectorAllParams selector)
  pure
    ( fmap
        (elementHandleFromRef (channelConnection (elementHandleChannel handle)))
        (elementHandleQuerySelectorAllResultElements result)
    )

waitForSelectorOnHandle :: ElementHandle -> Selector -> WaitForState -> IO (Maybe ElementHandle)
waitForSelectorOnHandle handle selector state =
  waitForSelectorOnHandleWith handle selector state defaultTimeout

waitForSelectorOnHandleWith :: ElementHandle -> Selector -> WaitForState -> Timeout -> IO (Maybe ElementHandle)
waitForSelectorOnHandleWith handle selector state timeout = do
  result <-
    send
      (elementHandleChannel handle)
      (MethodName "waitForSelector")
      (mkElementHandleWaitForSelectorParams selector timeout)
        { elementHandleWaitForSelectorParamsState = Just (toProtocolState state)
        }
  pure
    ( fmap
        (elementHandleFromRef (channelConnection (elementHandleChannel handle)))
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

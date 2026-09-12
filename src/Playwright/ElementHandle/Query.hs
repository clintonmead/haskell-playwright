module Playwright.ElementHandle.Query
  ( querySelectorAllHandle
  , querySelectorHandle
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
  , mkElementHandleQuerySelectorAllParams
  , mkElementHandleQuerySelectorParams
  )
import Playwright.Selector
  ( Selector
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

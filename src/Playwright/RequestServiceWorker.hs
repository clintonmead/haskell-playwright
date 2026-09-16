module Playwright.RequestServiceWorker
  ( requestServiceWorker
  ) where

import Playwright.Monad
  ( Playwright
  )
import Playwright.Request
  ( Request
  , requestServiceWorkerRef
  )
import Playwright.Worker
  ( Worker
  , workerFromRef
  )

requestServiceWorker :: Request -> Playwright (Maybe Worker)
requestServiceWorker request = do
  ref <- requestServiceWorkerRef request
  pure (fmap workerFromRef ref)

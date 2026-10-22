module Playwright.ContextEvents
  ( onContextPageClose
  , onContextPageLoad
  ) where

import Playwright.BrowserContext
  ( BrowserContext
  , contextPages
  , onContextPage
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.Page
  ( Page
  , onPageClose
  )
import Playwright.Page.Wait
  ( onLoad
  )

-- | Runs the callback with each page of the context when that page's load
-- event fires, including pages that already exist. The callback is given the
-- page, as upstream's 'pageload'.
onContextPageLoad :: BrowserContext -> (Page -> Playwright ()) -> Playwright ()
onContextPageLoad ctx callback = do
  existing <- contextPages ctx
  mapM_ (\page -> onLoad page (callback page)) existing
  onContextPage ctx (\page -> onLoad page (callback page))

-- | Runs the callback with each page of the context when that page closes,
-- including pages that already exist, as upstream's 'pageclose'.
onContextPageClose :: BrowserContext -> (Page -> Playwright ()) -> Playwright ()
onContextPageClose ctx callback = do
  existing <- contextPages ctx
  mapM_ (\page -> onPageClose page (callback page)) existing
  onContextPage ctx (\page -> onPageClose page (callback page))

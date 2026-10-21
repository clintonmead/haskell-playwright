module Playwright.BeforeunloadSpec
  ( tests
  ) where

import Playwright
  ( AriaRole (AriaRole)
  , ClickOptions (clickOptionsNoWaitAfter, clickOptionsTimeout)
  , ContentType (ContentType)
  , Dialog (dialogDefaultValue, dialogMessage, dialogType)
  , DialogType (DialogBeforeUnload)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType, fulfillStatus)
  , HttpStatus (HttpStatus)
  , UrlMatch (UrlMatchGlob)
  , acceptDialog
  , armDialog
  , click
  , clickWith
  , closeContext
  , closePage
  , closePageWith
  , defaultClickOptions
  , defaultFulfillOptions
  , defaultGetByRoleOptions
  , defaultGotoOptions
  , defaultNewContextOptions
  , dismissDialog
  , evaluate
  , evaluateText
  , fulfillRoute
  , getByRole
  , goto
  , isClosed
  , milliseconds
  , newPage
  , route
  , setContent
  , urlFromText
  , waitForClose
  , waitForURL
  )
import Playwright.Harness
  ( locate
  , withAssets
  , withAssetsContextWith
  , assertEqual
  , assertFailure
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( wait
  , withAsync
  )

tests :: TestTree
tests =
  testGroup
    "beforeunload"
    [ testCase "should close browser with beforeunload page" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/beforeunload.html")) defaultGotoOptions
          click (locate page "body")
    , testCase "should close browsercontext with beforeunload page" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          _ <- goto page (urlFromText (prefix <> "/beforeunload.html")) defaultGotoOptions
          click (locate page "body")
          closeContext ctx
    , testCase "should be able to navigate away from page with beforeunload" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/beforeunload.html")) defaultGotoOptions
          click (locate page "body")
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          pure ()
    , testCase "should close page with beforeunload listener" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx _ -> do
          page <- newPage ctx
          _ <- goto page (urlFromText (prefix <> "/beforeunload.html")) defaultGotoOptions
          click (locate page "body")
          closePage page
    , testCase "should run beforeunload if asked for" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx _ -> do
          page <- newPage ctx
          _ <- goto page (urlFromText (prefix <> "/beforeunload.html")) defaultGotoOptions
          click (locate page "body")
          takeDialog <- armDialog page
          withAsync (closePageWith page True) $ \closing -> do
            dialog <- takeDialog
            assertEqual "type" DialogBeforeUnload (dialogType dialog)
            assertEqual "default" "" (dialogDefaultValue dialog)
            assertEqual "message" "" (dialogMessage dialog)
            withAsync (waitForClose page) $ \closed -> do
              acceptDialog dialog
              wait closed
            wait closing
    , testCase "should access page after beforeunload" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx _ -> do
          page <- newPage ctx
          _ <- goto page (urlFromText (prefix <> "/beforeunload.html")) defaultGotoOptions
          click (locate page "body")
          takeDialog <- armDialog page
          withAsync (closePageWith page True) $ \closing -> do
            dialog <- takeDialog
            dismissDialog dialog
            wait closing
          _ <- evaluateText page "() => document.title"
          pure ()
    , testCase "should not stall on evaluate when dismissing beforeunload" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx _ -> do
          page <- newPage ctx
          _ <- goto page (urlFromText (prefix <> "/beforeunload.html")) defaultGotoOptions
          click (locate page "body")
          takeDialog <- armDialog page
          withAsync
            ( do
                dialog <- takeDialog
                dismissDialog dialog
            )
            $ \_ -> do
              _ <- evaluate page "() => { window.location.reload(); }"
              pure ()
    , testCase "should not stall on click when dismissing beforeunload" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx _ -> do
          page <- newPage ctx
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent page ("<a href=\"" <> prefix <> "/frames/one-frame.html\">click me</a>")
          _ <- evaluate page "() => { window.onbeforeunload = () => false; }"
          takeDialog <- armDialog page
          withAsync
            ( do
                dialog <- takeDialog
                dismissDialog dialog
            )
            $ \_ ->
              clickWith
                (getByRole page (AriaRole "link") defaultGetByRoleOptions)
                defaultClickOptions {clickOptionsNoWaitAfter = Just True}
          _ <- evaluate page "() => { window.onbeforeunload = null; }"
          case milliseconds 5000 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              clickWith
                (getByRole page (AriaRole "link") defaultGetByRoleOptions)
                defaultClickOptions {clickOptionsTimeout = Just timeout}
          waitForURL page (UrlMatchGlob (prefix <> "/frames/one-frame.html"))
    , testCase "should support dismissing the dialog multiple times" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx _ -> do
          page <- newPage ctx
          _ <- goto page (urlFromText (prefix <> "/beforeunload.html")) defaultGotoOptions
          click (locate page "body")
          takeDialog <- armDialog page
          withAsync (closePageWith page True) $ \closing -> do
            dialog <- takeDialog
            dismissDialog dialog
            wait closing
          takeAgain <- armDialog page
          withAsync (closePageWith page True) $ \closing -> do
            dialog2 <- takeAgain
            dismissDialog dialog2
            wait closing
    , testCase "should support closing the page after a previous dismiss" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx _ -> do
          page <- newPage ctx
          _ <- goto page (urlFromText (prefix <> "/beforeunload.html")) defaultGotoOptions
          click (locate page "body")
          takeDialog <- armDialog page
          withAsync (closePageWith page True) $ \closing -> do
            dialog <- takeDialog
            dismissDialog dialog
            wait closing
          closePage page
          assertEqual "closed" True =<< isClosed page
    , testCase "should support closing the page via a subsequent onbeforeunload dialog" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx _ -> do
          page <- newPage ctx
          _ <- goto page (urlFromText (prefix <> "/beforeunload.html")) defaultGotoOptions
          click (locate page "body")
          takeDialog <- armDialog page
          withAsync (closePageWith page True) $ \closing -> do
            dialog <- takeDialog
            withAsync (waitForClose page) $ \closed -> do
              acceptDialog dialog
              wait closed
            wait closing
          assertEqual "closed" True =<< isClosed page
    , testCase "does not get stalled by beforeUnload" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx _ -> do
          page <- newPage ctx
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <-
            evaluate
              page
              "() => { window.addEventListener('beforeunload', event => { event.preventDefault(); }); }"
          click (locate page "body")
          route page (UrlMatchGlob "**/api") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus 200)
                , fulfillBody = Just (FulfillText "ok")
                , fulfillContentType = Just (ContentType "text/plain")
                }
          _ <- evaluate page "async () => fetch(new URL('/api', window.location.href))"
          takeDialog <- armDialog page
          withAsync (closePageWith page True) $ \closing -> do
            dialog <- takeDialog
            dismissDialog dialog
            wait closing
          _ <- evaluate page "async () => fetch(new URL('/api', window.location.href))"
          pure ()
    ]

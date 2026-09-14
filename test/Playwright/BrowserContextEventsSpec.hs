module Playwright.BrowserContextEventsSpec
  ( tests
  ) where

import Network.HTTP.Types
  ( hContentType
  , status200
  , status404
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
import Playwright
  ( ConsoleMessage (consoleMessageArgs, consoleMessagePage, consoleMessageText)
  , Dialog (dialogMessage, dialogPage)
  , acceptDialogWith
  , asElement
  , click
  , clickHandle
  , defaultGotoOptions
  , evaluate
  , evaluateOnHandleText
  , evaluateText
  , goto
  , pagesEqual
  , setContent
  , urlFromText
  , waitForContextConsole
  , waitForContextDialog
  , waitForContextPage
  , waitForDialog
  , waitForFunction
  , waitForPopup
  , dialogsEqual
  )
import Playwright.Harness
  ( locate
  , requireHandle
  , withChromiumContext
  , withLocalApp
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
    "browsercontext-events"
    [ testCase "console event should work" $
        withChromiumContext $ \ctx page ->
          withAsync (waitForContextConsole ctx) $ \pending -> do
            _ <- evaluate page "() => console.log('hello')"
            message <- wait pending
            assertEqual "text" "hello" (consoleMessageText message)
            case consoleMessagePage message of
              Nothing ->
                assertFailure "expected page"
              Just origin ->
                assertEqual "page" True (pagesEqual origin page)
    , testCase "console event should work with element handles" $
        withChromiumContext $ \ctx page -> do
          setContent page "<body>hello</body>"
          withAsync (waitForContextConsole ctx) $ \pending -> do
            _ <- evaluate page "() => console.log(document.body)"
            message <- wait pending
            case consoleMessageArgs message of
              body : _ -> do
                assertEqual "BODY" "BODY" =<< evaluateOnHandleText body "x => x.nodeName"
                element <- requireHandle =<< asElement body
                clickHandle element
              _ ->
                assertFailure "expected console arg"
    , testCase "console event should work in popup" $
        withChromiumContext $ \ctx page ->
          withAsync (waitForContextConsole ctx) $ \consolePending ->
            withAsync (waitForPopup page) $ \popupPending -> do
              _ <- evaluate page "() => { const win = window.open(''); win.console.log('hello'); }"
              message <- wait consolePending
              popup <- wait popupPending
              assertEqual "text" "hello" (consoleMessageText message)
              case consoleMessagePage message of
                Nothing ->
                  assertFailure "expected popup page"
                Just origin ->
                  assertEqual "popup" True (pagesEqual origin popup)
    , testCase "dialog event should work" $
        withChromiumContext $ \ctx page ->
          withAsync (waitForContextDialog ctx) $ \contextPending ->
            withAsync (waitForDialog page) $ \pagePending ->
              withAsync (evaluateText page "() => prompt('hey?')") $ \evalPending -> do
                dialog1 <- wait contextPending
                dialog2 <- wait pagePending
                assertEqual "same" True (dialogsEqual dialog1 dialog2)
                assertEqual "message" "hey?" (dialogMessage dialog1)
                case dialogPage dialog1 of
                  Nothing ->
                    assertFailure "expected page"
                  Just origin ->
                    assertEqual "page" True (pagesEqual origin page)
                acceptDialogWith dialog1 (Just "hello")
                result <- wait evalPending
                assertEqual "hello" "hello" result
    , testCase "dialog event should work in popup" $
        withChromiumContext $ \ctx page ->
          withAsync (waitForContextDialog ctx) $ \dialogPending ->
            withAsync (waitForPopup page) $ \popupPending ->
              withAsync
                (evaluateText page "() => { const win = window.open(''); return win.prompt('hey?'); }")
                $ \evalPending -> do
                  dialog <- wait dialogPending
                  popup <- wait popupPending
                  assertEqual "message" "hey?" (dialogMessage dialog)
                  case dialogPage dialog of
                    Nothing ->
                      assertFailure "expected popup"
                    Just origin ->
                      assertEqual "popup" True (pagesEqual origin popup)
                  acceptDialogWith dialog (Just "hello")
                  result <- wait evalPending
                  assertEqual "hello" "hello" result
    , testCase "dialog event should work with inline script tag" $
        withChromiumContext $ \ctx page ->
          withLocalApp popupPromptApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            setContent page ("<a href='" <> prefix <> "/popup.html' target=_blank>Click me</a>")
            withAsync (waitForContextDialog ctx) $ \dialogPending ->
              withAsync (waitForContextPage ctx) $ \pagePending -> do
                click (locate page "a")
                dialog <- wait dialogPending
                popup <- wait pagePending
                assertEqual "message" "hey?" (dialogMessage dialog)
                case dialogPage dialog of
                  Nothing ->
                    assertFailure "expected popup"
                  Just origin ->
                    assertEqual "popup" True (pagesEqual origin popup)
                acceptDialogWith dialog (Just "hello")
                _ <- waitForFunction popup "() => window.result === 'hello'"
                pure ()
    ]

popupPromptApp :: Application
popupPromptApp request respond =
  case pathInfo request of
    ["empty.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] "")
    ["popup.html"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/html")]
            "<script>window.result = prompt('hey?')</script>"
        )
    _ ->
      respond (responseLBS status404 [] "")

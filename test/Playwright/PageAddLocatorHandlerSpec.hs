module Playwright.PageAddLocatorHandlerSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Exception
  ( try
  )
import Control.Monad
  ( forever
  , when
  )
import Data.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.Maybe
  ( isNothing
  )
import Data.Text
  ( Text
  , isInfixOf
  , pack
  )
import Playwright
  ( AddLocatorHandlerOptions (addLocatorHandlerNoWaitAfter, addLocatorHandlerTimes)
  , AriaRole (AriaRole)
  , ClickOptions (clickOptionsForce, clickOptionsTimeout)
  , Exactness (Inexact)
  , GetByRoleOptions (getByRoleName)
  , Locator
  , LocatorHandlerTimes (LocatorHandlerTimes)
  , Page
  , SpecialJsValue (SpecialUndefined)
  , Timeout
  , WaitForState (WaitForHidden, WaitForVisible)
  , addInitScript
  , addLocatorHandler
  , addLocatorHandlerWith
  , childLocator
  , click
  , clickWith
  , defaultAddLocatorHandlerOptions
  , defaultClickOptions
  , defaultGetByRoleOptions
  , defaultGotoOptions
  , evalOnSelectorText
  , evaluate
  , evaluateBool
  , evaluateNumber
  , evaluateSpecial
  , frameLocator
  , getByRole
  , getByText
  , goto
  , hover
  , isVisible
  , milliseconds
  , querySelector
  , removeLocatorHandler
  , selectorFromText
  , urlFromText
  , waitFor
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
  , withAssetPage
  , withAssets
  )
import Test.Tasty
  ( TestTree
  , localOption
  , mkTimeout
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , assertFailure
  , testCase
  )

tests :: TestTree
tests =
  localOption (mkTimeout 30000000) $
    testGroup
      "page-add-locator-handler"
      [ testCase "should work" $
          withHandleLocator $ \page -> do
            beforeCount <- newIORef (0 :: Int)
            afterCount <- newIORef (0 :: Int)
            let
              original = getByText page "This interstitial covers the button" Inexact
            addLocatorHandler page original $ \_ -> do
              modifyIORef' beforeCount (+ 1)
              click (locate page "#close")
              modifyIORef' afterCount (+ 1)
            mapM_ (runCoveredClick page beforeCount afterCount) coveredCases
      , testCase "should work with a custom check" $
          withHandleLocator $ \page -> do
            addLocatorHandlerWith
              page
              (locate page "body")
              ( \_ -> do
                  visible <- isVisible (getByText page "This interstitial covers the button" Inexact)
                  when visible (click (locate page "#close"))
              )
              defaultAddLocatorHandlerOptions {addLocatorHandlerNoWaitAfter = Just True}
            mapM_ (runCustomCheck page) customCheckCases
      , testCase "should work with locator.hover()" $
          withHandleLocator $ \page -> do
            addLocatorHandler page (getByText page "This interstitial covers the button" Inexact) $ \_ ->
              click (locate page "#close")
            hover (locate page "#aside")
            _ <-
              evaluate
                page
                "() => window.setupAnnoyingInterstitial('pointerover', 1, 'capture')"
            hover (locate page "#target")
            visible <- isVisible (locate page "#interstitial")
            assertEqual "hidden" False visible
            color <-
              evalOnSelectorText
                page
                (selectorFromText "#target")
                "e => window.getComputedStyle(e).backgroundColor"
            assertEqual "hover" "rgb(255, 255, 0)" color
      , testCase "should not work with force:true" $
          withHandleLocator $ \page -> do
            addLocatorHandler page (getByText page "This interstitial covers the button" Inexact) $ \_ ->
              click (locate page "#close")
            hover (locate page "#aside")
            _ <- evaluate page "() => window.setupAnnoyingInterstitial('none', 1)"
            timeout <- requireTimeout 2000
            clickWith
              (locate page "#target")
              defaultClickOptions
                { clickOptionsForce = Just True
                , clickOptionsTimeout = Just timeout
                }
            visible <- isVisible (locate page "#interstitial")
            assertEqual "still visible" True visible
            special <- evaluateSpecial page "() => window.clicked"
            assertEqual "not clicked" SpecialUndefined special
      , testCase "should throw when handler times out" $
          withHandleLocator $ \page -> do
            called <- newIORef (0 :: Int)
            addLocatorHandler page (getByText page "This interstitial covers the button" Inexact) $ \_ -> do
              modifyIORef' called (+ 1)
              forever (threadDelay maxBound)
            hover (locate page "#aside")
            _ <-
              evaluate
                page
                "() => { window.clicked = 0; window.setupAnnoyingInterstitial('mouseover', 1); }"
            timeout <- requireTimeout 3000
            assertProtocolContains "Timeout 3000ms exceeded" $
              clickWith
                (locate page "#target")
                defaultClickOptions {clickOptionsTimeout = Just timeout}
            assertProtocolContains "Timeout 3000ms exceeded" $
              clickWith
                (locate page "#target")
                defaultClickOptions {clickOptionsTimeout = Just timeout}
            count <- readIORef called
            assertEqual "once" 1 count
      , testCase "should work with locator.waitFor" $
          withHandleLocator $ \page -> do
            called <- newIORef (0 :: Int)
            addLocatorHandler page (getByText page "This interstitial covers the button" Inexact) $ \_ -> do
              modifyIORef' called (+ 1)
              click (locate page "#close")
            _ <-
              evaluate
                page
                "() => { window.clicked = 0; window.setupAnnoyingInterstitial('remove', 1); }"
            waitFor (locate page "#target") WaitForVisible
            visible <- isVisible (locate page "#interstitial")
            assertEqual "hidden" False visible
            count <- readIORef called
            assertEqual "called" 1 count
      , testCase "should work when owner frame detaches" $
          withAssetPage "empty.html" $ \page -> do
            _ <-
              evaluate
                page
                "() => {\n\
                \  const iframe = document.createElement('iframe');\n\
                \  iframe.src = 'data:text/html,<body>hello from iframe</body>';\n\
                \  document.body.append(iframe);\n\
                \  const target = document.createElement('button');\n\
                \  target.textContent = 'Click me';\n\
                \  target.id = 'target';\n\
                \  target.addEventListener('click', () => window._clicked = true);\n\
                \  document.body.appendChild(target);\n\
                \  const closeButton = document.createElement('button');\n\
                \  closeButton.textContent = 'close';\n\
                \  closeButton.id = 'close';\n\
                \  closeButton.addEventListener('click', () => iframe.remove());\n\
                \  document.body.appendChild(closeButton);\n\
                \}"
            addLocatorHandler
              page
              ( childLocator
                  (frameLocator page (selectorFromText "iframe"))
                  (selectorFromText "body")
              )
              $ \_ ->
                click (locate page "#close")
            click (locate page "#target")
            iframe <- querySelector page (selectorFromText "iframe")
            assertEqual "gone" True (isNothing iframe)
            clicked <- evaluateBool page "() => window._clicked"
            assertEqual "clicked" True clicked
      , testCase "should work with times: option" $
          withHandleLocator $ \page -> do
            called <- newIORef (0 :: Int)
            addLocatorHandlerWith
              page
              (locate page "body")
              (\_ -> modifyIORef' called (+ 1))
              defaultAddLocatorHandlerOptions
                { addLocatorHandlerNoWaitAfter = Just True
                , addLocatorHandlerTimes = Just (LocatorHandlerTimes 2)
                }
            hover (locate page "#aside")
            _ <-
              evaluate
                page
                "() => { window.clicked = 0; window.setupAnnoyingInterstitial('mouseover', 4); }"
            timeout <- requireTimeout 3000
            assertProtocolContainsAll
              [ "Timeout 3000ms exceeded"
              , "intercepts pointer events"
              ]
              ( clickWith
                  (locate page "#target")
                  defaultClickOptions {clickOptionsTimeout = Just timeout}
              )
            count <- readIORef called
            assertEqual "times" 2 count
            clicked <- evaluateNumber page "() => window.clicked"
            assertEqual "not clicked" 0 clicked
            visible <- isVisible (locate page "#interstitial")
            assertEqual "visible" True visible
      , testCase "should wait for hidden by default" $
          withHandleLocator $ \page -> do
            called <- newIORef (0 :: Int)
            addLocatorHandler page (closeButton page) $ \button -> do
              modifyIORef' called (+ 1)
              click button
            hover (locate page "#aside")
            _ <-
              evaluate
                page
                "() => { window.clicked = 0; window.setupAnnoyingInterstitial('timeout', 1); }"
            click (locate page "#target")
            clicked <- evaluateNumber page "() => window.clicked"
            assertEqual "clicked" 1 clicked
            visible <- isVisible (locate page "#interstitial")
            assertEqual "hidden" False visible
            count <- readIORef called
            assertEqual "called" 1 count
      , testCase "should wait for hidden by default 2" $
          withHandleLocator $ \page -> do
            called <- newIORef (0 :: Int)
            addLocatorHandler page (closeButton page) $ \_ ->
              modifyIORef' called (+ 1)
            hover (locate page "#aside")
            _ <-
              evaluate
                page
                "() => { window.clicked = 0; window.setupAnnoyingInterstitial('hide', 1); }"
            timeout <- requireTimeout 3000
            assertProtocolContains "locator handler has finished, waiting for" $
              clickWith
                (locate page "#target")
                defaultClickOptions {clickOptionsTimeout = Just timeout}
            clicked <- evaluateNumber page "() => window.clicked"
            assertEqual "not clicked" 0 clicked
            visible <- isVisible (locate page "#interstitial")
            assertEqual "visible" True visible
            count <- readIORef called
            assertEqual "called" 1 count
      , testCase "should work with noWaitAfter" $
          withHandleLocator $ \page -> do
            called <- newIORef (0 :: Int)
            addLocatorHandlerWith
              page
              (closeButton page)
              ( \button -> do
                  modifyIORef' called (+ 1)
                  count <- readIORef called
                  if count == 1
                    then click button
                    else waitFor (locate page "#interstitial") WaitForHidden
              )
              defaultAddLocatorHandlerOptions {addLocatorHandlerNoWaitAfter = Just True}
            hover (locate page "#aside")
            _ <-
              evaluate
                page
                "() => { window.clicked = 0; window.setupAnnoyingInterstitial('timeout', 1); }"
            click (locate page "#target")
            clicked <- evaluateNumber page "() => window.clicked"
            assertEqual "clicked" 1 clicked
            visible <- isVisible (locate page "#interstitial")
            assertEqual "hidden" False visible
            count <- readIORef called
            assertEqual "called" 2 count
      , testCase "should removeLocatorHandler" $
          withHandleLocator $ \page -> do
            called <- newIORef (0 :: Int)
            addLocatorHandler page (closeButton page) $ \button -> do
              modifyIORef' called (+ 1)
              click button
            _ <-
              evaluate
                page
                "() => { window.clicked = 0; window.setupAnnoyingInterstitial('hide', 1); }"
            click (locate page "#target")
            count1 <- readIORef called
            assertEqual "first" 1 count1
            clicked1 <- evaluateNumber page "() => window.clicked"
            assertEqual "clicked" 1 clicked1
            visible1 <- isVisible (locate page "#interstitial")
            assertEqual "hidden" False visible1
            _ <-
              evaluate
                page
                "() => { window.clicked = 0; window.setupAnnoyingInterstitial('hide', 1); }"
            removeLocatorHandler page (closeButton page)
            timeout <- requireTimeout 3000
            assertProtocolContains "Timeout 3000ms exceeded" $
              clickWith
                (locate page "#target")
                defaultClickOptions {clickOptionsTimeout = Just timeout}
            count2 <- readIORef called
            assertEqual "still one" 1 count2
            clicked2 <- evaluateNumber page "() => window.clicked"
            assertEqual "blocked" 0 clicked2
            visible2 <- isVisible (locate page "#interstitial")
            assertEqual "visible" True visible2
      ]

coveredCases :: [(Text, Int, Maybe Text)]
coveredCases =
  [ ("mouseover", 1, Nothing)
  , ("mouseover", 1, Just "capture")
  , ("mouseover", 2, Nothing)
  , ("mouseover", 2, Just "capture")
  , ("pointerover", 1, Nothing)
  , ("pointerover", 1, Just "capture")
  , ("none", 1, Nothing)
  , ("remove", 1, Nothing)
  , ("hide", 1, Nothing)
  ]

customCheckCases :: [(Text, Int)]
customCheckCases =
  [ ("mouseover", 2)
  , ("none", 1)
  , ("remove", 1)
  , ("hide", 1)
  ]

runCoveredClick :: Page -> IORef Int -> IORef Int -> (Text, Int, Maybe Text) -> IO ()
runCoveredClick page beforeCount afterCount (event, times, capture) = do
  hover (locate page "#aside")
  writeIORef beforeCount 0
  writeIORef afterCount 0
  setupInterstitial page event times capture
  before0 <- readIORef beforeCount
  after0 <- readIORef afterCount
  assertEqual "before setup" 0 before0
  assertEqual "after setup" 0 after0
  click (locate page "#target")
  before1 <- readIORef beforeCount
  after1 <- readIORef afterCount
  assertEqual "before" times before1
  assertEqual "after" times after1
  clicked <- evaluateNumber page "() => window.clicked"
  assertEqual "clicked" 1 clicked
  visible <- isVisible (locate page "#interstitial")
  assertEqual "hidden" False visible

runCustomCheck :: Page -> (Text, Int) -> IO ()
runCustomCheck page (event, times) = do
  hover (locate page "#aside")
  setupInterstitial page event times Nothing
  click (locate page "#target")
  clicked <- evaluateNumber page "() => window.clicked"
  assertEqual "clicked" 1 clicked
  visible <- isVisible (locate page "#interstitial")
  assertEqual "hidden" False visible

setupInterstitial :: Page -> Text -> Int -> Maybe Text -> IO ()
setupInterstitial page event times capture = do
  let
    captureArg =
      case capture of
        Nothing ->
          ""
        Just value ->
          ", '" <> value <> "'"
  _ <-
    evaluate
      page
      ( "() => { window.clicked = 0; window.setupAnnoyingInterstitial('"
          <> event
          <> "', "
          <> pack (show times)
          <> captureArg
          <> "); }"
      )
  pure ()

closeButton :: Page -> Locator
closeButton page =
  getByRole
    page
    (AriaRole "button")
    defaultGetByRoleOptions {getByRoleName = Just "close"}

withHandleLocator :: (Page -> IO a) -> IO a
withHandleLocator action =
  withAssets $ \prefix page -> do
    addInitScript
      page
      "window.builtins = { setTimeout: window.setTimeout.bind(window), clearTimeout: window.clearTimeout.bind(window), requestAnimationFrame: window.requestAnimationFrame.bind(window) };"
    _ <-
      goto
        page
        (urlFromText (prefix <> "/input/handle-locator.html"))
        defaultGotoOptions
    action page

requireTimeout :: Double -> IO Timeout
requireTimeout n =
  case milliseconds n of
    Nothing ->
      assertFailure ("milliseconds " <> show n)
    Just timeout ->
      pure timeout

assertProtocolContainsAll :: [Text] -> IO a -> IO ()
assertProtocolContainsAll needles action = do
  result <- tryProtocol action
  case result of
    Left (ProtocolRpc failure) ->
      mapM_ (requireNeedle (protocolFailureMessage failure)) needles
    Left err ->
      assertFailure ("expected ProtocolRpc, got: " <> show err)
    Right _ ->
      assertFailure "expected protocol error"

tryProtocol :: IO a -> IO (Either PlaywrightError a)
tryProtocol action =
  try action

requireNeedle :: Text -> Text -> IO ()
requireNeedle haystack needle =
  if needle `isInfixOf` haystack
    then pure ()
    else
      assertFailure
        ( "expected protocol error to contain "
            <> show needle
            <> ", got: "
            <> show haystack
        )

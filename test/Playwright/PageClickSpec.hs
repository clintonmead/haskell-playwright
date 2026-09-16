module Playwright.PageClickSpec
  ( tests
  ) where

import UnliftIO.Async
  ( Async
  , mapConcurrently_
  , poll
  , wait
  , withAsync
  )
import UnliftIO.IORef
  ( modifyIORef'
  , newIORef
  , readIORef
  )
import Control.Monad
  ( forM_
  )
import Data.Text
  ( Text
  , pack
  , replace
  )
import Playwright
  ( ClickOptions (clickOptionsButton, clickOptionsClickCount, clickOptionsForce, clickOptionsModifiers, clickOptionsPosition, clickOptionsTimeout)
  , ConsoleMessage (consoleMessageText)
  , Frame
  , Key (Key)
  , KeyboardModifier (KeyboardModifierShift)
  , LoadState (LoadStateLoad)
  , MouseButton (MouseButtonRight)
  , Page
  , Playwright
  , Point (Point)
  , SpecialJsValue (SpecialNull)
  , StyleTag (styleTagContent)
  , Timeout
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , addStyleTag
  , asElement
  , click
  , clickWith
  , contentFrameHandle
  , cssPixelFromInt
  , dblclick
  , defaultClickOptions
  , defaultGotoOptions
  , emptyStyleTag
  , evalOnSelector
  , evaluate
  , evaluateBool
  , evaluateHandle
  , evaluateNumber
  , evaluateOnElement
  , evaluateOnFrameText
  , evaluateSpecial
  , evaluateText
  , evaluateTexts
  , fill
  , goto
  , innerText
  , keyboardDown
  , keyboardUp
  , locatorOnFrame
  , milliseconds
  , onConsole
  , querySelector
  , selectorFromText
  , setContent
  , setViewportSize
  , urlFromText
  , waitForLoadState
  , waitForPopup
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
  , requireHandle
  , withAssetPage
  , withAssets
  , withChromiumPage
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

tests :: TestTree
tests =
  testGroup
    "page-click"
    [ testCase "should click the button" $
        withAssetPage "input/button.html" $ \page -> do
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should click svg" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<svg height=\"100\" width=\"100\"><circle onclick=\"window.__CLICKED=42\" cx=\"50\" cy=\"50\" r=\"40\" stroke=\"black\" stroke-width=\"3\" fill=\"red\" /></svg>"
          click (locate page "circle")
          assertEqual "clicked" (42 :: Double) =<< evaluateNumber page "() => window.__CLICKED"
    , testCase "should click the button if window.Node is removed" $
        withAssetPage "input/button.html" $ \page -> do
          _ <- evaluate page "() => { delete window.Node; }"
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should click on a span with an inline element inside" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<style>span::before { content: 'q'; }</style><span onclick='window.CLICKED=42'></span>"
          click (locate page "span")
          assertEqual "clicked" (42 :: Double) =<< evaluateNumber page "() => window.CLICKED"
    , testCase "should click the aligned 1x1 div" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"width: 1px; height: 1px;\" onclick=\"window.__clicked = true\"></div>"
          click (locate page "div")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should click the half-aligned 1x1 div" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"margin-left: 20.5px; margin-top: 11.5px; width: 1px; height: 1px;\" onclick=\"window.__clicked = true\"></div>"
          click (locate page "div")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should click the unaligned 1x1 div v1" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"margin-left: 20.23px; margin-top: 11.65px; width: 1px; height: 1px;\" onclick=\"window.__clicked = true\"></div>"
          click (locate page "div")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should click the unaligned 1x1 div v2" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"margin-left: 20.68px; margin-top: 11.13px; width: 1px; height: 1px;\" onclick=\"window.__clicked = true\"></div>"
          click (locate page "div")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should click the unaligned 1x1 div v3" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"margin-left: 20.68px; margin-top: 11.52px; width: 1px; height: 1px;\" onclick=\"window.__clicked = true\"></div>"
          click (locate page "div")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should click the unaligned 1x1 div v4" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"margin-left: 20.15px; margin-top: 11.24px; width: 1px; height: 1px;\" onclick=\"window.__clicked = true\"></div>"
          click (locate page "div")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should click the button after navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/input/button.html")) defaultGotoOptions
          click (locate page "button")
          _ <- goto page (urlFromText (prefix <> "/input/button.html")) defaultGotoOptions
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should click when one of inline box children is outside of viewport" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<style>i { position: absolute; top: -1000px; }</style><span onclick='window.CLICKED = 42;'><i>woof</i><b>doggo</b></span>"
          click (locate page "span")
          assertEqual "clicked" (42 :: Double) =<< evaluateNumber page "() => window.CLICKED"
    , testCase "should update modifiers correctly" $
        withAssetPage "input/button.html" $ \page -> do
          clickWith (locate page "button") defaultClickOptions {clickOptionsModifiers = Just [KeyboardModifierShift]}
          assertEqual "shift" True =<< evaluateBool page "() => shiftKey"
          clickWith (locate page "button") defaultClickOptions {clickOptionsModifiers = Just []}
          assertEqual "cleared" False =<< evaluateBool page "() => shiftKey"
          keyboardDown page (Key "Shift")
          clickWith (locate page "button") defaultClickOptions {clickOptionsModifiers = Just []}
          assertEqual "override" False =<< evaluateBool page "() => shiftKey"
          click (locate page "button")
          assertEqual "held" True =<< evaluateBool page "() => shiftKey"
          keyboardUp page (Key "Shift")
          click (locate page "button")
          assertEqual "released" False =<< evaluateBool page "() => shiftKey"
    , testCase "should click button inside frameset" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/frameset.html")) defaultGotoOptions
          handle <- requireHandle =<< querySelector page (selectorFromText "frame")
          _ <- evaluateOnElement handle "frame => { frame.src = '/input/button.html'; }"
          frame <- requireFrame =<< contentFrameHandle handle
          click (locatorOnFrame page frame (selectorFromText "button"))
          assertEqual "clicked" "Clicked" =<< evaluateOnFrameText frame "() => window.result"
    , testCase "should issue clicks in parallel in page and popup" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/counter.html")) defaultGotoOptions
          popup <-
            withAsync (waitForPopup page) $ \pending -> do
              _ <- evaluate page "() => window.open('/counter.html')"
              wait pending
          waitForLoadState popup LoadStateLoad
          mapConcurrently_
            ( \i ->
                if i `mod` 3 == (0 :: Int)
                  then click (locate popup "button")
                  else click (locate page "button")
            )
            ([0 .. 20] :: [Int])
          assertEqual "page" (14 :: Double) =<< evaluateNumber page "() => window.count"
          assertEqual "popup" (7 :: Double) =<< evaluateNumber popup "() => window.count"
    , testCase "should click the button after a cross origin navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/input/button.html")) defaultGotoOptions
          click (locate page "button")
          _ <- goto page (urlFromText (replace "127.0.0.1" "localhost" prefix <> "/input/button.html")) defaultGotoOptions
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should select the text by triple clicking" $
        withAssetPage "input/textarea.html" $ \page -> do
          let
            text :: Text
            text = "This is the text that we are going to try to select. Let's see how it goes."
          fill (locate page "textarea") text
          timeout <- requireTimeout 30000
          clickWith
            (locate page "textarea")
            defaultClickOptions {clickOptionsClickCount = Just 3, clickOptionsTimeout = Just timeout}
          selected <-
            evaluateText
              page
              "() => { const textarea = document.querySelector('textarea'); return textarea.value.substring(textarea.selectionStart, textarea.selectionEnd); }"
          assertEqual "selected" text selected
    , testCase "should click offscreen buttons" $
        withAssetPage "offscreenbuttons.html" $ \page -> do
          messages <- newIORef ([] :: [Text])
          onConsole page $ \message ->
            modifyIORef' messages (<> [consoleMessageText message])
          forM_ ([0 .. 10] :: [Int]) $ \i -> do
            _ <- evaluate page "() => window.scrollTo(0, 0)"
            click (locate page ("#btn" <> pack (show i)))
          assertEqual
            "messages"
            [ "button #0 clicked"
            , "button #1 clicked"
            , "button #2 clicked"
            , "button #3 clicked"
            , "button #4 clicked"
            , "button #5 clicked"
            , "button #6 clicked"
            , "button #7 clicked"
            , "button #8 clicked"
            , "button #9 clicked"
            , "button #10 clicked"
            ]
            =<< readIORef messages
    , testCase "should waitFor visible when already visible" $
        withAssetPage "input/button.html" $ \page -> do
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should not wait with force" $
        withAssetPage "input/button.html" $ \page -> do
          _ <- evalOnSelector page (selectorFromText "button") "b => { b.style.display = 'none'; }"
          assertProtocolContains
            "Element is not visible"
            (clickWith (locate page "button") defaultClickOptions {clickOptionsForce = Just True})
          assertEqual "not clicked" "Was not clicked" =<< evaluateText page "() => window.result"
    , testCase "should waitFor display:none to be gone" $
        waitForStyleGone "display" "none" "block"
    , testCase "should waitFor visibility:hidden to be gone" $
        waitForStyleGone "visibility" "hidden" "visible"
    , testCase "should waitFor visible when parent is hidden" $
        withAssetPage "input/button.html" $ \page -> do
          _ <- evalOnSelector page (selectorFromText "button") "b => { b.parentElement.style.display = 'none'; }"
          withAsync (click (locate page "button")) $ \pending -> do
            rafraf page 5
            stillPending pending
            _ <- evalOnSelector page (selectorFromText "button") "b => { b.parentElement.style.display = 'block'; }"
            wait pending
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should click wrapped links" $
        withAssetPage "wrappedlink.html" $ \page -> do
          click (locate page "a")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should click on checkbox input and toggle" $
        withAssetPage "input/checkbox.html" $ \page -> do
          assertEqual "null" SpecialNull =<< evaluateSpecial page "() => window.result.check"
          click (locate page "input#agree")
          assertEqual "checked" True =<< evaluateBool page "() => window.result.check"
          assertEqual
            "events"
            ["mouseover", "mouseenter", "mousemove", "mousedown", "mouseup", "click", "input", "change"]
            =<< evaluateTexts page "() => window.result.events"
          click (locate page "input#agree")
          assertEqual "unchecked" False =<< evaluateBool page "() => window.result.check"
    , testCase "should click on checkbox label and toggle" $
        withAssetPage "input/checkbox.html" $ \page -> do
          assertEqual "null" SpecialNull =<< evaluateSpecial page "() => window.result.check"
          click (locate page "label[for=\"agree\"]")
          assertEqual "checked" True =<< evaluateBool page "() => window.result.check"
          assertEqual "events" ["click", "input", "change"] =<< evaluateTexts page "() => window.result.events"
          click (locate page "label[for=\"agree\"]")
          assertEqual "unchecked" False =<< evaluateBool page "() => window.result.check"
    , testCase "should scroll and click the button" $
        withAssetPage "input/scrollable.html" $ \page -> do
          click (locate page "#button-5")
          assertEqual "5" "clicked" =<< innerText (locate page "#button-5")
          click (locate page "#button-80")
          assertEqual "80" "clicked" =<< innerText (locate page "#button-80")
    , testCase "should scroll and click the button with smooth scroll behavior" $
        withAssetPage "input/scrollable.html" $ \page -> do
          _ <- addStyleTag page emptyStyleTag {styleTagContent = Just "html { scroll-behavior: smooth; }"}
          forM_ ([0 .. 4] :: [Int]) $ \_ -> do
            click (locate page "#button-80")
            assertEqual "80" "clicked" =<< innerText (locate page "#button-80")
            click (locate page "#button-20")
            assertEqual "20" "clicked" =<< innerText (locate page "#button-20")
    , testCase "should double click the button" $
        withAssetPage "input/button.html" $ \page -> do
          _ <-
            evaluate
              page
              "() => { window.double = false; const button = document.querySelector('button'); button.addEventListener('dblclick', () => { window.double = true; }); }"
          dblclick (locate page "button")
          assertEqual "double" True =<< evaluateBool page "() => window.double"
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should click a partially obscured button" $
        withAssetPage "input/button.html" $ \page -> do
          _ <-
            evaluate
              page
              "() => { const button = document.querySelector('button'); button.textContent = 'Some really long text that will go offscreen'; button.style.position = 'absolute'; button.style.left = '368px'; }"
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should click a rotated button" $
        withAssetPage "input/rotatedButton.html" $ \page -> do
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should fire contextmenu event on right click" $
        withAssetPage "input/scrollable.html" $ \page -> do
          clickWith
            (locate page "#button-8")
            defaultClickOptions {clickOptionsButton = Just MouseButtonRight}
          assertEqual "menu" "context menu" =<< innerText (locate page "#button-8")
    , testCase "should click links which cause navigation" $
        withAssets $ \prefix page -> do
          setContent page ("<a href=\"" <> prefix <> "/empty.html\">empty.html</a>")
          click (locate page "a")
    , testCase "should click the button inside an iframe" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent page "<div style=\"width:100px;height:100px\">spacer</div>"
          frame <- attachFrame page prefix "button-test" (prefix <> "/input/button.html")
          click (locatorOnFrame page frame (selectorFromText "button"))
          assertEqual "clicked" "Clicked" =<< evaluateOnFrameText frame "() => window.result"
    , testCase "should click the button behind sticky header" $
        withChromiumPage $ \page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 240))
          setContent
            page
            "<style>* { padding: 0; margin: 0; } li { height: 80px; border: 1px solid black; } ol { padding-top: 160px; } div.fixed { position: fixed; z-index: 1001; width: 100%; background: red; height: 160px; }</style><div class=fixed></div><ol><li>hi1</li><li>hi2</li><li>hi3</li><li>hi4</li><li>hi5</li><li>hi6</li><li>hi7</li><li>hi8</li><li id=target onclick=\"window.__clicked = true\">hi9</li><li>hi10</li><li>hi11</li><li>hi12</li><li>hi13</li><li id=li14>hi14</li></ol>"
          _ <- evalOnSelector page (selectorFromText "#li14") "e => e.scrollIntoView()"
          click (locate page "#target")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should click the button behind position:absolute header" $
        withChromiumPage $ \page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 240))
          setContent
            page
            "<style>* { padding: 0; margin: 0; } li { height: 80px; border: 1px solid black; } ol { height: 100vh; overflow: scroll; padding-top: 160px; } body { position: relative; } div.fixed { position: absolute; top: 0; z-index: 1001; width: 100%; background: red; height: 160px; }</style><ol><li>hi1</li><li>hi2</li><li>hi3</li><li>hi4</li><li>hi5</li><li>hi6</li><li>hi7</li><li>hi8</li><li id=target onclick=\"window.__clicked = true\">hi9</li><li>hi10</li><li>hi11</li><li>hi12</li><li>hi13</li><li id=li14>hi14</li></ol><div class=fixed>Overlay</div>"
          _ <-
            evalOnSelector
              page
              (selectorFromText "ol")
              "e => { const target = document.querySelector('#target'); e.scrollTo({ top: target.offsetTop, behavior: 'instant' }); }"
          click (locate page "#target")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should click the button with px border with offset" $
        clickWithOffset "8px" 20 10
    , testCase "should click the button with em border with offset" $
        withAssetPage "input/button.html" $ \page -> do
          _ <- evalOnSelector page (selectorFromText "button") "button => { button.style.borderWidth = '2em'; }"
          _ <- evalOnSelector page (selectorFromText "button") "button => { button.style.fontSize = '12px'; }"
          clickWith
            (locate page "button")
            defaultClickOptions {clickOptionsPosition = Just (Point (cssPixelFromInt 20) (cssPixelFromInt 10))}
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
          assertEqual "offsetX" (20 :: Double) =<< evaluateNumber page "() => window.offsetX"
          assertEqual "offsetY" (10 :: Double) =<< evaluateNumber page "() => window.offsetY"
    , testCase "should click a very large button with offset" $
        withAssetPage "input/button.html" $ \page -> do
          _ <- evalOnSelector page (selectorFromText "button") "button => { button.style.borderWidth = '8px'; }"
          _ <- evalOnSelector page (selectorFromText "button") "button => { button.style.height = button.style.width = '2000px'; }"
          clickWith
            (locate page "button")
            defaultClickOptions {clickOptionsPosition = Just (Point (cssPixelFromInt 1900) (cssPixelFromInt 1910))}
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
          assertEqual "offsetX" (1900 :: Double) =<< evaluateNumber page "() => window.offsetX"
          assertEqual "offsetY" (1910 :: Double) =<< evaluateNumber page "() => window.offsetY"
    , testCase "should click a button in scrolling container with offset" $
        withAssetPage "input/button.html" $ \page -> do
          _ <-
            evalOnSelector
              page
              (selectorFromText "button")
              "button => { const container = document.createElement('div'); container.style.overflow = 'auto'; container.style.width = '200px'; container.style.height = '200px'; button.parentElement.insertBefore(container, button); container.appendChild(button); button.style.height = '2000px'; button.style.width = '2000px'; button.style.borderWidth = '8px'; }"
          clickWith
            (locate page "button")
            defaultClickOptions {clickOptionsPosition = Just (Point (cssPixelFromInt 1900) (cssPixelFromInt 1910))}
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
          assertEqual "offsetX" (1900 :: Double) =<< evaluateNumber page "() => window.offsetX"
          assertEqual "offsetY" (1910 :: Double) =<< evaluateNumber page "() => window.offsetY"
    ]

waitForStyleGone :: Text -> Text -> Text -> IO ()
waitForStyleGone property hidden shown =
  withAssetPage "input/button.html" $ \page -> do
    _ <- evalOnSelector page (selectorFromText "button") ("b => { b.style." <> property <> " = '" <> hidden <> "'; }")
    timeout <- requireTimeout 0
    withAsync (clickWith (locate page "button") defaultClickOptions {clickOptionsTimeout = Just timeout}) $ \pending -> do
      rafraf page 5
      stillPending pending
      assertEqual "not clicked" "Was not clicked" =<< evaluateText page "() => window.result"
      _ <- evalOnSelector page (selectorFromText "button") ("b => { b.style." <> property <> " = '" <> shown <> "'; }")
      wait pending
    assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"

clickWithOffset :: Text -> Int -> Int -> IO ()
clickWithOffset border x y =
  withAssetPage "input/button.html" $ \page -> do
    _ <- evalOnSelector page (selectorFromText "button") ("button => { button.style.borderWidth = '" <> border <> "'; }")
    clickWith
      (locate page "button")
      defaultClickOptions {clickOptionsPosition = Just (Point (cssPixelFromInt x) (cssPixelFromInt y))}
    assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    assertEqual "offsetX" (fromIntegral x :: Double) =<< evaluateNumber page "() => window.offsetX"
    assertEqual "offsetY" (fromIntegral y :: Double) =<< evaluateNumber page "() => window.offsetY"

attachFrame :: Page -> Text -> Text -> Text -> Playwright Frame
attachFrame page _prefix frameId src = do
  handle <-
    evaluateHandle
      page
      ( "async () => { const frame = document.createElement('iframe'); frame.src = '"
          <> src
          <> "'; frame.id = '"
          <> frameId
          <> "'; document.body.appendChild(frame); await new Promise(x => frame.onload = x); return frame; }"
      )
  element <- requireHandle =<< asElement handle
  requireFrame =<< contentFrameHandle element

requireFrame :: Maybe Frame -> Playwright Frame
requireFrame found =
  case found of
    Just frame ->
      pure frame
    Nothing ->
      assertFailure "expected frame"

requireTimeout :: Double -> Playwright Timeout
requireTimeout n =
  case milliseconds n of
    Just timeout ->
      pure timeout
    Nothing ->
      assertFailure ("timeout " <> show n)

rafraf :: Page -> Int -> Playwright ()
rafraf page count =
  forM_ ([1 .. count] :: [Int]) $ \_ -> do
    _ <-
      evaluate
        page
        "async () => { await new Promise(f => window.requestAnimationFrame(() => window.requestAnimationFrame(f))); }"
    pure ()

stillPending :: Async a -> Playwright ()
stillPending pending = do
  status <- poll pending
  case status of
    Nothing ->
      pure ()
    Just _ ->
      assertFailure "expected click still waiting"


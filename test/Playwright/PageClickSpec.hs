module Playwright.PageClickSpec
  ( tests
  ) where

import UnliftIO.Async
  ( mapConcurrently_
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
  , replicateM
  )
import Data.Text
  ( Text
  , pack
  , replace
  )
import Playwright
  ( AriaRole (AriaRole)
  , ClickOptions (clickOptionsButton, clickOptionsClickCount, clickOptionsForce, clickOptionsModifiers, clickOptionsPosition, clickOptionsScroll, clickOptionsSteps, clickOptionsTimeout, clickOptionsTrial)
  , ConsoleMessage (consoleMessageText)
  , Frame
  , GetByRoleOptions (getByRoleName)
  , Key (Key)
  , KeyboardModifier (KeyboardModifierShift)
  , LoadState (LoadStateLoad)
  , MouseButton (MouseButtonRight)
  , Page
  , Playwright
  , Point (Point)
  , SpecialJsValue (SpecialNull, SpecialUndefined)
  , StyleTag (styleTagContent)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , addInitScript
  , addStyleTag
  , childFrameLocator
  , childLocator
  , click
  , clickHandleWith
  , clickWith
  , hoverWith
  , contentFrameHandle
  , cssPixelFromInt
  , dblclick
  , dblclickWith
  , defaultClickOptions
  , defaultGetByRoleOptions
  , defaultGotoOptions
  , emptyStyleTag
  , evalOnSelector
  , evaluate
  , evaluateBool
  , evaluateNumber
  , evaluateOnElement
  , evaluateOnFrameText
  , evaluateOnLocator
  , evaluateSpecial
  , evaluateText
  , evaluateTexts
  , fill
  , frameLocator
  , getByRole
  , goto
  , innerText
  , keyboardDown
  , keyboardUp
  , locatorOnFrame
  , mouseDown
  , mouseMove
  , milliseconds
  , mouseUp
  , onConsole
  , querySelector
  , selectorFromText
  , setContent
  , setViewportSize
  , urlFromText
  , waitForConsoleWhere
  , waitForLoadState
  , waitForPopup
  )
import Playwright.DblclickOptions
  ( DblclickOptions (dblclickOptionsTrial)
  , defaultDblclickOptions
  )
import Playwright.HoverOptions
  ( HoverOptions (hoverOptionsScroll, hoverOptionsTimeout)
  , defaultHoverOptions
  )
import Playwright.Error
  ( PlaywrightError
  )
import Playwright.ScrollOption
  ( ScrollOption (ScrollNone)
  )
import UnliftIO.Exception
  ( try
  )
import Playwright.Harness
  ( requireTimeout
  , assertStillPending
  , rafraf
  , attachFrame
  , assertProtocolContains
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
            assertStillPending pending
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
          frame <- attachFrame page "button-test" (prefix <> "/input/button.html")
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
    , testCase "should wait for stable position" $
        withAssetPage "input/button.html" $ \page -> do
          _ <-
            evalOnSelector
              page
              (selectorFromText "button")
              "button => { button.style.transition = 'margin 500ms linear 0s'; button.style.marginLeft = '200px'; button.style.borderWidth = '0'; button.style.width = '200px'; button.style.height = '20px'; button.style.display = 'block'; document.body.style.margin = '0'; }"
          rafraf page 1
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
          assertEqual "pageX" (300 :: Double) =<< evaluateNumber page "() => window.pageX"
          assertEqual "pageY" (10 :: Double) =<< evaluateNumber page "() => window.pageY"
    , testCase "should wait for becoming hit target" $
        withAssetPage "input/button.html" $ \page -> do
          addFlyover page
          withAsync (click (locate page "button")) $ \pending -> do
            assertStillPending pending
            _ <- evalOnSelector page (selectorFromText ".flyover") "flyOver => { flyOver.style.left = '0'; }"
            rafraf page 5
            assertStillPending pending
            _ <- evalOnSelector page (selectorFromText ".flyover") "flyOver => { flyOver.style.left = '200px'; }"
            wait pending
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should wait for becoming hit target with trial run" $
        withAssetPage "input/button.html" $ \page -> do
          addFlyover page
          withAsync (clickWith (locate page "button") defaultClickOptions {clickOptionsTrial = Just True}) $ \pending -> do
            assertStillPending pending
            _ <- evalOnSelector page (selectorFromText ".flyover") "flyOver => { flyOver.style.left = '0'; }"
            rafraf page 5
            assertStillPending pending
            _ <- evalOnSelector page (selectorFromText ".flyover") "flyOver => { flyOver.style.left = '200px'; }"
            wait pending
          assertEqual "not clicked" "Was not clicked" =<< evaluateText page "() => window.result"
    , testCase "trial run should work with short timeout" $
        withAssetPage "input/button.html" $ \page -> do
          _ <- evalOnSelector page (selectorFromText "button") "button => { button.disabled = true; }"
          timeout <- requireTimeout 2000
          assertProtocolContains
            "click action (trial run)"
            ( clickWith
                (locate page "button")
                defaultClickOptions {clickOptionsTrial = Just True, clickOptionsTimeout = Just timeout}
            )
          assertEqual "not clicked" "Was not clicked" =<< evaluateText page "() => window.result"
    , testCase "trial run should not click" $
        withAssetPage "input/button.html" $ \page -> do
          clickWith (locate page "button") defaultClickOptions {clickOptionsTrial = Just True}
          assertEqual "not clicked" "Was not clicked" =<< evaluateText page "() => window.result"
    , testCase "should fail when obscured and not waiting for hit target" $
        withAssetPage "input/button.html" $ \page -> do
          handle <- requireHandle =<< querySelector page (selectorFromText "button")
          _ <-
            evaluate
              page
              "() => { document.body.style.position = 'relative'; const blocker = document.createElement('div'); blocker.style.position = 'absolute'; blocker.style.width = '400px'; blocker.style.height = '20px'; blocker.style.left = '0'; blocker.style.top = '0'; document.body.appendChild(blocker); }"
          clickHandleWith handle defaultClickOptions {clickOptionsForce = Just True}
          assertEqual "not clicked" "Was not clicked" =<< evaluateText page "() => window.result"
    , testCase "should wait for button to be enabled" $
        waitUntilEnabled "<button onclick=\"window.__CLICKED=true;\" disabled><span>Click target</span></button>" "text=Click target" "button"
    , testCase "should wait for input to be enabled" $
        waitUntilEnabled "<input onclick=\"window.__CLICKED=true;\" disabled>" "input" "input"
    , testCase "should wait for select to be enabled" $
        waitUntilEnabled
          "<select disabled><option selected>Hello</option></select><script>document.querySelector('select').addEventListener('mousedown', event => { window.__CLICKED=true; event.preventDefault(); });</script>"
          "select"
          "select"
    , testCase "should click disabled div" $
        withChromiumPage $ \page -> do
          setContent page "<div onclick=\"window.__CLICKED=true\" disabled>Click target</div>"
          click (locate page "text=Click target")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__CLICKED"
    , testCase "should wait for BUTTON to be clickable when it has pointer-events:none" $
        withChromiumPage $ \page -> do
          setContent page "<button onclick=\"window.__CLICKED=true\" style=\"pointer-events:none\"><span>Click target</span></button>"
          withAsync (click (locate page "text=Click target")) $ \pending -> do
            rafraf page 5
            assertEqual "undefined" SpecialUndefined =<< evaluateSpecial page "() => window.__CLICKED"
            assertStillPending pending
            _ <- evaluate page "() => document.querySelector('button').style.removeProperty('pointer-events')"
            wait pending
          assertEqual "clicked" True =<< evaluateBool page "() => window.__CLICKED"
    , testCase "should wait for LABEL to be clickable when it has pointer-events:none" $
        withChromiumPage $ \page -> do
          setContent page "<label onclick=\"window.__CLICKED=true\" style=\"pointer-events:none\"><span>Click target</span></label>"
          withAsync (click (locate page "text=Click target")) $ \pending -> do
            forM_ ([0 .. 4] :: [Int]) $ \_ ->
              assertEqual "undefined" SpecialUndefined =<< evaluateSpecial page "() => window.__CLICKED"
            _ <- evaluate page "() => document.querySelector('label').style.removeProperty('pointer-events')"
            wait pending
          assertEqual "clicked" True =<< evaluateBool page "() => window.__CLICKED"
    , testCase "should click an offscreen element when scroll-behavior is smooth" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div style=\"border: 1px solid black; height: 500px; overflow: auto; width: 500px; scroll-behavior: smooth\"><button style=\"margin-top: 2000px\" onClick=\"window.clicked = true\">hi</button></div>"
          click (locate page "button")
          assertEqual "clicked" True =<< evaluateBool page "() => window.clicked"
    , testCase "should report nice error when element is detached and force-clicked" $
        withAssetPage "input/animating-button.html" $ \page -> do
          _ <- evaluate page "() => addButton()"
          handle <- requireHandle =<< querySelector page (selectorFromText "button")
          _ <- evaluate page "() => stopButton(true)"
          assertProtocolContains
            "Element is not attached to the DOM"
            (clickHandleWith handle defaultClickOptions {clickOptionsForce = Just True})
          assertEqual "undefined" SpecialUndefined =<< evaluateSpecial page "() => window.clicked"
    , testCase "should fail when element detaches after animation" $
        withAssetPage "input/animating-button.html" $ \page -> do
          _ <- evaluate page "() => addButton()"
          handle <- requireHandle =<< querySelector page (selectorFromText "button")
          withAsync (clickHandleWith handle defaultClickOptions) $ \pending -> do
            _ <- evaluate page "() => stopButton(true)"
            assertProtocolContains "Element is not attached to the DOM" (wait pending)
          assertEqual "undefined" SpecialUndefined =<< evaluateSpecial page "() => window.clicked"
    , testCase "should retry when element detaches after animation" $
        withAssetPage "input/animating-button.html" $ \page -> do
          _ <- evaluate page "() => addButton()"
          withAsync (click (locate page "button")) $ \pending -> do
            assertStillPending pending
            _ <- evaluate page "() => stopButton(true)"
            _ <- evaluate page "() => addButton()"
            assertStillPending pending
            _ <- evaluate page "() => stopButton(true)"
            _ <- evaluate page "() => addButton()"
            assertStillPending pending
            _ <- evaluate page "() => stopButton(false)"
            wait pending
          assertEqual "clicked" True =<< evaluateBool page "() => window.clicked"
    , testCase "should retry when element is animating from outside the viewport" $
        withChromiumPage $ \page -> do
          setContent page animatingButtonMarkup
          handle <- requireHandle =<< querySelector page (selectorFromText "button")
          withAsync (clickHandleWith handle defaultClickOptions) $ \pending -> do
            _ <- evaluateOnElement handle "button => { button.className = 'animated'; }"
            wait pending
          assertEqual "clicked" True =<< evaluateBool page "() => window.clicked"
    , testCase "should fail when element is animating from outside the viewport with force" $
        withChromiumPage $ \page -> do
          setContent page animatingButtonMarkup
          handle <- requireHandle =<< querySelector page (selectorFromText "button")
          withAsync (clickHandleWith handle defaultClickOptions {clickOptionsForce = Just True}) $ \pending -> do
            _ <- evaluateOnElement handle "button => { button.className = 'animated'; }"
            assertProtocolContains "Element is outside of the viewport" (wait pending)
          assertEqual "undefined" SpecialUndefined =<< evaluateSpecial page "() => window.clicked"
    , testCase "should dispatch microtasks in order" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<button id=button>Click me</button><script>let mutationCount = 0; const observer = new MutationObserver(() => { ++mutationCount; }); observer.observe(document.body, { attributes: true, childList: true, subtree: true }); button.addEventListener('mousedown', () => { mutationCount = 0; document.body.appendChild(document.createElement('div')); }); button.addEventListener('mouseup', () => { window.result = mutationCount; });</script>"
          click (locate page "button")
          assertEqual "mutations" (1 :: Double) =<< evaluateNumber page "() => window.result"
    , testCase "should click the button when window.innerWidth is corrupted" $
        withAssetPage "input/button.html" $ \page -> do
          _ <- evaluate page "() => { Object.defineProperty(window, 'innerWidth', { value: 0 }); }"
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should click zero-sized input by label" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<label>Click me<input onclick=\"window.__clicked=true\" style=\"width:0;height:0;padding:0;margin:0;border:0;\"></label>"
          click (locate page "text=Click me")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should climb dom for inner label with pointer-events:none" $
        withChromiumPage $ \page -> do
          setContent page "<button onclick=\"window.__CLICKED=true;\"><label style=\"pointer-events:none\">Click target</label></button>"
          click (locate page "text=Click target")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__CLICKED"
    , testCase "should climb up to [role=button]" $
        withChromiumPage $ \page -> do
          setContent page "<div role=button onclick=\"window.__CLICKED=true;\"><div style=\"pointer-events:none\"><span><div>Click target</div></span></div>"
          click (locate page "text=Click target")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__CLICKED"
    , testCase "should climb up to a anchor" $
        withChromiumPage $ \page -> do
          setContent page "<a href=\"#\" onclick=\"window.__CLICKED=true\" id=\"outer\"><div id=\"intermediate\"><div id=\"inner\" style=\"pointer-events: none\">Inner</div></div></a>"
          click (locate page "#inner")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__CLICKED"
    , testCase "should climb up to a [role=link]" $
        withChromiumPage $ \page -> do
          setContent page "<div role=link onclick=\"window.__CLICKED=true\" id=\"outer\"><div id=\"inner\" style=\"pointer-events: none\">Inner</div></div>"
          click (locate page "#inner")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__CLICKED"
    , testCase "should click in an iframe with border" $
        clickIframeTarget iframeBorderMarkup
    , testCase "should click in an iframe with border 2" $
        clickIframeTarget iframeBorderMarkup2
    , testCase "should click in a transformed iframe" $
        clickIframeTarget iframeTransformedMarkup
    , testCase "should click in a transformed iframe with force" $
        withChromiumPage $ \page -> do
          setContent page iframeForceMarkup
          clickWith (childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "div")) defaultClickOptions {clickOptionsForce = Just True}
          assertEqual "clicked" True =<< evaluateBool page "() => window._clicked"
    , testCase "should click in a nested transformed iframe" $
        withChromiumPage $ \page -> do
          setContent page nestedIframeMarkup
          let
            target =
              childLocator
                (childFrameLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "iframe"))
                (selectorFromText "div")
          _ <- evaluateOnLocator target "div => { div.addEventListener('click', () => { window.top._clicked = true; }); }"
          click target
          assertEqual "clicked" True =<< evaluateBool page "() => window._clicked"
    , testCase "ensure events are dispatched in the individual tasks" $
        withChromiumPage $ \page -> do
          addInitScript
            page
            "window.builtins = { setTimeout: window.setTimeout.bind(window), clearTimeout: window.clearTimeout.bind(window), requestAnimationFrame: window.requestAnimationFrame.bind(window) };"
          setContent
            page
            "<div id=\"outer\" style=\"background: #d4d4d4; width: 60px; height: 60px;\"><div id=\"inner\" style=\"background: #adadad; width: 46px; height: 46px;\"></div></div>"
          _ <-
            evaluate
              page
              "() => { function onClick(name) { console.log('click ' + name); window.builtins.setTimeout(function() { console.log('timeout ' + name); }, 0); void Promise.resolve().then(function() { console.log('promise ' + name); }); } document.getElementById('inner').addEventListener('click', () => onClick('inner')); document.getElementById('outer').addEventListener('click', () => onClick('outer')); }"
          messages <-
            withAsync
              (replicateM 6 (fmap consoleMessageText (waitForConsoleWhere page (const True))))
              $ \pending -> do
                click (locate page "#inner")
                wait pending
          assertEqual
            "order"
            ["click inner", "promise inner", "click outer", "promise outer", "timeout inner", "timeout outer"]
            messages
    , testCase "should click if opened select covers the button" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div><select><option>very long text #1</option><option>very long text #2</option><option>very long text #3</option><option>very long text #4</option><option>very long text #5</option><option>very long text #6</option></select></div><div><button onclick=\"window.__CLICKED=42\">clickme</button></div>"
          click (locate page "select")
          click (locate page "button")
          assertEqual "clicked" (42 :: Double) =<< evaluateNumber page "() => window.__CLICKED"
    , testCase "should fire contextmenu event on right click in correct order" $
        withChromiumPage $ \page -> do
          setContent page "<button id=\"target\">Click me</button>"
          _ <-
            evaluate
              page
              "() => { const logEvent = e => console.log(e.type); document.addEventListener('mousedown', logEvent); document.addEventListener('mouseup', logEvent); document.addEventListener('contextmenu', logEvent); }"
          entries <- newIORef ([] :: [Text])
          onConsole page $ \message ->
            modifyIORef' entries (<> [consoleMessageText message])
          clickWith
            (getByRole page (AriaRole "button") defaultGetByRoleOptions {getByRoleName = Just "Click me"})
            defaultClickOptions {clickOptionsButton = Just MouseButtonRight}
          assertEqual "order" ["mousedown", "contextmenu", "mouseup"] =<< readIORef entries
    , testCase "should set PointerEvent.pressure on pointerdown" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<button id=\"target\">Click me</button><script>window.pressures = []; document.addEventListener('pointerdown', e => window.pressures.push(['pointerdown', e.pressure])); document.addEventListener('pointerup', e => window.pressures.push(['pointerup', e.pressure]));</script>"
          click (locate page "button")
          assertEqual "pressures" True
            =<< evaluateBool
              page
              "() => { const p = window.pressures; const has = wanted => p.some(x => JSON.stringify(x) === JSON.stringify(wanted)); return has(['pointerdown', 0.5]) && has(['pointerup', 0]); }"
    , testCase "should set PointerEvent.pressure on pointermove" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<body style=\"margin: 0; padding: 0;\"><div id=\"target\" style=\"width: 500px; height: 500px; background-color: red;\"></div><script>window.pressures = []; document.addEventListener('pointermove', e => window.pressures.push([e.pressure, e.clientX, e.clientY]));</script></body>"
          click (locate page "div#target")
          mouseMove page (Point (cssPixelFromInt 10) (cssPixelFromInt 10))
          mouseDown page
          mouseMove page (Point (cssPixelFromInt 250) (cssPixelFromInt 250))
          mouseUp page
          mouseMove page (Point (cssPixelFromInt 50) (cssPixelFromInt 50))
          assertEqual "pressures" True
            =<< evaluateBool
              page
              "() => { const p = window.pressures; const has = wanted => p.some(x => JSON.stringify(x) === JSON.stringify(wanted)); return has([0, 250, 250]) && has([0, 10, 10]) && has([0.5, 250, 250]) && has([0, 50, 50]); }"
    , testCase "should click into shadow root with slotted div" $
        withChromiumPage $ \page -> do
          setContent page "<my-button><template shadowrootmode=\"open\"><button><slot></slot></button></template><div>Foo</div></my-button>"
          click (getByRole page (AriaRole "button") defaultGetByRoleOptions {getByRoleName = Just "Foo"})
    , testCase "should click shadow root button" $
        withChromiumPage $ \page -> do
          setContent page "<my-button><template shadowrootmode=\"open\"><button><slot></slot></button></template><div>Foo</div></my-button>"
          click (locate page "my-button")
    , testCase "should click with tweened mouse movement" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<body style=\"margin: 0; padding: 0; height: 500px; width: 500px;\"><div style=\"position: relative; top: 280px; left: 150px; width: 100px; height: 40px\">Click me</div></body>"
          mouseMove page (Point (cssPixelFromInt 100) (cssPixelFromInt 100))
          _ <-
            evaluate
              page
              "() => { window.result = []; document.addEventListener('mousemove', event => { window.result.push([event.clientX, event.clientY]); }); }"
          clickWith (locate page "div") defaultClickOptions {clickOptionsSteps = Just 5}
          assertEqual
            "path"
            True
            =<< evaluateBool
              page
              "() => JSON.stringify(window.result) === JSON.stringify([[120,140],[140,180],[160,220],[180,260],[200,300]])"
    , testCase "should not scroll the page when scroll is none" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"height: 2000px;\"></div><button onclick=\"window._clicked=true\">click me</button>"
          assertProtocolContains
            "element is outside of the viewport"
            (clickWith (locate page "button") defaultClickOptions {clickOptionsScroll = Just ScrollNone, clickOptionsTimeout = milliseconds 2000})
          assertEqual "not clicked" False =<< evaluateBool page "() => !!window._clicked"
          assertEqual "not scrolled" (0 :: Double) =<< evaluateNumber page "() => window.scrollY"
    , testCase "should click in-viewport element when scroll is none" $
        withChromiumPage $ \page -> do
          setContent page "<button onclick=\"window._clicked=true\">click me</button><div style=\"height: 2000px;\"></div>"
          clickWith (locate page "button") defaultClickOptions {clickOptionsScroll = Just ScrollNone, clickOptionsTimeout = milliseconds 2000}
          assertEqual "clicked" True =<< evaluateBool page "() => !!window._clicked"
    , testCase "should not scroll nested container when scroll is none" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div style=\"height: 100px; width: 100px; overflow-y: scroll;\"><div style=\"height: 50px;\">A</div><div style=\"height: 50px;\">B</div><button style=\"height: 50px; width: 100px;\" onclick=\"window._clicked=true\">C</button></div>"
          result <-
            try (clickWith (locate page "button") defaultClickOptions {clickOptionsScroll = Just ScrollNone, clickOptionsTimeout = milliseconds 2000})
              :: Playwright (Either PlaywrightError ())
          assertEqual "failed without scroll" True (either (const True) (const False) result)
          assertEqual "not clicked" False =<< evaluateBool page "() => !!window._clicked"
          clickWith (locate page "button") defaultClickOptions {clickOptionsTimeout = milliseconds 2000}
          assertEqual "clicked with default scroll" True =<< evaluateBool page "() => !!window._clicked"
    , testCase "should not scroll on hover when scroll is none" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div style=\"height: 2000px;\"></div><div onmouseover=\"window._hovered=true\" style=\"width: 50px; height: 50px;\">hover me</div>"
          assertProtocolContains
            "element is outside of the viewport"
            (hoverWith (locate page "div >> text=hover me") defaultHoverOptions {hoverOptionsScroll = Just ScrollNone, hoverOptionsTimeout = milliseconds 2000})
          assertEqual "not hovered" False =<< evaluateBool page "() => !!window._hovered"
          assertEqual "not scrolled" (0 :: Double) =<< evaluateNumber page "() => window.scrollY"
    , testCase "trial run should not double click" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/input/button.html")) defaultGotoOptions
          _ <-
            evaluate
              page
              "() => { window['double'] = false; document.querySelector('button').addEventListener('dblclick', () => { window['double'] = true; }); }"
          dblclickWith (locate page "button") defaultDblclickOptions {dblclickOptionsTrial = Just True}
          assertEqual "double" False =<< evaluateBool page "() => window['double']"
          assertEqual "result" "Was not clicked" =<< evaluateText page "() => window.result"
    ]

waitUntilEnabled :: Text -> Text -> Text -> IO ()
waitUntilEnabled markup selector tag =
  withChromiumPage $ \page -> do
    setContent page markup
    withAsync (click (locate page selector)) $ \pending -> do
      rafraf page 5
      assertEqual "undefined" SpecialUndefined =<< evaluateSpecial page "() => window.__CLICKED"
      assertStillPending pending
      _ <- evaluate page ("() => document.querySelector('" <> tag <> "').removeAttribute('disabled')")
      wait pending
    assertEqual "clicked" True =<< evaluateBool page "() => window.__CLICKED"

addFlyover :: Page -> Playwright ()
addFlyover page =
  evalOnSelector
    page
    (selectorFromText "button")
    "button => { button.style.borderWidth = '0'; button.style.width = '200px'; button.style.height = '20px'; document.body.style.margin = '0'; document.body.style.position = 'relative'; const flyOver = document.createElement('div'); flyOver.className = 'flyover'; flyOver.style.position = 'absolute'; flyOver.style.width = '400px'; flyOver.style.height = '20px'; flyOver.style.left = '-200px'; flyOver.style.top = '0'; flyOver.style.background = 'red'; document.body.appendChild(flyOver); }"
    >> pure ()

clickIframeTarget :: Text -> IO ()
clickIframeTarget markup =
  withChromiumPage $ \page -> do
    setContent page markup
    click (childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "div"))
    assertEqual "clicked" True =<< evaluateBool page "() => window._clicked"

animatingButtonMarkup :: Text
animatingButtonMarkup =
  "<style>@keyframes move { from { left: -300px; } to { left: 0; } } button { position: absolute; left: -300px; top: 0; bottom: 0; width: 200px; } button.animated { animation: 1s linear 1s move forwards; }</style><div style=\"position: relative; width: 300px; height: 300px;\"><button onclick=\"window.clicked=true\"></button></div>"

iframeBorderMarkup :: Text
iframeBorderMarkup =
  "<style>body, html, iframe { margin: 0; padding: 0; border: none; box-sizing: border-box; } iframe { border: 4px solid black; background: gray; margin-left: 33px; margin-top: 24px; width: 400px; height: 400px; }</style><iframe srcdoc=\"<style>body, html { margin: 0; padding: 0; } div { margin-left: 10px; margin-top: 20px; width: 2px; height: 2px; }</style><div>Target</div><script>document.querySelector('div').addEventListener('click', () => window.top._clicked = true);</script>\"></iframe>"

iframeBorderMarkup2 :: Text
iframeBorderMarkup2 =
  "<style>body, html, iframe { margin: 0; padding: 0; border: none; } iframe { border: 4px solid black; background: gray; margin-left: 33px; margin-top: 24px; width: 400px; height: 400px; }</style><iframe srcdoc=\"<style>body, html { margin: 0; padding: 0; } div { margin-left: 10px; margin-top: 20px; width: 2px; height: 2px; }</style><div>Target</div><script>document.querySelector('div').addEventListener('click', () => window.top._clicked = true);</script>\"></iframe>"

iframeTransformedMarkup :: Text
iframeTransformedMarkup =
  "<style>body, html, iframe { margin: 0; padding: 0; border: none; } iframe { border: 4px solid black; background: gray; margin-left: 33px; margin-top: 24px; width: 400px; height: 400px; transform: translate(100px, 100px) scale(1.2) rotate3d(1, 1, 1, 25deg); }</style><iframe srcdoc=\"<style>body, html { margin: 0; padding: 0; } div { margin-left: 10px; margin-top: 20px; width: 2px; height: 2px; }</style><div>Target</div><script>document.querySelector('div').addEventListener('click', () => window.top._clicked = true);</script>\"></iframe>"

iframeForceMarkup :: Text
iframeForceMarkup =
  "<style>body, html, iframe { margin: 0; padding: 0; border: none; } iframe { background: gray; margin-left: 33px; margin-top: 24px; width: 400px; height: 400px; transform: translate(-40px, -40px) scale(0.8); }</style><iframe srcdoc=\"<style>body, html { margin: 0; padding: 0; } div { margin-left: 10px; margin-top: 20px; width: 2px; height: 2px; }</style><div>Target</div><script>document.querySelector('div').addEventListener('click', () => window.top._clicked = true);</script>\"></iframe>"

nestedIframeMarkup :: Text
nestedIframeMarkup =
  "<style>body, html, iframe { margin: 0; padding: 0; box-sizing: border-box; } iframe { border: 1px solid black; background: gray; margin-left: 33px; margin-top: 24px; width: 400px; height: 400px; transform: scale(0.8); }</style><iframe srcdoc=\"<style>body, html, iframe { margin: 0; padding: 0; box-sizing: border-box; } iframe { border: 3px solid black; background: gray; margin-left: 18px; margin-top: 14px; width: 200px; height: 200px; transform: scale(0.7); }</style><iframe srcdoc='<style>div { margin-left: 10px; margin-top: 20px; width: 2px; height: 2px; }</style><div>Target</div>'></iframe>\"></iframe>"


waitForStyleGone :: Text -> Text -> Text -> IO ()
waitForStyleGone property hidden shown =
  withAssetPage "input/button.html" $ \page -> do
    _ <- evalOnSelector page (selectorFromText "button") ("b => { b.style." <> property <> " = '" <> hidden <> "'; }")
    timeout <- requireTimeout 0
    withAsync (clickWith (locate page "button") defaultClickOptions {clickOptionsTimeout = Just timeout}) $ \pending -> do
      rafraf page 5
      assertStillPending pending
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

requireFrame :: Maybe Frame -> Playwright Frame
requireFrame found =
  case found of
    Just frame ->
      pure frame
    Nothing ->
      assertFailure "expected frame"


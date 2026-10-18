module Playwright.HitTargetSpec
  ( tests
  ) where

import Data.Text
  ( Text
  , isInfixOf
  )
import Playwright
  ( ClickOptions (clickOptionsTimeout)
  , SpecialJsValue (SpecialUndefined)
  , asElement
  , childFrameLocator
  , childLocator
  , click
  , clickHandle
  , clickWith
  , defaultClickOptions
  , dragAndDrop
  , evalOnSelector
  , evaluate
  , evaluateBool
  , evaluateHandleExpression
  , evaluateNumber
  , evaluateSpecial
  , evaluateText
  , evaluateTexts
  , frameLocator
  , frames
  , innerText
  , isVisible
  , locatorOnFrame
  , selectorFromText
  , setContent
  , url
  , urlToText
  , waitForTimeout
  )
import Playwright.Harness
  ( requireTimeout
  , assertProtocolContains
  , locate
  , requireHandle
  , withAssetPage
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
    "hit-target"
    [ testCase "should block all events when hit target is wrong" $
        withAssetPage "input/button.html" $ \page -> do
          _ <-
            evaluate
              page
              "() => {\n\
              \  const blocker = document.createElement('div');\n\
              \  blocker.style.position = 'absolute';\n\
              \  blocker.style.width = '400px';\n\
              \  blocker.style.height = '400px';\n\
              \  blocker.style.left = '0';\n\
              \  blocker.style.top = '0';\n\
              \  document.body.appendChild(blocker);\n\
              \  const allEvents = [];\n\
              \  window.allEvents = allEvents;\n\
              \  for (const name of ['mousedown', 'mouseup', 'click', 'dblclick', 'auxclick', 'contextmenu', 'pointerdown', 'pointerup']) {\n\
              \    window.addEventListener(name, e => allEvents.push(e.type));\n\
              \    blocker.addEventListener(name, e => allEvents.push(e.type));\n\
              \  }\n\
              \}"
          timeout <- requireTimeout 3000
          assertProtocolContains
            "Timeout 3000ms exceeded"
            (clickWith (locate page "button") defaultClickOptions {clickOptionsTimeout = Just timeout})
          waitForTimeout page =<< requireTimeout 1000
          assertEqual "events" [] =<< evaluateTexts page "() => window.allEvents"
    , testCase "should block click when mousedown fails" $
        withAssetPage "input/button.html" $ \page -> do
          _ <-
            evalOnSelector
              page
              (selectorFromText "button")
              "button => {\n\
              \  button.addEventListener('mousemove', () => {\n\
              \    button.style.marginLeft = '100px';\n\
              \  });\n\
              \  const allEvents = [];\n\
              \  window.allEvents = allEvents;\n\
              \  for (const name of ['mousemove', 'mousedown', 'mouseup', 'click', 'dblclick', 'auxclick', 'contextmenu', 'pointerdown', 'pointerup'])\n\
              \    button.addEventListener(name, e => allEvents.push(e.type));\n\
              \}"
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
          assertEqual
            "events"
            [ "mousemove"
            , "mousemove"
            , "pointerdown"
            , "mousedown"
            , "pointerup"
            , "mouseup"
            , "click"
            ]
            =<< evaluateTexts page "() => window.allEvents"
    , testCase "should click when element detaches in mousedown" $
        withAssetPage "input/button.html" $ \page -> do
          _ <-
            evalOnSelector
              page
              (selectorFromText "button")
              "button => {\n\
              \  button.addEventListener('mousedown', () => {\n\
              \    window.result = 'Mousedown';\n\
              \    button.remove();\n\
              \  });\n\
              \}"
          timeout <- requireTimeout 15000
          clickWith (locate page "button") defaultClickOptions {clickOptionsTimeout = Just timeout}
          assertEqual "mousedown" "Mousedown" =<< evaluateText page "() => window.result"
    , testCase "should block all events when hit target is wrong and element detaches" $
        withAssetPage "input/button.html" $ \page -> do
          _ <-
            evalOnSelector
              page
              (selectorFromText "button")
              "button => {\n\
              \  const blocker = document.createElement('div');\n\
              \  blocker.style.position = 'absolute';\n\
              \  blocker.style.width = '400px';\n\
              \  blocker.style.height = '400px';\n\
              \  blocker.style.left = '0';\n\
              \  blocker.style.top = '0';\n\
              \  document.body.appendChild(blocker);\n\
              \  window.addEventListener('mousemove', () => button.remove());\n\
              \  const allEvents = [];\n\
              \  window.allEvents = allEvents;\n\
              \  for (const name of ['mousedown', 'mouseup', 'click', 'dblclick', 'auxclick', 'contextmenu', 'pointerdown', 'pointerup']) {\n\
              \    window.addEventListener(name, e => allEvents.push(e.type));\n\
              \    blocker.addEventListener(name, e => allEvents.push(e.type));\n\
              \  }\n\
              \}"
          timeout <- requireTimeout 3000
          assertProtocolContains
            "Timeout 3000ms exceeded"
            (clickWith (locate page "button") defaultClickOptions {clickOptionsTimeout = Just timeout})
          waitForTimeout page =<< requireTimeout 1000
          assertEqual "events" [] =<< evaluateTexts page "() => window.allEvents"
    , testCase "should not block programmatic events" $
        withAssetPage "input/button.html" $ \page -> do
          _ <-
            evalOnSelector
              page
              (selectorFromText "button")
              "button => {\n\
              \  button.addEventListener('mousemove', () => {\n\
              \    button.style.marginLeft = '100px';\n\
              \    button.dispatchEvent(new MouseEvent('click'));\n\
              \  });\n\
              \  const allEvents = [];\n\
              \  window.allEvents = allEvents;\n\
              \  button.addEventListener('click', e => {\n\
              \    if (!e.isTrusted)\n\
              \      allEvents.push(e.type);\n\
              \  });\n\
              \}"
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
          assertEqual "programmatic" ["click", "click"] =<< evaluateTexts page "() => window.allEvents"
    , testCase "should click the button again after document.write" $
        withAssetPage "input/button.html" $ \page -> do
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
          _ <-
            evaluate
              page
              "() => {\n\
              \  document.open();\n\
              \  document.write('<button onclick=\"window.result2 = true\"></button>');\n\
              \  document.close();\n\
              \}"
          click (locate page "button")
          assertEqual "rewritten" True =<< evaluateBool page "() => window.result2"
    , testCase "should work with mui select" $
        withAssetPage "mui.html" $ \page -> do
          _ <-
            evaluate
              page
              "() => {\n\
              \  renderComponent(e(MaterialUI.FormControl, { fullWidth: true }, [\n\
              \    e(MaterialUI.InputLabel, { id: 'demo-simple-select-label' }, ['Age']),\n\
              \    e(MaterialUI.Select, {\n\
              \      labelId: 'demo-simple-select-label',\n\
              \      id: 'demo-simple-select',\n\
              \      value: 10,\n\
              \      label: 'Age',\n\
              \    }, [\n\
              \      e(MaterialUI.MenuItem, { value: 10 }, ['Ten']),\n\
              \      e(MaterialUI.MenuItem, { value: 20 }, ['Twenty']),\n\
              \      e(MaterialUI.MenuItem, { value: 30 }, ['Thirty']),\n\
              \    ]),\n\
              \  ]));\n\
              \}"
          click (locate page "div.MuiFormControl-root:has-text(\"Age\")")
          assertEqual "thirty" True =<< isVisible (locate page "text=Thirty")
    , testCase "should work with drag and drop that moves the element under cursor" $
        withAssetPage "input/drag-n-drop-manual.html" $ \page -> do
          dragAndDrop (locate page "#from") (locate page "#to")
          assertEqual "dropped" "Dropped" =<< innerText (locate page "#to")
    , testCase "should work with block inside inline" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div><span><div id=\"target\" onclick=\"window._clicked=true\">Romimine</div></span></div>"
          click (locate page "#target")
          assertEqual "clicked" True =<< evaluateBool page "() => window._clicked"
    , testCase "should work with block-block-block inside inline-inline" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div><a href=\"#ney\"><div><span><a href=\"#yay\"><div><h3 id=\"target\">Romimine</h3></div></a></span></div></a></div>"
          click (locate page "#target")
          current <- urlToText <$> url page
          assertEqual "hash" True ("#yay" `isInfixOf` current)
    , testCase "should work with block inside inline in shadow dom" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div></div><script>\n\
            \const root = document.querySelector('div');\n\
            \const shadowRoot = root.attachShadow({ mode: 'open' });\n\
            \const span = document.createElement('span');\n\
            \shadowRoot.appendChild(span);\n\
            \const div = document.createElement('div');\n\
            \span.appendChild(div);\n\
            \div.id = 'target';\n\
            \div.addEventListener('click', () => window._clicked = true);\n\
            \div.textContent = 'Hello';\n\
            \</script>"
          click (locate page "#target")
          assertEqual "clicked" True =<< evaluateBool page "() => window._clicked"
    , testCase "should not click iframe overlaying the target" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<button style=\"position: absolute; left: 250px;bottom: 0;height: 40px;width: 200px;\" onclick=\"window._clicked=1\">click-me</button>\n\
            \<div style=\"background: transparent; bottom: 0px; left: 0px; margin: 0px; padding: 0px; position: fixed; z-index: 2147483647;\">\n\
            \  <iframe srcdoc=\"<body onclick='window.top._clicked=2' style='background-color:red;height:40px;'></body>\" style=\"display: block; border: 0px; width: 100vw; height: 48px;\"></iframe>\n\
            \</div>"
          timeout <- requireTimeout 3000
          assertProtocolContains
            "intercepts pointer events"
            (clickWith (locate page "text=click-me") defaultClickOptions {clickOptionsTimeout = Just timeout})
          special <- evaluateSpecial page "() => window._clicked"
          assertEqual "not clicked" SpecialUndefined special
    , testCase "should not click an element overlaying iframe with the target" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div onclick='window.top._clicked=1'>padding</div>\n\
            \<iframe width=600 height=600 srcdoc=\"<iframe srcdoc='<div onclick=&quot;window.top._clicked=2&quot;>padding</div><div onclick=&quot;window.top._clicked=3&quot;>inner</div>'></iframe><div onclick='window.top._clicked=4'>outer</div>\"></iframe>\n\
            \<div onclick='window.top._clicked=5' style=\"position: absolute; left: 0; right: 0; top: 0; bottom: 0; background: rgba(255, 0, 0, 0.1); padding: 200px;\">PINK OVERLAY</div>"
          let
            target =
              childLocator
                ( childFrameLocator
                    (frameLocator page (selectorFromText "iframe"))
                    (selectorFromText "iframe")
                )
                (selectorFromText "text=inner")
          timeout <- requireTimeout 3000
          assertProtocolContains
            "intercepts pointer events"
            (clickWith target defaultClickOptions {clickOptionsTimeout = Just timeout})
          special <- evaluateSpecial page "() => window._clicked"
          assertEqual "not clicked" SpecialUndefined special
          _ <- evalOnSelector page (selectorFromText "text=overlay") "e => { e.style.display = 'none'; }"
          click target
          assertEqual "inner" (3 :: Double) =<< evaluateNumber page "() => window._clicked"
    , testCase "should click into frame inside closed shadow root" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div id=framecontainer></div>\n\
            \<script>\n\
            \  const iframe = document.createElement('iframe');\n\
            \  iframe.setAttribute('name', 'myframe');\n\
            \  iframe.setAttribute('srcdoc', '<div onclick=\"window.top.__clicked = true\">click me</div>');\n\
            \  const div = document.getElementById('framecontainer');\n\
            \  const host = div.attachShadow({ mode: 'closed' });\n\
            \  host.appendChild(iframe);\n\
            \</script>"
          allFrames <- frames page
          case allFrames of
            _ : frame : _ -> do
              click (locatorOnFrame page frame (selectorFromText "text=click me"))
              assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
            _ ->
              assertFailure "expected iframe"
    , testCase "should click an element inside closed shadow root" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div id=container></div>\n\
            \<script>\n\
            \  const span = document.createElement('span');\n\
            \  span.textContent = 'click me';\n\
            \  span.addEventListener('click', () => window.__clicked = true);\n\
            \  const div = document.getElementById('container');\n\
            \  const host = div.attachShadow({ mode: 'closed' });\n\
            \  host.appendChild(span);\n\
            \  window.__target = span;\n\
            \</script>"
          handle <- evaluateHandleExpression page "window.__target"
          element <- requireHandle =<< asElement handle
          clickHandle element
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should detect overlay from another shadow root" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<style>\n\
            \  div > div { position: absolute; top: 0; left: 0; width: 10px; height: 10px; }\n\
            \  span { display: block; position: absolute; left: 0; top: 0; width: 300px; height: 300px; }\n\
            \</style>\n\
            \<div style=\"position:relative; width:300px; height:300px\">\n\
            \  <div id=container1></div>\n\
            \  <div id=container2></div>\n\
            \</div>\n\
            \<script>\n\
            \  for (const id of ['container1', 'container2']) {\n\
            \    const span = document.createElement('span');\n\
            \    span.id = id + '-span';\n\
            \    span.textContent = 'click me';\n\
            \    span.style.display = 'block';\n\
            \    span.style.position = 'absolute';\n\
            \    span.style.left = '20px';\n\
            \    span.style.top = '20px';\n\
            \    span.style.width = '300px';\n\
            \    span.style.height = '300px';\n\
            \    span.addEventListener('click', () => window.__clicked = id);\n\
            \    const div = document.getElementById(id);\n\
            \    const host = div.attachShadow({ mode: 'open' });\n\
            \    host.appendChild(span);\n\
            \  }\n\
            \</script>"
          timeout <- requireTimeout 3000
          assertProtocolContains
            "intercepts pointer events"
            ( clickWith
                (locate page "#container1 >> text=click me")
                defaultClickOptions {clickOptionsTimeout = Just timeout}
            )
    , testCase "should detect overlaid element in a transformed iframe" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<style>\n\
            \  body, html, iframe { margin: 0; padding: 0; border: none; }\n\
            \  iframe { border: 4px solid black; background: gray; margin-left: 33px; margin-top: 24px; width: 400px; height: 400px; transform: scale(1.2); }\n\
            \</style>\n\
            \<iframe srcdoc=\"\n\
            \  <style>body, html { margin: 0; padding: 0; } div { margin-left: 10px; margin-top: 20px; width: 2px; height: 2px; } section { position: absolute; top: 0; left: 0; bottom: 0; right: 0; }</style>\n\
            \  <div>Target</div>\n\
            \  <section>Overlay</section>\n\
            \  <script>document.querySelector('div').addEventListener('click', () => window.top._clicked = true);</script>\n\
            \\"></iframe>"
          let
            target =
              childLocator
                (frameLocator page (selectorFromText "iframe"))
                (selectorFromText "div")
          timeout <- requireTimeout 5000
          assertProtocolContains
            "intercepts pointer events"
            (clickWith target defaultClickOptions {clickOptionsTimeout = Just timeout})
    , testCase "should click in iframe with padding" $
        clickPaddedIframe "border-box"
    , testCase "should click in iframe with padding 2" $
        clickPaddedIframe "content-box"
    , testCase "should click in custom element" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<html><body><my-input></my-input>\n\
            \<script>\n\
            \  class MyInput extends HTMLElement {\n\
            \    connectedCallback() {\n\
            \      this.attachShadow({mode:'open'});\n\
            \      this.shadowRoot.innerHTML = '<div><span><input type=\"text\" /></span></div>';\n\
            \      this.shadowRoot.querySelector('input').addEventListener('click', () => window.__clicked = true);\n\
            \    }\n\
            \  }\n\
            \  customElements.define('my-input', MyInput);\n\
            \</script></body></html>"
          click (locate page "input")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    ]

clickPaddedIframe :: Text -> IO ()
clickPaddedIframe boxSizing =
  withChromiumPage $ \page -> do
    setContent
      page
      ( "<style>body, html, iframe { margin: 0; padding: 0; border: none; box-sizing: "
          <> boxSizing
          <> "; } iframe { background: gray; width: 200px; height: 200px; padding-top: 100px; }</style>\n\
             \<iframe srcdoc=\"<style>body, html { margin: 0; padding: 0; } div { height: 100px; }</style>\
             \<div>Non-target</div><div id=target>Target</div><div>Non-target</div>\
             \<script>document.querySelector('#target').addEventListener('click', () => window.top._clicked = true);</script>\"></iframe>"
      )
    click
      ( childLocator
          (frameLocator page (selectorFromText "iframe"))
          (selectorFromText "#target")
      )
    assertEqual "clicked" True =<< evaluateBool page "() => window._clicked"


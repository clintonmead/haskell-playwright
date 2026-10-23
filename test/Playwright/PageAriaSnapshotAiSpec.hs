-- Ports upstream tests/page/page-aria-snapshot-ai.spec.ts.
--
-- Every upstream case takes its snapshot with @mode: 'ai'@, which the driver
-- distills and annotates with element refs. The Haskell client only passes the
-- mode through, so these cases exercise the driver's AI output.
--
-- Upstream @toContainYaml@ is approximated by line containment
-- (@yamlContainsLines@): each expected line, with its indentation ignored,
-- must occur as a line of the snapshot. Upstream @toHaveText@ is approximated
-- by comparing the stripped @textContent@ of the ref'd element.
--
-- Upstream sub-assertions that call @locator.normalize()@ and compare the
-- resolved locator string are not ported: the Haskell client does not expose
-- @normalize@. The rest of those cases is kept.
--
-- Skipped upstream cases:
--   "should gracefully fallback when child frame cant be captured": the driver
--     renders a child generic node with text ">" under the iframe, where the
--     upstream expectation has the iframe with no children. The divergence is
--     in the driver output, not in the Haskell client, so the case is not ported.
module Playwright.PageAriaSnapshotAiSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.ByteString.Lazy qualified as BSL
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
  ( AriaSnapshotMode (AriaSnapshotModeAi)
  , AriaSnapshotOptions
  , ariaSnapshotOptionsDepth
  , ariaSnapshotOptionsMode
  , ariaSnapshotOptionsTimeout
  , Locator
  , LoadState (LoadStateCommit)
  , Page
  , Playwright
  , WaitForState (WaitForAttached)
  , ariaSnapshot
  , ariaSnapshotWith
  , childLocator
  , defaultAriaSnapshotOptions
  , defaultGotoOptions
  , defaultSetContentOptions
  , setContentOptionsWaitUntil
  , evaluate
  , evaluateOnLocator
  , focus
  , frameLocator
  , frames
  , currentUrl
  , urlToText
  , goto
  , locatorCount
  , locatorOnFrame
  , pageAriaSnapshotWith
  , reload
  , selectorFromText
  , setContent
  , setContentWith
  , textContent
  , urlFromText
  , waitForFunction
  , waitForSelector
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , locate
  , requireTimeout
  , withAssets
  , withChromiumPage
  , withLocalApp
  )
import Playwright.AriaSnapshotSupport
  ( textLines
  , yamlContainsLines
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadString)
  , SerializedValue (SerializedNode)
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
    "page-aria-snapshot-ai"
    [ testCase "should generate refs" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<button>One</button>"
                , "<button>Two</button>"
                , "<button>Three</button>"
                ]
            )
          snapshot1 <- snapshotForAI page
          containsYaml
            snapshot1
            [ "- generic [active] [ref=e1]:"
            , "- button \"One\" [ref=e2]"
            , "- button \"Two\" [ref=e3]"
            , "- button \"Three\" [ref=e4]"
            ]
          expectText (locate page "aria-ref=e2") "One"
          expectText (locate page "aria-ref=e3") "Two"
          expectText (locate page "aria-ref=e4") "Three"
          _ <- evaluateOnLocator (locate page "aria-ref=e3") "(e) => { e.textContent = 'Not Two'; }"
          snapshot2 <- snapshotForAI page
          containsYaml
            snapshot2
            [ "- generic [active] [ref=e1]:"
            , "- button \"One\" [ref=e2]"
            , "- button \"Not Two\" [ref=e5]"
            , "- button \"Three\" [ref=e4]"
            ]
    , testCase "should list iframes" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<h1>Hello</h1>"
                , "<iframe name=\"foo\" src=\"data:text/html,<h1>World</h1>\"></iframe>"
                ]
            )
          snapshot1 <- snapshotForAI page
          assertBool "snapshot lists an iframe" ("- iframe" `T.isInfixOf` snapshot1)
          frameSnapshot <- ariaSnapshot (childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "body"))
          assertEqual "frame snapshot" "- heading \"World\" [level=1]" frameSnapshot
    , testCase "should snapshot a locator inside an iframe" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<h1>Main Page</h1>"
                , "<iframe srcdoc=\"<ul><li>Item 1</li><li>Item 2</li></ul>\"></iframe>"
                ]
            )
          fs <- frames page
          snapshot <- locatorSnapshotForAI (locatorOnFrame page (fs !! 1) (selectorFromText "ul")) defaultAriaSnapshotOptions
          containsYaml
            snapshot
            [ "- list [ref=f1e1]:"
            , "- listitem [ref=f1e2]: Item 1"
            , "- listitem [ref=f1e3]: Item 2"
            ]
    , testCase "should limit depth across iframe boundary" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<nav>"
                , "  <iframe srcdoc=\"<ul><li><button>Deep</button></li></ul>\"></iframe>"
                , "</nav>"
                ]
            )
          snapshot <- snapshotForAIWith page defaultAriaSnapshotOptions {ariaSnapshotOptionsDepth = Just 3}
          containsYaml
            snapshot
            [ "- navigation [ref=e2]:"
            , "- iframe [ref=e3]:"
            , "- list [ref=f1e2]:"
            , "- listitem [ref=f1e3]"
            ]
          assertBool "depth-limited snapshot omits the button" (not ("button" `T.isInfixOf` snapshot))
    , testCase "should stitch all frame snapshots" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/nested-frames.html")) defaultGotoOptions
          snapshot <- snapshotForAI page
          containsYaml
            snapshot
            [ "- generic [active] [ref=e1]:"
            , "- iframe [ref=e2]:"
            , "- generic [ref=f1e1]:"
            , "- iframe [ref=f1e2]:"
            , "- generic [ref=f3e1]: Hi, I'm frame"
            , "- iframe [ref=f1e3]:"
            , "- generic [ref=f4e1]: Hi, I'm frame"
            , "- iframe [ref=e3]:"
            , "- generic [ref=f2e1]: Hi, I'm frame"
            ]
          href1 <- hrefOf (locate page "aria-ref=e1")
          assertEqual "main frame href" (prefix <> "/frames/nested-frames.html") href1
          href2 <- hrefOf (locate page "aria-ref=f1e2")
          assertEqual "first nested frame href" (prefix <> "/frames/two-frames.html") href2
          href3 <- hrefOf (locate page "aria-ref=f4e2")
          assertEqual "deepest frame href" (prefix <> "/frames/frame.html") href3
          -- The upstream normalize() checks are not ported; see the module header.
          error' <- staleRefCount (locate page "aria-ref=e1000")
          assertEqual "unknown ref resolves to nothing" 0 error'
    , testCase "should re-number refs across navigations but not same-document navigations" $
        withChromiumPage $ \page -> withLocalApp (routedApp [("/one.html", "<button>One</button>"), ("/two.html", "<button>Two</button>")]) $ \prefix -> do
          -- The first committed document keeps the base seq, so the main frame has no prefix.
          _ <- goto page (urlFromText (prefix <> "/one.html")) defaultGotoOptions
          oneSnapshot <- snapshotForAI page
          let oneRef = refAfter "button \"One\" [ref=" oneSnapshot
          expectText (locate page ("aria-ref=" <> oneRef)) "One"

          -- Cross-document navigation re-numbers the main frame, so its refs gain a frame prefix.
          _ <- goto page (urlFromText (prefix <> "/two.html")) defaultGotoOptions
          twoSnapshot <- snapshotForAI page
          let twoRef = refAfter "button \"Two\" [ref=" twoSnapshot
          assertBool "re-numbered ref has a frame prefix" ("f" `T.isPrefixOf` twoRef)
          expectText (locate page ("aria-ref=" <> twoRef)) "Two"

          -- The stale ref from the previous document must not resolve against the new one.
          staleCount <- staleRefCount (locate page ("aria-ref=" <> oneRef))
          assertEqual "stale ref resolves to nothing" 0 staleCount

          -- Same-document navigation keeps refs intact.
          _ <- evaluate page "() => history.pushState({}, '', '/pushed.html')"
          afterPush <- snapshotForAI page
          assertBool "same-document navigation keeps refs" (("button \"Two\" [ref=" <> twoRef <> "]") `T.isInfixOf` afterPush)
          expectText (locate page ("aria-ref=" <> twoRef)) "Two"
    , testCase "should persist iframe references" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<ul>"
                , "  <li><iframe srcdoc=\"<button>button1</button>\"></iframe></li>"
                , "  <li><iframe srcdoc=\"<button>button2</button>\"></iframe></li>"
                , "</ul>"
                ]
            )
          snapshot1 <- snapshotForAI page
          containsYaml
            snapshot1
            [ "- list [ref=e2]:"
            , "- listitem [ref=e3]:"
            , "- iframe [ref=e4]:"
            , "- button \"button1\" [ref=f1e2]"
            , "- listitem [ref=e5]:"
            , "- iframe [ref=e6]:"
            , "- button \"button2\" [ref=f2e2]"
            ]

          _ <- evaluate page "() => document.querySelector('iframe').remove()"
          snapshot2 <- snapshotForAI page
          containsYaml
            snapshot2
            [ "- list [ref=e2]:"
            , "- listitem [ref=e3]"
            , "- listitem [ref=e5]:"
            , "- iframe [ref=e6]:"
            , "- button \"button2\" [ref=f2e2]"
            ]
          expectText (locate page "aria-ref=f2e2") "button2"

          _ <-
            evaluate
              page
              ( textLines
                  [ "() => {"
                  , "  const frame = document.createElement('iframe');"
                  , "  frame.setAttribute('srcdoc', '<button>button1</button>');"
                  , "  document.querySelector('li').appendChild(frame);"
                  , "}"
                  ]
              )
          snapshot3 <- snapshotForAI page
          containsYaml
            snapshot3
            [ "- list [ref=e2]:"
            , "- listitem [ref=e3]:"
            , "- iframe [ref=e7]:"
            , "- button \"button1\" [ref=f3e2]"
            , "- listitem [ref=e5]:"
            , "- iframe [ref=e6]:"
            , "- button \"button2\" [ref=f2e2]"
            ]
          expectText (locate page "aria-ref=f3e2") "button1"
          expectText (locate page "aria-ref=f2e2") "button2"
    , testCase "should not generate refs for elements with pointer-events:none" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<button style='pointer-events: none'>no-ref</button>"
                , "<div style='pointer-events: none'>"
                , "  <button style='pointer-events: auto'>with-ref</button>"
                , "</div>"
                , "<div style='pointer-events: none'>"
                , "  <div style='pointer-events: initial'>"
                , "    <button>with-ref</button>"
                , "  </div>"
                , "</div>"
                , "<div style='pointer-events: none'>"
                , "  <div style='pointer-events: auto'>"
                , "    <button>with-ref</button>"
                , "  </div>"
                , "</div>"
                , "<div style='pointer-events: auto'>"
                , "  <div style='pointer-events: none'>"
                , "    <button>no-ref</button>"
                , "  </div>"
                , "</div>"
                ]
            )
          snapshot <- snapshotForAI page
          containsYaml
            snapshot
            [ "- generic [active] [ref=e1]:"
            , "- button \"no-ref\""
            , "- button \"with-ref\" [ref=e2]"
            , "- button \"with-ref\" [ref=e4]"
            , "- button \"with-ref\" [ref=e6]"
            , "- generic [ref=e7]:"
            , "- generic:"
            , "- button \"no-ref\""
            ]
    , testCase "emit generic roles for nodes w/o roles" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<style>"
                , "input {"
                , "  width: 0;"
                , "  height: 0;"
                , "  opacity: 0;"
                , "}"
                , "</style>"
                , "<div>"
                , "  <label>"
                , "    <span>"
                , "      <input type='radio' value='Apple' checked=''>"
                , "    </span>"
                , "    <span>Apple</span>"
                , "  </label>"
                , "  <label>"
                , "    <span>"
                , "      <input type='radio' value='Pear'>"
                , "    </span>"
                , "    <span>Pear</span>"
                , "  </label>"
                , "  <label>"
                , "    <span>"
                , "      <input type='radio' value='Orange'>"
                , "    </span>"
                , "    <span>Orange</span>"
                , "  </label>"
                , "</div>"
                ]
            )
          snapshot <- snapshotForAI page
          containsYaml
            snapshot
            [ "- generic [ref=e2]:"
            , "- generic [ref=e3]:"
            , "- generic [ref=e4]:"
            , "- radio \"Apple\" [checked]"
            , "- text: Apple"
            , "- generic [ref=e5]:"
            , "- generic [ref=e6]:"
            , "- radio \"Pear\""
            , "- text: Pear"
            , "- generic [ref=e7]:"
            , "- generic [ref=e8]:"
            , "- radio \"Orange\""
            , "- text: Orange"
            ]
    , testCase "should collapse generic nodes" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<div>"
                , "  <div>"
                , "    <div>"
                , "      <button>Button</button>"
                , "    </div>"
                , "  </div>"
                , "</div>"
                ]
            )
          snapshot <- snapshotForAI page
          containsYaml snapshot ["- button \"Button\" [ref=e5]"]
    , testCase "should include cursor pointer hint" $
        withChromiumPage $ \page -> do
          setContent page "<button style='cursor: pointer'>Button</button>"
          snapshot <- snapshotForAI page
          containsYaml snapshot ["- button \"Button\" [ref=e2] [cursor=pointer]"]
    , testCase "should not nest cursor pointer hints" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<a style='cursor: pointer' href='about:blank'>"
                , "  Link with a button"
                , "  <button style='cursor: pointer'>Button</button>"
                , "</a>"
                ]
            )
          -- The link's name is redundant - "Link with a button" prints as text and "Button" as the button -
          -- so it is dropped even though the node is clickable.
          snapshot <- snapshotForAI page
          containsYaml
            snapshot
            [ "- link [ref=e2] [cursor=pointer]:"
            , "- /url: about:blank"
            , "- text: Link with a button"
            , "- button \"Button\" [ref=e3]"
            ]
    , testCase "should omit names that just repeat printed descendant nodes" $
        withChromiumPage $ \page -> do
          setContent page "<h3><a style='cursor: pointer' href='/issues/1'>Clipboard API</a></h3>"
          snapshot <- snapshotForAI page
          containsYaml
            snapshot
            [ "- heading [level=3] [ref=e2]:"
            , "- link \"Clipboard API\" [ref=e3] [cursor=pointer]:"
            , "- /url: /issues/1"
            ]
    , testCase "should omit redundant name when a contributing wrapper is collapsed" $
        withChromiumPage $ \page -> do
          -- The flex span contributes to the heading's name, but is then removed from the tree as a
          -- single-child generic wrapper. Its contribution is fully represented by the link, so the
          -- heading's name is still redundant.
          setContent
            page
            "<h3><span style='display: flex'><a style='cursor: pointer' href='/issues/1'>Clipboard API</a></span></h3>"
          snapshot <- snapshotForAI page
          containsYaml
            snapshot
            [ "- heading [level=3] [ref=e2]:"
            , "- link \"Clipboard API\" [ref=e4] [cursor=pointer]:"
            , "- /url: /issues/1"
            ]
    , testCase "should omit redundant name when a contributor is a skipped leaf generic" $
        withChromiumPage $ \page -> do
          -- The outer span is not collapsed (its child is an element, not text), so it becomes a leaf
          -- generic node that contributes to both names. The link's rendered name covers it, which in turn
          -- makes the heading's name redundant.
          setContent
            page
            "<h3><a style='cursor: pointer' href='/issues/1'><span><span>Clipboard API</span></span></a></h3>"
          snapshot <- snapshotForAI page
          containsYaml
            snapshot
            [ "- heading [level=3] [ref=e2]:"
            , "- link \"Clipboard API\" [ref=e3] [cursor=pointer]:"
            , "- /url: /issues/1"
            ]
    , testCase "should keep the name when the contributing wrapper collapses into repeating text" $
        -- Upstream annotation: https://github.com/microsoft/playwright/issues/41985
        withChromiumPage $ \page -> do
          -- The outer span is marked as represented while it still wraps the icon. Once the nameless
          -- image is dropped, the span collapses into a lone text repeating the button's name and removes
          -- itself, so the button must keep the name derived from it.
          setContent
            page
            ( textLines
                [ "<button>"
                , "  <span>"
                , "    <span>"
                , "      <svg focusable='false' tabindex='-1' aria-hidden='true' viewBox='0 0 448 512'>"
                , "        <path d='M416 208H272V64c0-17.67-14.33-32-32-32h-32c-17.67 0-32 14.33-32 32v144H32c-17.67 0-32 14.33-32 32v32c0 17.67 14.33 32 32 32h144v144c0 17.67 14.33 32 32 32h32c17.67 0 32-14.33 32-32V304h144c17.67 0 32-14.33 32-32v-32c0-17.67-14.33-32-32-32z'/>"
                , "      </svg>"
                , "    </span>"
                , "    <span>Add New Item</span>"
                , "  </span>"
                , "</button>"
                ]
            )
          snapshot <- snapshotForAI page
          containsYaml snapshot ["- button \"Add New Item\" [ref=e2]"]
    , testCase "should keep names not derived from printed nodes" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<h3 aria-label='Clipboard API issue'><a style='cursor: pointer' href='/issues/1'>Clipboard API</a></h3>"
          snapshot <- snapshotForAI page
          containsYaml
            snapshot
            [ "- heading \"Clipboard API issue\" [level=3] [ref=e2]:"
            , "- link \"Clipboard API\" [ref=e3] [cursor=pointer]:"
            , "- /url: /issues/1"
            ]
    , testCase "should omit images without an accessible name" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<img src='" <> gifDataUrl <> "'>"
                , "<img alt='A cat' src='" <> gifDataUrl <> "'>"
                , "<img style='cursor: pointer' src='" <> gifDataUrl <> "'>"
                ]
            )
          snapshot <- snapshotForAI page
          -- A nameless image that cannot be clicked carries no information and is omitted.
          containsYaml
            snapshot
            [ "- generic [active] [ref=e1]:"
            , "- img \"A cat\" [ref=e3]"
            , "- img [ref=e4] [cursor=pointer]"
            ]
          assertBool "omitted image takes no ref" (not ("[ref=e2]" `T.isInfixOf` snapshot))
    , testCase "should omit a nameless image nested inside a link" $
        withChromiumPage $ \page -> do
          -- The decorative image has no name, so it is dropped even though it sits inside a clickable link.
          setContent
            page
            ( "<a style='cursor: pointer' href='/issue/1'>Open issue <img src='" <> gifDataUrl <> "'></a>"
            )
          snapshot <- snapshotForAI page
          containsYaml
            snapshot
            [ "- link \"Open issue\" [ref=e2] [cursor=pointer]:"
            , "- /url: /issue/1"
            ]
          assertBool "nameless image is omitted" (not ("img" `T.isInfixOf` snapshot))
    , testCase "should keep icon-only clickable elements" $
        withChromiumPage $ \page -> do
          -- Upstream annotation: https://github.com/microsoft/playwright/issues/42013
          let icon :: Text
              icon = "<svg viewBox='0 0 1024 1024'><use xlink:href='#icon'></use></svg>"
          setContent
            page
            ( textLines
                [ "<div style='cursor: pointer' aria-haspopup='true'><i>" <> icon <> "</i></div>"
                , "<img style='cursor: pointer' src='" <> gifDataUrl <> "'>"
                , "<a style='cursor: pointer' href='/target'>" <> icon <> "</a>"
                , "<div onclick='void 0'>" <> icon <> "</div>"
                ]
            )
          snapshot <- snapshotForAI page
          containsYaml
            snapshot
            [ "- generic [active] [ref=e1]:"
            , "- generic [ref=e2] [cursor=pointer]"
            , "- img [ref=e5] [cursor=pointer]"
            , "- link [ref=e6] [cursor=pointer]:"
            , "- /url: /target"
            ]
    , testCase "should omit leaf generic whose text is already in an ancestor name" $
        withChromiumPage $ \page -> do
          -- The inner element is block so it survives as its own generic node (an inline single-text span
          -- would be collapsed into the link instead). It inherits the link's pointer cursor.
          setContent
            page
            "<a style='cursor: pointer' href='/issues/15860'><div>[Feature] a dedicated clipboard API</div></a>"
          snapshot <- snapshotForAI page
          -- The link keeps its name, and the inner leaf generic that the name was computed from is dropped
          -- because its text is already shown by the name.
          containsYaml
            snapshot
            [ "- link \"[Feature] a dedicated clipboard API\" [ref=e2] [cursor=pointer]:"
            , "- /url: /issues/15860"
            ]
          assertBool "inner leaf generic is dropped" (not ("[ref=e3]" `T.isInfixOf` snapshot))
    , testCase "should omit name-repeating generic behind a wrapper" $
        withChromiumPage $ \page -> do
          -- The leaf generic that repeats the link's name sits inside a nameless wrapper. Its text is
          -- first inlined into the wrapper, which then faces the link and removes itself.
          setContent
            page
            "<a style='cursor: pointer' href='/labels'><span style='display: inline-block'><span style='display: inline-block'><span>P3-collecting-feedback</span></span></span></a>"
          snapshot <- snapshotForAI page
          containsYaml
            snapshot
            [ "- link \"P3-collecting-feedback\" [ref=e2] [cursor=pointer]:"
            , "- /url: /labels"
            ]
          assertEqual "name appears once" 2 (length (T.splitOn "P3-collecting-feedback" snapshot))
    , testCase "should resolve refs of distilled-away nodes" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<a style='cursor: pointer' href='/issues/15860'><div>[Feature] a dedicated clipboard API</div></a>"
          snapshot <- snapshotForAI page
          -- The inner leaf generic is distilled away, but its ref still resolves to the element.
          assertBool "distilled ref is omitted from the snapshot" (not ("[ref=e3]" `T.isInfixOf` snapshot))
          expectText (locate page "aria-ref=e3") "[Feature] a dedicated clipboard API"
    , testCase "should not distill snapshots outside of ai mode" $
        withChromiumPage $ \page -> do
          setContent page "<h3><a href='/issues/1'>Clipboard API</a></h3>"
          -- The heading name would be dropped as redundant in ai mode; matching mode keeps it.
          -- Upstream uses toMatchAriaSnapshot; here the default snapshot is checked for the same lines.
          snapshot <- ariaSnapshot (locate page "body")
          containsYaml
            snapshot
            [ "- heading \"Clipboard API\" [level=3]:"
            , "- link \"Clipboard API\":"
            , "- /url: /issues/1"
            ]
    , testCase "should truncate data url in link" $
        withChromiumPage $ \page -> do
          setContent page ("<a href='data:text/html;base64," <> base64Paragraph <> "'>a link</a>")
          snapshot <- snapshotForAI page
          assertBool "data url is truncated" ("/url: data:text/html;base64,…" `T.isInfixOf` snapshot)
          assertBool "base64 payload is not printed" (not (base64Paragraph `T.isInfixOf` snapshot))
    , testCase "should auto-wait for navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/frame.html")) defaultGotoOptions
          snapshot <-
            withAsync (reload page) $ \reloadDone -> do
              snap <- snapshotForAI page
              _ <- wait reloadDone
              pure snap
          -- The snapshot races the reload, which may re-number the main frame, so accept any ref.
          assertBool
            ("snapshot shows the frame text: " <> T.unpack snapshot)
            ( any
                ( \line ->
                    "- generic [active] [ref=" `T.isPrefixOf` T.strip line
                      && "]: Hi, I'm frame" `T.isSuffixOf` T.strip line
                )
                (T.lines snapshot)
            )
    , testCase "should auto-wait for blocking CSS" $
        withChromiumPage $ \page -> withLocalApp blockingCssApp $ \prefix -> do
          setContentWith
            page
            ( textLines
                [ "<script src='" <> prefix <> "/css'></script>"
                , "<p>Hello World</p>"
                ]
            )
            defaultSetContentOptions {setContentOptionsWaitUntil = Just LoadStateCommit}
          snapshot <- snapshotForAI page
          assertBool "snapshot waits for the blocking css" ("Hello World" `T.isInfixOf` snapshot)
    , testCase "should show visible children of hidden elements" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<div style='visibility: hidden'>"
                , "  <div style='visibility: visible'>"
                , "    <button>Visible</button>"
                , "  </div>"
                , "  <div style='visibility: hidden'>"
                , "    <button style='visibility: visible'>Visible</button>"
                , "  </div>"
                , "  <div>"
                , "    <div style='visibility: visible'>"
                , "      <button style='visibility: hidden'>Hidden</button>"
                , "    </div>"
                , "    <button>Hidden</button>"
                , "  </div>"
                , "</div>"
                ]
            )
          snapshot <- snapshotForAI page
          assertEqual
            "visible children are kept"
            ( textLines
                [ "- generic [active] [ref=e1]:"
                , "  - button \"Visible\" [ref=e3]"
                , "  - button \"Visible\" [ref=e4]"
                ]
            )
            snapshot
    , testCase "should include active element information" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<button id='btn1'>Button 1</button>"
                , "<button id='btn2' autofocus>Button 2</button>"
                , "<div>Not focusable</div>"
                ]
            )
          -- Wait for autofocus to take effect
          _ <- waitForFunction page "() => document.activeElement?.id === 'btn2'"
          snapshot <- snapshotForAI page
          containsYaml
            snapshot
            [ "- generic [ref=e1]:"
            , "- button \"Button 1\" [ref=e2]"
            , "- button \"Button 2\" [active] [ref=e3]"
            , "- generic [ref=e4]: Not focusable"
            ]
    , testCase "should update active element on focus" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<input id='input1' placeholder='First input'>"
                , "<input id='input2' placeholder='Second input'>"
                ]
            )
          -- Initially there shouldn't be an active element on the inputs
          initialSnapshot <- snapshotForAI page
          containsYaml
            initialSnapshot
            [ "- generic [active] [ref=e1]:"
            , "- textbox \"First input\" [ref=e2]"
            , "- textbox \"Second input\" [ref=e3]"
            ]

          -- Focus the second input
          focus (locate page "#input2")

          -- After focus, the second input should be active
          afterFocusSnapshot <- snapshotForAI page
          containsYaml
            afterFocusSnapshot
            [ "- generic [ref=e1]:"
            , "- textbox \"First input\" [ref=e2]"
            , "- textbox \"Second input\" [active] [ref=e3]"
            ]
    , testCase "should mark iframe as active when it contains focused element" $
        withChromiumPage $ \page -> do
          -- Create a simple HTML file for the iframe
          setContent
            page
            ( textLines
                [ "<input id='regular-input' placeholder='Regular input'>"
                , "<iframe src=\"data:text/html,<input id='iframe-input' placeholder='Input in iframe'>\" tabindex='0'></iframe>"
                ]
            )
          -- Test 1: Focus the input inside the iframe
          focus (childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "#iframe-input"))
          inputInIframeFocusedSnapshot <- snapshotForAI page
          -- The iframe should be marked as active when it contains a focused element
          containsYaml
            inputInIframeFocusedSnapshot
            [ "- generic [ref=e1]:"
            , "- textbox \"Regular input\" [ref=e2]"
            , "- iframe [active] [ref=e3]:"
            , "- textbox \"Input in iframe\" [active] [ref=f1e2]"
            ]
    , testCase "return empty snapshot when iframe is not loaded" $
        -- Upstream annotation: https://github.com/microsoft/playwright/pull/36710
        withAssets $ \prefix page -> do
          setContent
            page
            ( textLines
                [ "<div style='height: 5000px;'>Test</div>"
                , "<iframe loading='lazy' src='" <> prefix <> "/frame.html'></iframe>"
                ]
            )
          -- Wait for the iframe to load
          _ <- waitForSelector page (selectorFromText "iframe") WaitForAttached
          -- Get the snapshot of the page
          snapshot <- snapshotForAIWithTimeout page 3000
          -- The iframe should be present but empty
          containsYaml
            snapshot
            [ "- generic [active] [ref=e1]:"
            , "- generic [ref=e2]: Test"
            , "- iframe [ref=e3]"
            ]
    , testCase "should support many properties on iframes" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<input id='regular-input' placeholder='Regular input'>"
                , "<iframe style='cursor: pointer' src=\"data:text/html,<input id='iframe-input' placeholder='Input in iframe'/>\" tabindex='0'></iframe>"
                ]
            )
          -- Test 1: Focus the input inside the iframe
          focus (childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "#iframe-input"))
          inputInIframeFocusedSnapshot <- snapshotForAI page
          containsYaml
            inputInIframeFocusedSnapshot
            [ "- generic [ref=e1]:"
            , "- textbox \"Regular input\" [ref=e2]"
            , "- iframe [active] [ref=e3] [cursor=pointer]:"
            , "- textbox \"Input in iframe\" [active] [ref=f1e2]"
            ]
    , testCase "should snapshot frameset pages" $
        -- Upstream annotation: https://github.com/microsoft/playwright/issues/41784
        withChromiumPage $ \page -> withLocalApp framesetRoutes $ \prefix -> do
          _ <- goto page (urlFromText (prefix <> "/frameset.html")) defaultGotoOptions
          snapshot <- snapshotForAIWithTimeout page 3000
          containsYaml
            snapshot
            [ "- generic [active] [ref=e1]:"
            , "- generic [ref=e2]:"
            , "- iframe [ref=e3]:"
            , "- button \"Button one\" [ref=f1e2]"
            , "- iframe [ref=e4]:"
            , "- button \"Button two\" [ref=f2e2]"
            , "- iframe [ref=e5]:"
            , "- button \"Button three\" [ref=f3e2]"
            ]
          expectText (locate page "aria-ref=f2e2") "Button two"
    , testCase "should snapshot a locator inside a frameset frame" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/frameset.html")) defaultGotoOptions
          fs <- frames page
          urls <- traverse (fmap urlToText . currentUrl) fs
          let matching = [f | (f, u) <- zip fs urls, "/frames/frame.html" `T.isSuffixOf` u]
          snapshot <-
            case matching of
              (frame : _) -> locatorSnapshotForAI (locatorOnFrame page frame (selectorFromText "body")) defaultAriaSnapshotOptions
              [] -> pure "no frame at /frames/frame.html"
          containsYaml snapshot ["- generic [ref=f1e1]: Hi, I'm frame"]
          expectText (locate page "aria-ref=f1e1") "Hi, I'm frame"
          -- The upstream normalize() check is not ported; see the module header.
    , testCase "should stitch iframes inside a frameset frame" $
        withChromiumPage $ \page -> withLocalApp framesetWithIframeRoutes $ \prefix -> do
          _ <- goto page (urlFromText (prefix <> "/frameset-with-iframe.html")) defaultGotoOptions
          snapshot <- snapshotForAI page
          containsYaml
            snapshot
            [ "- iframe [ref=e2]:"
            , "- generic [ref=f1e1]:"
            , "- button \"In frame\" [ref=f1e2]"
            , "- iframe [ref=f1e3]:"
            , "- button \"In iframe\" [ref=f2e2]"
            ]
          expectText (locate page "aria-ref=f1e2") "In frame"
          expectText (locate page "aria-ref=f2e2") "In iframe"
          -- The upstream normalize() check is not ported; see the module header.
    , testCase "should stitch nested frameset documents" $
        withChromiumPage $ \page -> withLocalApp nestedFramesetRoutes $ \prefix -> do
          _ <- goto page (urlFromText (prefix <> "/outer-frameset.html")) defaultGotoOptions
          snapshot <- snapshotForAI page
          containsYaml
            snapshot
            [ "- iframe [ref=e2]:"
            , "- iframe [ref=f1e3]:"
            , "- button \"Leaf button\" [ref=f2e2]"
            ]
          expectText (locate page "aria-ref=f2e2") "Leaf button"
    , testCase "should collapse inline generic nodes" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<ul>"
                , "  <li><b>3</b> <abbr>bds</abbr></li>"
                , "  <li><b>2</b> <abbr>ba</abbr></li>"
                , "  <li><b>1,200</b> <abbr>sqft</abbr></li>"
                , "</ul>"
                , "<ul>"
                , "  <li><div>3</div></li>"
                , "  <li><div>2</div></li>"
                , "  <li><div>1,200</div></li>"
                , "</ul>"
                ]
            )
          snapshot1 <- snapshotForAI page
          containsYaml
            snapshot1
            [ "- generic [active] [ref=e1]:"
            , "- list [ref=e2]:"
            , "- listitem [ref=e3]: 3 bds"
            , "- listitem [ref=e4]: 2 ba"
            , "- listitem [ref=e5]: 1,200 sqft"
            , "- list [ref=e6]:"
            , "- listitem [ref=e7]:"
            , "- generic [ref=e8]: \"3\""
            , "- listitem [ref=e9]:"
            , "- generic [ref=e10]: \"2\""
            , "- listitem [ref=e11]:"
            , "- generic [ref=e12]: 1,200"
            ]
    , testCase "should inline single leaf generic child into parent generic" $
        withChromiumPage $ \page -> do
          -- The nameless images are distilled away, so each wrapper is left with a single leaf generic
          -- child, whose text is inlined into the wrapper - recursively for the second one.
          setContent
            page
            ( textLines
                [ "<div><img src='" <> gifDataUrl <> "'><div>Status: Open.</div></div>"
                , "<div><img src='" <> gifDataUrl <> "'><div><img src='" <> gifDataUrl <> "'><div>Nested twice.</div></div></div>"
                ]
            )
          snapshot <- snapshotForAI page
          containsYaml
            snapshot
            [ "- generic [active] [ref=e1]:"
            , "- generic [ref=e2]: \"Status: Open.\""
            , "- generic [ref=e5]: Nested twice."
            ]
    , testCase "should inline a deeply nested generic" $
        withChromiumPage $ \page -> do
          -- Every wrapper contains a nameless image (distilled away) plus a single generic child, so the
          -- text bubbles up the whole chain - all the way into the body.
          let img = "<img src='" <> gifDataUrl <> "'>"
          setContent
            page
            ( "<div>" <> img <> "<div>" <> img <> "<div>" <> img <> "<div>" <> img <> "<div>Deeply nested.</div></div></div></div></div>"
            )
          snapshot <- snapshotForAI page
          containsYaml snapshot ["- generic [active] [ref=e1]: Deeply nested."]
          assertBool "nameless images are distilled away" (not ("img" `T.isInfixOf` snapshot))
    , testCase "should not remove generic nodes with title" $
        withChromiumPage $ \page -> do
          setContent page "<div title='Element title'>Element content</div>"
          snapshot <- snapshotForAI page
          containsYaml snapshot ["- generic \"Element title\" [ref=e2]: Element content"]
    , testCase "should limit depth" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<ul>"
                , "  <li>item1</li>"
                , "  <a href='about:blank' style='cursor:pointer'>link</a>"
                , "  <li>"
                , "    <ul id=target>"
                , "      <li>item2</li>"
                , "      <li>"
                , "        <ul>"
                , "          <li>item3</li>"
                , "        </ul>"
                , "      </li>"
                , "    </ul>"
                , "  </li>"
                , "</ul>"
                ]
            )
          snapshot1 <- snapshotForAIWith page defaultAriaSnapshotOptions {ariaSnapshotOptionsDepth = Just 1}
          containsYaml
            snapshot1
            [ "- list [ref=e2]:"
            , "- listitem [ref=e3]: item1"
            , "- link \"link\" [ref=e4] [cursor=pointer]:"
            , "- /url: about:blank"
            , "- listitem [ref=e5]"
            ]

          snapshot2 <- snapshotForAIWith page defaultAriaSnapshotOptions {ariaSnapshotOptionsDepth = Just 3}
          containsYaml
            snapshot2
            [ "- list [ref=e2]:"
            , "- listitem [ref=e3]: item1"
            , "- link \"link\" [ref=e4] [cursor=pointer]:"
            , "- /url: about:blank"
            , "- listitem [ref=e5]:"
            , "- list [ref=e6]:"
            , "- listitem [ref=e7]: item2"
            , "- listitem [ref=e8]"
            ]

          snapshot3 <- snapshotForAIWith page defaultAriaSnapshotOptions {ariaSnapshotOptionsDepth = Just 100}
          containsYaml
            snapshot3
            [ "- list [ref=e2]:"
            , "- listitem [ref=e3]: item1"
            , "- link \"link\" [ref=e4] [cursor=pointer]:"
            , "- /url: about:blank"
            , "- listitem [ref=e5]:"
            , "- list [ref=e6]:"
            , "- listitem [ref=e7]: item2"
            , "- listitem [ref=e8]:"
            , "- list [ref=e9]:"
            , "- listitem [ref=e10]: item3"
            ]

          snapshot4 <- locatorSnapshotForAI (locate page "#target") defaultAriaSnapshotOptions {ariaSnapshotOptionsDepth = Just 1}
          containsYaml
            snapshot4
            [ "- list [ref=e6]:"
            , "- listitem [ref=e7]: item2"
            , "- listitem [ref=e8]"
            ]
    , testCase "should annotate aria-hidden elements" $
        -- Upstream annotation: https://github.com/microsoft/playwright/issues/42223
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<h2>Visible heading</h2>"
                , "<div aria-hidden='true'>"
                , "  <h1>Hidden heading</h1>"
                , "  <p>Hidden content</p>"
                , "</div>"
                , "<h2>After hidden</h2>"
                ]
            )
          snapshot <- snapshotForAI page
          containsYaml
            snapshot
            [ "- generic [active] [ref=e1]:"
            , "- heading \"Visible heading\" [level=2] [ref=e2]"
            , "- generic [aria-hidden] [ref=e3]:"
            , "- heading [level=1] [ref=e4]: Hidden heading"
            , "- paragraph [ref=e5]: Hidden content"
            , "- heading \"After hidden\" [level=2] [ref=e6]"
            ]

          -- Default snapshot excludes aria-hidden elements entirely.
          defaultSnapshot <- ariaSnapshot (locate page "body")
          assertBool "default snapshot omits aria-hidden content" (not ("Hidden content" `T.isInfixOf` defaultSnapshot))
    , testCase "should only annotate the top element in a hidden subtree" $
        -- Upstream annotation: https://github.com/microsoft/playwright/issues/42223
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<div aria-hidden='true'>"
                , "  <h1>Heading</h1>"
                , "  <p>Paragraph</p>"
                , "</div>"
                ]
            )
          -- Only the element with aria-hidden="true" gets the annotation, not its descendants.
          snapshot <- snapshotForAI page
          containsYaml
            snapshot
            [ "- generic [aria-hidden] [ref=e2]:"
            , "- heading [level=1] [ref=e3]: Heading"
            , "- paragraph [ref=e4]: Paragraph"
            ]
    ]

-- | A 1x1 transparent GIF, used for the nameless-image cases.
gifDataUrl :: Text
gifDataUrl = "data:image/gif;base64,R0lGODlhAQABAAAAACwAAAAAAQABAAA="

-- | Base64 of @<p>hello</p>@, the payload of the data URL in the link case.
base64Paragraph :: Text
base64Paragraph = "PHA+aGVsbG88L3A+"

-- | Takes an AI-mode page snapshot with the given options.
snapshotForAIWith :: Page -> AriaSnapshotOptions -> Playwright Text
snapshotForAIWith page opts =
  pageAriaSnapshotWith page opts {ariaSnapshotOptionsMode = Just AriaSnapshotModeAi}

snapshotForAI :: Page -> Playwright Text
snapshotForAI page = snapshotForAIWith page defaultAriaSnapshotOptions

snapshotForAIWithTimeout :: Page -> Double -> Playwright Text
snapshotForAIWithTimeout page ms = do
  timeout <- requireTimeout ms
  snapshotForAIWith page defaultAriaSnapshotOptions {ariaSnapshotOptionsTimeout = Just timeout}

-- | AI-mode snapshot of a single locator.
locatorSnapshotForAI :: Locator -> AriaSnapshotOptions -> Playwright Text
locatorSnapshotForAI loc opts =
  ariaSnapshotWith loc opts {ariaSnapshotOptionsMode = Just AriaSnapshotModeAi}

-- | Approximates @toContainYaml@: every expected line must occur in the snapshot.
containsYaml :: Text -> [Text] -> Playwright ()
containsYaml snapshot expected =
  assertBool
    ("snapshot does not contain the expected lines:\n" <> T.unpack snapshot)
    (yamlContainsLines snapshot expected)

-- | Approximates @toHaveText@ on a locator that matches one element.
expectText :: Locator -> Text -> Playwright ()
expectText loc expected = do
  actual <- textContent loc
  assertEqual "text content" (Just expected) (fmap T.strip actual)

-- | Number of elements an @aria-ref@ locator resolves to. A stale or unknown
-- ref resolves to none.
staleRefCount :: Locator -> Playwright Int
staleRefCount = locatorCount

-- | Text of a ref'd element's @location.href@ through its owner document.
hrefOf :: Locator -> Playwright Text
hrefOf loc = do
  value <- evaluateOnLocator loc "(e) => e.ownerDocument.defaultView.location.href"
  pure (stringValue value)

stringValue :: SerializedValue -> Text
stringValue value =
  case value of
    SerializedNode (PayloadString s) _ -> s
    _ -> T.pack (show value)

-- | The ref printed for the first line that starts with @marker@, e.g. @e2@
-- for @button "One" [ref=@ in the snapshot.
refAfter :: Text -> Text -> Text
refAfter marker snapshot =
  T.takeWhile (/= ']') (T.drop (T.length marker) (snd (T.breakOn marker snapshot)))

-- | Serves each listed path as a text/html page and 404s otherwise.
routedApp :: [(Text, Text)] -> Application
routedApp routes request respond =
  case lookup ("/" <> T.intercalate "/" (pathInfo request)) routes of
    Just body ->
      respond (responseLBS status200 [(hContentType, "text/html")] (BSL.fromStrict (TE.encodeUtf8 body)))
    Nothing -> respond (responseLBS status404 [(hContentType, "text/plain")] "not found")

-- | Serves /css after a one second delay, so the page parser blocks on it.
blockingCssApp :: Application
blockingCssApp request respond =
  case pathInfo request of
    ["css"] -> do
      threadDelay 1000000
      respond (responseLBS status200 [(hContentType, "text/css")] "body { monospace }")
    _ -> respond (responseLBS status404 [(hContentType, "text/plain")] "not found")

framesetRoutes :: Application
framesetRoutes =
  routedApp
    [ ("/frameset.html", "<frameset rows=\"50%,50%\"><frameset cols=\"50%,50%\"><frame src=\"/frame-one.html\"><frame src=\"/frame-two.html\"></frameset><frame src=\"/frame-three.html\"></frameset>")
    , ("/frame-one.html", "<button>Button one</button>")
    , ("/frame-two.html", "<button>Button two</button>")
    , ("/frame-three.html", "<button>Button three</button>")
    ]

framesetWithIframeRoutes :: Application
framesetWithIframeRoutes =
  routedApp
    [ ("/frameset-with-iframe.html", "<frameset><frame src=\"/frame-with-iframe.html\"></frameset>")
    , ("/frame-with-iframe.html", "<button>In frame</button><iframe srcdoc=\"<button>In iframe</button>\"></iframe>")
    ]

nestedFramesetRoutes :: Application
nestedFramesetRoutes =
  routedApp
    [ ("/outer-frameset.html", "<frameset><frame src=\"/inner-frameset.html\"></frameset>")
    , ("/inner-frameset.html", "<frameset><frameset><frame src=\"/leaf.html\"></frameset></frameset>")
    , ("/leaf.html", "<button>Leaf button</button>")
    ]

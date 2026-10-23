-- | Ports of upstream tests/page/expect-to-have-text.spec.ts.
--
-- Failure cases assert that the matcher throws. The upstream assertions on
-- the rendered failure message (expected/received lines, diffs and call log)
-- are not ported, because the Haskell client does not decode the driver's
-- error details or formatted message into the exception.
module Playwright.ExpectToHaveTextSpec
  ( tests
  ) where

import Control.Monad
  ( void
  )
import Data.Text
  ( Text
  )
import qualified Data.Text as T
import Playwright
  ( ExpectOptions (expectIgnoreCase, expectIsNot, expectTimeout, expectUseInnerText)
  , ExpectedText (ExpectedRegex, ExpectedString)
  , Page
  , Playwright
  , defaultExpectOptions
  , evaluate
  , expectContainText
  , expectContainTextArray
  , expectHaveText
  , expectHaveTextArray
  , setContent
  )
import Playwright.Error
  ( PlaywrightError
  )
import Playwright.Harness
  ( assertBool
  , locate
  , withChromiumPage
  )
import Playwright.Timeout
  ( milliseconds
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( concurrently_
  )
import UnliftIO.Exception
  ( try
  )

tests :: TestTree
tests =
  testGroup
    "expect-to-have-text"
    [ testGroup
        "toHaveText with regex"
        [ testCase "pass" $
            withChromiumPage $ \page -> do
              setContent page "<div id=node>Text   content</div>"
              let node = locate page "#node"
              expectHaveText defaultExpectOptions node (rx "Text" "")
              -- Should not normalize whitespace.
              expectHaveText defaultExpectOptions node (rx "Text   content" "")
              -- Should respect ignoreCase.
              expectHaveText (caseOpts True) node (rx "text   content" "")
              -- Should override regex flag with ignoreCase.
              expectHaveText (notOpts (caseOpts False)) node (rx "text   content" "i")
        , testCase "fail" $
            withChromiumPage $ \page -> do
              setContent page "<div id=node>Text content</div>"
              assertFails $
                expectHaveText (timeoutOpts 1000) (locate page "#node") (rx "Text 2" "")
        ]
    , testGroup
        "toContainText with regex"
        [ testCase "pass" $
            withChromiumPage $ \page -> do
              setContent page "<div id=node>Text   content</div>"
              let node = locate page "#node"
              expectContainText defaultExpectOptions node (rx "ex" "")
              -- Should not normalize whitespace.
              expectContainText defaultExpectOptions node (rx "ext   cont" "")
        , testCase "fail" $
            withChromiumPage $ \page -> do
              setContent page "<div id=node>Text content</div>"
              assertFails $
                expectContainText (timeoutOpts 1000) (locate page "#node") (rx "ex2" "")
        ]
    , testGroup
        "toContainText with string"
        [ testCase "pass" $
            withChromiumPage $ \page -> do
              setContent page "<div id=node>Text   content</div>"
              expectContainText defaultExpectOptions (locate page "#node") (ExpectedString "content")
        , testCase "fail" $
            withChromiumPage $ \page -> do
              setContent page "<div id=node>Text content</div>"
              assertFails $
                expectContainText (timeoutOpts 1000) (locate page "#node") (ExpectedString "foo")
        ]
    , testGroup
        "toHaveText with text"
        [ testCase "pass" $
            withChromiumPage $ \page -> do
              setContent page "<div id=node><span></span>Text \ncontent&nbsp;    </div>"
              let node = locate page "#node"
              -- Should normalize whitespace.
              expectHaveText defaultExpectOptions node (ExpectedString "Text                        content")
              -- Should normalize zero width whitespace.
              expectHaveText defaultExpectOptions node (ExpectedString "T\x200b\&e\x200b\&x\x200b\&t content")
              -- Should support ignoreCase.
              expectHaveText (caseOpts True) node (ExpectedString "text CONTENT")
              -- Should support falsy ignoreCase.
              expectHaveText (notOpts (caseOpts False)) node (ExpectedString "TEXT")
              -- Should normalize soft hyphens.
              expectHaveText defaultExpectOptions node (ExpectedString "T\x00ad\&e\x00ad\&xt content")
        , testCase "pass contain" $
            withChromiumPage $ \page -> do
              setContent page "<div id=node>Text content</div>"
              let node = locate page "#node"
              expectContainText defaultExpectOptions node (ExpectedString "Text")
              -- Should normalize whitespace.
              expectContainText defaultExpectOptions node (ExpectedString "   ext        cont\n  ")
              -- Should support ignoreCase.
              expectContainText (caseOpts True) node (ExpectedString "EXT")
              -- Should support falsy ignoreCase.
              expectContainText (notOpts (caseOpts False)) node (ExpectedString "TEXT")
        , testCase "fail" $
            withChromiumPage $ \page -> do
              setContent page "<div id=node>Text content</div>"
              assertFails $
                expectHaveText (timeoutOpts 1000) (locate page "#node") (ExpectedString "Text")
        , testCase "pass eventually" $
            withChromiumPage $ \page -> do
              setContent page "<div id=node>Text content</div>"
              concurrently_
                (expectHaveText defaultExpectOptions (locate page "#node") (rx "Text 2" ""))
                ( do
                    delay page 1000
                    void
                      ( evaluate
                          page
                          "() => document.querySelector('#node').textContent = 'Text 2 content'"
                      )
                )
        , testCase "with userInnerText" $
            withChromiumPage $ \page -> do
              setContent page "<div id=node>Text <span hidden>garbage</span> content</div>"
              expectHaveText innerTextOpts (locate page "#node") (ExpectedString "Text content")
        , testCase "in shadow dom" $
            withChromiumPage $ \page -> do
              setContent
                page
                ( T.unlines
                    [ "<div></div>"
                    , "<script>"
                    , "  const div = document.querySelector('div');"
                    , "  const span = document.createElement('span');"
                    , "  span.textContent = 'some text';"
                    , "  div.attachShadow({ mode: 'open' }).appendChild(span);"
                    , "</script>"
                    ]
                )
              let spanLocator = locate page "span"
                  divLocator = locate page "div"
              expectHaveText defaultExpectOptions spanLocator (ExpectedString "some text")
              expectContainText defaultExpectOptions spanLocator (ExpectedString "text")
              expectHaveText defaultExpectOptions divLocator (ExpectedString "some text")
              expectContainText defaultExpectOptions divLocator (ExpectedString "text")
              expectHaveText innerTextOpts spanLocator (ExpectedString "some text")
              expectContainText innerTextOpts spanLocator (ExpectedString "text")
              -- Playwright intentionally does not perform innerText piercing on shadow dom.
              expectHaveText (notOpts innerTextOpts) divLocator (ExpectedString "some text")
              expectContainText (notOpts innerTextOpts) divLocator (ExpectedString "text")
        , testCase "fail with impossible timeout" $
            withChromiumPage $ \page -> do
              setContent page "<div id=node>Text content</div>"
              assertFails $
                expectHaveText (timeoutOpts 1) (locate page "#node") (ExpectedString "Text")
        , testCase "do not show \"element(s) not found\" when the real failure is a string mismatch" $
            withChromiumPage $ \page -> do
              -- Upstream skips this case when PW_CLOCK is frozen. The Haskell
              -- port has no clock mode, so it always runs.
              setContent
                page
                ( T.unlines
                    [ "<div>Initial</div>"
                    , "<script>"
                    , "  const field = document.querySelector('div');"
                    , "  setTimeout(() => {"
                    , "    field.id = 'field';"
                    , "    field.textContent = 'Final value';"
                    , "  }, 1000);"
                    , "</script>"
                    ]
                )
              assertFails $
                expectHaveText (timeoutOpts 3000) (locate page "#field") (ExpectedString "Something")
        ]
    , testGroup
        "not.toHaveText"
        [ testCase "pass" $
            withChromiumPage $ \page -> do
              setContent page "<div id=node>Text content</div>"
              let node = locate page "#node"
              expectHaveText (notOpts defaultExpectOptions) node (ExpectedString "Text2")
              -- Should be case-sensitive by default.
              expectHaveText (notOpts defaultExpectOptions) node (ExpectedString "TEXT")
        , testCase "fail" $
            withChromiumPage $ \page -> do
              setContent page "<div id=node>Text content</div>"
              assertFails $
                expectHaveText (notOpts (timeoutOpts 1000)) (locate page "#node") (ExpectedString "Text content")
        , testCase "should work when selector does not match" $
            withChromiumPage $ \page -> do
              setContent page "<div>hello</div>"
              assertFails $
                expectHaveText (notOpts (timeoutOpts 1000)) (locate page "span") (ExpectedString "hello")
        ]
    , testGroup
        "toHaveText with array"
        [ testCase "pass" $
            withChromiumPage $ \page -> do
              setContent page "<div>Text    \n1</div><div>Text   2a</div>"
              let divs = locate page "div"
              -- Should only normalize whitespace in the first item.
              expectHaveTextArray defaultExpectOptions divs [ExpectedString "Text  1", rx "Text   \\d+a" ""]
              -- Should support ignoreCase.
              expectHaveTextArray (caseOpts True) divs [ExpectedString "tEXT 1", ExpectedString "TExt 2A"]
        , testCase "pass lazy" $
            withChromiumPage $ \page -> do
              setContent page "<div id=div></div>"
              concurrently_
                (expectHaveTextArray defaultExpectOptions (locate page "p") [ExpectedString "Text 1", ExpectedString "Text 2"])
                ( do
                    delay page 500
                    void
                      ( evaluate
                          page
                          "() => document.querySelector('div').innerHTML = '<p>Text 1</p><p>Text 2</p>'"
                      )
                )
        , testCase "pass empty" $
            withChromiumPage $ \page -> do
              setContent page "<div></div>"
              expectHaveTextArray defaultExpectOptions (locate page "p") []
        , testCase "pass not empty" $
            withChromiumPage $ \page -> do
              setContent page "<div><p>Test</p></div>"
              expectHaveTextArray (notOpts defaultExpectOptions) (locate page "p") []
        , testCase "pass on empty" $
            withChromiumPage $ \page -> do
              setContent page "<div></div>"
              expectHaveTextArray (notOpts defaultExpectOptions) (locate page "p") [ExpectedString "Test"]
        , testCase "fail on not+empty" $
            withChromiumPage $ \page -> do
              setContent page "<div></div>"
              assertFails $
                expectHaveTextArray (notOpts (timeoutOpts 1000)) (locate page "p") []
        , testCase "pass eventually empty" $
            withChromiumPage $ \page -> do
              setContent page "<div id=div><p>Text</p></div>"
              concurrently_
                (expectHaveTextArray (notOpts defaultExpectOptions) (locate page "p") [])
                ( do
                    delay page 500
                    void (evaluate page "() => document.querySelector('div').innerHTML = ''")
                )
        , testCase "fail" $
            withChromiumPage $ \page -> do
              setContent page "<div>Text 1</div><div>Text 3</div>"
              assertFails $
                expectHaveTextArray
                  (timeoutOpts 1000)
                  (locate page "div")
                  [ExpectedString "Text 1", rx "Text \\d" "", ExpectedString "Extra"]
        , testCase "fail on repeating array matchers" $
            withChromiumPage $ \page -> do
              setContent page "<div>KekFoo</div>"
              assertFails $
                expectContainTextArray
                  (timeoutOpts 1000)
                  (locate page "div")
                  [ExpectedString "KekFoo", ExpectedString "KekFoo", ExpectedString "KekFoo"]
        ]
    , testGroup
        "toContainText with array"
        [ testCase "pass" $
            withChromiumPage $ \page -> do
              setContent page "<div>Text \n1</div><div>Text2</div><div>Text3</div>"
              let divs = locate page "div"
              expectContainTextArray defaultExpectOptions divs [ExpectedString "ext     1", rx "ext3" ""]
              -- Should support ignoreCase.
              expectContainTextArray (caseOpts True) divs [ExpectedString "EXT 1", ExpectedString "eXt3"]
        , testCase "fail" $
            withChromiumPage $ \page -> do
              setContent page "<div>Text 1</div><div>Text 3</div>"
              assertFails $
                expectContainTextArray (timeoutOpts 1000) (locate page "div") [ExpectedString "Text 2"]
        ]
    ]

rx :: Text -> Text -> ExpectedText
rx = ExpectedRegex

timeoutOpts :: Double -> ExpectOptions
timeoutOpts ms = defaultExpectOptions {expectTimeout = milliseconds ms}

notOpts :: ExpectOptions -> ExpectOptions
notOpts options = options {expectIsNot = True}

caseOpts :: Bool -> ExpectOptions
caseOpts value = defaultExpectOptions {expectIgnoreCase = Just value}

innerTextOpts :: ExpectOptions
innerTextOpts = defaultExpectOptions {expectUseInnerText = Just True}

-- | Run a matcher that is expected to throw. The matcher's own failure
-- message is not checked; see the module header.
assertFails :: Playwright () -> Playwright ()
assertFails action = do
  outcome <- try action :: Playwright (Either PlaywrightError ())
  case outcome of
    Left _ -> pure ()
    Right _ -> assertBool "expectation should have failed" False

-- | Pause inside the page without blocking the Playwright connection.
delay :: Page -> Int -> Playwright ()
delay page ms =
  void
    ( evaluate
        page
        ("() => new Promise(resolve => setTimeout(resolve, " <> T.pack (show ms) <> "))")
    )

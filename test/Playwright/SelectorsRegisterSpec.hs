module Playwright.SelectorsRegisterSpec
  ( tests
  ) where

import Control.Exception
  ( bracket
  , try
  )
import Data.Maybe
  ( isNothing
  )
import Data.Text
  ( Text
  , unpack
  )
import Data.Text.Encoding
  ( decodeUtf8
  )
import Playwright
  ( Browser
  , Page
  , Playwright
  , chromium
  , closeContext
  , defaultGotoOptions
  , defaultLaunchOptions
  , defaultRegisterSelectorOptions
  , evalOnSelectorAllBool
  , evalOnSelectorAllNumber
  , evalOnSelectorText
  , evaluate
  , evaluateText
  , goto
  , newContext
  , newPage
  , pageGetAttribute
  , pageInnerHtml
  , pageInnerText
  , pageIsVisible
  , pageTextContent
  , querySelector
  , registerSelector
  , registerSelectorContentScript
  , registerSelectorWith
  , selectorFromText
  , setContent
  , urlFromText
  , withBrowser
  , withPage
  , withPlaywright
  )
import Playwright.Error
  ( PlaywrightError (SelectorsRegister)
  )
import Playwright.Harness
  ( assertProtocolContains
  , readTestAsset
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , assertFailure
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "selectors-register"
    [ testCase "textContent should be atomic" $
        withPwPage $ \pw page -> do
          registerSelector pw "textContent" mutatingTextEngine
          _ <-
            goto
              page
              (urlFromText "data:text/html,<div>Hello</div>")
              defaultGotoOptions
          assertEqual "textContent" (Just "Hello")
            =<< pageTextContent page (selectorFromText "textContent=div")
          assertEqual "modified" "modified"
            =<< evaluateText page "() => document.querySelector('div').textContent"
    , testCase "innerText should be atomic" $
        withPwPage $ \pw page -> do
          registerSelector pw "innerText" mutatingTextEngine
          _ <-
            goto
              page
              (urlFromText "data:text/html,<div>Hello</div>")
              defaultGotoOptions
          assertEqual "innerText" "Hello"
            =<< pageInnerText page (selectorFromText "innerText=div")
          assertEqual "modified" "modified"
            =<< evaluateText page "() => document.querySelector('div').innerText"
    , testCase "innerHTML should be atomic" $
        withPwPage $ \pw page -> do
          registerSelector pw "innerHTML" mutatingTextEngine
          _ <-
            goto
              page
              (urlFromText "data:text/html,<div>Hello<span>world</span></div>")
              defaultGotoOptions
          assertEqual "innerHTML" "Hello<span>world</span>"
            =<< pageInnerHtml page (selectorFromText "innerHTML=div")
          assertEqual "modified" "modified"
            =<< evaluateText page "() => document.querySelector('div').innerHTML"
    , testCase "getAttribute should be atomic" $
        withPwPage $ \pw page -> do
          registerSelector pw "getAttribute" mutatingAttrEngine
          _ <-
            goto
              page
              (urlFromText "data:text/html,<div foo=hello></div>")
              defaultGotoOptions
          assertEqual "getAttribute" (Just "hello")
            =<< pageGetAttribute page (selectorFromText "getAttribute=div") "foo"
          assertEqual "modified" "modified"
            =<< evaluateText page "() => document.querySelector('div').getAttribute('foo')"
    , testCase "isVisible should be atomic" $
        withPwPage $ \pw page -> do
          registerSelector pw "isVisible" mutatingVisibleEngine
          _ <-
            goto
              page
              (urlFromText "data:text/html,<div>Hello</div>")
              defaultGotoOptions
          assertEqual "isVisible" True
            =<< pageIsVisible page (selectorFromText "isVisible=div")
          assertEqual "display" "none"
            =<< evaluateText page "() => document.querySelector('div').style.display"
    , testCase "should take java-style string" $
        withPwPage $ \pw page -> do
          registerSelector pw "objectLiteral" objectLiteralEngine
          _ <-
            goto
              page
              (urlFromText "data:text/html,<div>Hello</div>")
              defaultGotoOptions
          _ <- pageTextContent page (selectorFromText "objectLiteral=div")
          pure ()
    , testCase "should work" $
        withPwBrowser $ \pw browser -> do
          registerSelector pw "tag" tagSelectorEngine
          bracket (newContext browser) closeContext $ \ctx -> do
            registerSelector pw "tag2" tagSelectorEngine
            page <- newPage ctx
            setContent page "<div><span></span></div><div></div>"
            assertEqual "tag DIV" "DIV"
              =<< evalOnSelectorText page (selectorFromText "tag=DIV") "e => e.nodeName"
            assertEqual "tag SPAN" "SPAN"
              =<< evalOnSelectorText page (selectorFromText "tag=SPAN") "e => e.nodeName"
            assertEqual "tag DIVs" (2 :: Double)
              =<< evalOnSelectorAllNumber page (selectorFromText "tag=DIV") "es => es.length"
            assertEqual "tag2 DIV" "DIV"
              =<< evalOnSelectorText page (selectorFromText "tag2=DIV") "e => e.nodeName"
            assertEqual "tag2 SPAN" "SPAN"
              =<< evalOnSelectorText page (selectorFromText "tag2=SPAN") "e => e.nodeName"
            assertEqual "tag2 DIVs" (2 :: Double)
              =<< evalOnSelectorAllNumber page (selectorFromText "tag2=DIV") "es => es.length"
            assertProtocolContains "Unknown engine \"tAG\" while parsing selector tAG=DIV" $
              querySelector page (selectorFromText "tAG=DIV")
    , testCase "should work with path" $
        withPwBrowser $ \pw browser -> do
          source <- fmap decodeUtf8 (readTestAsset "sectionselectorengine.js")
          registerSelector pw "foo" source
          withPage browser $ \page -> do
            setContent page "<section></section>"
            assertEqual "section" "SECTION"
              =<< evalOnSelectorText page (selectorFromText "foo=whatever") "e => e.nodeName"
    , testCase "should work in main and isolated world" $
        withPwBrowser $ \pw browser -> do
          registerSelector pw "main" dummyWorldEngine
          registerSelectorWith
            pw
            "isolated"
            dummyWorldEngine
            defaultRegisterSelectorOptions
              { registerSelectorContentScript = Just True
              }
          withPage browser $ \page -> do
            setContent page "<div><span><section></section></span></div>"
            _ <- evaluate page "() => { window['__answer'] = document.querySelector('span'); }"
            assertEqual "main" "SPAN"
              =<< evalOnSelectorText page (selectorFromText "main=ignored") "e => e.nodeName"
            assertEqual "css main" "SPAN"
              =<< evalOnSelectorText page (selectorFromText "css=div >> main=ignored") "e => e.nodeName"
            assertEqual "main $$eval defined" True
              =<< evalOnSelectorAllBool page (selectorFromText "main=ignored") "es => window['__answer'] !== undefined"
            assertEqual "main $$eval length" (3 :: Double)
              =<< evalOnSelectorAllNumber page (selectorFromText "main=ignored") "es => es.filter(e => e).length"
            isolated <- querySelector page (selectorFromText "isolated=ignored")
            assertEqual "isolated $" True (isNothing isolated)
            isolatedCss <- querySelector page (selectorFromText "css=div >> isolated=ignored")
            assertEqual "isolated css $" True (isNothing isolatedCss)
            assertEqual "isolated $$eval defined" True
              =<< evalOnSelectorAllBool page (selectorFromText "isolated=ignored") "es => window['__answer'] !== undefined"
            assertEqual "isolated $$eval length" (3 :: Double)
              =<< evalOnSelectorAllNumber page (selectorFromText "isolated=ignored") "es => es.filter(e => e).length"
            assertEqual "main isolated" "SPAN"
              =<< evalOnSelectorText page (selectorFromText "main=ignored >> isolated=ignored") "e => e.nodeName"
            assertEqual "isolated main" "SPAN"
              =<< evalOnSelectorText page (selectorFromText "isolated=ignored >> main=ignored") "e => e.nodeName"
            assertEqual "main css" "SECTION"
              =<< evalOnSelectorText page (selectorFromText "main=ignored >> css=section") "e => e.nodeName"
    , testCase "should handle errors" $
        withPwPage $ \pw page -> do
          assertProtocolContains
            "Unknown engine \"neverregister\" while parsing selector neverregister=ignored"
            (querySelector page (selectorFromText "neverregister=ignored"))
          assertSelectorsRegister
            "selectors.register: Selector engine name may only contain [a-zA-Z0-9_] characters"
            (registerSelector pw "$" tagSelectorEngine)
          registerSelector pw "dummy" tagSelectorEngine
          registerSelector pw "duMMy" tagSelectorEngine
          assertSelectorsRegister
            "selectors.register: \"dummy\" selector engine has been already registered"
            (registerSelector pw "dummy" tagSelectorEngine)
          assertSelectorsRegister
            "selectors.register: \"css\" is a predefined selector engine"
            (registerSelector pw "css" tagSelectorEngine)
    , testCase "should throw already registered error when registering" $
        withPlaywright $ \pw -> do
          registerSelector pw "alreadyRegistered" tagSelectorEngine
          assertSelectorsRegister
            "selectors.register: \"alreadyRegistered\" selector engine has been already registered"
            (registerSelector pw "alreadyRegistered" tagSelectorEngine)
    , testCase "should not rely on engines working from the root" $
        withPwBrowser $ \pw browser -> do
          registerSelector pw "__value" valueEngine
          withPage browser $ \page -> do
            setContent page "<input id=input1 value=value1><input id=input2 value=value2>"
            assertEqual "id" "input2"
              =<< evalOnSelectorText page (selectorFromText "input >> __value=value2") "e => e.id"
    , testCase "should throw a nice error if the selector returns a bad value" $
        withPwBrowser $ \pw browser -> do
          registerSelector pw "__fake" fakeEngine
          withPage browser $ \page ->
            assertProtocolContains "Expected a Node but got [object Array]" $
              querySelector page (selectorFromText "__fake=value2")
    ]

withPwBrowser :: (Playwright -> Browser -> IO a) -> IO a
withPwBrowser action =
  withPlaywright $ \pw ->
    withBrowser (chromium pw) defaultLaunchOptions (action pw)

withPwPage :: (Playwright -> Page -> IO a) -> IO a
withPwPage action =
  withPwBrowser $ \pw browser ->
    withPage browser (action pw)

assertSelectorsRegister :: Text -> IO a -> IO ()
assertSelectorsRegister expected action = do
  result <- try action
  case result of
    Left (SelectorsRegister msg) ->
      assertEqual "selectors.register" expected msg
    Left err ->
      assertFailure ("expected SelectorsRegister, got: " <> show err)
    Right _ ->
      assertFailure ("expected SelectorsRegister: " <> unpack expected)

tagSelectorEngine :: Text
tagSelectorEngine =
  "(() => ({\n\
  \  query(root, selector) {\n\
  \    return root.querySelector(selector);\n\
  \  },\n\
  \  queryAll(root, selector) {\n\
  \    return Array.from(root.querySelectorAll(selector));\n\
  \  }\n\
  \}))()"

mutatingTextEngine :: Text
mutatingTextEngine =
  "(() => ({\n\
  \  query(root, selector) {\n\
  \    const result = root.querySelector(selector);\n\
  \    if (result)\n\
  \      void Promise.resolve().then(() => result.textContent = 'modified');\n\
  \    return result;\n\
  \  },\n\
  \  queryAll(root, selector) {\n\
  \    const result = Array.from(root.querySelectorAll(selector));\n\
  \    for (const e of result)\n\
  \      void Promise.resolve().then(() => e.textContent = 'modified');\n\
  \    return result;\n\
  \  }\n\
  \}))()"

mutatingAttrEngine :: Text
mutatingAttrEngine =
  "(() => ({\n\
  \  query(root, selector) {\n\
  \    const result = root.querySelector(selector);\n\
  \    if (result)\n\
  \      void Promise.resolve().then(() => result.setAttribute('foo', 'modified'));\n\
  \    return result;\n\
  \  },\n\
  \  queryAll(root, selector) {\n\
  \    const result = Array.from(root.querySelectorAll(selector));\n\
  \    for (const e of result)\n\
  \      void Promise.resolve().then(() => e.setAttribute('foo', 'modified'));\n\
  \    return result;\n\
  \  }\n\
  \}))()"

mutatingVisibleEngine :: Text
mutatingVisibleEngine =
  "(() => ({\n\
  \  query(root, selector) {\n\
  \    const result = root.querySelector(selector);\n\
  \    if (result)\n\
  \      void Promise.resolve().then(() => result.style.display = 'none');\n\
  \    return result;\n\
  \  },\n\
  \  queryAll(root, selector) {\n\
  \    const result = Array.from(root.querySelectorAll(selector));\n\
  \    for (const e of result)\n\
  \      void Promise.resolve().then(() => e.style.display = 'none');\n\
  \    return result;\n\
  \  }\n\
  \}))()"

objectLiteralEngine :: Text
objectLiteralEngine =
  "{\n\
  \  query(root, selector) {\n\
  \    return root.querySelector(selector);\n\
  \  },\n\
  \  queryAll(root, selector) {\n\
  \    return root.querySelectorAll(selector);\n\
  \  }\n\
  \}"

dummyWorldEngine :: Text
dummyWorldEngine =
  "(() => ({\n\
  \  query(root, selector) {\n\
  \    return window['__answer'];\n\
  \  },\n\
  \  queryAll(root, selector) {\n\
  \    return window['__answer'] ? [window['__answer'], document.body, document.documentElement] : [];\n\
  \  }\n\
  \}))()"

valueEngine :: Text
valueEngine =
  "(() => ({\n\
  \  query(root, selector) {\n\
  \    return root && root.value.includes(selector) ? root : undefined;\n\
  \  },\n\
  \  queryAll(root, selector) {\n\
  \    return root && root.value.includes(selector) ? [root] : [];\n\
  \  }\n\
  \}))()"

fakeEngine :: Text
fakeEngine =
  "(() => ({\n\
  \  query(root, selector) {\n\
  \    return [document.body];\n\
  \  },\n\
  \  queryAll(root, selector) {\n\
  \    return [[document.body]];\n\
  \  }\n\
  \}))()"

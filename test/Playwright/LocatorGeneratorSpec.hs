{-# LANGUAGE OverloadedStrings #-}

module Playwright.LocatorGeneratorSpec
  ( tests
  ) where

import Data.Text qualified as T
import Playwright.Harness
  ( assertEqual
  )
import Playwright.LocatorGenerator
  ( Language (..)
  , asLocator
  , asLocatorDescription
  , asLocators
  , locatorOrSelectorAsSelector
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

-- | Upstream parseLocator(lang, text, testIdAttr) maps to the parser entry point.
parseLocator :: Language -> T.Text -> T.Text -> T.Text
parseLocator = locatorOrSelectorAsSelector

-- Official cases that stay skipped (browser-only, or derived from live Locator
-- objects that the Haskell client does not expose):
-- * reverse engineer locators (upstream line 50): generate(page.*) reads `_selector`
--   from live Locator objects; the Haskell client has no equivalent handle.
-- * reverse engineer getByRole (193), ignore-case (267), ordered (288), regex (297),
--   hasText (327), hasNotText (350), visible (359), has (379), hasNot (400),
--   has + hasText (421), frameLocator (431), frameLocator without a selector (450),
--   generate multiple locators (464): each runs generate(page.*) over a live page.
-- * reverse engineer internal:has-text locators (552): needs window.playwright.selector
--   inside the page (generateForNode).
-- * should not oom in locator parser (689): needs a live page to call count() and read
--   the composite-locator error message from the browser-side engine.

tests :: TestTree
tests =
  testGroup
    "LocatorGenerator"
    [ testCase "refuses to translate internal:role with conflicting name/description exactness" $ do
        let conflicting = "internal:role=row[name=\"abc\"i][description=\"d\"s]"
            conflictingReversed = "internal:role=row[name=\"abc\"s][description=\"d\"i]"
        mapM_
          ( \lang -> do
              assertEqual "conflicting" conflicting (asLocator lang conflicting)
              assertEqual "conflicting reversed" conflictingReversed (asLocator lang conflictingReversed)
          )
          [JavaScript, Python, Java, CSharp]
    , testCase "asLocator internal:and" $ do
        assertEqual
          "javascript"
          "locator('div').and(locator('span').locator('article'))"
          (asLocator JavaScript "div >> internal:and=\"span >> article\"")
        assertEqual
          "python"
          "locator(\"div\").and_(locator(\"span\").locator(\"article\"))"
          (asLocator Python "div >> internal:and=\"span >> article\"")
        assertEqual
          "java"
          "locator(\"div\").and(locator(\"span\").locator(\"article\"))"
          (asLocator Java "div >> internal:and=\"span >> article\"")
        assertEqual
          "csharp"
          "Locator(\"div\").And(Locator(\"span\").Locator(\"article\"))"
          (asLocator CSharp "div >> internal:and=\"span >> article\"")
    , testCase "asLocator internal:or" $ do
        assertEqual
          "javascript"
          "locator('div').or(locator('span').locator('article'))"
          (asLocator JavaScript "div >> internal:or=\"span >> article\"")
        assertEqual
          "python"
          "locator(\"div\").or_(locator(\"span\").locator(\"article\"))"
          (asLocator Python "div >> internal:or=\"span >> article\"")
        assertEqual
          "java"
          "locator(\"div\").or(locator(\"span\").locator(\"article\"))"
          (asLocator Java "div >> internal:or=\"span >> article\"")
        assertEqual
          "csharp"
          "Locator(\"div\").Or(Locator(\"span\").Locator(\"article\"))"
          (asLocator CSharp "div >> internal:or=\"span >> article\"")
    , testCase "asLocator internal:chain" $ do
        assertEqual
          "javascript"
          "locator('div').locator(locator('span').locator('article'))"
          (asLocator JavaScript "div >> internal:chain=\"span >> article\"")
        assertEqual
          "python"
          "locator(\"div\").locator(locator(\"span\").locator(\"article\"))"
          (asLocator Python "div >> internal:chain=\"span >> article\"")
        assertEqual
          "java"
          "locator(\"div\").locator(locator(\"span\").locator(\"article\"))"
          (asLocator Java "div >> internal:chain=\"span >> article\"")
        assertEqual
          "csharp"
          "Locator(\"div\").Locator(Locator(\"span\").Locator(\"article\"))"
          (asLocator CSharp "div >> internal:chain=\"span >> article\"")
    , testCase "asLocator xpath" $ do
        let selector = "//*[contains(normalizer-text(), 'foo']"
        assertEqual
          "javascript"
          "locator('//*[contains(normalizer-text(), \\'foo\\']')"
          (asLocator JavaScript selector)
        assertEqual
          "python"
          "locator(\"//*[contains(normalizer-text(), 'foo']\")"
          (asLocator Python selector)
        assertEqual
          "java"
          "locator(\"//*[contains(normalizer-text(), 'foo']\")"
          (asLocator Java selector)
        assertEqual
          "csharp"
          "Locator(\"//*[contains(normalizer-text(), 'foo']\")"
          (asLocator CSharp selector)
        assertEqual
          "rt js 1"
          "//*[contains(normalizer-text(), 'foo']"
          (parseLocator JavaScript "locator('//*[contains(normalizer-text(), \\'foo\\']')" "data-testid")
        assertEqual
          "rt js 2"
          "//*[contains(normalizer-text(), 'foo']"
          (parseLocator JavaScript "locator(\"//*[contains(normalizer-text(), 'foo']\")" "data-testid")
        assertEqual
          "rt js 3"
          "xpath=//*[contains(normalizer-text(), 'foo']"
          (parseLocator JavaScript "locator('xpath=//*[contains(normalizer-text(), \\'foo\\']')" "data-testid")
        assertEqual
          "rt js 4"
          "xpath=//*[contains(normalizer-text(), 'foo']"
          (parseLocator JavaScript "locator(\"xpath=//*[contains(normalizer-text(), 'foo']\")" "data-testid")
        assertEqual
          "rt py 1"
          "//*[contains(normalizer-text(), 'foo']"
          (parseLocator Python "locator(\"//*[contains(normalizer-text(), 'foo']\")" "data-testid")
        assertEqual
          "rt py 2"
          "xpath=//*[contains(normalizer-text(), 'foo']"
          (parseLocator Python "locator(\"xpath=//*[contains(normalizer-text(), 'foo']\")" "data-testid")
        assertEqual
          "rt java 1"
          "//*[contains(normalizer-text(), 'foo']"
          (parseLocator Java "locator(\"//*[contains(normalizer-text(), 'foo']\")" "data-testid")
        assertEqual
          "rt java 2"
          "xpath=//*[contains(normalizer-text(), 'foo']"
          (parseLocator Java "locator(\"xpath=//*[contains(normalizer-text(), 'foo']\")" "data-testid")
        assertEqual
          "rt csharp 1"
          "//*[contains(normalizer-text(), 'foo']"
          (parseLocator CSharp "Locator(\"//*[contains(normalizer-text(), 'foo']\")" "data-testid")
        assertEqual
          "rt csharp 2"
          "xpath=//*[contains(normalizer-text(), 'foo']"
          (parseLocator CSharp "Locator(\"xpath=//*[contains(normalizer-text(), 'foo']\")" "data-testid")
    , testCase "parseLocator quotes" $ do
        assertEqual "js double-quoted dq" "text=\"bar\"" (parseLocator JavaScript "locator('text=\"bar\"')" "")
        assertEqual "js sq" "text='bar'" (parseLocator JavaScript "locator(\"text='bar'\")" "")
        assertEqual "js backtick" "text='bar'" (parseLocator JavaScript "locator(`text='bar'`)" "")
        assertEqual "py sq" "text='bar'" (parseLocator Python "locator(\"text='bar'\")" "")
        assertEqual "py mismatched" "" (parseLocator Python "locator('text=\"bar\"')" "")
        assertEqual "java sq" "text='bar'" (parseLocator Java "locator(\"text='bar'\")" "")
        assertEqual "java mismatched" "" (parseLocator Java "locator('text=\"bar\"')" "")
        assertEqual "csharp sq" "text='bar'" (parseLocator CSharp "Locator(\"text='bar'\")" "")
        assertEqual "csharp mismatched" "" (parseLocator CSharp "Locator('text=\"bar\"')" "")
        let mixedQuotes =
              "\n    locator(\"[id*=freetext-field]\")\n        .locator('input:below(:text(\"Assigned Number:\"))')\n        .locator(\"visible=true\")\n  "
        assertEqual
          "js multi-line mixed"
          "[id*=freetext-field] >> input:below(:text(\"Assigned Number:\")) >> visible=true"
          (parseLocator JavaScript mixedQuotes "")
    , testCase "parseLocator css" $ do
        assertEqual "js" ".foo" (parseLocator JavaScript "locator('.foo')" "")
        assertEqual "js css=" "css=.foo" (parseLocator JavaScript "locator('css=.foo')" "")
        assertEqual "py" ".foo" (parseLocator Python "locator(\".foo\")" "")
        assertEqual "py css=" "css=.foo" (parseLocator Python "locator(\"css=.foo\")" "")
        assertEqual "java" ".foo" (parseLocator Java "locator(\".foo\")" "")
        assertEqual "java css=" "css=.foo" (parseLocator Java "locator(\"css=.foo\")" "")
        assertEqual "csharp" ".foo" (parseLocator CSharp "Locator(\".foo\")" "")
        assertEqual "csharp css=" "css=.foo" (parseLocator CSharp "Locator(\"css=.foo\")" "")
    , testCase "parseLocator options" $ do
        assertEqual
          "empty options"
          "internal:role=heading"
          (parseLocator JavaScript "getByRole('heading', {})" "")
        assertEqual
          "checkbox options"
          "internal:role=checkbox[checked=false][include-hidden=true]"
          (parseLocator JavaScript "getByRole('checkbox', { checked:false, includeHidden: true })" "")
    , testCase "parse locators strictly" $ do
        let selector = "div >> internal:has-text=\"Goodbye world\"i >> span"
        -- Exact
        assertEqual
          "exact csharp"
          selector
          (parseLocator CSharp "Locator(\"div\").Filter(new() { HasText = \"Goodbye world\" }).Locator(\"span\")" "")
        assertEqual
          "exact java"
          selector
          (parseLocator Java "locator(\"div\").filter(new Locator.FilterOptions().setHasText(\"Goodbye world\")).locator(\"span\")" "")
        assertEqual
          "exact js"
          selector
          (parseLocator JavaScript "locator('div').filter({ hasText: 'Goodbye world' }).locator('span')" "")
        assertEqual
          "exact py"
          selector
          (parseLocator Python "locator(\"div\").filter(has_text=\"Goodbye world\").locator(\"span\")" "")
        -- Quotes
        assertEqual
          "quotes js"
          selector
          (parseLocator JavaScript "locator(\"div\").filter({ hasText: \"Goodbye world\" }).locator(\"span\")" "")
        assertNotEqual
          "quotes py mismatched"
          selector
          (parseLocator Python "locator('div').filter(has_text='Goodbye world').locator('span')" "")
        -- Whitespace
        assertEqual
          "whitespace csharp"
          selector
          (parseLocator CSharp "Locator(\"div\")  .  Filter (new ( ) {  HasText =    \"Goodbye world\" }).Locator(  \"span\"   )" "")
        assertEqual
          "whitespace java"
          selector
          (parseLocator Java "  locator(\"div\"  ).  filter(  new    Locator. FilterOptions    ( ) .setHasText(   \"Goodbye world\" ) ).locator(   \"span\")" "")
        assertEqual
          "whitespace js"
          selector
          (parseLocator JavaScript "locator\n('div')\n\n.filter({ hasText  : 'Goodbye world'\n }\n).locator('span')\n" "")
        assertEqual
          "whitespace py"
          selector
          (parseLocator Python "\tlocator(\t\"div\").filter(\thas_text=\"Goodbye world\"\t).locator\t(\"span\")" "")
        -- Extra symbols
        assertNotEqual
          "extra csharp"
          selector
          (parseLocator CSharp "Locator(\"div\").Filter(new() { HasText = \"Goodbye world\" }).Locator(\"span\"))" "")
        assertNotEqual
          "extra java"
          selector
          (parseLocator Java "locator(\"div\").filter(new Locator.FilterOptions().setHasText(\"Goodbye world\"))..locator(\"span\")" "")
        assertNotEqual
          "extra js"
          selector
          (parseLocator JavaScript "locator('div').filter({ hasText: 'Goodbye world' }}).locator('span')" "")
        assertNotEqual
          "extra py"
          selector
          (parseLocator Python "locator(\"div\").filter(has_text==\"Goodbye world\").locator(\"span\")" "")
    , testCase "parseLocator frames" $ do
        let frameText = "iframe >> internal:control=enter-frame >> internal:text=\"foo\"i"
        assertEqual "js contentFrame" frameText (parseLocator JavaScript "locator('iframe').contentFrame().getByText('foo')" "")
        assertEqual "js frameLocator" frameText (parseLocator JavaScript "frameLocator('iframe').getByText('foo')" "")
        assertEqual
          "js frameLocator css"
          "css=iframe >> internal:control=enter-frame >> internal:text=\"foo\"i"
          (parseLocator JavaScript "frameLocator('css=iframe').getByText('foo')" "")
        assertEqual
          "js getByTitle contentFrame"
          "internal:attr=[title=\"iframe title\"i] >> internal:control=enter-frame"
          (parseLocator JavaScript "getByTitle('iframe title').contentFrame()" "")
        assertEqual
          "asLocators frame"
          ["getByTitle('iframe title').contentFrame()"]
          (asLocators JavaScript "internal:attr=[title=\"iframe title\"i] >> internal:control=enter-frame")
        assertEqual "py content_frame" frameText (parseLocator Python "locator(\"iframe\").content_frame.get_by_text(\"foo\")" "")
        assertEqual "py frame_locator" frameText (parseLocator Python "frame_locator(\"iframe\").get_by_text(\"foo\")" "")
        assertEqual
          "py frame_locator css"
          "css=iframe >> internal:control=enter-frame >> internal:text=\"foo\"i"
          (parseLocator Python "frame_locator(\"css=iframe\").get_by_text(\"foo\")" "")
        assertEqual "csharp ContentFrame" frameText (parseLocator CSharp "Locator(\"iframe\").ContentFrame.GetByText(\"foo\")" "")
        assertEqual "csharp FrameLocator" frameText (parseLocator CSharp "FrameLocator(\"iframe\").GetByText(\"foo\")" "")
        assertEqual "java contentFrame" frameText (parseLocator Java "locator(\"iframe\").contentFrame().getByText(\"foo\")" "")
        assertEqual "java frameLocator" frameText (parseLocator Java "frameLocator(\"iframe\").getByText(\"foo\")" "")
    , testCase "asLocatorDescription invalid input" $ do
        assertEqual "describe string" "desc" (asLocatorDescription JavaScript "body >> internal:describe=\"desc\"")
        assertEqual "describe number" "locator('body')" (asLocatorDescription JavaScript "body >> internal:describe=12")
        assertEqual
          "xpath passthrough"
          "following-sibling::*[1]"
          (asLocatorDescription JavaScript "following-sibling::*[1]")
        assertEqual
          "describe then chain"
          "locator('body').locator('div')"
          (asLocatorDescription JavaScript "body >> internal:describe=\"desc\" >> div")
    ]

assertNotEqual :: String -> T.Text -> T.Text -> IO ()
assertNotEqual label unexpected actual =
  assertEqual
    (label <> ": expected a value different from " <> show unexpected)
    True
    (actual /= unexpected)

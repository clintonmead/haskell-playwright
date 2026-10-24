{-# LANGUAGE OverloadedStrings #-}

module Playwright.LocatorGeneratorSpec
  ( tests
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Text qualified as T
import Playwright
  ( Exactness (Exact, Inexact)
  , Locator
  , childFrameLocator
  , childLocator
  , contentFrame
  , filterHas
  , filterHasNot
  , filterHasNotText
  , filterHasText
  , filterHasTextRegex
  , filterVisible
  , first
  , frameLocator
  , getByLabel
  , getByText
  , getByTextRegex
  , getByPlaceholderRegex
  , lastLocator
  , locator
  , nth
  , selectorFromText
  , selectorToText
  , locatorSelector
  )
import Playwright.Harness
  ( assertEqual
  , withChromiumPage
  )
import Playwright.Locator qualified as L
import Playwright.LocatorGenerator
  ( Language (JavaScript, Python, Java, CSharp)
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

-- The reverse engineer cases build Locators with the public API and read the
-- selector through `locatorSelector`, then check asLocator for each language.
--
-- Official cases that stay skipped in this slice (outside the 11-case scope):
-- * reverse engineer locators (upstream line 50), reverse engineer getByRole (193)
--   and generate multiple locators (464): they are not part of this slice.
-- * reverse engineer internal:has-text locators (552): needs window.playwright.selector
--   inside the page (generateForNode).
-- * should not oom in locator parser (689): needs a live page to call count() and read
--   the composite-locator error message from the browser-side engine.

tests :: TestTree
tests =
  testGroup
    "LocatorGenerator"
    [ testCase "refuses to translate internal:role with conflicting name/description exactness" $ do
        let conflicting :: T.Text
            conflicting = "internal:role=row[name=\"abc\"i][description=\"d\"s]"
            conflictingReversed :: T.Text
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
        let selector :: T.Text
            selector = "//*[contains(normalizer-text(), 'foo']"
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
        let mixedQuotes :: T.Text
            mixedQuotes =
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
        let selector :: T.Text
            selector = "div >> internal:has-text=\"Goodbye world\"i >> span"
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
        let frameText :: T.Text
            frameText = "iframe >> internal:control=enter-frame >> internal:text=\"foo\"i"
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
    , testCase "reverse engineer ignore-case locators" $
        withChromiumPage $ \page -> do
          expectGenerated "getByText hello my" (selectorOf (getByText page "hello my\nwo\"rld" Inexact))
            ( "getByText('hello my\\nwo\"rld')"
            , "get_by_text(\"hello my\\nwo\\\"rld\")"
            , "getByText(\"hello my\\nwo\\\"rld\")"
            , "GetByText(\"hello my\\nwo\\\"rld\")"
            )
          expectGenerated "getByText spaces" (selectorOf (getByText page "hello       my     wo\"rld" Inexact))
            ( "getByText('hello       my     wo\"rld')"
            , "get_by_text(\"hello       my     wo\\\"rld\")"
            , "getByText(\"hello       my     wo\\\"rld\")"
            , "GetByText(\"hello       my     wo\\\"rld\")"
            )
          expectGenerated "getByLabel hello my" (selectorOf (getByLabel page "hello my\nwo\"rld" Inexact))
            ( "getByLabel('hello my\\nwo\"rld')"
            , "get_by_label(\"hello my\\nwo\\\"rld\")"
            , "getByLabel(\"hello my\\nwo\\\"rld\")"
            , "GetByLabel(\"hello my\\nwo\\\"rld\")"
            )
    , testCase "reverse engineer ordered locators" $
        withChromiumPage $ \page ->
          expectGenerated "nth first last" (selectorOf (lastLocator (first (nth (locator page (selectorFromText "div")) 3))))
            ( "locator('div').nth(3).first().last()"
            , "locator(\"div\").nth(3).first.last"
            , "locator(\"div\").nth(3).first().last()"
            , "Locator(\"div\").Nth(3).First.Last"
            )
    , testCase "reverse engineer locators with regex" $
        withChromiumPage $ \page -> do
          expectGenerated "getByText he/sl" (selectorOf (getByTextRegex page "/he\\/\\sl\\nlo/"))
            ( "getByText(/he\\/\\sl\\nlo/)"
            , "get_by_text(re.compile(r\"he/\\sl\\nlo\"))"
            , "getByText(Pattern.compile(\"he\\\\/\\\\sl\\\\nlo\"))"
            , "GetByText(new Regex(\"he\\\\/\\\\sl\\\\nlo\"))"
            )
          expectGenerated "getByPlaceholder he/sl" (selectorOf (getByPlaceholderRegex page "/he\\/\\sl\\nlo/"))
            ( "getByPlaceholder(/he\\/\\sl\\nlo/)"
            , "get_by_placeholder(re.compile(r\"he/\\sl\\nlo\"))"
            , "getByPlaceholder(Pattern.compile(\"he\\\\/\\\\sl\\\\nlo\"))"
            , "GetByPlaceholder(new Regex(\"he\\\\/\\\\sl\\\\nlo\"))"
            )
          expectGenerated "getByText hel-quote" (selectorOf (getByTextRegex page "/hel\"lo/"))
            ( "getByText(/hel\"lo/)"
            , "get_by_text(re.compile(r\"hel\\\"lo\"))"
            , "getByText(Pattern.compile(\"hel\\\"lo\"))"
            , "GetByText(new Regex(\"hel\\\"lo\"))"
            )
          expectGenerated "getByPlaceholder hel-quote" (selectorOf (getByPlaceholderRegex page "/hel\"lo/"))
            ( "getByPlaceholder(/hel\"lo/)"
            , "get_by_placeholder(re.compile(r\"hel\\\"lo\"))"
            , "getByPlaceholder(Pattern.compile(\"hel\\\"lo\"))"
            , "GetByPlaceholder(new Regex(\"hel\\\"lo\"))"
            )
    , testCase "reverse engineer hasText" $
        withChromiumPage $ \page -> do
          let base = getByText page "Hello" Inexact
          expectGenerated "hasText string" (selectorOf (filterHasText base "wo\"rld\n"))
            ( "getByText('Hello').filter({ hasText: 'wo\"rld\\n' })"
            , "get_by_text(\"Hello\").filter(has_text=\"wo\\\"rld\\n\")"
            , "getByText(\"Hello\").filter(new Locator.FilterOptions().setHasText(\"wo\\\"rld\\n\"))"
            , "GetByText(\"Hello\").Filter(new() { HasText = \"wo\\\"rld\\n\" })"
            )
          expectGenerated "hasText regex slash" (selectorOf (filterHasTextRegex base "/wo\\/\\srld\\n/"))
            ( "getByText('Hello').filter({ hasText: /wo\\/\\srld\\n/ })"
            , "get_by_text(\"Hello\").filter(has_text=re.compile(r\"wo/\\srld\\n\"))"
            , "getByText(\"Hello\").filter(new Locator.FilterOptions().setHasText(Pattern.compile(\"wo\\\\/\\\\srld\\\\n\")))"
            , "GetByText(\"Hello\").Filter(new() { HasTextRegex = new Regex(\"wo\\\\/\\\\srld\\\\n\") })"
            )
          expectGenerated "hasText regex quote" (selectorOf (filterHasTextRegex base "/wor\"ld/"))
            ( "getByText('Hello').filter({ hasText: /wor\"ld/ })"
            , "get_by_text(\"Hello\").filter(has_text=re.compile(r\"wor\\\"ld\"))"
            , "getByText(\"Hello\").filter(new Locator.FilterOptions().setHasText(Pattern.compile(\"wor\\\"ld\")))"
            , "GetByText(\"Hello\").Filter(new() { HasTextRegex = new Regex(\"wor\\\"ld\") })"
            )
    , testCase "reverse engineer hasNotText" $
        withChromiumPage $ \page ->
          expectGenerated "hasNotText string" (selectorOf (filterHasNotText (getByText page "Hello" Inexact) "wo\"rld\n"))
            ( "getByText('Hello').filter({ hasNotText: 'wo\"rld\\n' })"
            , "get_by_text(\"Hello\").filter(has_not_text=\"wo\\\"rld\\n\")"
            , "getByText(\"Hello\").filter(new Locator.FilterOptions().setHasNotText(\"wo\\\"rld\\n\"))"
            , "GetByText(\"Hello\").Filter(new() { HasNotText = \"wo\\\"rld\\n\" })"
            )
    , testCase "reverse engineer visible" $
        withChromiumPage $ \page -> do
          let base = getByText page "Hello" Inexact
              divSel = selectorFromText "div"
          expectGenerated "visible()" (selectorOf (childLocator (filterVisible base True) divSel))
            ( "getByText('Hello').visible().locator('div')"
            , "get_by_text(\"Hello\").visible.locator(\"div\")"
            , "getByText(\"Hello\").visible().locator(\"div\")"
            , "GetByText(\"Hello\").Visible.Locator(\"div\")"
            )
          expectGenerated "filter visible false" (selectorOf (childLocator (filterVisible base False) divSel))
            ( "getByText('Hello').filter({ visible: false }).locator('div')"
            , "get_by_text(\"Hello\").filter(visible=False).locator(\"div\")"
            , "getByText(\"Hello\").filter(new Locator.FilterOptions().setVisible(false)).locator(\"div\")"
            , "GetByText(\"Hello\").Filter(new() { Visible = false }).Locator(\"div\")"
            )
          let selector = selectorOf (filterVisible base True)
          assertEqual "parse js" selector (parseLocator JavaScript "getByText('Hello').filter({ visible: true })" "")
          assertEqual "parse java" selector (parseLocator Java "getByText(\"Hello\").filter(new Locator.FilterOptions().setVisible(true))" "")
          assertEqual "parse py" selector (parseLocator Python "get_by_text(\"Hello\").filter(visible=True)" "")
          assertEqual "parse csharp" selector (parseLocator CSharp "GetByText(\"Hello\").Filter(new() { Visible = true })" "")
    , testCase "reverse engineer has" $
        withChromiumPage $ \page -> do
          let base = getByText page "Hello" Inexact
              divLoc = locator page (selectorFromText "div")
          expectGenerated "has getByText" (selectorOf (filterHas base (L.getByText divLoc "bye" Inexact)))
            ( "getByText('Hello').filter({ has: locator('div').getByText('bye') })"
            , "get_by_text(\"Hello\").filter(has=locator(\"div\").get_by_text(\"bye\"))"
            , "getByText(\"Hello\").filter(new Locator.FilterOptions().setHas(locator(\"div\").getByText(\"bye\")))"
            , "GetByText(\"Hello\").Filter(new() { Has = Locator(\"div\").GetByText(\"bye\") })"
            )
          let inner = filterHas (locator page (selectorFromText "div")) (locator page (selectorFromText "span"))
              outer = filterHas (locator page (selectorFromText "section")) inner
              chained = filterHas (filterHasText outer "foo") (locator page (selectorFromText "a"))
          expectGenerated "has nested" (selectorOf chained)
            ( "locator('section').filter({ has: locator('div').filter({ has: locator('span') }) }).filter({ hasText: 'foo' }).filter({ has: locator('a') })"
            , "locator(\"section\").filter(has=locator(\"div\").filter(has=locator(\"span\"))).filter(has_text=\"foo\").filter(has=locator(\"a\"))"
            , "locator(\"section\").filter(new Locator.FilterOptions().setHas(locator(\"div\").filter(new Locator.FilterOptions().setHas(locator(\"span\"))))).filter(new Locator.FilterOptions().setHasText(\"foo\")).filter(new Locator.FilterOptions().setHas(locator(\"a\")))"
            , "Locator(\"section\").Filter(new() { Has = Locator(\"div\").Filter(new() { Has = Locator(\"span\") }) }).Filter(new() { HasText = \"foo\" }).Filter(new() { Has = Locator(\"a\") })"
            )
    , testCase "reverse engineer hasNot" $
        withChromiumPage $ \page -> do
          let base = getByText page "Hello" Inexact
              divLoc = locator page (selectorFromText "div")
          expectGenerated "hasNot getByText" (selectorOf (filterHasNot base (L.getByText divLoc "bye" Inexact)))
            ( "getByText('Hello').filter({ hasNot: locator('div').getByText('bye') })"
            , "get_by_text(\"Hello\").filter(has_not=locator(\"div\").get_by_text(\"bye\"))"
            , "getByText(\"Hello\").filter(new Locator.FilterOptions().setHasNot(locator(\"div\").getByText(\"bye\")))"
            , "GetByText(\"Hello\").Filter(new() { HasNot = Locator(\"div\").GetByText(\"bye\") })"
            )
          let inner = filterHasNot (locator page (selectorFromText "div")) (locator page (selectorFromText "span"))
              outer = filterHas (locator page (selectorFromText "section")) inner
              chained = filterHasNot (filterHasText outer "foo") (locator page (selectorFromText "a"))
          expectGenerated "hasNot nested" (selectorOf chained)
            ( "locator('section').filter({ has: locator('div').filter({ hasNot: locator('span') }) }).filter({ hasText: 'foo' }).filter({ hasNot: locator('a') })"
            , "locator(\"section\").filter(has=locator(\"div\").filter(has_not=locator(\"span\"))).filter(has_text=\"foo\").filter(has_not=locator(\"a\"))"
            , "locator(\"section\").filter(new Locator.FilterOptions().setHas(locator(\"div\").filter(new Locator.FilterOptions().setHasNot(locator(\"span\"))))).filter(new Locator.FilterOptions().setHasText(\"foo\")).filter(new Locator.FilterOptions().setHasNot(locator(\"a\")))"
            , "Locator(\"section\").Filter(new() { Has = Locator(\"div\").Filter(new() { HasNot = Locator(\"span\") }) }).Filter(new() { HasText = \"foo\" }).Filter(new() { HasNot = Locator(\"a\") })"
            )
    , testCase "reverse engineer has + hasText" $
        withChromiumPage $ \page -> do
          let sectionText = filterHasText (locator page (selectorFromText "section")) "foo"
              combined = childLocator (filterHas sectionText (locator page (selectorFromText "div"))) (selectorFromText "a")
          expectGenerated "has + hasText" (selectorOf combined)
            ( "locator('section').filter({ hasText: 'foo' }).filter({ has: locator('div') }).locator('a')"
            , "locator(\"section\").filter(has_text=\"foo\").filter(has=locator(\"div\")).locator(\"a\")"
            , "locator(\"section\").filter(new Locator.FilterOptions().setHasText(\"foo\")).filter(new Locator.FilterOptions().setHas(locator(\"div\"))).locator(\"a\")"
            , "Locator(\"section\").Filter(new() { HasText = \"foo\" }).Filter(new() { Has = Locator(\"div\") }).Locator(\"a\")"
            )
    , testCase "reverse engineer frameLocator" $
        withChromiumPage $ \page -> do
          let iframeLoc = frameLocator page (selectorFromText "iframe")
              foo = L.getByText iframeLoc "foo" Exact
              ownerFrame = childLocator foo (selectorFromText "frame")
              afterFirst = contentFrame (first ownerFrame)
              final = childLocator (childFrameLocator afterFirst (selectorFromText "iframe")) (selectorFromText "span")
          expectGenerated "frameLocator chain" (selectorOf final)
            ( "locator('iframe').contentFrame().getByText('foo', { exact: true }).locator('frame').first().contentFrame().locator('iframe').contentFrame().locator('span')"
            , "locator(\"iframe\").content_frame.get_by_text(\"foo\", exact=True).locator(\"frame\").first.content_frame.locator(\"iframe\").content_frame.locator(\"span\")"
            , "locator(\"iframe\").contentFrame().getByText(\"foo\", new FrameLocator.GetByTextOptions().setExact(true)).locator(\"frame\").first().contentFrame().locator(\"iframe\").contentFrame().locator(\"span\")"
            , "Locator(\"iframe\").ContentFrame.GetByText(\"foo\", new() { Exact = true }).Locator(\"frame\").First.ContentFrame.Locator(\"iframe\").ContentFrame.Locator(\"span\")"
            )
          -- Frame locators with ">>" in the owner selector are not restored back due to ambiguity.
          let divIframe = childLocator (frameLocator page (selectorFromText "div >> iframe")) (selectorFromText "span")
          assertEqual
            "frameLocator with >> owner"
            "locator('div').locator('iframe').contentFrame().locator('span')"
            (asLocator JavaScript (selectorOf divIframe))
    , testCase "reverse engineer frameLocator without a selector" $
        withChromiumPage $ \page -> do
          -- The Haskell client has no argument-less frameLocator, so the any-frame
          -- selector is built directly with `locator`, which is the same selector text.
          let anyFrame = locator page (selectorFromText "internal:control=any-frame")
              final = childLocator (L.getByText anyFrame "foo" Inexact) (selectorFromText "span")
          expectGenerated "frameLocator() chain" (selectorOf final)
            ( "frameLocator().getByText('foo').locator('span')"
            , "frame_locator().get_by_text(\"foo\").locator(\"span\")"
            , "frameLocator().getByText(\"foo\").locator(\"span\")"
            , "FrameLocator().GetByText(\"foo\").Locator(\"span\")"
            )
          assertEqual "any-frame js" "frameLocator()" (asLocator JavaScript "internal:control=any-frame")
          assertEqual "any-frame py" "frame_locator()" (asLocator Python "internal:control=any-frame")
          assertEqual "any-frame java" "frameLocator()" (asLocator Java "internal:control=any-frame")
          assertEqual "any-frame csharp" "FrameLocator()" (asLocator CSharp "internal:control=any-frame")
    ]

-- | The Haskell Locator's selector text, the input to asLocator (upstream `_selector`).
selectorOf :: Locator -> T.Text
selectorOf = selectorToText . locatorSelector

-- | Checks the generated code for each language:
-- (javascript, python, java, csharp), in that order.
expectGenerated :: MonadIO m => String -> T.Text -> (T.Text, T.Text, T.Text, T.Text) -> m ()
expectGenerated label sel (js, py, java, cs) = do
  assertEqual (label <> " javascript") js (asLocator JavaScript sel)
  assertEqual (label <> " python") py (asLocator Python sel)
  assertEqual (label <> " java") java (asLocator Java sel)
  assertEqual (label <> " csharp") cs (asLocator CSharp sel)

assertNotEqual :: String -> T.Text -> T.Text -> IO ()
assertNotEqual label unexpected actual =
  assertEqual
    (label <> ": expected a value different from " <> show unexpected)
    True
    (actual /= unexpected)

module Playwright.SlowMoSpec
  ( tests
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Text
  ( Text
  )
import GHC.Clock
  ( getMonotonicTime
  )
import Playwright
  ( DomEventType (DomEventType)
  , ElementHandle
  , Frame
  , Key (Key)
  , InputFile
  , LaunchOptions (launchOptionsSlowMo)
  , Page
  , Playwright
  , SelectOption (SelectValue)
  , check
  , click
  , dblclick
  , defaultGotoOptions
  , defaultLaunchOptions
  , defaultNewContextOptions
  , dispatchEvent
  , dispatchEventHandle
  , fill
  , focus
  , focusHandle
  , goto
  , gotoFrame
  , hover
  , hoverHandle
  , locatorOnFrame
  , press
  , pressHandle
  , querySelector
  , reload
  , selectOption
  , selectOptionHandle
  , selectorFromText
  , setContent
  , setContentOnFrame
  , setInputFiles
  , setInputFilesHandle
  , typeHandle
  , typeText
  , uncheck
  , uncheckHandle
  , urlFromText
  , checkHandle
  , clickHandle
  , dblclickHandle
  , fillHandle
  , locator
  )
import Playwright.Harness
  ( assertBool
  , attachFrame
  , requireHandle
  , requireTimeout
  , withAssetsLaunchWith
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
    "slowmo"
    [ pageTest "Page SlowMo check" (\page -> check (locator page (selectorFromText ".check")))
    , pageTest "Page SlowMo click" (\page -> click (locator page (selectorFromText "button")))
    , pageTest "Page SlowMo dblclick" (\page -> dblclick (locator page (selectorFromText "button")))
    , pageTest "Page SlowMo dispatchEvent" (\page -> dispatchEvent (locator page (selectorFromText "button")) (DomEventType "click"))
    , pageTest "Page SlowMo fill" (\page -> fill (locator page (selectorFromText ".fill")) "foo")
    , pageTest "Page SlowMo focus" (\page -> focus (locator page (selectorFromText "button")))
    , pageTest "Page SlowMo goto" (\page -> goto page (urlFromText "about:blank") defaultGotoOptions)
    , pageTest "Page SlowMo hover" (\page -> hover (locator page (selectorFromText "button")))
    , pageTest "Page SlowMo press" (\page -> press (locator page (selectorFromText "button")) (Key "Enter"))
    , pageTest "Page SlowMo reload" (\page -> reload page)
    , pageTest "Page SlowMo selectOption" (\page -> selectOption (locator page (selectorFromText "select")) [SelectValue "foo"])
    , pageTest "Page SlowMo setInputFiles" (\page -> setInputFiles (locator page (selectorFromText ".file")) noFiles)
    , pageTest "Page SlowMo type" (\page -> typeText (locator page (selectorFromText ".fill")) "a")
    , pageTest "Page SlowMo uncheck" (\page -> uncheck (locator page (selectorFromText ".uncheck")))
    , frameTest "Frame SlowMo check" (\page frame -> check (locatorOnFrame page frame (selectorFromText ".check")))
    , frameTest "Frame SlowMo click" (\page frame -> click (locatorOnFrame page frame (selectorFromText "button")))
    , frameTest "Frame SlowMo dblclick" (\page frame -> dblclick (locatorOnFrame page frame (selectorFromText "button")))
    , frameTest "Frame SlowMo dispatchEvent" (\page frame -> dispatchEvent (locatorOnFrame page frame (selectorFromText "button")) (DomEventType "click"))
    , frameTest "Frame SlowMo fill" (\page frame -> fill (locatorOnFrame page frame (selectorFromText ".fill")) "foo")
    , frameTest "Frame SlowMo focus" (\page frame -> focus (locatorOnFrame page frame (selectorFromText "button")))
    , frameTest "Frame SlowMo goto" (\_ frame -> gotoFrame frame (urlFromText "about:blank") defaultGotoOptions)
    , frameTest "Frame SlowMo hover" (\page frame -> hover (locatorOnFrame page frame (selectorFromText "button")))
    , frameTest "Frame SlowMo press" (\page frame -> press (locatorOnFrame page frame (selectorFromText "button")) (Key "Enter"))
    , frameTest "Frame SlowMo selectOption" (\page frame -> selectOption (locatorOnFrame page frame (selectorFromText "select")) [SelectValue "foo"])
    , frameTest "Frame SlowMo setInputFiles" (\page frame -> setInputFiles (locatorOnFrame page frame (selectorFromText ".file")) noFiles)
    , frameTest "Frame SlowMo type" (\page frame -> typeText (locatorOnFrame page frame (selectorFromText ".fill")) "a")
    , frameTest "Frame SlowMo uncheck" (\page frame -> uncheck (locatorOnFrame page frame (selectorFromText ".uncheck")))
    , elementTest "ElementHandle SlowMo check" ".check" checkHandle
    , elementTest "ElementHandle SlowMo click" "button" clickHandle
    , elementTest "ElementHandle SlowMo dblclick" "button" dblclickHandle
    , elementTest "ElementHandle SlowMo dispatchEvent" "button" (\element -> dispatchEventHandle element (DomEventType "click"))
    , elementTest "ElementHandle SlowMo fill" ".fill" (\element -> fillHandle element "foo")
    , elementTest "ElementHandle SlowMo focus" "button" focusHandle
    , elementTest "ElementHandle SlowMo hover" "button" hoverHandle
    , elementTest "ElementHandle SlowMo press" "button" (\element -> pressHandle element (Key "Enter"))
    , elementTest "ElementHandle SlowMo selectOption" "select" (\element -> selectOptionHandle element [SelectValue "foo"])
    , elementTest "ElementHandle SlowMo setInputFiles" ".file" (\element -> setInputFilesHandle element noFiles)
    , elementTest "ElementHandle SlowMo type" ".fill" (\element -> typeHandle element "a")
    , elementTest "ElementHandle SlowMo uncheck" ".uncheck" uncheckHandle
    ]

-- | The delay the browser is launched with. Each action must take at least this long.
slowMoMillis :: Double
slowMoMillis = 100

noFiles :: [InputFile]
noFiles = []

slowMoPage :: Text
slowMoPage =
  "<button>a</button>"
    <> "<input type=\"checkbox\" class=\"check\">"
    <> "<input type=\"checkbox\" checked=true class=\"uncheck\">"
    <> "<input class=\"fill\">"
    <> "<select><option>foo</option></select>"
    <> "<input type=\"file\" class=\"file\">"

-- | Launches Chromium with slowMo and runs the action with the test asset prefix, page.
withSlowMo :: (Text -> Page -> Playwright a) -> IO a
withSlowMo action = do
  slowMo <- requireTimeout slowMoMillis
  withAssetsLaunchWith
    defaultLaunchOptions {launchOptionsSlowMo = Just slowMo}
    defaultNewContextOptions
    (\prefix _ page -> action prefix page)

-- | Runs the task and asserts that it was delayed by at least the slowMo amount.
checkSlowMo :: Playwright a -> Playwright ()
checkSlowMo task = do
  start <- liftIO getMonotonicTime
  _ <- task
  end <- liftIO getMonotonicTime
  assertBool "slowMo delay was applied" ((end - start) * 1000 >= slowMoMillis)

pageTest :: String -> (Page -> Playwright a) -> TestTree
pageTest title task =
  testCase title $
    withSlowMo $ \_ page -> do
      setContent page slowMoPage
      checkSlowMo (task page)

frameTest :: String -> (Page -> Frame -> Playwright a) -> TestTree
frameTest title task =
  testCase title $
    withSlowMo $ \prefix page -> do
      frame <- attachFrame page "frame1" (prefix <> "/empty.html")
      setContentOnFrame frame slowMoPage
      checkSlowMo (task page frame)

elementTest :: String -> Text -> (ElementHandle -> Playwright a) -> TestTree
elementTest title selector task =
  testCase title $
    withSlowMo $ \_ page -> do
      setContent page slowMoPage
      element <- requireHandle =<< querySelector page (selectorFromText selector)
      checkSlowMo (task element)

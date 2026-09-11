module Playwright
  ( Browser (..)
  , BrowserContext (..)
  , BrowserName (..)
  , BrowserType (..)
  , CheckOptions (..)
  , Frame (..)
  , GotoOptions (..)
  , Key (..)
  , LaunchOptions (..)
  , Locator (..)
  , Page (..)
  , Playwright (..)
  , Response (..)
  , ScreenshotOptions (..)
  , Selector
  , Timeout
  , Url
  , browserFromRef
  , browserTypeFromRef
  , check
  , checkWith
  , childLocator
  , chromium
  , click
  , closeBrowser
  , closeContext
  , closePage
  , content
  , contextFromRef
  , dblclick
  , defaultCheckOptions
  , defaultGotoOptions
  , defaultLaunchOptions
  , defaultScreenshotOptions
  , defaultTimeout
  , evaluate
  , evaluateBool
  , evaluateOnLocator
  , evaluateOnLocatorBool
  , evaluateOnLocatorText
  , evaluateOnLocatorWith
  , evaluateText
  , fill
  , firefox
  , focus
  , getAttribute
  , goto
  , hover
  , initializePlaywright
  , innerHtml
  , innerText
  , inputValue
  , isChecked
  , isDisabled
  , isEditable
  , isEnabled
  , isHidden
  , isVisible
  , launch
  , locator
  , locatorCount
  , milliseconds
  , nestedSelector
  , newContext
  , newPage
  , pageFromRef
  , press
  , screenshot
  , selectorFromText
  , selectorToText
  , setChecked
  , setContent
  , textArgument
  , textContent
  , timeoutToDouble
  , title
  , typeText
  , uncheck
  , uncheckWith
  , urlFromText
  , urlToText
  , webkit
  , withBrowser
  , withPage
  , withPlaywright
  ) where

import Playwright.Browser
  ( Browser (Browser, browserChannel)
  , browserFromRef
  , closeBrowser
  , newContext
  , withPage
  )
import Playwright.BrowserContext
  ( BrowserContext (BrowserContext, browserContextChannel)
  , closeContext
  , contextFromRef
  , newPage
  )
import Playwright.BrowserName
  ( BrowserName (Chromium, Firefox, WebKit)
  )
import Playwright.BrowserType
  ( BrowserType (BrowserType, browserTypeChannel, browserTypeName)
  , browserTypeFromRef
  , launch
  , withBrowser
  )
import Playwright.CheckOptions
  ( CheckOptions (CheckOptions, checkOptionsForce, checkOptionsTimeout, checkOptionsTrial)
  , defaultCheckOptions
  )
import Playwright.Evaluate
  ( evaluate
  , evaluateBool
  , evaluateOnLocator
  , evaluateOnLocatorBool
  , evaluateOnLocatorText
  , evaluateOnLocatorWith
  , evaluateText
  )
import Playwright.Frame
  ( Frame (Frame, frameChannel, frameParent, frameUrl)
  )
import Playwright.GotoOptions
  ( GotoOptions (GotoOptions, gotoOptionsReferer, gotoOptionsTimeout, gotoOptionsWaitUntil)
  , defaultGotoOptions
  )
import Playwright.Instance
  ( Playwright (Playwright, playwrightChannel, playwrightChromium, playwrightFirefox, playwrightWebkit)
  , chromium
  , firefox
  , initializePlaywright
  , webkit
  , withPlaywright
  )
import Playwright.Key
  ( Key (Key, keyToText)
  )
import Playwright.LaunchOptions
  ( LaunchOptions (LaunchOptions, launchOptionsChromiumSandbox, launchOptionsHeadless, launchOptionsSlowMo, launchOptionsTimeout)
  , defaultLaunchOptions
  )
import Playwright.Locator
  ( Locator (Locator, locatorFrame, locatorSelector)
  , check
  , checkWith
  , childLocator
  , click
  , dblclick
  , fill
  , focus
  , getAttribute
  , hover
  , innerHtml
  , innerText
  , inputValue
  , isChecked
  , isDisabled
  , isEditable
  , isEnabled
  , isHidden
  , isVisible
  , locatorCount
  , press
  , setChecked
  , textContent
  , typeText
  , uncheck
  , uncheckWith
  )
import Playwright.Page
  ( Page (Page, pageChannel, pageMainFrame)
  , closePage
  , content
  , goto
  , locator
  , pageFromRef
  , screenshot
  , setContent
  , title
  )
import Playwright.Response
  ( Response (Response, responseChannel)
  )
import Playwright.ScreenshotOptions
  ( ScreenshotOptions (ScreenshotOptions, screenshotOptionsFullPage, screenshotOptionsTimeout)
  , defaultScreenshotOptions
  )
import Playwright.Selector
  ( Selector
  , nestedSelector
  , selectorFromText
  , selectorToText
  )
import Playwright.SerializedArgument
  ( textArgument
  )
import Playwright.Timeout
  ( Timeout
  , defaultTimeout
  , milliseconds
  , timeoutToDouble
  )
import Playwright.Url
  ( Url
  , urlFromText
  , urlToText
  )

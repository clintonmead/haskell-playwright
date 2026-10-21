module Playwright.UrlMatchSpec
  ( tests
  ) where

import Data.Text
  ( pack
  )
import Playwright
  ( UrlMatch (UrlMatchGlob)
  , urlFromText
  )
import Playwright.Harness
  ( assertEqual
  )
import Playwright.UrlMatch
  ( matchesUrlMatch
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

-- | Glob cases from the upstream interception spec, checked against the same
-- URLs. Each entry is (glob, URL, whether the glob should match it).
globCases :: [(String, String, Bool)]
globCases =
  [ ("**/*.js", "https://localhost:8080/foo.js", True)
  , ("**/*.css", "https://localhost:8080/foo.js", False)
  , ("*.js", "https://localhost:8080/foo.js", False)
  , ("https://**/*.js", "https://localhost:8080/foo.js", True)
  , ("http://localhost:8080/simple/path.js", "http://localhost:8080/simple/path.js", True)
  , ("**/{a,b}.js", "https://localhost:8080/a.js", True)
  , ("**/{a,b}.js", "https://localhost:8080/b.js", True)
  , ("**/{a,b}.js", "https://localhost:8080/c.js", False)
  , ("**/*.{png,jpg,jpeg}", "https://localhost:8080/c.jpg", True)
  , ("**/*.{png,jpg,jpeg}", "https://localhost:8080/c.jpeg", True)
  , ("**/*.{png,jpg,jpeg}", "https://localhost:8080/c.png", True)
  , ("**/*.{png,jpg,jpeg}", "https://localhost:8080/c.css", False)
  , ("foo*", "foo.js", True)
  , ("foo*", "foo/bar.js", False)
  , ("http://localhost:3000/signin-oidc*", "http://localhost:3000/signin-oidc/foo", False)
  , ("http://localhost:3000/signin-oidc*", "http://localhost:3000/signin-oidcnice", True)
  , ("**/*.js", "/foo.js", True)
  , ("asd/**.js", "/foo.js", False)
  , ("**/*.js", "bar_foo.js", False)
  , ("**/api/v[0-9]", "http://example.com/api/v[0-9]", True)
  , ("**/api/v[0-9]", "http://example.com/api/version", False)
  , ("**/api\\?param", "http://example.com/api?param", True)
  , ("**/api\\?param", "http://example.com/api-param", False)
  , ( "**/three-columns/settings.html\\?**id=settings-**"
    , "http://mydomain:8080/blah/blah/three-columns/settings.html?id=settings-e3c58efe-02e9-44b0-97ac-dd138100cf7c&blah"
    , True
    )
  , ("**/index?hello", "http://localhost/index1hello", False)
  , ("**/index?hello", "http://localhost/index?hello", True)
  , ("**/index\\?hello", "http://localhost/index?hello", True)
  , ("**/index\\?hello", "http://localhost/index1hello", False)
  ]

tests :: TestTree
tests =
  testGroup
    "url-match"
    [ testCase "globs match as the TypeScript client matches them" $
        mapM_ check globCases
    ]
  where
    check :: (String, String, Bool) -> IO ()
    check (glob, url, expected) =
      assertEqual
        (glob <> " against " <> url)
        expected
        (matchesUrlMatch (UrlMatchGlob (pack glob)) (urlFromText (pack url)))

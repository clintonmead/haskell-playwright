module Playwright.ProxyPatternSpec
  ( tests
  ) where

import Data.Text (Text)
import Playwright.ProxyPattern
  ( parseProxyPattern
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  , (@?=)
  )

tests :: TestTree
tests =
  testGroup
    "proxy-pattern"
    [ testCase "socks proxy patter matcher" testSocksProxyPatternMatcher
    ]

-- Each entry is (pattern, host, port, expected). Mismatches are gathered so a
-- single failure reports every wrong expectation, like upstream expect.soft.
type Expectation = (Text, Text, Int, Bool)

testSocksProxyPatternMatcher :: IO ()
testSocksProxyPatternMatcher = do
  let failures = [describe e | e <- expectations, not (check e)]
  failures @?= []
  where
    check :: Expectation -> Bool
    check (pattern, host, port, expected) = parseProxyPattern pattern host port == expected
    describe :: Expectation -> String
    describe (pattern, host, port, expected) =
      show (pattern, host, port) <> " expected " <> show expected

expectations :: [Expectation]
expectations =
  concat
    [ -- "*" matches everything, including IP literals and IPv6 brackets.
      star
    , loopback
    , loopbackPort
    , dotCom80
    , exactHost
    , wildcardCom
    , ipPort
    , mixedTokens
    , ipGlob
    , questionLiteral
    ]

star :: [Expectation]
star =
      [ ("*", "example.com", 80, True)
      , ("*", "some.long.example.com", 80, True)
      , ("*", "localhost", 3000, True)
      , ("*", "foo.localhost", 3000, True)
      , ("*", "127.0.0.1", 9222, True)
      , ("*", "123.123.123.123", 9222, True)
      , ("*", "[2001:db8:3333:4444:CCCC:DDDD:EEEE:FFFF]", 8080, True)
      , ("*", "[::1]", 5000, True)
      ]

loopback :: [Expectation]
loopback =
      [ ("<loopback>", "example.com", 80, False)
      , ("<loopback>", "some.long.example.com", 80, False)
      , ("<loopback>", "localhost", 3000, True)
      , ("<loopback>", "foo.localhost", 3000, True)
      , ("<loopback>", "127.0.0.1", 9222, True)
      , ("<loopback>", "123.123.123.123", 9222, False)
      , ("<loopback>", "[2001:db8:3333:4444:CCCC:DDDD:EEEE:FFFF]", 8080, False)
      , ("<loopback>", "[::1]", 5000, True)
      ]

loopbackPort :: [Expectation]
loopbackPort =
      [ ("<loopback>:3000", "example.com", 80, False)
      , ("<loopback>:3000", "some.long.example.com", 80, False)
      , ("<loopback>:3000", "localhost", 3000, True)
      , ("<loopback>:3000", "foo.localhost", 3000, True)
      , ("<loopback>:3000", "127.0.0.1", 9222, False)
      , ("<loopback>:3000", "123.123.123.123", 9222, False)
      , ("<loopback>:3000", "[2001:db8:3333:4444:CCCC:DDDD:EEEE:FFFF]", 8080, False)
      , ("<loopback>:3000", "[::1]", 5000, False)
      ]

dotCom80 :: [Expectation]
dotCom80 =
      [ (".com:80", "example.com", 80, True)
      , (".com:80", "some.long.example.com", 80, True)
      , (".com:80", "localhost", 3000, False)
      , (".com:80", "foo.localhost", 3000, False)
      , (".com:80", "127.0.0.1", 9222, False)
      , (".com:80", "123.123.123.123", 9222, False)
      , (".com:80", "[2001:db8:3333:4444:CCCC:DDDD:EEEE:FFFF]", 8080, False)
      , (".com:80", "[::1]", 5000, False)
      ]

exactHost :: [Expectation]
exactHost =
      [ ("example.com", "example.com", 80, True)
      , ("example.com", "some.long.example.com", 80, False)
      , ("example.com", "localhost", 3000, False)
      , ("example.com", "foo.localhost", 3000, False)
      , ("example.com", "127.0.0.1", 9222, False)
      , ("example.com", "123.123.123.123", 9222, False)
      , ("example.com", "[2001:db8:3333:4444:CCCC:DDDD:EEEE:FFFF]", 8080, False)
      , ("example.com", "[::1]", 5000, False)
      ]

wildcardCom :: [Expectation]
wildcardCom =
      [ ("*.com", "example.com", 80, True)
      , ("*.com", "some.long.example.com", 80, True)
      , ("*.com", "localhost", 3000, False)
      , ("*.com", "foo.localhost", 3000, False)
      , ("*.com", "127.0.0.1", 9222, False)
      , ("*.com", "123.123.123.123", 9222, False)
      , ("*.com", "[2001:db8:3333:4444:CCCC:DDDD:EEEE:FFFF]", 8080, False)
      , ("*.com", "[::1]", 5000, False)
      ]

ipPort :: [Expectation]
ipPort =
      [ ("123.123.123.123:9222", "example.com", 80, False)
      , ("123.123.123.123:9222", "some.long.example.com", 80, False)
      , ("123.123.123.123:9222", "localhost", 3000, False)
      , ("123.123.123.123:9222", "foo.localhost", 3000, False)
      , ("123.123.123.123:9222", "127.0.0.1", 9222, False)
      , ("123.123.123.123:9222", "123.123.123.123", 9222, True)
      , ("123.123.123.123:9222", "[2001:db8:3333:4444:CCCC:DDDD:EEEE:FFFF]", 8080, False)
      , ("123.123.123.123:9222", "[::1]", 5000, False)
      ]

mixedTokens :: [Expectation]
mixedTokens =
      [ ("example.com:80,localhost,*:9222", "example.com", 80, True)
      , ("example.com:80,localhost,*:9222", "some.long.example.com", 80, False)
      , ("example.com:80,localhost,*:9222", "localhost", 3000, True)
      , ("example.com:80,localhost,*:9222", "foo.localhost", 3000, False)
      , ("example.com:80,localhost,*:9222", "127.0.0.1", 9222, True)
      , ("example.com:80,localhost,*:9222", "123.123.123.123", 9222, True)
      , ("example.com:80,localhost,*:9222", "[2001:db8:3333:4444:CCCC:DDDD:EEEE:FFFF]", 8080, False)
      , ("example.com:80,localhost,*:9222", "[::1]", 5000, False)
      ]

ipGlob :: [Expectation]
ipGlob =
      [ ("127.*.*.1", "example.com", 80, False)
      , ("127.*.*.1", "some.long.example.com", 80, False)
      , ("127.*.*.1", "localhost", 3000, False)
      , ("127.*.*.1", "foo.localhost", 3000, False)
      , ("127.*.*.1", "127.0.0.1", 9222, False)
      , ("127.*.*.1", "123.123.123.123", 9222, False)
      , ("127.*.*.1", "[2001:db8:3333:4444:CCCC:DDDD:EEEE:FFFF]", 8080, False)
      , ("127.*.*.1", "[::1]", 5000, False)
      ]

questionLiteral :: [Expectation]
questionLiteral =
      [ ("foo?/bar.*.com", "foo?/bar.X.com", 80, True)
      , ("foo?/bar.*.com", "foo?/bar.Y.com", 80, True)
      , ("foo?/bar.*.com", "foo?/bar.com", 80, False)
      , ("foo?/bar.*.com", "fo/bar.X.com", 80, False)
      , ("foo?/bar.*.com", "fo?/bar.X.com", 80, False)
      ]

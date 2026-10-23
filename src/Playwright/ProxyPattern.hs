module Playwright.ProxyPattern
  ( parseProxyPattern
  ) where

import Data.List (unsnoc)
import Data.Text (Text)
import Data.Text qualified as T

-- | Compile a proxy bypass pattern (the syntax accepted by
-- 'Playwright.ProxySettings.proxySettingsBypass') into a predicate over a
-- host and port. An empty pattern matches nothing.
--
-- This follows the "Proxy bypass rules" syntax without implicit and negative
-- rules: @*@, @<loopback>@, @.suffix@, @*@-globs, IPv4/IPv6 literals, and an
-- optional @:port@ on each comma-separated token.
-- https://source.chromium.org/chromium/chromium/src/+/main:net/docs/proxy.md;l=331
parseProxyPattern :: Text -> Text -> Int -> Bool
parseProxyPattern pattern
  | T.null pattern = \_ _ -> False
  | otherwise =
      let matchers = map tokenMatcher (T.splitOn "," pattern)
       in \host port -> any (\matcher -> matcher host port) matchers

tokenMatcher :: Text -> Text -> Int -> Bool
tokenMatcher token
  | host == "<loopback>" = \h p -> portOk p && isLoopback h
  | host == "*" = \_ p -> portOk p
  | isIPv4 host || isIPv6 host = \h p -> h == host && portOk p
  | otherwise =
      let pat = if T.isPrefixOf "." host then "*" <> host else host
       in \h p -> portOk p && not (isIPAddress h) && globMatch pat h
  where
    (host, mport) = splitPort token
    portOk p = maybe True (== p) mport

-- Split a trailing ":digits" port off a token. The host keeps any other colons,
-- so IPv6 literals such as "[::1]" are left intact.
splitPort :: Text -> (Text, Maybe Int)
splitPort token =
  let (revDigits, revRest) = T.span isDigitChar (T.reverse token)
      digits = T.reverse revDigits
   in case T.uncons revRest of
        Just (':', hostRev) | not (T.null digits) -> (T.reverse hostRev, Just (read (T.unpack digits)))
        _ -> (token, Nothing)
  where
    isDigitChar c = c >= '0' && c <= '9'

isLoopback :: Text -> Bool
isLoopback h =
  h == "localhost"
    || T.isSuffixOf ".localhost" h
    || h == "127.0.0.1"
    || h == "[::1]"

isIPAddress :: Text -> Bool
isIPAddress h = isIPv4 h || isIPv6 h

isIPv4 :: Text -> Bool
isIPv4 h =
  case T.splitOn "." h of
    [a, b, c, d] -> all isDecimal [a, b, c, d]
    _ -> False
  where
    isDecimal part = not (T.null part) && T.all (\ch -> ch >= '0' && ch <= '9') part

isIPv6 :: Text -> Bool
isIPv6 h =
  T.any (== ':') h && T.all isIPv6Char h
  where
    isIPv6Char ch =
      ch == ':' || ch == '.' || (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')

-- Glob matching where only @*@ is special, anchored at both ends.
globMatch :: Text -> Text -> Bool
globMatch pat s =
  case T.splitOn "*" pat of
    [p] -> s == p
    p : rest -> case unsnoc rest of
      Nothing -> False
      Just (mids, suffix) -> case T.stripPrefix p s >>= T.stripSuffix suffix of
        Nothing -> False
        Just inner -> matchMiddles mids inner
    [] -> False

-- Each middle literal must occur, in order, in the remaining text. Leftmost
-- greedy placement is sufficient for patterns that only contain @*@.
matchMiddles :: [Text] -> Text -> Bool
matchMiddles [] _ = True
matchMiddles (m : ms) s
  | T.null m = matchMiddles ms s
  | otherwise =
      let (_, after) = T.breakOn m s
       in if m `T.isPrefixOf` after
            then matchMiddles ms (T.drop (T.length m) after)
            else False

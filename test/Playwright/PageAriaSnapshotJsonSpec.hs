-- Ports upstream tests/page/page-aria-snapshot-json.spec.ts.
--
-- Expected trees are decoded from JSON text and compared as aeson 'Value's,
-- which matches upstream's `toEqual` on the parsed structure.
module Playwright.PageAriaSnapshotJsonSpec
  ( tests
  ) where

import Data.Aeson
  ( Result (Success)
  , Value (Array, Object, String)
  , eitherDecodeStrict
  , fromJSON
  )
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Char qualified as C
import Data.Foldable
  ( toList
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Playwright
  ( AriaSnapshotMode (AriaSnapshotModeAi)
  , AriaSnapshotOptions (ariaSnapshotOptionsBoxes, ariaSnapshotOptionsDepth, ariaSnapshotOptionsMode)
  , Playwright
  , ariaSnapshotJSON
  , Page
  , defaultAriaSnapshotOptions
  , pageAriaSnapshotJSONWith
  , setContent
  , textContent
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , locate
  , withChromiumPage
  )
import Playwright.AriaSnapshotSupport
  ( textLines
  )
import Playwright.Json.OpenJson
  ( OpenJson (OpenJson)
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
    "page-aria-snapshot-json"
    [ testCase "should snapshot roles, names and text" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<h1>title</h1>"
                , "<ul aria-label='my list'>"
                , "  <li>one</li>"
                , "  <li>two</li>"
                , "</ul>"
                ]
            )
          checkJson
            page
            defaultAriaSnapshotOptions
            [ "[{\"role\":\"heading\",\"name\":\"title\",\"level\":1},"
            , "{\"role\":\"list\",\"name\":\"my list\",\"children\":["
            , "{\"role\":\"listitem\",\"text\":\"one\"},"
            , "{\"role\":\"listitem\",\"text\":\"two\"}]}]"
            ]
    , testCase "should snapshot flags as properties" $
        withChromiumPage $ \page -> do
          setContent
            page
            (textLines ["<input type='checkbox' title='Check' checked>", "<button disabled>Click</button>"])
          checkJson
            page
            defaultAriaSnapshotOptions
            [ "[{\"role\":\"checkbox\",\"name\":\"Check\",\"checked\":true},"
            , "{\"role\":\"button\",\"name\":\"Click\",\"disabled\":true}]"
            ]
    , testCase "should snapshot link url and textbox value" $
        withChromiumPage $ \page -> do
          setContent
            page
            (textLines ["<a href='https://example.com/'>Link</a>", "<input title='Input' value='hello'>"])
          checkJson
            page
            defaultAriaSnapshotOptions
            [ "[{\"role\":\"link\",\"name\":\"Link\",\"url\":\"https://example.com/\"},"
            , "{\"role\":\"textbox\",\"name\":\"Input\",\"text\":\"hello\"}]"
            ]
    , testCase "should snapshot text fragments in children" $
        withChromiumPage $ \page -> do
          setContent page "<p>Hello <a href='/link'>world</a> again</p>"
          checkJson
            page
            defaultAriaSnapshotOptions
            [ "[{\"role\":\"paragraph\",\"children\":["
            , "\"Hello\","
            , "{\"role\":\"link\",\"name\":\"world\",\"url\":\"/link\"},"
            , "\"again\"]}]"
            ]
    , testCase "should snapshot top-level text fragments as text nodes" $
        withChromiumPage $ \page -> do
          setContent page "Hello <button>One</button> again"
          checkJson
            page
            defaultAriaSnapshotOptions
            [ "[{\"role\":\"text\",\"text\":\"Hello\"},"
            , "{\"role\":\"button\",\"name\":\"One\"},"
            , "{\"role\":\"text\",\"text\":\"again\"}]"
            ]
    , testCase "should generate refs in ai mode" $
        withChromiumPage $ \page -> do
          setContent page (textLines ["<button>One</button>", "<button>Two</button>"])
          checkJson
            page
            aiOptions
            [ "[{\"role\":\"generic\",\"active\":true,\"ref\":\"e1\",\"children\":["
            , "{\"role\":\"button\",\"name\":\"One\",\"ref\":\"e2\"},"
            , "{\"role\":\"button\",\"name\":\"Two\",\"ref\":\"e3\"}]}]"
            ]
          -- Upstream: expect(page.locator('aria-ref=e2')).toHaveText('One').
          text <- textContent (locate page "aria-ref=e2")
          assertEqual "aria-ref=e2 text" (Just "One") text
    , testCase "should mark clickable elements with cursor in ai mode" $
        withChromiumPage $ \page -> do
          setContent page "<button style='cursor: pointer'>One</button>"
          json <- pageJson page aiOptions
          case findNode (roleIs "button") (nodesOf json) of
            Nothing -> assertFailure "no button node"
            Just button ->
              assertEqual "cursor" (Just (String "pointer")) (field "cursor" button)
    , testCase "should snapshot iframes in ai mode" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<h1>Hello</h1>"
                , "<iframe srcdoc=\"<button>In frame</button>\"></iframe>"
                ]
            )
          json <- pageJson page aiOptions
          iframe <- requireNode "iframe" (findNode (roleIs "iframe") (nodesOf json))
          assertBool "iframe has ref" (maybe False isNonEmptyString (field "ref" iframe))
          button <- requireNode "button" (findNode (roleIs "button") (childrenOf iframe))
          assertEqual "button name" (Just (String "In frame")) (field "name" button)
          case field "ref" button of
            Just (String ref) -> assertBool ("frame ref shape: " <> T.unpack ref) (isFrameRef ref)
            _ -> assertFailure "button has no string ref"
    , testCase "should limit depth" $
        withChromiumPage $ \page -> do
          setContent page "<ul><li><button>One</button></li></ul>"
          checkJson
            page
            defaultAriaSnapshotOptions {ariaSnapshotOptionsDepth = Just 1}
            [ "[{\"role\":\"list\",\"children\":["
            , "{\"role\":\"listitem\"}]}]"
            ]
    , testCase "should include boxes when requested" $
        withChromiumPage $ \page -> do
          setContent page "<button>One</button>"
          json <- pageJson page defaultAriaSnapshotOptions {ariaSnapshotOptionsBoxes = Just True}
          button <- requireNode "button" (findNode (roleIs "button") (nodesOf json))
          box <- case field "box" button of
            Just (Object b) -> pure b
            _ -> assertFailure "button has no box object"
          let numberAt key = KM.lookup key box >>= numberValue
          assertBool "box has x" (maybe False (const True) (numberAt "x"))
          assertBool "box has y" (maybe False (const True) (numberAt "y"))
          assertBool "box width > 0" (maybe False (> 0) (numberAt "width"))
          assertBool "box height > 0" (maybe False (> 0) (numberAt "height"))
    , testCase "should snapshot a locator" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<h1>title</h1>"
                , "<ul>"
                , "  <li>one</li>"
                , "  <li>two</li>"
                , "</ul>"
                ]
            )
          json <- openJsonValue' =<< ariaSnapshotJSON (locate page "ul")
          assertEqual
            "locator json"
            (decodeJson "[{\"role\":\"list\",\"children\":[{\"role\":\"listitem\",\"text\":\"one\"},{\"role\":\"listitem\",\"text\":\"two\"}]}]")
            json
    ]

aiOptions :: AriaSnapshotOptions
aiOptions = defaultAriaSnapshotOptions {ariaSnapshotOptionsMode = Just AriaSnapshotModeAi}

-- | Fetches the page's JSON snapshot and compares it with the expected JSON text.
checkJson :: Page -> AriaSnapshotOptions -> [Text] -> Playwright ()
checkJson page opts expected = do
  actual <- pageJson page opts
  assertEqual "aria snapshot json" (decodeJson (T.concat expected)) actual

pageJson :: Page -> AriaSnapshotOptions -> Playwright Value
pageJson page opts = openJsonValue' =<< pageAriaSnapshotJSONWith page opts

openJsonValue' :: OpenJson -> Playwright Value
openJsonValue' (OpenJson v) = pure v

decodeJson :: Text -> Value
decodeJson txt =
  case eitherDecodeStrict (TE.encodeUtf8 txt) of
    Right v -> v
    Left err -> error ("invalid expected JSON: " <> err)

requireNode :: String -> Maybe Value -> Playwright Value
requireNode what = maybe (assertFailure ("no " <> what <> " node")) pure

roleIs :: Text -> Value -> Bool
roleIs role (Object o) = KM.lookup "role" o == Just (String role)
roleIs _ _ = False

field :: Text -> Value -> Maybe Value
field key (Object o) = KM.lookup (K.fromText key) o
field _ _ = Nothing

nodesOf :: Value -> [Value]
nodesOf (Array xs) = toList xs
nodesOf _ = []

childrenOf :: Value -> [Value]
childrenOf v = case field "children" v of
  Just (Array xs) -> toList xs
  _ -> []

-- | Depth-first search, matching upstream findNode (skips string fragments).
findNode :: (Value -> Bool) -> [Value] -> Maybe Value
findNode _ [] = Nothing
findNode p (node : rest)
  | p node = Just node
  | otherwise = case findNode p (childrenOf node) of
      Just found -> Just found
      Nothing -> findNode p rest

isNonEmptyString :: Value -> Bool
isNonEmptyString (String s) = not (T.null s)
isNonEmptyString _ = False

numberValue :: Value -> Maybe Double
numberValue v = case fromJSON v of
  Success d -> Just d
  _ -> Nothing

-- | Matches upstream /^f\d+e\d+$/.
isFrameRef :: Text -> Bool
isFrameRef ref = case T.stripPrefix "f" ref of
  Nothing -> False
  Just afterF ->
    let (frameDigits, afterDigits) = T.span C.isDigit afterF
     in not (T.null frameDigits)
          && case T.stripPrefix "e" afterDigits of
            Nothing -> False
            Just afterE ->
              let (elemDigits, rest) = T.span C.isDigit afterE
               in not (T.null elemDigits) && T.null rest

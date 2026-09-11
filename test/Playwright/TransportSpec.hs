module Playwright.TransportSpec
  ( tests
  ) where

import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as BS
import Hedgehog
  ( Gen
  , Property
  , failure
  , forAll
  , property
  , (===)
  )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Playwright.Transport.Frame
  ( FrameError (FrameTooLarge)
  , encodeFrame
  , maxFrameSize
  , splitFrames
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertBool
  , assertEqual
  , testCase
  )
import Test.Tasty.Hedgehog
  ( testProperty
  )

tests :: TestTree
tests =
  testGroup
    "transport frames"
    [ testCase "encode then split yields the payload" $ do
        let
          payload :: ByteString
          payload = "hello"
        case encodeFrame payload of
          Left err ->
            assertBool ("encode failed: " <> show err) False
          Right framed ->
            case splitFrames framed of
              Left err ->
                assertBool ("split failed: " <> show err) False
              Right (leftover, frames) -> do
                assertEqual "no leftover" BS.empty leftover
                assertEqual "one frame" [payload] frames
    , testCase "oversize payload is rejected" $
        case encodeFrame (BS.replicate (fromIntegral maxFrameSize + 1) 0) of
          Left (FrameTooLarge _) ->
            pure ()
          other ->
            assertBool ("expected FrameTooLarge, got " <> show other) False
    , testProperty "roundtrip encode/split" roundtripProperty
    , testProperty "concatenated frames split" concatenatedProperty
    ]

roundtripProperty :: Property
roundtripProperty =
  property $ do
    payload <- forAll genPayload
    case encodeFrame payload of
      Left _ ->
        failure
      Right framed ->
        case splitFrames framed of
          Left _ ->
            failure
          Right (leftover, frames) -> do
            leftover === BS.empty
            frames === [payload]

concatenatedProperty :: Property
concatenatedProperty =
  property $ do
    payloads <- forAll (Gen.list (Range.linear 1 5) genPayload)
    case traverse encodeFrame payloads of
      Left _ ->
        failure
      Right framed ->
        case splitFrames (BS.concat framed) of
          Left _ ->
            failure
          Right (leftover, decoded) -> do
            leftover === BS.empty
            decoded === payloads

genPayload :: Gen ByteString
genPayload =
  Gen.bytes (Range.linear 0 4096)

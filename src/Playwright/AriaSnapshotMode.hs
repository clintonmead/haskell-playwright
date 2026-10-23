module Playwright.AriaSnapshotMode
  ( AriaSnapshotMode (..)
  ) where

-- | How an aria snapshot is produced. @ai@ includes element refs suited to
-- AI consumers; @default@ is the plain accessibility tree.
data AriaSnapshotMode
  = AriaSnapshotModeAi
  | AriaSnapshotModeDefault
  deriving stock (Eq, Ord, Show)

module Playwright.AriaSnapshotSupport
  ( textLines
  , yamlContainsLines
  ) where

import Data.Text
  ( Text
  )
import Data.Text qualified as T

-- | Joins literal lines with newlines. Used for HTML fixtures and for the
-- expected YAML or JSON output, where each line keeps its own indentation.
textLines :: [Text] -> Text
textLines = T.intercalate "\n"

-- | Approximates @toContainYaml@ for the cases that only need the expected
-- lines to appear in the snapshot: each non-blank expected line, with its
-- indentation ignored, must occur as a line of the snapshot.
yamlContainsLines :: Text -> [Text] -> Bool
yamlContainsLines snapshot expected =
  all (`elem` actualLines) (filter (not . T.null) (map T.strip expected))
  where
    actualLines = map T.strip (T.lines snapshot)

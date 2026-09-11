module Playwright.UrlGlob
  ( UrlGlob (..)
  , matchesUrlGlob
  ) where

import Data.Text
  ( Text
  , unpack
  )
import Playwright.Url
  ( Url
  , urlToText
  )

newtype UrlGlob = UrlGlob
  { urlGlobToText :: Text
  }
  deriving stock (Eq, Show)

matchesUrlGlob :: UrlGlob -> Url -> Bool
matchesUrlGlob glob url =
  globMatch (unpack (urlGlobToText glob)) (unpack (urlToText url))

globMatch :: String -> String -> Bool
globMatch pattern text =
  case (pattern, text) of
    ([], []) ->
      True
    ('*' : '*' : rest, remaining) ->
      globMatch rest remaining
        || case remaining of
          [] ->
            False
          (_ : more) ->
            globMatch ('*' : '*' : rest) more
    ('*' : rest, remaining) ->
      globMatch rest remaining
        || case remaining of
          [] ->
            False
          ('/' : _) ->
            False
          (_ : more) ->
            globMatch ('*' : rest) more
    ('?' : rest, _ : more) ->
      globMatch rest more
    (p : ps, c : cs) ->
      p == c && globMatch ps cs
    _ ->
      False

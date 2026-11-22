{-# LANGUAGE OverloadedStrings #-}

-- | The SQL counterpart of 'Data.Text.toCaseFold' used by venue search
-- (EVT-VENUE-SEARCH-001).
--
-- PostgreSQL 17 has no case-folding function and @lower()@ is not
-- 'T.toCaseFold' (it keeps @ß@, and its result depends on the database
-- locale). 'T.toCaseFold' maps every character on its own, with no context, so
-- it is fully described by the table of characters it changes. This module
-- derives that table from 'T.toCaseFold' itself and renders the SQL function
-- @venue_search_fold(text)@. The migration carries the rendered text; a test
-- keeps the two identical and compares the installed function with
-- 'T.toCaseFold' on every Unicode scalar value.
--
-- The function has three branches that compute the same result and differ
-- only in cost: ASCII-only input, input below U+0250 (Latin), and anything
-- else.
--
-- This module depends on @base@ and @text@ only, so the SQL can be rendered
-- without building the server:
--
-- > stack exec -- runghc -isrc scripts/render-venue-search-fold.hs
module TDF.Venues.SearchFold
  ( venueSearchFoldEntries
  , venueSearchFoldLatinCeiling
  , venueSearchFoldFunctionSql
  ) where

import           Data.Char (ord)
import           Data.Text (Text)
import qualified Data.Text as T
import           Numeric   (showHex)

-- | Every scalar value 'T.toCaseFold' changes, with its folded form, in code
-- point order. Surrogates are not scalar values and are skipped.
venueSearchFoldEntries :: [(Char, Text)]
venueSearchFoldEntries =
  [ (c, folded)
  | c <- ['\x1' .. '\xD7FF'] ++ ['\xE000' .. '\x10FFFF']
  , let folded = T.toCaseFold (T.singleton c)
  , folded /= T.singleton c
  ]

-- | Input whose characters are all below this code point takes the short table.
venueSearchFoldLatinCeiling :: Char
venueSearchFoldLatinCeiling = '\x250'

-- | @CREATE OR REPLACE FUNCTION venue_search_fold(text)@. Pure ASCII, so the
-- migration file does not depend on the client encoding.
venueSearchFoldFunctionSql :: Text
venueSearchFoldFunctionSql =
  T.unlines
    [ "CREATE OR REPLACE FUNCTION venue_search_fold(input text) RETURNS text"
    , "LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE AS $venue_search_fold$"
    , "SELECT CASE"
    , "  WHEN octet_length(input) = char_length(input) THEN lower(input COLLATE \"C\")"
    , "  WHEN input COLLATE \"C\" ~ " <> latinPattern <> " THEN"
    , "    " <> foldExpression latin
    , "  ELSE"
    , "    " <> foldExpression venueSearchFoldEntries
    , "END"
    , "$venue_search_fold$;"
    ]
  where
    latin = filter ((< venueSearchFoldLatinCeiling) . fst) venueSearchFoldEntries
    latinPattern =
      "U&'^[\\0001-" <> escapeChar (pred venueSearchFoldLatinCeiling) <> "]*$'"

-- | One @translate@ for the characters that fold to one character, wrapped in
-- one @replace@ for each character that folds to several. A folded form is
-- never itself changed by folding, so the order of the steps does not matter.
foldExpression :: [(Char, Text)] -> Text
foldExpression entries =
  foldl wrap translated expanding
  where
    single = [(c, T.head folded) | (c, folded) <- entries, T.length folded == 1]
    expanding = [(c, folded) | (c, folded) <- entries, T.length folded /= 1]
    translated =
      "translate(input, " <> literal (map fst single) <> ", " <> literal (map snd single) <> ")"
    wrap inner (c, folded) =
      "replace(" <> inner <> ", " <> literal [c] <> ", " <> literal (T.unpack folded) <> ")"

literal :: String -> Text
literal chars = "U&'" <> T.concat (map escapeChar chars) <> "'"

escapeChar :: Char -> Text
escapeChar c =
  let hex = showHex (ord c) ""
   in if length hex <= 4
        then "\\" <> T.pack (replicate (4 - length hex) '0' <> hex)
        else "\\+" <> T.pack (replicate (6 - length hex) '0' <> hex)

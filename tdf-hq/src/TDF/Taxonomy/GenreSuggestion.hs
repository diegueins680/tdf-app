{-# LANGUAGE OverloadedStrings #-}

-- | Validation of artist genre suggestions (TAX-GENRE-SUGGESTION-001).
--
-- A suggestion has a Spanish and an English name. Before it is stored, each
-- name is cleaned and cased for its language, compared with every existing
-- genre in both languages (ignoring case, accents, spacing and hyphens), and
-- checked word by word against the vocabulary of existing genre names.
--
-- * An exact match (same normalized key) is a duplicate: the artist is
--   offered the existing genre instead.
-- * A near match (small edit distance) is a probable typo: the artist is
--   offered the existing name and may confirm the suggestion is different,
--   which sends it to review.
-- * A suggestion whose words are all known, in both names, is approved
--   automatically. Anything the checks cannot settle goes to review.
module TDF.Taxonomy.GenreSuggestion
  ( ExistingGenre(..)
  , SuggestionLanguage(..)
  , NameCheck(..)
  , SuggestionVerdict(..)
  , SuggestionCheck(..)
  , WordCorrection(..)
  , cleanGenreName
  , caseGenreName
  , genreKey
  , foldAccents
  , editDistance
  , similarityThreshold
  , checkGenreSuggestion
  , reviewReasonCodes
  ) where

import Data.Char (isAlphaNum, isDigit, isLetter, isLower, isUpper, toLower, toUpper)
import Data.List (minimumBy, nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)

data ExistingGenre = ExistingGenre
  { egId :: UUID
  , egNameEs :: Text
  , egNameEn :: Text
  , egParentId :: Maybe UUID
  } deriving (Eq, Show)

data SuggestionLanguage = Spanish | English
  deriving (Eq, Show)

data WordCorrection = WordCorrection
  { wcWord :: Text
  , wcSuggestion :: Text
  } deriving (Eq, Show)

-- | Result for one language's name.
data NameCheck = NameCheck
  { ncName :: Text
    -- ^ Cleaned and cased name that would be stored.
  , ncKey :: Text
    -- ^ Accent-, case-, space- and punctuation-insensitive key.
  , ncUnknownWords :: [Text]
    -- ^ Words that appear in no existing genre name (either language).
  , ncCorrections :: [WordCorrection]
    -- ^ Known words close to an unknown word ("electronika" -> "electrónica").
  } deriving (Eq, Show)

data SuggestionVerdict
  = Duplicate ExistingGenre
  | ProbableTypo ExistingGenre
  | NeedsReview [Text]
  | AutoApprove
  deriving (Eq, Show)

data SuggestionCheck = SuggestionCheck
  { scSpanish :: NameCheck
  , scEnglish :: NameCheck
  , scVerdict :: SuggestionVerdict
  } deriving (Eq, Show)

-- | Collapse whitespace and reject unsafe or out-of-range names. Letters
-- (any script, accents included), digits, spaces and @- & ' . / +@ are allowed.
cleanGenreName :: Text -> Either Text Text
cleanGenreName raw
  | T.length cleaned < 2 = Left "genre names need at least 2 characters"
  | T.length cleaned > 60 = Left "genre names are limited to 60 characters"
  | not (T.all allowed cleaned) = Left "genre names may only contain letters, numbers, spaces and - & ' . / +"
  | not (T.any isLetter cleaned) = Left "genre names need at least one letter"
  | otherwise = Right cleaned
  where
    cleaned = T.unwords (T.words raw)
    allowed c = isLetter c || isDigit c || c == ' ' || c `elem` ("-&'./+" :: String)

-- | Case a cleaned name for its language. Spanish uses sentence case
-- ("Cumbia villera"), English title case with lowercase joining words
-- ("Drum and Bass"). A token typed with deliberate inner capitals or as a
-- short acronym ("EDM", "R&B", "UK") keeps its casing, a letter prefix before
-- a hyphen keeps the rest lowercase ("K-pop"), and a word whose casing is
-- established by an existing genre name ("Hip-Hop", "R&B") uses that casing.
caseGenreName :: Map.Map Text Text -> Set.Set Text -> SuggestionLanguage -> Text -> Text
caseGenreName established known language name =
  T.unwords (zipWith caseWord [0 :: Int ..] (T.words name))
  where
    caseWord position word
      | Just canonical <- Map.lookup (T.toLower word) established = canonical
      | deliberate word && not (genreKey word `Set.member` known) = word
      | otherwise = case language of
          Spanish -> if position == 0 then capitalizeHyphenated Spanish word else T.toLower word
          English
            | position > 0 && T.toLower word `Set.member` joiningWords -> T.toLower word
            | otherwise -> capitalizeHyphenated English word
    joiningWords = Set.fromList ["and", "of", "the", "n", "y", "de", "del", "la", "el", "en"]

-- | Inner capitals after a lowercase letter ("DnB", "iPop") or an all-caps
-- token of two or three letters ("EDM", "R&B", "UK", "IDM"). Longer all-caps
-- input ("ROCK") is treated as shouting, not as an acronym.
deliberate :: Text -> Bool
deliberate word =
  let letters = T.filter isLetter word
      innerCapital = any (\(a, b) -> isLower a && isUpper b) (T.zip word (T.drop 1 word))
      shortAcronym = T.length letters >= 2 && T.length letters <= 3 && T.all isUpper letters
  in innerCapital || shortAcronym

capitalizeHyphenated :: SuggestionLanguage -> Text -> Text
capitalizeHyphenated language word =
  case T.splitOn "-" word of
    [] -> word
    (first : rest) ->
      let firstCased = capitalize first
          singleLetterPrefix = T.length first == 1
          restCased = map (\part -> if singleLetterPrefix || language == Spanish then T.toLower part else capitalize part) rest
      in T.intercalate "-" (firstCased : restCased)
  where
    capitalize part = case T.uncons (T.toLower part) of
      Just (c, cs) -> T.cons (toUpper c) cs
      Nothing -> part

-- | Accent-, case-, space- and punctuation-insensitive comparison key.
-- "Hip-Hop", "hip hop" and "hiphop" share the key "hiphop".
genreKey :: Text -> Text
genreKey = T.filter isAlphaNum . T.map toLower . foldAccents

-- | Fold the Latin accents used in Spanish, Portuguese and French names.
foldAccents :: Text -> Text
foldAccents = T.map fold
  where
    fold c = Map.findWithDefault c c accentMap
    accentMap = Map.fromList (concatMap expand groups)
    expand (base, variants) = [(v, base) | v <- variants]
    groups =
      [ ('a', "áàâäãå"), ('e', "éèêë"), ('i', "íìîï"), ('o', "óòôöõ"), ('u', "úùûü")
      , ('n', "ñ"), ('c', "ç"), ('y', "ýÿ")
      , ('A', "ÁÀÂÄÃÅ"), ('E', "ÉÈÊË"), ('I', "ÍÌÎÏ"), ('O', "ÓÒÔÖÕ"), ('U', "ÚÙÛÜ")
      , ('N', "Ñ"), ('C', "Ç"), ('Y', "Ý")
      ]

-- | Levenshtein distance (insertions, deletions, substitutions).
editDistance :: Text -> Text -> Int
editDistance left right = last (last rows)
  where
    a = T.unpack left
    b = T.unpack right
    n = length b
    firstRow = [0 .. n]
    rows = scanl step firstRow (zip [1 ..] a)
    step previous (i, ca) =
      let row = scanl (cell previous ca) i (zip3 [1 ..] b previous)
      in row
    cell previous ca leftValue (j, cb, diagonal) =
      let above = previous !! j
          cost = if ca == cb then 0 else 1
      in minimum [above + 1, leftValue + 1, diagonal + cost]

-- | Edits tolerated before a near name counts as a probable typo: none for
-- very short keys, one up to six characters, two beyond.
similarityThreshold :: Text -> Int
similarityThreshold key
  | T.length key <= 3 = 0
  | T.length key <= 6 = 1
  | otherwise = 2

-- | Check a suggestion. The names are raw artist input; the established map
-- (lowercased word -> casing) and the vocabulary are derived from the
-- existing genres. @confirmedDistinct@ is the artist's confirmation, after
-- seeing a near match, that the genre is a different one.
checkGenreSuggestion :: [ExistingGenre] -> Bool -> Text -> Text -> Either Text SuggestionCheck
checkGenreSuggestion existing confirmedDistinct rawEs rawEn = do
  cleanedEs <- cleanGenreName rawEs
  cleanedEn <- cleanGenreName rawEn
  let spanish = nameCheck Spanish cleanedEs
      english = nameCheck English cleanedEn
      keys = nub [ncKey spanish, ncKey english]
      exact = [genre | genre <- existing, any (`elem` keys) (genreKeys genre)]
      near =
        [ (distance, genre)
        | genre <- existing
        , key <- keys
        , existingKey <- genreKeys genre
        , let distance = editDistance key existingKey
        , distance > 0
        , distance <= similarityThreshold (if T.length key < T.length existingKey then key else existingKey)
        ]
      unknownWords = map T.toLower (nubOn genreKey (ncUnknownWords spanish <> ncUnknownWords english))
      reasons =
        ["similar_to_existing" | not (null near)]
          <> map ("unknown_word:" <>) unknownWords
      verdict = case (exact, near) of
        (genre : _, _) -> Duplicate genre
        ([], _ : _) | not confirmedDistinct -> ProbableTypo (snd (minimumBy (comparing fst) near))
        _ | null reasons -> AutoApprove
          | otherwise -> NeedsReview reasons
  pure (SuggestionCheck spanish english verdict)
  where
    genreKeys genre = nub [genreKey (egNameEs genre), genreKey (egNameEn genre)]
    spanishWords = concatMap (T.words . egNameEs) existing
    englishWords = concatMap (T.words . egNameEn) existing
    existingWords = spanishWords <> englishWords
    -- Hyphenated names contribute their parts too ("Hip-hop" -> hip, hop).
    -- When both spellings exist ("Reggaetón", "Reggaeton"), keep the accented
    -- one so a correction never drops an accent.
    vocabulary =
      Map.fromListWith (\new old -> if foldAccents old == old && foldAccents new /= new then new else old)
        [ (genreKey part, part)
        | word <- existingWords
        , part <- word : splitParts word
        , not (T.null (genreKey part))
        ]
    known = Map.keysSet vocabulary
    -- Casing an existing name establishes for its own language only: the
    -- Spanish catalog writes "Hip-hop", the English one "Hip-Hop".
    establishedFor wordsOfLanguage =
      Map.fromList
        [ (T.toLower word, word)
        | word <- wordsOfLanguage
        , deliberate word || T.any (== '-') word || T.any (== '&') word
        ]
    -- An acronym typed in one name ("EDM") sets its casing in the other.
    typedAcronyms = Map.fromList [(T.toLower word, word) | word <- T.words rawEs <> T.words rawEn, deliberate word]
    nameCheck language cleaned =
      let established = Map.union (establishedFor (if language == Spanish then spanishWords else englishWords)) typedAcronyms
          cased = caseGenreName established known language cleaned
          parts = concatMap (\word -> if null (splitParts word) then [word] else splitParts word) (T.words cased)
          unknown =
            nubOn genreKey
              [ part
              | part <- parts
              , let key = genreKey part
              , not (T.null key)
              , not (Map.member key vocabulary)
              , not (isJoining key)
              ]
          corrections = mapMaybe correction unknown
      in NameCheck cased (genreKey cased) unknown corrections
    splitParts word =
      let pieces = filter (not . T.null) (T.split (`elem` ("-/" :: String)) word)
      in if length pieces > 1 then pieces else []
    nubOn f = go Set.empty
      where
        go _ [] = []
        go seen (x : xs)
          | f x `Set.member` seen = go seen xs
          | otherwise = x : go (Set.insert (f x) seen) xs
    isJoining key = key `elem` ["and", "of", "the", "n", "y", "de", "del", "la", "el", "en"]
    correction word =
      let key = genreKey word
          candidates =
            sortOn fst
              [ (distance, knownWord)
              | (knownKey, knownWord) <- Map.toList vocabulary
              , let distance = editDistance key knownKey
              , distance > 0
              , distance <= max 1 (similarityThreshold key)
              ]
      in case candidates of
           (_, knownWord) : _ -> Just (WordCorrection word (T.toLower knownWord))
           [] -> Nothing

-- | Stable reason codes stored with a pending suggestion.
reviewReasonCodes :: SuggestionVerdict -> [Text]
reviewReasonCodes verdict = case verdict of
  NeedsReview reasons -> reasons
  _ -> []

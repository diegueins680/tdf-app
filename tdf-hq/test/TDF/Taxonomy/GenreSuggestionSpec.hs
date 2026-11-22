{-# LANGUAGE OverloadedStrings #-}

-- | TAX-GENRE-SUGGESTION-001: casing, spelling, duplicate and review checks.
module TDF.Taxonomy.GenreSuggestionSpec (spec) where

import Data.Either (isLeft)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (isAsciiLower)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Test.Hspec
import Test.QuickCheck

import TDF.Server.Taxonomy (genreSuggestionSlug)
import TDF.Taxonomy.GenreSuggestion

existing :: [ExistingGenre]
existing =
  [ genre 1 "Hip-hop" "Hip-Hop", genre 2 "R&B" "R&B", genre 3 "Rock" "Rock", genre 4 "Pop" "Pop"
  , genre 5 "Reggaetón" "Reggaeton", genre 6 "Electrónica" "Electronic", genre 7 "Cumbia" "Cumbia"
  , genre 8 "Drum and bass" "Drum and Bass", genre 9 "Latina" "Latin"
  ]
  where
    genre n es en = ExistingGenre (fromJust (UUID.fromString ("00000000-0000-4000-8000-00000000000" <> show (n :: Int)))) es en Nothing

check :: Text -> Text -> Either Text SuggestionCheck
check = checkGenreSuggestion existing False

names :: Either Text SuggestionCheck -> Maybe (Text, Text)
names = either (const Nothing) (\c -> Just (ncName (scSpanish c), ncName (scEnglish c)))

verdict :: Either Text SuggestionCheck -> Maybe SuggestionVerdict
verdict = either (const Nothing) (Just . scVerdict)

isDuplicateOf :: Text -> Maybe SuggestionVerdict -> Bool
isDuplicateOf name (Just (Duplicate g)) = egNameEs g == name
isDuplicateOf _ _ = False

spec :: Spec
spec = describe "TAX-GENRE-SUGGESTION-001 genre suggestion checks" $ do
  describe "casing" $ do
    it "uses sentence case in Spanish and title case in English" $
      names (check "cumbia   villera" "cumbia villera") `shouldBe` Just ("Cumbia villera", "Cumbia Villera")
    it "keeps English joining words lowercase" $
      names (check "rock y blues" "rock and blues") `shouldBe` Just ("Rock y blues", "Rock and Blues")
    it "keeps short acronyms and carries one typed in either name to the other" $
      names (check "edm" "EDM") `shouldBe` Just ("EDM", "EDM")
    it "does not treat a shouted word as an acronym" $
      names (check "ROCK PROGRESIVO" "PROGRESSIVE ROCK") `shouldBe` Just ("Rock progresivo", "Progressive Rock")
    it "keeps a letter prefix before a hyphen ('K-pop')" $
      names (check "k-pop" "k-pop") `shouldBe` Just ("K-pop", "K-pop")
    it "uses each language's established casing for hyphenated words" $
      caseGenreName (Map.fromList [("hip-hop", "Hip-Hop")]) Set.empty English "hip-hop latino" `shouldBe` "Hip-Hop Latino"

  describe "duplicates and typos" $ do
    it "treats spacing, hyphens, case and accents as the same genre" $ do
      verdict (check "hip hop" "hip hop") `shouldSatisfy` isDuplicateOf "Hip-hop"
      verdict (check "HIPHOP" "hiphop") `shouldSatisfy` isDuplicateOf "Hip-hop"
      verdict (check "reggaeton" "reggaetón") `shouldSatisfy` isDuplicateOf "Reggaetón"
    it "matches a duplicate in the other language" $
      verdict (check "Electronic" "Electrónica") `shouldSatisfy` isDuplicateOf "Electrónica"
    it "offers the existing name for a probable typo, keeping its accent" $ do
      let result = check "Regueton" "Regueton"
      verdict result `shouldSatisfy` \v -> case v of
        Just (ProbableTypo g) -> egNameEs g == "Reggaetón"
        _ -> False
      fmap (map wcSuggestion . ncCorrections . scSpanish) (either (const Nothing) Just result) `shouldBe` Just ["reggaetón"]
    it "sends a confirmed near match to review instead of refusing it" $
      verdict (checkGenreSuggestion existing True "Regueton" "Regueton")
        `shouldBe` Just (NeedsReview ["similar_to_existing", "unknown_word:regueton"])

  describe "automatic approval" $ do
    it "approves a name made only of known words" $ do
      verdict (check "pop rock" "pop rock") `shouldBe` Just AutoApprove
      verdict (check "rock latino" "latin rock") `shouldSatisfy` (/= Just AutoApprove)
      verdict (check "cumbia pop" "cumbia pop") `shouldBe` Just AutoApprove
    it "sends new words to review, listing each once" $
      verdict (check "Cumbia villera" "Cumbia Villera") `shouldBe` Just (NeedsReview ["unknown_word:villera"])
    it "suggests a known word for a misspelled one" $
      fmap (ncCorrections . scSpanish) (either (const Nothing) Just (check "electronika pop" "electronic pop"))
        `shouldBe` Just [WordCorrection "Electronika" "electrónica"]

  describe "input safety" $ do
    it "rejects names that are too short, too long or unsafe" $ do
      check "x" "x" `shouldSatisfy` isLeft
      check (mconcat (replicate 61 "a")) "ok" `shouldSatisfy` isLeft
      check "<script>" "rock" `shouldSatisfy` isLeft
      check "123" "rock" `shouldSatisfy` isLeft
    it "is insensitive to case and accents in its comparison key" $
      property $ \(ASCIIString raw) ->
        let rest = T.pack (filter isAsciiLower raw)
        in genreKey ("é" <> rest) == genreKey ("E" <> rest)

  describe "genre codes" $
    it "slugs names to lowercase ASCII" $ do
      genreSuggestionSlug "Cumbia Villera" `shouldBe` "cumbia-villera"
      genreSuggestionSlug "Rock en Español" `shouldBe` "rock-en-espanol"
      genreSuggestionSlug "R&B" `shouldBe` "r-b"

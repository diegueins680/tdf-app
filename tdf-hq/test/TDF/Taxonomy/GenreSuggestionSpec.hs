{-# LANGUAGE OverloadedStrings #-}

-- | TAX-GENRE-SUGGESTION-001: casing, spelling, duplicate and review checks.
module TDF.Taxonomy.GenreSuggestionSpec (spec) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString.Char8 as BS
import Data.Char (digitToInt, isAsciiLower)
import Data.Int (Int64)
import Data.List (sort)
import Data.Pool (destroyAllResources)
import qualified Data.UUID.V4 as Random
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (toSqlKey)
import qualified Database.PostgreSQL.Simple as PG
import Servant ((:<|>)(..), errHTTPCode, runHandler)
import System.Environment (lookupEnv)
import TDF.API.Taxonomy (GenreSuggestionRequest(..), GenreSuggestionApproval(..))
import TDF.Auth (AuthedUser(..))
import TDF.DB (Env(..))
import TDF.Models (RoleEnum(Admin))
import TDF.Test.DisposablePostgres (isDisposablePostgresUrl)
import Data.Either (isLeft)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Test.Hspec
import Test.QuickCheck

import TDF.Server.Taxonomy (genreSuggestionSlug, taxonomyProtectedServer)
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

  it "publishes one genre when artists submit or administrators approve the same bilingual name concurrently" $ do
    configured <- lookupEnv "TDF_DISCOVERY_TEST_DATABASE_URL"
    ci <- lookupEnv "CI"
    case configured of
      Nothing -> pendingWith "Set TDF_DISCOVERY_TEST_DATABASE_URL to the migrated discovery test database"
      Just url | isDisposablePostgresUrl (ci == Just "true") url -> publicationRace url
      Just _ -> expectationFailure "Expected a disposable discovery test database"

  describe "automatic approval" $ do
    it "approves a name made only of known words" $ do
      verdict (check "pop rock" "pop rock") `shouldBe` Just AutoApprove
      verdict (check "rock latino" "latin rock") `shouldSatisfy` (/= Just AutoApprove)
      verdict (check "cumbia pop" "cumbia pop") `shouldBe` Just AutoApprove
    it "reviews unfamiliar single letters in either language and allows known joining words" $ do
      verdict (check "X Q" "X Q") `shouldBe` Just (NeedsReview ["unknown_word:x", "unknown_word:q"])
      verdict (check "cumbia y pop" "cumbia and pop") `shouldBe` Just AutoApprove
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

publicationRace :: String -> IO ()
publicationRace url =
  bracket (PG.connectPostgreSQL connection) PG.close $ \db ->
    bracket (runNoLoggingT (createPostgresqlPool connection 2)) destroyAllResources $ \pool ->
      bracket (newArtist db) (cleanup db) $ \artist -> do
        nonce <- Random.nextRandom
        let vocabulary = ["rock", "pop", "cumbia"]
            wordsForTest = [vocabulary !! (digitToInt c `mod` 3) | c <- take 8 (UUID.toString nonce)]
            name = T.unwords wordsForTest
            user = AuthedUser (toSqlKey artist) [Admin] mempty Nothing Nothing
            _ :<|> submit :<|> _ :<|> _ :<|> approve :<|> _ :<|> _ = taxonomyProtectedServer user
            run action = runHandler (runReaderT action (Env pool (error "Genre handlers do not use app configuration")))
            statuses (first, second) = sort (map (either errHTTPCode (const 200)) [first, second])
            request = GenreSuggestionRequest ("rock " <> name) ("rock " <> name) Nothing Nothing
        submissions <- concurrently (run (submit request)) (run (submit request))
        statuses submissions `shouldBe` [200,409]
        let pending label = do
              [PG.Only key] <- PG.query db
                "INSERT INTO genre_suggestion(suggested_by_party_id,submitted_name,name_es,name_en,normalized_key) VALUES (?,?,?,?,replace(gen_random_uuid()::text,'-','')) RETURNING id"
                (artist, label :: Text, label, label)
              pure key
            final = GenreSuggestionApproval ("pop " <> name) ("pop " <> name) Nothing
        first <- pending "First pending fixture"
        second <- pending "Second pending fixture"
        approvals <- concurrently (run (approve first final)) (run (approve second final))
        statuses approvals `shouldBe` [200,409]
        counts <- PG.query db "SELECT count(*) FROM genre WHERE created_by=?" (PG.Only artist)
        counts `shouldBe` [PG.Only (2 :: Int)]
  where
    connection = BS.pack url
    newArtist db = do
      [PG.Only artist] <- PG.query_ db "INSERT INTO party(display_name,is_org,created_at) VALUES ('Genre concurrency fixture',false,now()) RETURNING id"
      pure (artist :: Int64)
    cleanup db artist = PG.withTransaction db $ do
      void (PG.execute db "DELETE FROM genre_suggestion WHERE suggested_by_party_id=?" (PG.Only artist))
      -- Published catalog rows retain their audit history in the disposable database.
      void (PG.execute db "UPDATE genre SET active=false WHERE created_by=?" (PG.Only artist))

{-# LANGUAGE OverloadedStrings #-}

module TDF.Profiles.ArtistSpec (spec) where

import qualified Data.Aeson as A
import Data.Either (isLeft)
import Data.Int (Int64)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.Persist
import Database.Persist.Sql (SqlBackend, SqlPersistT, fromSqlKey, rawExecute)
import Database.Persist.Sqlite (runSqlite)
import Test.Hspec

import TDF.DTO (ArtistProfileDTO (..), ArtistProfilePhotoUpdate (..), ArtistProfileUpsert (..))
import TDF.Models
import TDF.Profiles.Artist (
    loadArtistProfileBySlugDTO,
    loadOrCreateArtistProfileDTO,
    searchArtistProfilesDTO,
    upsertArtistProfileRecord,
    validateArtistProfileUpsert,
 )

spec :: Spec
spec = do
    describe "ArtistProfileUpsert FromJSON" $ do
        it "accepts canonical artist profile write payloads" $
            case A.eitherDecode
                "{\"apuArtistId\":42,\"apuDisplayName\":\"Los Mentores\",\"apuSlug\":\"los-mentores\",\"apuBio\":\"Fusionando ritmos latinos con neo soul.\",\"apuCity\":\"Quito\",\"apuGenreIds\":[\"10000000-0000-4000-8000-000000000001\"]}" of
                Left err ->
                    expectationFailure ("Expected canonical artist profile payload to decode, got: " <> err)
                Right payload -> do
                    apuArtistId payload `shouldBe` 42
                    apuDisplayName payload `shouldBe` Just "Los Mentores"
                    apuSlug payload `shouldBe` Just "los-mentores"
                    apuBio payload `shouldBe` Just "Fusionando ritmos latinos con neo soul."
                    apuCity payload `shouldBe` Just "Quito"
                    apuGenreIds payload `shouldBe` [latinGenreId]

        it "rejects unexpected artist profile keys so typoed writes fail explicitly" $ do
            (A.eitherDecode
                "{\"apuArtistId\":42,\"apuDisplayName\":\"Los Mentores\",\"apuGenreIds\":[],\"displayName\":\"ignored by mistake\"}"
                    :: Either String ArtistProfileUpsert)
                `shouldSatisfy` isLeft
            (A.eitherDecode
                "{\"apuArtistId\":42,\"apuDisplayName\":\"Los Mentores\",\"apuGenreIds\":[],\"unexpected\":true}"
                    :: Either String ArtistProfileUpsert)
                `shouldSatisfy` isLeft

        it "rejects the obsolete copied genre-label field" $
            (A.eitherDecode
                "{\"apuArtistId\":42,\"apuGenreIds\":[],\"apuGenres\":\"Rock\"}"
                    :: Either String ArtistProfileUpsert)
                `shouldSatisfy` isLeft

    describe "ArtistProfilePhotoUpdate FromJSON" $ do
        it "accepts canonical artist photo payloads" $
            case A.eitherDecode "{\"apuHeroImageUrl\":\"https://cdn.tdf/hero.jpg\"}" of
                Left err ->
                    expectationFailure ("Expected canonical artist photo payload to decode, got: " <> err)
                Right (ArtistProfilePhotoUpdate heroImageUrl) ->
                    heroImageUrl `shouldBe` "https://cdn.tdf/hero.jpg"

        it "rejects unexpected artist photo keys" $
            (A.eitherDecode
                "{\"apuHeroImageUrl\":\"https://cdn.tdf/hero.jpg\",\"heroImageUrl\":\"ignored\"}"
                    :: Either String ArtistProfilePhotoUpdate)
                `shouldSatisfy` isLeft

    describe "Artist profile helpers" $ do
        it "normalizes path-safe profile slugs before artist profile writes persist them" $ do
            fmap apuSlug
                (validateArtistProfileUpsert
                    (baseProfileUpsert { apuSlug = Just "  Mentores-Del-Aire  " }))
                `shouldBe` Right (Just "mentores-del-aire")
            fmap apuSlug
                (validateArtistProfileUpsert (baseProfileUpsert { apuSlug = Just "   " }))
                `shouldBe` Right Nothing

        it "rejects path-ambiguous profile slugs before profile writes can miss public lookups" $ do
            let assertInvalid rawSlug expectedMessage =
                    case validateArtistProfileUpsert
                        (baseProfileUpsert { apuSlug = Just rawSlug }) of
                        Left err -> T.unpack err `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure ("Expected invalid profile slug error, got " <> show value)
            assertInvalid "mentores del aire" "only lowercase ASCII letters"
            assertInvalid "mentores/del-aire" "only lowercase ASCII letters"
            assertInvalid "mentores?draft=true" "only lowercase ASCII letters"
            assertInvalid "-mentores" "only lowercase ASCII letters"
            assertInvalid ("mentores" <> T.singleton '\x202E') "only lowercase ASCII letters"
            assertInvalid (T.replicate 97 "a") "96 characters or fewer"

        it "normalizes public artist profile URLs before profile writes persist them" $
            case validateArtistProfileUpsert
                ( baseProfileUpsert
                    { apuHeroImageUrl = Just "  https://cdn.tdf/hero.jpg  "
                    , apuSpotifyUrl = Just "   "
                    , apuWebsiteUrl = Just "http://artist.example"
                    }
                ) of
                Left err ->
                    expectationFailure ("Expected valid profile URL fields, got " <> T.unpack err)
                Right payload -> do
                    apuHeroImageUrl payload `shouldBe` Just "https://cdn.tdf/hero.jpg"
                    apuSpotifyUrl payload `shouldBe` Nothing
                    apuWebsiteUrl payload `shouldBe` Just "http://artist.example"

        it "rejects unsafe public artist profile URLs before they can be rendered from profiles" $ do
            let assertInvalid payload expectedMessage =
                    case validateArtistProfileUpsert payload of
                        Left err -> T.unpack err `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure ("Expected invalid profile URL error, got " <> show value)
            assertInvalid
                (baseProfileUpsert { apuWebsiteUrl = Just "javascript:alert(1)" })
                "websiteUrl must be an absolute public http or https URL"
            assertInvalid
                (baseProfileUpsert { apuHeroImageUrl = Just "https://cdn.tdf/hero image.jpg" })
                "heroImageUrl must not contain whitespace"
            assertInvalid
                ( baseProfileUpsert
                    { apuYoutubeUrl =
                        Just ("https://youtube.com/watch" <> T.singleton '\x202E')
                    }
                )
                "youtubeUrl must not contain whitespace"
            assertInvalid
                (baseProfileUpsert { apuSpotifyUrl = Just "https://" })
                "spotifyUrl must be an absolute public http or https URL"
            assertInvalid
                (baseProfileUpsert { apuHeroImageUrl = Just "http://localhost:5173/hero.jpg" })
                "heroImageUrl must be an absolute public http or https URL"
            assertInvalid
                (baseProfileUpsert { apuWebsiteUrl = Just "https://artist.example@evil.test" })
                "websiteUrl must be an absolute public http or https URL"
            assertInvalid
                (baseProfileUpsert { apuFeaturedVideoUrl = Just "https://127.0.0.1/private" })
                "featuredVideoUrl must be an absolute public http or https URL"
            assertInvalid
                (baseProfileUpsert { apuFeaturedVideoUrl = Just (T.replicate 2049 "a") })
                "featuredVideoUrl must be 2048 characters or fewer"

        it "returns an initialized profile when none exists" $ do
            dto <- runInMemory $ do
                partyId <- insertParty "Aurora"
                loadOrCreateArtistProfileDTO partyId
            apDisplayName dto `shouldBe` "Aurora"
            apFollowerCount dto `shouldBe` 0
            apSlug dto `shouldBe` Nothing
            apHasUserAccount dto `shouldBe` False

        it "upserts profile data and reports follower counts" $ do
            dto <- runInMemory $ do
                now <- liftIO getCurrentTime
                artistId <- insertParty "Los Mentores"
                insertFanFollow artistId "Carla"
                insertFanFollow artistId "Edu"
                let payload =
                        ArtistProfileUpsert
                            { apuArtistId = fromSqlKey artistId
                            , apuDisplayName = Just "Los Mentores"
                            , apuSlug = Just "los-mentores"
                            , apuBio = Just "Fusionando ritmos latinos con neo soul."
                            , apuCity = Just "Quito"
                            , apuHeroImageUrl = Just "https://cdn.tdf/hero.jpg"
                            , apuSpotifyArtistId = Just "spotify-123"
                            , apuSpotifyUrl = Just "https://open.spotify.com/artist/spotify-123"
                            , apuYoutubeChannelId = Just "yt-chan"
                            , apuYoutubeUrl = Just "https://youtube.com/@tdf"
                            , apuWebsiteUrl = Just "https://tdfrecords.com/mentores"
                            , apuFeaturedVideoUrl = Just "https://youtube.com/watch?v=123"
                            , apuGenreIds = [latinGenreId, soulGenreId]
                            , apuHighlights = Just "Ganadores del IMAGINE 2024"
                            }
                upsertArtistProfileRecord artistId payload now >>= requireRight
            apSlug dto `shouldBe` Just "los-mentores"
            apCity dto `shouldBe` Just "Quito"
            apFollowerCount dto `shouldBe` 2
            apSpotifyUrl dto `shouldBe` Just "https://open.spotify.com/artist/spotify-123"
            apGenreIds dto `shouldBe` [latinGenreId, soulGenreId]
            apGenres dto `shouldBe` Just "Latina, Soul"
            apHasUserAccount dto `shouldBe` False

        it "rejects duplicate genre ids before creating a profile or memberships" $ do
            (result, profileCount, membershipCount) <- runInMemory $ do
                now <- liftIO getCurrentTime
                artistId <- insertParty "Duplicado"
                result <-
                    upsertArtistProfileRecord
                        artistId
                        (baseProfileUpsert
                            { apuArtistId = fromSqlKey artistId
                            , apuGenreIds = [latinGenreId, latinGenreId]
                            })
                        now
                profileCount <- count [ArtistProfileArtistPartyId ==. artistId]
                membershipCount <-
                    count [ArtistProfileGenreMembershipArtistPartyId ==. artistId]
                pure (result, profileCount, membershipCount)

            expectLeftContaining "must not contain duplicates" result
            profileCount `shouldBe` 0
            membershipCount `shouldBe` 0

        it "rejects unknown genre ids before creating a profile or memberships" $ do
            let unknownGenreId = requiredUuid "10000000-0000-4000-8000-000000000099"
            (result, profileCount, membershipCount) <- runInMemory $ do
                now <- liftIO getCurrentTime
                artistId <- insertParty "Desconocido"
                result <-
                    upsertArtistProfileRecord
                        artistId
                        (baseProfileUpsert
                            { apuArtistId = fromSqlKey artistId
                            , apuGenreIds = [unknownGenreId]
                            })
                        now
                profileCount <- count [ArtistProfileArtistPartyId ==. artistId]
                membershipCount <-
                    count [ArtistProfileGenreMembershipArtistPartyId ==. artistId]
                pure (result, profileCount, membershipCount)

            expectLeftContaining "unknown genre id" result
            profileCount `shouldBe` 0
            membershipCount `shouldBe` 0

        it "rejects inactive genre ids before creating a profile or memberships" $ do
            (result, profileCount, membershipCount) <- runInMemory $ do
                rawExecute
                    "UPDATE genre SET active = 0 WHERE id = ?"
                    [toPersistValue latinGenreId]
                now <- liftIO getCurrentTime
                artistId <- insertParty "Inactivo"
                result <-
                    upsertArtistProfileRecord
                        artistId
                        (baseProfileUpsert
                            { apuArtistId = fromSqlKey artistId
                            , apuGenreIds = [latinGenreId]
                            })
                        now
                profileCount <- count [ArtistProfileArtistPartyId ==. artistId]
                membershipCount <-
                    count [ArtistProfileGenreMembershipArtistPartyId ==. artistId]
                pure (result, profileCount, membershipCount)

            expectLeftContaining "is not active and published" result
            profileCount `shouldBe` 0
            membershipCount `shouldBe` 0

        it "trims optional artist profile text fields and drops explicit blanks instead of storing whitespace-only data" $ do
            dto <- runInMemory $ do
                now <- liftIO getCurrentTime
                artistId <- insertParty "   Mentores del Aire   "
                let payload =
                        ArtistProfileUpsert
                            { apuArtistId = fromSqlKey artistId
                            , apuDisplayName = Just "  Mentores del Aire  "
                            , apuSlug = Just "  mentores-del-aire  "
                            , apuBio = Just "   "
                            , apuCity = Just "  Quito  "
                            , apuHeroImageUrl = Just "  https://cdn.tdf/hero.jpg  "
                            , apuSpotifyArtistId = Just "  spotify-456  "
                            , apuSpotifyUrl = Just "   "
                            , apuYoutubeChannelId = Just "  yt-mentores  "
                            , apuYoutubeUrl = Just " https://youtube.com/@mentores "
                            , apuWebsiteUrl = Just "   "
                            , apuFeaturedVideoUrl = Just "  https://youtube.com/watch?v=456  "
                            , apuGenreIds = [latinGenreId]
                            , apuHighlights = Just "   "
                            }
                upsertArtistProfileRecord artistId payload now >>= requireRight

            apDisplayName dto `shouldBe` "Mentores del Aire"
            apSlug dto `shouldBe` Just "mentores-del-aire"
            apBio dto `shouldBe` Nothing
            apCity dto `shouldBe` Just "Quito"
            apHeroImageUrl dto `shouldBe` Just "https://cdn.tdf/hero.jpg"
            apSpotifyArtistId dto `shouldBe` Just "spotify-456"
            apSpotifyUrl dto `shouldBe` Nothing
            apYoutubeChannelId dto `shouldBe` Just "yt-mentores"
            apYoutubeUrl dto `shouldBe` Just "https://youtube.com/@mentores"
            apWebsiteUrl dto `shouldBe` Nothing
            apFeaturedVideoUrl dto `shouldBe` Just "https://youtube.com/watch?v=456"
            apGenreIds dto `shouldBe` [latinGenreId]
            apGenres dto `shouldBe` Just "Latina"
            apHighlights dto `shouldBe` Nothing

        it "loads public artist profiles directly by slug" $ do
            dto <- runInMemory $ do
                now <- liftIO getCurrentTime
                artistId <- insertParty "Los Mentores"
                _ <- upsertArtistProfileRecord
                    artistId
                    (baseProfileUpsert
                        { apuArtistId = fromSqlKey artistId
                        , apuSlug = Just "los-mentores"
                        , apuCity = Just "Quito"
                        })
                    now
                loadArtistProfileBySlugDTO "LOS-MENTORES"

            fmap apArtistId dto `shouldSatisfy` maybe False (> 0)
            fmap apSlug dto `shouldBe` Just (Just "los-mentores")

        it "filters artist discovery by free text and genre" $ do
            results <- runInMemory $ do
                now <- liftIO getCurrentTime
                mentoresId <- insertParty "Los Mentores"
                ruidoId <- insertParty "Ruido Blanco"
                _ <- upsertArtistProfileRecord
                    mentoresId
                    (baseProfileUpsert
                        { apuArtistId = fromSqlKey mentoresId
                        , apuDisplayName = Just "Los Mentores"
                        , apuSlug = Just "los-mentores"
                        , apuCity = Just "Quito"
                        , apuGenreIds = [soulGenreId, latinGenreId]
                        })
                    now
                _ <- upsertArtistProfileRecord
                    ruidoId
                    (baseProfileUpsert
                        { apuArtistId = fromSqlKey ruidoId
                        , apuDisplayName = Just "Ruido Blanco"
                        , apuSlug = Just "ruido-blanco"
                        , apuCity = Just "Guayaquil"
                        , apuGenreIds = [rockGenreId]
                        })
                    now
                searchArtistProfilesDTO (Just "quito") (Just soulGenreId)

            map apDisplayName results `shouldBe` ["Los Mentores"]

-- Helpers

baseProfileUpsert :: ArtistProfileUpsert
baseProfileUpsert =
    ArtistProfileUpsert
        { apuArtistId = 42
        , apuDisplayName = Just "Los Mentores"
        , apuSlug = Nothing
        , apuBio = Nothing
        , apuCity = Nothing
        , apuHeroImageUrl = Nothing
        , apuSpotifyArtistId = Nothing
        , apuSpotifyUrl = Nothing
        , apuYoutubeChannelId = Nothing
        , apuYoutubeUrl = Nothing
        , apuWebsiteUrl = Nothing
        , apuFeaturedVideoUrl = Nothing
        , apuGenreIds = []
        , apuHighlights = Nothing
        }

latinGenreId, soulGenreId, rockGenreId :: UUID
latinGenreId = requiredUuid "10000000-0000-4000-8000-000000000001"
soulGenreId = requiredUuid "10000000-0000-4000-8000-000000000002"
rockGenreId = requiredUuid "10000000-0000-4000-8000-000000000003"

catalogId, publishedStateId, workflowId :: UUID
catalogId = requiredUuid "20000000-0000-4000-8000-000000000001"
publishedStateId = requiredUuid "20000000-0000-4000-8000-000000000002"
workflowId = requiredUuid "20000000-0000-4000-8000-000000000003"

requiredUuid :: Text -> UUID
requiredUuid raw =
    case UUID.fromText raw of
        Just value -> value
        Nothing -> error ("invalid test UUID: " <> T.unpack raw)

requireRight :: (MonadFail m) => Either Text a -> m a
requireRight = either (fail . T.unpack) pure

expectLeftContaining :: (Show a) => String -> Either Text a -> Expectation
expectLeftContaining expected result =
    case result of
        Left err -> T.unpack err `shouldContain` expected
        Right value ->
            expectationFailure
                ("Expected validation failure containing " <> show expected <> ", got: " <> show value)

runInMemory :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runInMemory action =
    runSqlite ":memory:" $ do
        initializeTestSchema
        action

initializeTestSchema :: (MonadIO m) => SqlPersistT m ()
initializeTestSchema = do
    rawExecute "PRAGMA foreign_keys = ON" []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"party\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"legal_name\" VARCHAR NULL,\
        \\"display_name\" VARCHAR NOT NULL,\
        \\"is_org\" BOOLEAN NOT NULL,\
        \\"tax_id\" VARCHAR NULL,\
        \\"primary_email\" VARCHAR NULL,\
        \\"primary_phone\" VARCHAR NULL,\
        \\"whatsapp\" VARCHAR NULL,\
        \\"instagram\" VARCHAR NULL,\
        \\"emergency_contact\" VARCHAR NULL,\
        \\"notes\" VARCHAR NULL,\
        \\"stripe_customer_id\" VARCHAR NULL,\
        \\"country_code\" VARCHAR NULL,\
        \\"country_id\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"user_credential\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"party_id\" INTEGER NOT NULL,\
        \\"username\" VARCHAR NOT NULL,\
        \\"password_hash\" VARCHAR NOT NULL,\
        \\"active\" BOOLEAN NOT NULL,\
        \CONSTRAINT \"unique_credential_username\" UNIQUE (\"username\"),\
        \FOREIGN KEY(\"party_id\") REFERENCES \"party\"(\"id\") ON DELETE CASCADE\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"artist_profile\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"artist_party_id\" INTEGER NOT NULL,\
        \\"slug\" VARCHAR NULL,\
        \\"bio\" VARCHAR NULL,\
        \\"city\" VARCHAR NULL,\
        \\"country_code\" VARCHAR NULL,\
        \\"country_id\" VARCHAR NULL,\
        \\"hero_image_url\" VARCHAR NULL,\
        \\"spotify_artist_id\" VARCHAR NULL,\
        \\"spotify_url\" VARCHAR NULL,\
        \\"youtube_channel_id\" VARCHAR NULL,\
        \\"youtube_url\" VARCHAR NULL,\
        \\"website_url\" VARCHAR NULL,\
        \\"featured_video_url\" VARCHAR NULL,\
        \\"genres\" VARCHAR NULL,\
        \\"highlights\" VARCHAR NULL,\
        \\"stripe_account_id\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NULL,\
        \CONSTRAINT \"unique_artist_profile\" UNIQUE (\"artist_party_id\"),\
        \FOREIGN KEY(\"artist_party_id\") REFERENCES \"party\"(\"id\") ON DELETE CASCADE\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"artist_profile_enrichment\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"artist_party_id\" INTEGER NOT NULL,\
        \\"official_name\" VARCHAR NULL,\
        \\"country\" VARCHAR NULL,\
        \\"instagram_url\" VARCHAR NULL,\
        \\"social_links\" VARCHAR NULL,\
        \\"discography\" VARCHAR NULL,\
        \\"achievements\" VARCHAR NULL,\
        \\"hero_original_url\" VARCHAR NULL,\
        \\"hero_square_url\" VARCHAR NULL,\
        \\"hero_landscape_url\" VARCHAR NULL,\
        \\"hero_responsive_urls\" VARCHAR NULL,\
        \\"hero_focal_point\" VARCHAR NULL,\
        \\"last_verified_at\" TIMESTAMP NULL,\
        \\"confidence\" REAL NULL,\
        \\"review_status\" VARCHAR NOT NULL DEFAULT 'unverified',\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NOT NULL,\
        \CONSTRAINT \"unique_artist_profile_enrichment\" UNIQUE (\"artist_party_id\"),\
        \FOREIGN KEY(\"artist_party_id\") REFERENCES \"party\"(\"id\") ON DELETE CASCADE\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"fan_follow\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"fan_party_id\" INTEGER NOT NULL,\
        \\"artist_party_id\" INTEGER NOT NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \CONSTRAINT \"unique_fan_follow\" UNIQUE (\"fan_party_id\", \"artist_party_id\"),\
        \FOREIGN KEY(\"fan_party_id\") REFERENCES \"party\"(\"id\") ON DELETE CASCADE,\
        \FOREIGN KEY(\"artist_party_id\") REFERENCES \"party\"(\"id\") ON DELETE CASCADE\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"workflow_state\" (\
        \\"id\" VARCHAR PRIMARY KEY, \"workflow_id\" VARCHAR NOT NULL,\
        \\"code\" VARCHAR NOT NULL, \"name_es\" VARCHAR NOT NULL, \"name_en\" VARCHAR NOT NULL,\
        \\"description_es\" VARCHAR NULL, \"description_en\" VARCHAR NULL,\
        \\"sort_order\" INTEGER NOT NULL, \"terminal\" BOOLEAN NOT NULL, \"active\" BOOLEAN NOT NULL,\
        \\"created_at\" TIMESTAMP NOT NULL, \"updated_at\" TIMESTAMP NOT NULL, \"version\" INTEGER NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"catalog_definition\" (\
        \\"id\" VARCHAR PRIMARY KEY, \"code\" VARCHAR NOT NULL, \"classification\" VARCHAR NOT NULL,\
        \\"entity_kind\" VARCHAR NOT NULL, \"name_es\" VARCHAR NOT NULL, \"name_en\" VARCHAR NOT NULL,\
        \\"description_es\" VARCHAR NULL, \"description_en\" VARCHAR NULL,\
        \\"public_read\" BOOLEAN NOT NULL, \"sensitive\" BOOLEAN NOT NULL, \"ordering_mode\" VARCHAR NOT NULL,\
        \\"workflow_id\" VARCHAR NOT NULL, \"source_name\" VARCHAR NULL, \"source_version\" VARCHAR NULL,\
        \\"source_effective_date\" DATE NULL, \"last_synced_at\" TIMESTAMP NULL,\
        \\"cache_revision\" INTEGER NOT NULL, \"active\" BOOLEAN NOT NULL,\
        \\"created_at\" TIMESTAMP NOT NULL, \"updated_at\" TIMESTAMP NOT NULL, \"version\" INTEGER NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"genre\" (\
        \\"id\" VARCHAR PRIMARY KEY, \"catalog_id\" VARCHAR NOT NULL, \"code\" VARCHAR NOT NULL,\
        \\"parent_id\" VARCHAR NULL, \"name_es\" VARCHAR NOT NULL, \"name_en\" VARCHAR NOT NULL,\
        \\"description_es\" VARCHAR NULL, \"description_en\" VARCHAR NULL, \"current_slug\" VARCHAR NULL,\
        \\"sort_order\" INTEGER NOT NULL, \"active\" BOOLEAN NOT NULL, \"workflow_state_id\" VARCHAR NOT NULL,\
        \\"created_by\" INTEGER NULL, \"updated_by\" INTEGER NULL, \"approved_by\" INTEGER NULL,\
        \\"created_at\" TIMESTAMP NOT NULL, \"updated_at\" TIMESTAMP NOT NULL,\
        \\"effective_from\" DATE NULL, \"effective_until\" DATE NULL, \"published_revision\" INTEGER NOT NULL,\
        \\"deprecated_at\" TIMESTAMP NULL, \"replacement_id\" VARCHAR NULL, \"external_code\" VARCHAR NULL,\
        \\"external_source\" VARCHAR NULL, \"source_version\" VARCHAR NULL, \"usage_count\" INTEGER NOT NULL,\
        \\"version\" INTEGER NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"artist_profile_genre_membership\" (\
        \\"artist_party_id\" INTEGER NOT NULL, \"genre_id\" VARCHAR NOT NULL,\
        \\"sort_order\" INTEGER NOT NULL, \"created_at\" TIMESTAMP NOT NULL,\
        \PRIMARY KEY (\"artist_party_id\", \"genre_id\")\
        \)"
        []
    rawExecute
        "INSERT INTO workflow_state (id, workflow_id, code, name_es, name_en, description_es, description_en, sort_order, terminal, active, created_at, updated_at, version) VALUES (?, ?, 'published', 'Publicado', 'Published', NULL, NULL, 0, 0, 1, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, 1)"
        [toPersistValue publishedStateId, toPersistValue workflowId]
    rawExecute
        "INSERT INTO catalog_definition (id, code, classification, entity_kind, name_es, name_en, description_es, description_en, public_read, sensitive, ordering_mode, workflow_id, source_name, source_version, source_effective_date, last_synced_at, cache_revision, active, created_at, updated_at, version) VALUES (?, 'genres', 'dynamic-business-catalog', 'genre', 'Géneros', 'Genres', NULL, NULL, 1, 0, 'manual', ?, NULL, NULL, NULL, NULL, 1, 1, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, 1)"
        [toPersistValue catalogId, toPersistValue workflowId]
    insertGenreRow latinGenreId "latin" "Latina" "Latin" 0
    insertGenreRow soulGenreId "soul" "Soul" "Soul" 1
    insertGenreRow rockGenreId "rock" "Rock" "Rock" 2
  where
    insertGenreRow :: (MonadIO m) => UUID -> Text -> Text -> Text -> Int64 -> SqlPersistT m ()
    insertGenreRow genreId code nameEs nameEn sortOrder =
        rawExecute
            "INSERT INTO genre (id, catalog_id, code, parent_id, name_es, name_en, description_es, description_en, current_slug, sort_order, active, workflow_state_id, created_by, updated_by, approved_by, created_at, updated_at, effective_from, effective_until, published_revision, deprecated_at, replacement_id, external_code, external_source, source_version, usage_count, version) VALUES (?, ?, ?, NULL, ?, ?, NULL, NULL, NULL, ?, 1, ?, NULL, NULL, NULL, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, NULL, NULL, 1, NULL, NULL, NULL, NULL, NULL, 0, 1)"
            [ toPersistValue genreId
            , toPersistValue catalogId
            , PersistText code
            , PersistText nameEs
            , PersistText nameEn
            , PersistInt64 sortOrder
            , toPersistValue publishedStateId
            ]

insertParty :: (MonadIO m) => Text -> SqlPersistT m PartyId
insertParty name = do
    now <- liftIO getCurrentTime
    insert
        Party
            { partyLegalName = Nothing
            , partyDisplayName = name
            , partyIsOrg = False
            , partyTaxId = Nothing
            , partyPrimaryEmail = Nothing
            , partyPrimaryPhone = Nothing
            , partyWhatsapp = Nothing
            , partyInstagram = Nothing
            , partyEmergencyContact = Nothing
            , partyNotes = Nothing
            , partyStripeCustomerId = Nothing
            , partyCountryCode = Nothing
            , partyCountryId = Nothing
            , partyCreatedAt = now
            }

insertFanFollow :: (MonadIO m) => PartyId -> Text -> SqlPersistT m ()
insertFanFollow artistId fanName = do
    fanId <- insertParty fanName
    now <- liftIO getCurrentTime
    _ <-
        insert
            FanFollow
                { fanFollowFanPartyId = fanId
                , fanFollowArtistPartyId = artistId
                , fanFollowCreatedAt = now
                }
    pure ()

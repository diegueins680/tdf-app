{-# LANGUAGE OverloadedStrings #-}

module TDF.Artists.EnrichmentSpec (spec) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Text (Text)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import Database.Persist (Entity(..), Filter, getBy, insert, selectList, (==.))
import Database.Persist.Sql (SqlPersistT, fromSqlKey, rawExecute, runSqlPool, toSqlKey)
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec

import TDF.API.Admin
  ( ArtistEnrichmentDecision(..)
  , ArtistEnrichmentRunDTO(..)
  , ArtistEnrichmentRunUpdate(..)
  , ArtistEnrichmentSuggestionCreate(..)
  , ArtistEnrichmentSuggestionDTO(..)
  , ArtistIdentityCandidateDTO(..)
  )
import TDF.Artists.Enrichment
  ( DiscoveryReference(..)
  , automaticIdentityMatchAllowed
  , artistNameAliasCandidate
  , independentSignalCount
  , matchConfidence
  , normalizeDiscoveredName
  , normalizeArtistName
  , persistDiscoveryReference
  , slugifyArtistName
  , createArtistSuggestion
  , decideArtistIdentityCandidate
  , decideArtistSuggestion
  , updateArtistEnrichmentRun
  )
import TDF.Models
  ( ArtistEnrichmentRun(..)
  , ArtistFieldChange
  , ArtistIdentityCandidate(..)
  , ArtistInventoryReference(..)
  , ArtistInventoryReferenceId
  , EntityField(ArtistInventoryReferenceNormalizedName)
  , ArtistProfile(..)
  , Party(..)
  , PartyId
  , Unique(UniqueArtistProfile)
  )

spec :: Spec
spec = describe "artist enrichment identity policy" $ do
  it "normalizes aliases without losing accents from the stored original" $ do
    normalizeArtistName "  Diego   SAÁ " `shouldBe` "diego saa"
    slugifyArtistName "Diego Saá" `shouldBe` "diego-saa"

  it "removes only the live-session input-list UI suffix from the match key" $ do
    normalizeDiscoveredName "live_session_intake.band_name" "Machaka - Input List"
      `shouldBe` "machaka"
    normalizeDiscoveredName "pipeline_card.artist" "Machaka - Input List"
      `shouldBe` "machaka input list"

  it "flags formatting aliases for review without treating unrelated names as matches" $ do
    artistNameAliasCandidate "Skankafe" "Skanka Fe" `shouldBe` True
    artistNameAliasCandidate "E Quimika Soul" "Quimika Soul" `shouldBe` True
    artistNameAliasCandidate "Juano Ledesma (duplicado)" "Juano Ledesma" `shouldBe` True
    artistNameAliasCandidate "Lord Ethnic" "Lord Invader" `shouldBe` False

  it "counts relationship families once even when several rows repeat them" $
    independentSignalCount
      [ "fan_follow.artist_party_id"
      , "fan_club.artist_party_id"
      , "event_artist"
      , "event_artist"
      ] `shouldBe` 2

  it "counts every artist-bearing platform relationship family" $
    independentSignalCount
      [ "service_order.artist_id"
      , "service_storefront_order.artist_name"
      , "catalog_credit.party_id"
      , "artist_promo_slot.artist_party_id"
      , "band_member.party_id"
      , "engagement_event.target_artist_id"
      , "event_live_broadcast.artist_id"
      , "artist_genre.artist_id"
      , "artist_follow.artist_id"
      , "social_sync_account.artist_profile_id"
      ] `shouldBe` 10

  it "does not auto-match from name or one source alone" $ do
    automaticIdentityMatchAllowed ["artist_profile"] 1 `shouldBe` False
    automaticIdentityMatchAllowed ["social_artist_profile"] 0 `shouldBe` False

  it "allows two independent signals only when there is no homonym" $ do
    automaticIdentityMatchAllowed ["social_artist_profile", "event_artist"] 1
      `shouldBe` True
    automaticIdentityMatchAllowed ["social_artist_profile", "event_artist"] 2
      `shouldBe` False

  it "keeps confidence bounded and rewards stable TDF identities" $ do
    matchConfidence ["social_artist_profile", "event_artist"] False
      `shouldSatisfy` (\value -> value >= 0.779 && value <= 0.99)
    matchConfidence ["artist_profile", "artist_release"] True
      `shouldSatisfy` (\value -> value >= 0.919 && value <= 0.99)

  it "preserves resolved identities when an indirect reference is rediscovered" $ do
    pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
    (promoted, rediscovered) <- runSqlPool (do
      createInventorySchema
      let partyId = toSqlKey 42
          direct = DiscoveryReference "event_artist" "event-1" "Stable Artist"
            (Just partyId) (Just 7)
          indirect = DiscoveryReference "event_artist" "event-1" "Stable Artist"
            Nothing Nothing
      _ <- persistDiscoveryReference fixedTime indirect
      promotedRow <- persistDiscoveryReference fixedTime direct
      rediscoveredRow <- persistDiscoveryReference fixedTime indirect
      pure (entityVal promotedRow, entityVal rediscoveredRow)) pool
    artistInventoryReferenceArtistPartyId promoted `shouldBe` Just (toSqlKey 42)
    artistInventoryReferenceSocialArtistId promoted `shouldBe` Just 7
    artistInventoryReferenceArtistPartyId rediscovered `shouldBe` Just (toSqlKey 42)
    artistInventoryReferenceSocialArtistId rediscovered `shouldBe` Just 7

  it "claims external execution atomically and rejects a recent second runner" $ do
    pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
    runId <- runSqlPool (do
      createRunSchema
      insert ArtistEnrichmentRun
        { artistEnrichmentRunRunKey = "atomic-claim"
        , artistEnrichmentRunMode = "production"
        , artistEnrichmentRunScope = "full"
        , artistEnrichmentRunRequestedArtistId = Nothing
        , artistEnrichmentRunStatus = "completed"
        , artistEnrichmentRunPhase = "discovery"
        , artistEnrichmentRunCheckpoint = Nothing
        , artistEnrichmentRunCounters = Nothing
        , artistEnrichmentRunErrorSummary = Nothing
        , artistEnrichmentRunStartedAt = fixedTime
        , artistEnrichmentRunHeartbeatAt = fixedTime
        , artistEnrichmentRunFinishedAt = Just fixedTime
        }) pool
    first <- runSqlPool
      (updateArtistEnrichmentRun (fromSqlKey runId) externalClaim) pool
    second <- try
      (runSqlPool (updateArtistEnrichmentRun (fromSqlKey runId) externalClaim) pool)
      :: IO (Either SomeException ArtistEnrichmentRunDTO)
    aerStatus first `shouldBe` "running"
    aerPhase first `shouldBe` "external_research_claim"
    second `shouldSatisfy` either (const True) (const False)

  it "audits idempotent approval, rejection, and stale-current protection" $ do
    pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
    (rejected, approved, stale, storedBio, changes) <- runSqlPool (do
      createReviewSchema
      let now = UTCTime (fromGregorian 2026 8 5) (secondsToDiffTime 0)
      partyId <- insert Party
        { partyLegalName = Nothing
        , partyDisplayName = "Audit Artist"
        , partyIsOrg = True
        , partyTaxId = Nothing
        , partyPrimaryEmail = Nothing
        , partyPrimaryPhone = Nothing
        , partyWhatsapp = Nothing
        , partyInstagram = Nothing
        , partyEmergencyContact = Nothing
        , partyNotes = Nothing
        , partyStripeCustomerId = Nothing
        , partyCreatedAt = now
        }
      _ <- insert ArtistProfile
        { artistProfileArtistPartyId = partyId
        , artistProfileSlug = Just "audit-artist"
        , artistProfileBio = Nothing
        , artistProfileCity = Nothing
        , artistProfileHeroImageUrl = Nothing
        , artistProfileSpotifyArtistId = Nothing
        , artistProfileSpotifyUrl = Nothing
        , artistProfileYoutubeChannelId = Nothing
        , artistProfileYoutubeUrl = Nothing
        , artistProfileWebsiteUrl = Nothing
        , artistProfileFeaturedVideoUrl = Nothing
        , artistProfileGenres = Nothing
        , artistProfileHighlights = Nothing
        , artistProfileStripeAccountId = Nothing
        , artistProfileCreatedAt = now
        , artistProfileUpdatedAt = Just now
        }
      rejectSuggestion <- createArtistSuggestion now (mkSuggestion partyId Nothing "Rejected bio")
      rejectedResult <- decideArtistSuggestion "test" partyId (aesId rejectSuggestion)
        ArtistEnrichmentDecision
          { aedDecision = "reject", aedEditedValue = Nothing, aedNote = Just "Not corroborated" }
      approveSuggestion <- createArtistSuggestion now (mkSuggestion partyId Nothing "Approved bio")
      _approvedResult <- decideArtistSuggestion "test" partyId (aesId approveSuggestion)
        ArtistEnrichmentDecision
          { aedDecision = "approve", aedEditedValue = Nothing, aedNote = Just "Two official sources" }
      approvedAgain <- decideArtistSuggestion "test" partyId (aesId approveSuggestion)
        ArtistEnrichmentDecision
          { aedDecision = "approve", aedEditedValue = Nothing, aedNote = Just "Repeated safely" }
      staleSuggestion <- createArtistSuggestion now (mkSuggestion partyId (Just "Outdated bio") "Unsafe overwrite")
      staleResult <- decideArtistSuggestion "test" partyId (aesId staleSuggestion)
        ArtistEnrichmentDecision
          { aedDecision = "approve", aedEditedValue = Nothing, aedNote = Nothing }
      stored <- getBy (UniqueArtistProfile partyId)
      history <- selectList ([] :: [Filter ArtistFieldChange]) []
      pure (rejectedResult, approvedAgain, staleResult,
        stored >>= artistProfileBio . entityVal, history)) pool
    aesStatus rejected `shouldBe` "rejected"
    aesStatus approved `shouldBe` "approved"
    aesStatus stale `shouldBe` "superseded"
    storedBio `shouldBe` Just "Approved bio"
    length changes `shouldBe` 1

  it "supersedes sibling identity candidates after one normalized group is approved" $ do
    pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
    (approved, superseded, resolvedRows, firstPartyId) <- runSqlPool (do
      createReviewSchema
      firstParty <- insert (mkParty "First identity")
      secondParty <- insert (mkParty "Second identity")
      firstInventory <- insert (mkInventory "event_artist" "event-1" "Shared Artist")
      secondInventory <- insert (mkInventory "release_credit" "release-1" "Shared Artist")
      firstCandidate <- insert (mkIdentityCandidate firstInventory firstParty "candidate-a")
      secondCandidate <- insert (mkIdentityCandidate secondInventory secondParty "candidate-b")
      approvedResult <- decideArtistIdentityCandidate firstParty (fromSqlKey firstCandidate)
        ArtistEnrichmentDecision
          { aedDecision = "approve", aedEditedValue = Nothing, aedNote = Just "Corroborated" }
      supersededResult <- decideArtistIdentityCandidate firstParty (fromSqlKey secondCandidate)
        ArtistEnrichmentDecision
          { aedDecision = "approve", aedEditedValue = Nothing, aedNote = Just "Must not reassign" }
      rows <- selectList
        [ArtistInventoryReferenceNormalizedName ==. "shared artist"] []
      pure (approvedResult, supersededResult, rows, firstParty)) pool
    aicStatus approved `shouldBe` "approved"
    aicStatus superseded `shouldBe` "superseded"
    map (artistInventoryReferenceArtistPartyId . entityVal) resolvedRows
      `shouldBe` replicate 2 (Just firstPartyId)

mkParty :: Text -> Party
mkParty displayName = Party
  { partyLegalName = Nothing
  , partyDisplayName = displayName
  , partyIsOrg = True
  , partyTaxId = Nothing
  , partyPrimaryEmail = Nothing
  , partyPrimaryPhone = Nothing
  , partyWhatsapp = Nothing
  , partyInstagram = Nothing
  , partyEmergencyContact = Nothing
  , partyNotes = Nothing
  , partyStripeCustomerId = Nothing
  , partyCreatedAt = fixedTime
  }

mkInventory :: Text -> Text -> Text -> ArtistInventoryReference
mkInventory sourceType sourceId originalName = ArtistInventoryReference
  { artistInventoryReferenceIdempotencyKey = sourceType <> ":" <> sourceId
  , artistInventoryReferenceSourceType = sourceType
  , artistInventoryReferenceSourceRecordId = sourceId
  , artistInventoryReferenceOriginalName = originalName
  , artistInventoryReferenceNormalizedName = "shared artist"
  , artistInventoryReferenceArtistPartyId = Nothing
  , artistInventoryReferenceSocialArtistId = Nothing
  , artistInventoryReferenceAliases = Nothing
  , artistInventoryReferenceEvidence = Nothing
  , artistInventoryReferenceConfidence = Nothing
  , artistInventoryReferenceDisposition = "review"
  , artistInventoryReferenceFirstSeenAt = fixedTime
  , artistInventoryReferenceLastSeenAt = fixedTime
  }

mkIdentityCandidate
  :: ArtistInventoryReferenceId -> PartyId -> Text -> ArtistIdentityCandidate
mkIdentityCandidate inventoryId partyId idem = ArtistIdentityCandidate
  { artistIdentityCandidateInventoryReferenceId = inventoryId
  , artistIdentityCandidateArtistPartyId = Just partyId
  , artistIdentityCandidateProvider = "test"
  , artistIdentityCandidateExternalId = Just idem
  , artistIdentityCandidateCandidateUrl = Nothing
  , artistIdentityCandidateEvidence = "{\"signals\":[\"event\",\"release\"]}"
  , artistIdentityCandidateConfidence = 0.9
  , artistIdentityCandidateStatus = "pending"
  , artistIdentityCandidateIdempotencyKey = idem
  , artistIdentityCandidateCreatedAt = fixedTime
  , artistIdentityCandidateUpdatedAt = fixedTime
  , artistIdentityCandidateDecidedAt = Nothing
  , artistIdentityCandidateDecidedBy = Nothing
  , artistIdentityCandidateDecisionNote = Nothing
  }

mkSuggestion :: PartyId -> Maybe Text -> Text -> ArtistEnrichmentSuggestionCreate
mkSuggestion partyId current proposed = ArtistEnrichmentSuggestionCreate
  { aescArtistId = Just (fromSqlKey partyId)
  , aescInventoryReferenceId = Nothing
  , aescFieldName = "bio"
  , aescCurrentValue = current
  , aescProposedValue = Just proposed
  , aescConfidence = 0.95
  , aescAutoPublish = Just False
  , aescEvidence = "{\"signals\":[\"official_website\",\"spotify_release\"]}"
  }

createReviewSchema :: MonadIO m => SqlPersistT m ()
createReviewSchema = do
  rawExecute "CREATE TABLE party (id INTEGER PRIMARY KEY, legal_name TEXT, display_name TEXT NOT NULL, is_org BOOLEAN NOT NULL, tax_id TEXT, primary_email TEXT, primary_phone TEXT, whatsapp TEXT, instagram TEXT, emergency_contact TEXT, notes TEXT, stripe_customer_id TEXT, created_at TIMESTAMP NOT NULL)" []
  rawExecute "CREATE TABLE party_role (id INTEGER PRIMARY KEY, party_id INTEGER NOT NULL, role TEXT NOT NULL, active BOOLEAN NOT NULL, UNIQUE(party_id, role))" []
  rawExecute "CREATE TABLE artist_profile (id INTEGER PRIMARY KEY, artist_party_id INTEGER NOT NULL UNIQUE, slug TEXT, bio TEXT, city TEXT, hero_image_url TEXT, spotify_artist_id TEXT, spotify_url TEXT, youtube_channel_id TEXT, youtube_url TEXT, website_url TEXT, featured_video_url TEXT, genres TEXT, highlights TEXT, stripe_account_id TEXT, created_at TIMESTAMP NOT NULL, updated_at TIMESTAMP)" []
  rawExecute "CREATE TABLE artist_profile_enrichment (id INTEGER PRIMARY KEY, artist_party_id INTEGER NOT NULL UNIQUE, official_name TEXT, country TEXT, instagram_url TEXT, social_links TEXT, discography TEXT, achievements TEXT, hero_original_url TEXT, hero_square_url TEXT, hero_landscape_url TEXT, hero_responsive_urls TEXT, hero_focal_point TEXT, last_verified_at TIMESTAMP, confidence REAL, review_status TEXT NOT NULL DEFAULT 'unverified', created_at TIMESTAMP NOT NULL, updated_at TIMESTAMP NOT NULL)" []
  rawExecute "CREATE TABLE artist_inventory_reference (id INTEGER PRIMARY KEY, idempotency_key TEXT NOT NULL UNIQUE, source_type TEXT NOT NULL, source_record_id TEXT NOT NULL, original_name TEXT NOT NULL, normalized_name TEXT NOT NULL, artist_party_id INTEGER, social_artist_id INTEGER, aliases TEXT, evidence TEXT, confidence REAL, disposition TEXT NOT NULL, first_seen_at TIMESTAMP NOT NULL, last_seen_at TIMESTAMP NOT NULL)" []
  rawExecute "CREATE TABLE artist_identity_candidate (id INTEGER PRIMARY KEY, inventory_reference_id INTEGER NOT NULL, artist_party_id INTEGER, provider TEXT NOT NULL, external_id TEXT, candidate_url TEXT, evidence TEXT NOT NULL, confidence REAL NOT NULL, status TEXT NOT NULL, idempotency_key TEXT NOT NULL UNIQUE, created_at TIMESTAMP NOT NULL, updated_at TIMESTAMP NOT NULL, decided_at TIMESTAMP, decided_by INTEGER, decision_note TEXT)" []
  rawExecute "CREATE TABLE artist_enrichment_suggestion (id INTEGER PRIMARY KEY, artist_party_id INTEGER, inventory_reference_id INTEGER, field_name TEXT NOT NULL, current_value TEXT, proposed_value TEXT, confidence REAL NOT NULL, status TEXT NOT NULL, auto_publish BOOLEAN NOT NULL, evidence TEXT NOT NULL, idempotency_key TEXT NOT NULL UNIQUE, created_at TIMESTAMP NOT NULL, updated_at TIMESTAMP NOT NULL, decided_at TIMESTAMP, decided_by INTEGER, decision_note TEXT)" []
  rawExecute "CREATE TABLE artist_field_change (id INTEGER PRIMARY KEY, artist_party_id INTEGER NOT NULL, suggestion_id INTEGER, field_name TEXT NOT NULL, previous_value TEXT, new_value TEXT, evidence TEXT NOT NULL, confidence REAL NOT NULL, actor TEXT NOT NULL, changed_at TIMESTAMP NOT NULL, idempotency_key TEXT NOT NULL UNIQUE)" []

createInventorySchema :: MonadIO m => SqlPersistT m ()
createInventorySchema =
  rawExecute "CREATE TABLE artist_inventory_reference (id INTEGER PRIMARY KEY, idempotency_key TEXT NOT NULL UNIQUE, source_type TEXT NOT NULL, source_record_id TEXT NOT NULL, original_name TEXT NOT NULL, normalized_name TEXT NOT NULL, artist_party_id INTEGER, social_artist_id INTEGER, aliases TEXT, evidence TEXT, confidence REAL, disposition TEXT NOT NULL, first_seen_at TIMESTAMP NOT NULL, last_seen_at TIMESTAMP NOT NULL)" []

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 8 5) (secondsToDiffTime 0)

externalClaim :: ArtistEnrichmentRunUpdate
externalClaim = ArtistEnrichmentRunUpdate
  { aeruStatus = Just "running"
  , aeruPhase = Just "external_research_claim"
  , aeruCheckpoint = Just "{}"
  , aeruCounters = Nothing
  , aeruErrorSummary = Nothing
  }

createRunSchema :: MonadIO m => SqlPersistT m ()
createRunSchema =
  rawExecute "CREATE TABLE artist_enrichment_run (id INTEGER PRIMARY KEY, run_key TEXT NOT NULL UNIQUE, mode TEXT NOT NULL, scope TEXT NOT NULL, requested_artist_id INTEGER, status TEXT NOT NULL, phase TEXT NOT NULL, checkpoint TEXT, counters TEXT, error_summary TEXT, started_at TIMESTAMP NOT NULL, heartbeat_at TIMESTAMP NOT NULL, finished_at TIMESTAMP)" []

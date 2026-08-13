{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module TDF.Artists.Enrichment
  ( DiscoveryReference(..)
  , normalizeArtistName
  , normalizeDiscoveredName
  , artistNameAliasCandidate
  , slugifyArtistName
  , independentSignalCount
  , matchConfidence
  , automaticIdentityMatchAllowed
  , loadArtistEnrichmentOverview
  , runArtistEnrichment
  , runArtistEnrichmentWithKey
  , updateArtistEnrichmentRun
  , createArtistResearchSource
  , createArtistSuggestion
  , createArtistIdentityCandidate
  , decideArtistSuggestion
  , decideArtistSuggestionSet
  , decideArtistIdentityCandidate
  , createArtistMediaAsset
  , persistDiscoveryReference
  ) where

import           Control.Applicative            ((<|>))
import           Control.Monad                  (forM, forM_, unless, when)
import           Control.Monad.IO.Class         (liftIO)
import           Crypto.Hash                    (Digest, SHA256, hash)
import qualified Data.Aeson                     as Aeson
import           Data.Aeson                     (encode, object, (.:?), (.!=), (.=))
import           Data.Aeson.Types               (parseMaybe)
import qualified Data.ByteArray.Encoding        as BAE
import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (isAsciiLower, isAsciiUpper, isDigit, toLower)
import           Data.Int                       (Int64)
import           Data.List                      (nub)
import qualified Data.Map.Strict                as Map
import           Data.Maybe                     (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import           Data.Time                      (UTCTime, addUTCTime, defaultTimeLocale,
                                                  formatTime, getCurrentTime, parseTimeM)
import           Database.Persist
import           Database.Persist.Sql           (SqlBackend, SqlPersistT,
                                                  Single(..), fromSqlKey,
                                                  rawExecute, rawSql, toSqlKey,
                                                  updateWhereCount)
import           Text.Read                      (readMaybe)

import           TDF.API.Admin
import           TDF.Models
import qualified TDF.Models.SocialEventsModels  as Social
import qualified TDF.ModelsExtra                as ME
import qualified TDF.Trials.Server              as TrialsServer

data DiscoveryReference = DiscoveryReference
  { drSourceType     :: Text
  , drSourceRecordId :: Text
  , drOriginalName   :: Text
  , drArtistPartyId  :: Maybe PartyId
  , drSocialArtistId :: Maybe Int64
  } deriving (Eq, Show)

normalizeArtistName :: Text -> Text
normalizeArtistName = T.unwords . T.words . T.map normalizeChar . T.toLower . T.strip
  where
    normalizeChar ch
      | isAsciiLower ch || isDigit ch = ch
      | ch `elem` ("áàâäãå" :: String) = 'a'
      | ch `elem` ("éèêë" :: String) = 'e'
      | ch `elem` ("íìîï" :: String) = 'i'
      | ch `elem` ("óòôöõ" :: String) = 'o'
      | ch `elem` ("úùûü" :: String) = 'u'
      | ch == 'ñ' = 'n'
      | ch == 'ç' = 'c'
      | otherwise = ' '

slugifyArtistName :: Text -> Text
slugifyArtistName raw =
  let atoms = map transliterate (T.unpack (T.toLower raw))
      normalized = collapseHyphens (T.pack atoms)
  in T.take 96 (T.dropAround (== '-') normalized)
  where
    transliterate ch
      | isAsciiLower ch || isDigit ch = ch
      | isAsciiUpper ch = toLower ch
      | ch `elem` ("áàâäãå" :: String) = 'a'
      | ch `elem` ("éèêë" :: String) = 'e'
      | ch `elem` ("íìîï" :: String) = 'i'
      | ch `elem` ("óòôöõ" :: String) = 'o'
      | ch `elem` ("úùûü" :: String) = 'u'
      | ch == 'ñ' = 'n'
      | ch == 'ç' = 'c'
      | otherwise = '-'
    collapseHyphens = T.intercalate "-" . filter (not . T.null) . T.splitOn "-"

signalBucket :: Text -> Text
signalBucket source
  | source == "artist_profile" = "profile"
  | source == "party_role" = "role"
  | source == "artist_release" = "catalog"
  | source == "service_order.artist_id" = "service_order"
  | source == "service_storefront_order.artist_name" = "service_storefront"
  | source == "catalog_credit.party_id" = "catalog_credit"
  | source == "artist_promo_slot.artist_party_id" = "promotion"
  | source == "engagement_event.target_artist_id" = "engagement"
  | "fan_" `T.isPrefixOf` source = "fan_relationship"
  | source == "band" = "band"
  | source == "band_member.party_id" = "band_member"
  | "live_session" `T.isPrefixOf` source = "live_session"
  | source == "pipeline_card.artist" = "pipeline"
  | source == "social_artist_profile" = "social_profile"
  | source `elem` ["event_artist", "event_live_broadcast.artist_id"] = "event"
  | source == "artist_genre.artist_id" = "social_genre"
  | source == "artist_follow.artist_id" = "fan_relationship"
  | source == "external_artist_ref" = "external_event_provider"
  | "social_sync" `T.isPrefixOf` source = "social_sync"
  | otherwise = source

independentSignalCount :: [Text] -> Int
independentSignalCount = Set.size . Set.fromList . map signalBucket

matchConfidence :: [Text] -> Bool -> Double
matchConfidence sources hasStablePartyId =
  min 0.99 $ base + fromIntegral (max 0 (independentSignalCount sources - 1)) * 0.08
  where
    base = if hasStablePartyId then 0.84 else 0.70

-- | Automatic identity publication always needs two genuinely independent
-- signals and must stop on homonyms. Name similarity is intentionally absent.
automaticIdentityMatchAllowed :: [Text] -> Int -> Bool
automaticIdentityMatchAllowed sources candidateIdentityCount =
  candidateIdentityCount <= 1 && independentSignalCount sources >= 2

digestText :: Text -> Text
digestText input =
  TE.decodeUtf8
    (BAE.convertToBase BAE.Base16 (hash (TE.encodeUtf8 input) :: Digest SHA256))

keyText :: Show (Key record) => Key record -> Text
keyText = T.pack . show

intKeyText :: ToBackendKey SqlBackend record => Key record -> Text
intKeyText = T.pack . show . fromSqlKey

partyIdFromText :: Text -> Maybe PartyId
partyIdFromText raw = toSqlKey <$> (readMaybe (T.unpack (T.strip raw)) :: Maybe Int64)

tableExists :: Text -> SqlPersistT IO Bool
tableExists tableName = do
  rows <- rawSql
    "SELECT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema = 'public' AND table_name = ?)"
    [PersistText tableName] :: SqlPersistT IO [Single Bool]
  pure $ case rows of
    [Single present] -> present
    _ -> False

whenTableExists :: Text -> SqlPersistT IO [value] -> SqlPersistT IO [value]
whenTableExists tableName action = do
  present <- tableExists tableName
  if present then action else pure []

-- Input-list submissions use a UI suffix that is not part of the artist's
-- identity. Keep the original name for provenance while matching the base.
normalizeDiscoveredName :: Text -> Text -> Text
normalizeDiscoveredName source raw
  | source == "live_session_intake.band_name"
  , let suffix = " - input list"
  , suffix `T.isSuffixOf` T.toLower (T.strip raw) =
      normalizeArtistName (T.dropEnd (T.length suffix) (T.strip raw))
  | otherwise = normalizeArtistName raw

-- Conservative alias blocking catches formatting and explicit duplicate
-- suffixes. It is used to queue review, never as sufficient publication proof.
artistNameAliasCandidate :: Text -> Text -> Bool
artistNameAliasCandidate left right =
  not (Set.null (aliasForms left `Set.intersection` aliasForms right))
  where
    aliasForms raw = Set.fromList . filter (not . T.null) $
      [ T.concat meaningful
      , T.concat (dropSingleMarker meaningful)
      ]
      where
        meaningful = filter (`notElem` ["duplicate", "duplicado", "test", "delete", "me"])
          (T.words (normalizeArtistName raw))
        dropSingleMarker (token:rest) | T.length token == 1 = rest
        dropSingleMarker tokens = tokens

lookupExistingCandidates :: Map.Map Text [PartyId] -> Text -> [PartyId]
lookupExistingCandidates existingByName normalized = nub
  [ partyId
  | (existingName, partyIds) <- Map.toList existingByName
  , existingName == normalized || artistNameAliasCandidate existingName normalized
  , partyId <- partyIds
  ]

discoverArtistReferences :: SqlPersistT IO [DiscoveryReference]
discoverArtistReferences = do
  profileRefs <- do
    rows <- selectList ([] :: [Filter ArtistProfile]) []
    catMaybes <$> forM rows (\(Entity rowId row) ->
      mkPartyRef "artist_profile" (intKeyText rowId) (artistProfileArtistPartyId row))
  roleRefs <- do
    rows <- selectList
      [ PartyRoleActive ==. True
      , PartyRoleRole <-. [Artist, Artista]
      ] []
    catMaybes <$> forM rows (\(Entity rowId row) ->
      mkPartyRef "party_role" (intKeyText rowId) (partyRolePartyId row))
  releaseRefs <- do
    rows <- selectList ([] :: [Filter ArtistRelease]) []
    catMaybes <$> forM rows (\(Entity rowId row) ->
      mkPartyRef "artist_release" (intKeyText rowId) (artistReleaseArtistPartyId row))
  serviceOrderRefs <- do
    rows <- selectList [ServiceOrderArtistId !=. Nothing] []
    catMaybes <$> forM rows (\(Entity rowId row) ->
      maybe (pure Nothing) (mkPartyRef "service_order.artist_id" (intKeyText rowId))
        (serviceOrderArtistId row))
  fanFollowRefs <- do
    rows <- selectList ([] :: [Filter FanFollow]) []
    catMaybes <$> forM rows (\(Entity rowId row) ->
      mkPartyRef "fan_follow.artist_party_id" (intKeyText rowId) (fanFollowArtistPartyId row))
  fanClubRefs <- do
    rows <- selectList ([] :: [Filter FanClub]) []
    catMaybes <$> forM rows (\(Entity rowId row) ->
      mkPartyRef "fan_club.artist_party_id" (intKeyText rowId) (fanClubArtistPartyId row))
  bandRefs <- do
    rows <- selectList ([] :: [Filter ME.Band]) []
    pure
      [ DiscoveryReference "band" (keyText rowId) (ME.bandName row)
          (Just (ME.bandPartyId row)) Nothing
      | Entity rowId row <- rows
      ]
  bandMemberRefs <- whenTableExists "band_member" $ do
    rows <- selectList ([] :: [Filter ME.BandMember]) []
    catMaybes <$> forM rows (\(Entity rowId row) ->
      mkPartyRef "band_member.party_id" (keyText rowId) (ME.bandMemberPartyId row))
  intakeRefs <- do
    rows <- selectList ([] :: [Filter ME.LiveSessionIntake]) []
    pure
      [ DiscoveryReference "live_session_intake.band_name" (keyText rowId)
          (ME.liveSessionIntakeBandName row) Nothing Nothing
      | Entity rowId row <- rows
      ]
  musicianRefs <- do
    rows <- selectList ([] :: [Filter ME.LiveSessionMusician]) []
    pure
      [ DiscoveryReference "live_session_musician.name" (keyText rowId)
          (ME.liveSessionMusicianName row) (Just (ME.liveSessionMusicianPartyId row)) Nothing
      | Entity rowId row <- rows
      ]
  pipelineRefs <- do
    rows <- selectList [ME.PipelineCardArtist !=. Nothing] []
    pure
      [ DiscoveryReference "pipeline_card.artist" (keyText rowId) name Nothing Nothing
      | Entity rowId row <- rows
      , name <- maybeToListText (ME.pipelineCardArtist row)
      ]
  storefrontRefs <- whenTableExists "service_storefront_order" $ do
    rows <- selectList [ME.ServiceStorefrontOrderArtistName !=. Nothing] []
    pure
      [ DiscoveryReference "service_storefront_order.artist_name" (keyText rowId)
          artistName Nothing Nothing
      | Entity rowId row <- rows
      , artistName <- maybeToListText (ME.serviceStorefrontOrderArtistName row)
      ]
  catalogCreditRefs <- whenTableExists "catalog_credit" $ do
    rows <- rawSql
      "SELECT id::text, party_id::bigint FROM catalog_credit WHERE role IN ('MainArtist','FeaturedArtist','Performer','StudioMusician')"
      [] :: SqlPersistT IO [(Single Text, Single Int64)]
    catMaybes <$> forM rows (\(Single rowId, Single rawPartyId) ->
      mkPartyRef "catalog_credit.party_id" rowId (toSqlKey rawPartyId))
  promotionRefs <- whenTableExists "artist_promo_slot" $ do
    rows <- selectList ([] :: [Filter ArtistPromoSlot]) []
    catMaybes <$> forM rows (\(Entity rowId row) ->
      mkPartyRef "artist_promo_slot.artist_party_id" (intKeyText rowId)
        (artistPromoSlotArtistPartyId row))
  engagementRefs <- whenTableExists "engagement_event" $ do
    rows <- selectList [EngagementEventTargetArtistId !=. Nothing] []
    catMaybes <$> forM rows (\(Entity rowId row) ->
      maybe (pure Nothing) (mkPartyRef "engagement_event.target_artist_id" (intKeyText rowId))
        (engagementEventTargetArtistId row))
  socialProfileRefs <- do
    rows <- selectList ([] :: [Filter Social.ArtistProfile]) []
    pure
      [ DiscoveryReference "social_artist_profile" (intKeyText rowId)
          (Social.artistProfileName row)
          (Social.artistProfilePartyId row >>= partyIdFromText)
          (Just (fromSqlKey rowId))
      | Entity rowId row <- rows
      ]
  eventRefs <- do
    rows <- selectList ([] :: [Filter Social.EventArtist]) []
    catMaybes <$> forM rows (\(Entity _ row) -> do
      mArtist <- get (Social.eventArtistArtistId row)
      pure $ fmap (\artist ->
        DiscoveryReference "event_artist"
          (intKeyText (Social.eventArtistEventId row) <> ":" <>
            intKeyText (Social.eventArtistArtistId row))
          (Social.artistProfileName artist)
          (Social.artistProfilePartyId artist >>= partyIdFromText)
          (Just (fromSqlKey (Social.eventArtistArtistId row)))) mArtist)
  broadcastRefs <- whenTableExists "event_live_broadcast" $ do
    rows <- selectList ([] :: [Filter Social.EventLiveBroadcast]) []
    catMaybes <$> forM rows (\(Entity rowId row) ->
      mkSocialProfileRef "event_live_broadcast.artist_id" (keyText rowId)
        (Social.eventLiveBroadcastArtistId row))
  artistGenreRefs <- whenTableExists "artist_genre" $ do
    rows <- selectList ([] :: [Filter Social.ArtistGenre]) []
    catMaybes <$> forM rows (\(Entity rowId row) ->
      mkSocialProfileRef "artist_genre.artist_id" (keyText rowId)
        (Social.artistGenreArtistId row))
  socialArtistFollowRefs <- whenTableExists "artist_follow" $ do
    rows <- selectList ([] :: [Filter Social.ArtistFollow]) []
    catMaybes <$> forM rows (\(Entity rowId row) ->
      mkSocialProfileRef "artist_follow.artist_id" (keyText rowId)
        (Social.artistFollowArtistId row))
  externalRefs <- do
    rows <- selectList ([] :: [Filter Social.ExternalArtistRef]) []
    catMaybes <$> forM rows (\(Entity rowId row) -> do
      mArtist <- get (Social.externalArtistRefArtistId row)
      pure $ fmap (\artist ->
        DiscoveryReference "external_artist_ref" (intKeyText rowId)
          (Social.artistProfileName artist)
          (Social.artistProfilePartyId artist >>= partyIdFromText)
          (Just (fromSqlKey (Social.externalArtistRefArtistId row)))) mArtist)
  syncRefs <- do
    rows <- selectList [SocialSyncPostArtistPartyId !=. Nothing] []
    catMaybes <$> forM rows (\(Entity rowId row) ->
      maybe (pure Nothing) (mkPartyRef "social_sync_post.artist_party_id" (intKeyText rowId))
        (socialSyncPostArtistPartyId row))
  syncAccountProfileRefs <- do
    rows <- selectList [SocialSyncAccountArtistProfileId !=. Nothing] []
    catMaybes <$> forM rows (\(Entity rowId row) ->
      maybe (pure Nothing)
        (mkCoreProfileRef "social_sync_account.artist_profile_id" (intKeyText rowId))
        (socialSyncAccountArtistProfileId row))
  syncPostProfileRefs <- do
    rows <- selectList [SocialSyncPostArtistProfileId !=. Nothing] []
    catMaybes <$> forM rows (\(Entity rowId row) ->
      maybe (pure Nothing)
        (mkCoreProfileRef "social_sync_post.artist_profile_id" (intKeyText rowId))
        (socialSyncPostArtistProfileId row))
  pure . filter validReference . concat $
    [ profileRefs, roleRefs, releaseRefs, serviceOrderRefs, storefrontRefs
    , catalogCreditRefs, promotionRefs, engagementRefs, fanFollowRefs, fanClubRefs, bandRefs
    , bandMemberRefs, intakeRefs, musicianRefs, pipelineRefs, socialProfileRefs
    , eventRefs, broadcastRefs, artistGenreRefs, socialArtistFollowRefs
    , externalRefs, syncRefs, syncAccountProfileRefs, syncPostProfileRefs
    ]
  where
    mkPartyRef source recordId partyId = do
      mParty <- get partyId
      pure $ fmap (\party -> DiscoveryReference source recordId
        (partyDisplayName party) (Just partyId) Nothing) mParty
    mkCoreProfileRef source recordId profileId = do
      mProfile <- get profileId
      maybe (pure Nothing)
        (mkPartyRef source recordId . artistProfileArtistPartyId) mProfile
    mkSocialProfileRef source recordId socialArtistId = do
      mArtist <- get socialArtistId
      pure $ fmap (\artist -> DiscoveryReference source recordId
        (Social.artistProfileName artist)
        (Social.artistProfilePartyId artist >>= partyIdFromText)
        (Just (fromSqlKey socialArtistId))) mArtist
    validReference ref = not (T.null (normalizeArtistName (drOriginalName ref)))
    maybeToListText (Just value) | not (T.null (T.strip value)) = [T.strip value]
    maybeToListText _ = []

persistDiscoveryReference
  :: UTCTime
  -> DiscoveryReference
  -> SqlPersistT IO (Entity ArtistInventoryReference)
persistDiscoveryReference now DiscoveryReference{..} = do
  let originalName = T.strip drOriginalName
      originalMarker = T.toLower originalName
      normalized = normalizeDiscoveredName drSourceType originalName
      idem = digestText (T.intercalate "|" [drSourceType, drSourceRecordId, normalized])
      disposition
        | "delete me" `T.isInfixOf` normalized || "[test" `T.isPrefixOf` originalMarker = "obsolete_review"
        | otherwise = "discovered"
      record = ArtistInventoryReference
        { artistInventoryReferenceIdempotencyKey = idem
        , artistInventoryReferenceSourceType = drSourceType
        , artistInventoryReferenceSourceRecordId = drSourceRecordId
        , artistInventoryReferenceOriginalName = originalName
        , artistInventoryReferenceNormalizedName = normalized
        , artistInventoryReferenceArtistPartyId = drArtistPartyId
        , artistInventoryReferenceSocialArtistId = drSocialArtistId
        , artistInventoryReferenceAliases = Nothing
        , artistInventoryReferenceEvidence = Just . jsonText $ object
            [ "source" .= drSourceType
            , "recordId" .= drSourceRecordId
            ]
        , artistInventoryReferenceConfidence = Nothing
        , artistInventoryReferenceDisposition = disposition
        , artistInventoryReferenceFirstSeenAt = now
        , artistInventoryReferenceLastSeenAt = now
        }
  -- Indirect discovery sources often do not carry a party or social ID. Once a
  -- later matching pass has resolved either identity, a repeated discovery must
  -- not erase it with NULL. A non-null source identity may still promote the
  -- stored reference deterministically.
  _ <- upsert record $
    [ ArtistInventoryReferenceOriginalName =. originalName
    , ArtistInventoryReferenceLastSeenAt =. now
    ]
    <> maybe [] (pure . (ArtistInventoryReferenceArtistPartyId =.) . Just) drArtistPartyId
    <> maybe [] (pure . (ArtistInventoryReferenceSocialArtistId =.) . Just) drSocialArtistId
    <> if disposition == "obsolete_review"
      then [ArtistInventoryReferenceDisposition =. disposition]
      else []
  found <- getBy (UniqueArtistInventoryReference idem)
  maybe (liftIO (fail "artist inventory upsert did not return a row")) pure found

jsonText :: Aeson.ToJSON value => value -> Text
jsonText = TE.decodeUtf8 . BL.toStrict . encode

data MatchDisposition
  = MatchExisting PartyId Double
  | NeedsReview Text Double
  | ObsoleteReference
  deriving (Eq, Show)

classifyGroup
  :: Map.Map Text [PartyId]
  -> [Entity ArtistInventoryReference]
  -> MatchDisposition
classifyGroup existingByName rows
  | any ((== "obsolete_review") . artistInventoryReferenceDisposition . entityVal) rows =
      ObsoleteReference
  | length partyIds > 1 = NeedsReview "multiple_core_party_ids" confidence
  | [partyId] <- partyIds =
      if signalCount >= 2 || "artist_profile" `elem` sources
        then MatchExisting partyId confidence
        else NeedsReview "insufficient_independent_signals" confidence
  | length possibleExisting > 1 = NeedsReview "homonym_or_alias_existing_profiles" confidence
  | length possibleExisting == 1 = NeedsReview "requires_external_identity_corroboration" confidence
  | automaticIdentityMatchAllowed sources (length possibleExisting) =
      NeedsReview "requires_external_identity_corroboration" confidence
  | otherwise = NeedsReview "insufficient_independent_signals" confidence
  where
    values = map entityVal rows
    partyIds = nub (mapMaybe artistInventoryReferenceArtistPartyId values)
    sources = nub (map artistInventoryReferenceSourceType values)
    signalCount = independentSignalCount sources
    normalized = maybe "" (artistInventoryReferenceNormalizedName . entityVal) (listToMaybe rows)
    possibleExisting = lookupExistingCandidates existingByName normalized
    confidence = matchConfidence sources (not (null partyIds))

existingPartyNames :: SqlPersistT IO (Map.Map Text [PartyId])
existingPartyNames = do
  profiles <- selectList ([] :: [Filter ArtistProfile]) []
  pairs <- fmap catMaybes $ forM profiles $ \(Entity _ profile) -> do
    mParty <- get (artistProfileArtistPartyId profile)
    pure (fmap (\party ->
      (artistProfileArtistPartyId profile, normalizeArtistName (partyDisplayName party))) mParty)
  pure (Map.fromListWith (<>) [(name, [partyId]) | (partyId, name) <- pairs])

ensureCoreArtistProfile :: Text -> UTCTime -> SqlPersistT IO PartyId
ensureCoreArtistProfile artistName now = do
  partyId <- insert Party
    { partyLegalName = Nothing
    , partyDisplayName = T.strip artistName
    , partyIsOrg = True
    , partyTaxId = Nothing
    , partyPrimaryEmail = Nothing
    , partyPrimaryPhone = Nothing
    , partyWhatsapp = Nothing
    , partyInstagram = Nothing
    , partyEmergencyContact = Nothing
    , partyNotes = Just "Created by artist enrichment after two independent TDF signals."
    , partyStripeCustomerId = Nothing
    , partyCreatedAt = now
    }
  ensureArtistProfileForParty partyId artistName now
  pure partyId

ensureArtistProfileForParty :: PartyId -> Text -> UTCTime -> SqlPersistT IO ()
ensureArtistProfileForParty partyId artistName now = do
  _ <- insertUnique PartyRole
    { partyRolePartyId = partyId
    , partyRoleRole = Artist
    , partyRoleActive = True
    }
  slug <- uniqueSlug partyId artistName
  existing <- getBy (UniqueArtistProfile partyId)
  when (isNothing existing) $ insert_ ArtistProfile
      { artistProfileArtistPartyId = partyId
      , artistProfileSlug = Just slug
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

uniqueSlug :: PartyId -> Text -> SqlPersistT IO Text
uniqueSlug partyId artistName = do
  let base = fromMaybe ("artist-" <> T.pack (show (fromSqlKey partyId)))
        (nonBlank (slugifyArtistName artistName))
  collision <- selectFirst [ArtistProfileSlug ==. Just base] []
  pure $ case collision of
    Nothing -> base
    Just _ -> T.take 96 (base <> "-" <> T.pack (show (fromSqlKey partyId)))

nonBlank :: Text -> Maybe Text
nonBlank raw = let value = T.strip raw in if T.null value then Nothing else Just value

linkSocialProfiles :: PartyId -> [Entity ArtistInventoryReference] -> SqlPersistT IO ()
linkSocialProfiles partyId rows =
  forM_ socialIds $ \socialId ->
    update (toSqlKey socialId :: Social.ArtistProfileId)
      [Social.ArtistProfilePartyId =. Just (T.pack (show (fromSqlKey partyId)))]
  where
    socialIds = nub (mapMaybe (artistInventoryReferenceSocialArtistId . entityVal) rows)

ensureEnrichmentRow :: UTCTime -> PartyId -> SqlPersistT IO ()
ensureEnrichmentRow now partyId = do
  _ <- insertUnique ArtistProfileEnrichment
    { artistProfileEnrichmentArtistPartyId = partyId
    , artistProfileEnrichmentOfficialName = Nothing
    , artistProfileEnrichmentCountry = Nothing
    , artistProfileEnrichmentInstagramUrl = Nothing
    , artistProfileEnrichmentSocialLinks = Nothing
    , artistProfileEnrichmentDiscography = Nothing
    , artistProfileEnrichmentAchievements = Nothing
    , artistProfileEnrichmentHeroOriginalUrl = Nothing
    , artistProfileEnrichmentHeroSquareUrl = Nothing
    , artistProfileEnrichmentHeroLandscapeUrl = Nothing
    , artistProfileEnrichmentHeroResponsiveUrls = Nothing
    , artistProfileEnrichmentHeroFocalPoint = Nothing
    , artistProfileEnrichmentLastVerifiedAt = Nothing
    , artistProfileEnrichmentConfidence = Nothing
    , artistProfileEnrichmentReviewStatus = "unverified"
    , artistProfileEnrichmentCreatedAt = now
    , artistProfileEnrichmentUpdatedAt = now
    }
  pure ()

runArtistEnrichment
  :: Text
  -> ArtistEnrichmentRunRequest
  -> SqlPersistT IO ArtistEnrichmentRunDTO
runArtistEnrichment actor request = do
  now <- liftIO getCurrentTime
  let scope = maybe "full" (("artist:" <>) . T.pack . show) (aerrArtistId request)
      minuteKey = T.pack (formatTime defaultTimeLocale "%Y%m%dT%H%M" now)
      runKey = fromMaybe ("manual:" <> minuteKey <> ":" <> scope <> ":" <> aerrMode request)
        (aerrResumeRunKey request)
  runArtistEnrichmentWithKey actor runKey request

runArtistEnrichmentWithKey
  :: Text
  -> Text
  -> ArtistEnrichmentRunRequest
  -> SqlPersistT IO ArtistEnrichmentRunDTO
runArtistEnrichmentWithKey actor rawRunKey request = do
  now <- liftIO getCurrentTime
  let mode = T.toLower (T.strip (aerrMode request))
      requestedId = fmap toSqlKey (aerrArtistId request)
      scope = maybe "full" (("artist:" <>) . T.pack . show) (aerrArtistId request)
      runKey = T.strip rawRunKey
  unless (mode `elem` ["dry_run", "production"]) $
    liftIO (fail "artist enrichment mode must be dry_run or production")
  when (T.null runKey || T.length runKey > 200) $
    liftIO (fail "artist enrichment run key must contain 1-200 characters")
  existing <- getBy (UniqueArtistEnrichmentRun runKey)
  case existing of
    Just entity | artistEnrichmentRunStatus (entityVal entity) `elem` ["completed", "running"] ->
      pure (runEntityToDTO entity)
    Just entity -> executeRun actor mode request entity
    Nothing -> do
      runId <- insert ArtistEnrichmentRun
        { artistEnrichmentRunRunKey = runKey
        , artistEnrichmentRunMode = mode
        , artistEnrichmentRunScope = scope
        , artistEnrichmentRunRequestedArtistId = requestedId
        , artistEnrichmentRunStatus = "running"
        , artistEnrichmentRunPhase = "discovery"
        , artistEnrichmentRunCheckpoint = Nothing
        , artistEnrichmentRunCounters = Nothing
        , artistEnrichmentRunErrorSummary = Nothing
        , artistEnrichmentRunStartedAt = now
        , artistEnrichmentRunHeartbeatAt = now
        , artistEnrichmentRunFinishedAt = Nothing
        }
      executeRun actor mode request (Entity runId ArtistEnrichmentRun
        { artistEnrichmentRunRunKey = runKey
        , artistEnrichmentRunMode = mode
        , artistEnrichmentRunScope = scope
        , artistEnrichmentRunRequestedArtistId = requestedId
        , artistEnrichmentRunStatus = "running"
        , artistEnrichmentRunPhase = "discovery"
        , artistEnrichmentRunCheckpoint = Nothing
        , artistEnrichmentRunCounters = Nothing
        , artistEnrichmentRunErrorSummary = Nothing
        , artistEnrichmentRunStartedAt = now
        , artistEnrichmentRunHeartbeatAt = now
        , artistEnrichmentRunFinishedAt = Nothing
        })

updateArtistEnrichmentRun
  :: Int64
  -> ArtistEnrichmentRunUpdate
  -> SqlPersistT IO ArtistEnrichmentRunDTO
updateArtistEnrichmentRun rawRunId ArtistEnrichmentRunUpdate{..} = do
  when (rawRunId <= 0) (liftIO (fail "runId must be positive"))
  let runId = toSqlKey rawRunId
  current <- get runId >>= maybe (liftIO (fail "artist enrichment run not found")) pure
  status <- traverse validateRunStatus aeruStatus
  phase <- traverse (requireShortText "phase" 100) aeruPhase
  checkpoint <- traverse (requireShortText "checkpoint" 20000) aeruCheckpoint
  counters <- traverse (requireShortText "counters" 20000) aeruCounters
  errorSummary <- traverse (requireShortText "errorSummary" 12000) aeruErrorSummary
  when (all isNothing [status, phase, checkpoint, counters, errorSummary]) $
    liftIO (fail "run update requires at least one field")
  now <- liftIO getCurrentTime
  let finalStatus = fromMaybe (artistEnrichmentRunStatus current) status
      finishedAt = if finalStatus `elem` ["completed", "failed", "cancelled", "blocked"]
        then Just now else Nothing
      updates =
        [ ArtistEnrichmentRunHeartbeatAt =. now
        , ArtistEnrichmentRunFinishedAt =. finishedAt
        ]
        <> maybe [] (pure . (ArtistEnrichmentRunStatus =.)) status
        <> maybe [] (pure . (ArtistEnrichmentRunPhase =.)) phase
        <> maybe [] (pure . (ArtistEnrichmentRunCheckpoint =.) . Just) checkpoint
        <> maybe [] (pure . (ArtistEnrichmentRunCounters =.) . Just) counters
        <> maybe [] (pure . (ArtistEnrichmentRunErrorSummary =.) . Just) errorSummary
      isExternalClaim = phase == Just "external_research_claim"
  if isExternalClaim
    then do
      unless (status == Just "running") $
        liftIO (fail "external research claim must set status to running")
      let staleBefore = addUTCTime (negate (30 * 60)) now
      claimed <- updateWhereCount
        [ ArtistEnrichmentRunId ==. runId
        , FilterOr
            [ ArtistEnrichmentRunStatus !=. "running"
            , ArtistEnrichmentRunHeartbeatAt <. staleBefore
            ]
        ]
        updates
      when (claimed == 0) $
        liftIO (fail "artist enrichment run is already active")
    else update runId updates
  runEntityToDTO . Entity runId <$> getJust runId
  where
    validateRunStatus raw = do
      let value = T.toLower (T.strip raw)
      unless (value `elem` ["running", "completed", "failed", "cancelled", "blocked"]) $
        liftIO (fail "unsupported artist enrichment run status")
      pure value

executeRun
  :: Text
  -> Text
  -> ArtistEnrichmentRunRequest
  -> Entity ArtistEnrichmentRun
  -> SqlPersistT IO ArtistEnrichmentRunDTO
executeRun actor mode ArtistEnrichmentRunRequest{..} (Entity runId _runRow) = do
  now <- liftIO getCurrentTime
  update runId
    [ ArtistEnrichmentRunStatus =. "running"
    , ArtistEnrichmentRunPhase =. "discovery"
    , ArtistEnrichmentRunHeartbeatAt =. now
    , ArtistEnrichmentRunErrorSummary =. Nothing
    ]
  discovered <- discoverArtistReferences
  persisted <- mapM (persistDiscoveryReference now) discovered
  existingNames <- existingPartyNames
  let grouped = Map.elems (Map.fromListWith (<>)
        [ (artistInventoryReferenceNormalizedName row, [entity])
        | entity@(Entity _ row) <- persisted
        ])
      filteredGroups = case aerrArtistId of
        Nothing -> grouped
        Just rawArtistId -> filter (groupContainsArtist (toSqlKey rawArtistId)) grouped
      scopedGroups = maybe filteredGroups (`take` filteredGroups) (boundedBatch aerrBatchSize)
  counters <- foldGroups actor mode now existingNames scopedGroups
  profiles <- selectList ([] :: [Filter ArtistProfile]) []
  let scopedProfiles = case aerrArtistId of
        Nothing -> profiles
        Just rawArtistId -> filter
          ((== toSqlKey rawArtistId) . artistProfileArtistPartyId . entityVal) profiles
  corrected <- auditProfileSlugs actor mode now scopedProfiles
  staleProfiles <- auditProfileStaleness now (fromMaybe 90 aerrStaleDays) scopedProfiles
  finished <- liftIO getCurrentTime
  let counterJson = jsonText $ object
        [ "referencesDiscovered" .= length discovered
        , "referencesProcessed" .= sum (map length scopedGroups)
        , "candidateGroups" .= length scopedGroups
        , "matched" .= fst3 counters
        , "created" .= snd3 counters
        , "reviewQueued" .= thd3 counters
        , "profileCorrections" .= corrected
        , "staleProfiles" .= staleProfiles
        ]
  update runId
    [ ArtistEnrichmentRunStatus =. "completed"
    , ArtistEnrichmentRunPhase =. "reporting"
    , ArtistEnrichmentRunCheckpoint =. Just "complete"
    , ArtistEnrichmentRunCounters =. Just counterJson
    , ArtistEnrichmentRunHeartbeatAt =. finished
    , ArtistEnrichmentRunFinishedAt =. Just finished
    ]
  row <- getJust runId
  pure (runEntityToDTO (Entity runId row))
  where
    boundedBatch Nothing = Nothing
    boundedBatch (Just value) = Just (max 1 (min 10000 value))
    groupContainsArtist artistId = any
      ((== Just artistId) . artistInventoryReferenceArtistPartyId . entityVal)
    fst3 (a, _, _) = a
    snd3 (_, b, _) = b
    thd3 (_, _, c) = c

auditProfileStaleness
  :: UTCTime
  -> Int
  -> [Entity ArtistProfile]
  -> SqlPersistT IO Int
auditProfileStaleness now rawStaleDays profiles = fmap sum . forM profiles $ \(Entity _ profile) -> do
  let partyId = artistProfileArtistPartyId profile
      staleDays = max 7 (min 730 rawStaleDays)
      cutoff = addUTCTime (negate (fromIntegral staleDays * 86400)) now
  mExtra <- getBy (UniqueArtistProfileEnrichment partyId)
  let lastVerified = mExtra >>= artistProfileEnrichmentLastVerifiedAt . entityVal
      isStale = maybe True (< cutoff) lastVerified
  if not isStale
    then pure 0
    else do
      _ <- createArtistSuggestion now ArtistEnrichmentSuggestionCreate
        { aescArtistId = Just (fromSqlKey partyId)
        , aescInventoryReferenceId = Nothing
        , aescFieldName = "reviewStatus"
        , aescCurrentValue = artistProfileEnrichmentReviewStatus . entityVal <$> mExtra
        , aescProposedValue = Just "pending"
        , aescConfidence = 1
        , aescAutoPublish = Just False
        , aescEvidence = jsonText $ object
            [ "reason" .= ("verification_missing_or_stale" :: Text)
            , "staleDays" .= staleDays
            , "lastVerifiedAt" .= lastVerified
            ]
        }
      pure 1

foldGroups
  :: Text
  -> Text
  -> UTCTime
  -> Map.Map Text [PartyId]
  -> [[Entity ArtistInventoryReference]]
  -> SqlPersistT IO (Int, Int, Int)
foldGroups actor mode now existingByName = go (0, 0, 0)
  where
    go counts [] = pure counts
    go (matched, created, queued) (rows:rest) = do
      let disposition = classifyGroup existingByName rows
          sources = nub (map (artistInventoryReferenceSourceType . entityVal) rows)
          evidence = jsonText $ object
            [ "signals" .= map signalBucket sources
            , "sources" .= sources
            ]
      case disposition of
        ObsoleteReference -> do
          setRows rows "obsolete_review" (Just 1)
          go (matched, created, queued + 1) rest
        NeedsReview reason confidence -> do
          setRows rows "needs_review" (Just confidence)
          let internalPartyIds = nub (mapMaybe
                (artistInventoryReferenceArtistPartyId . entityVal) rows)
              normalizedName = maybe ""
                (artistInventoryReferenceNormalizedName . entityVal) (listToMaybe rows)
              externalNameCandidates = lookupExistingCandidates existingByName normalizedName
              candidatePartyIds = nub (internalPartyIds <> externalNameCandidates)
              candidateEvidence :: Maybe PartyId -> Text
              candidateEvidence candidateId = jsonText $ object
                [ "reason" .= reason
                , "match" .= evidence
                , "candidateArtistId" .= fmap fromSqlKey candidateId
                ]
          case candidatePartyIds of
            [] -> queueIdentity rows Nothing "tdf_inventory" Nothing Nothing
              (candidateEvidence Nothing) confidence now
            candidates -> forM_ candidates $ \candidateId ->
              queueIdentity rows (Just candidateId) "tdf_inventory" Nothing Nothing
                (candidateEvidence (Just candidateId)) confidence now
          go (matched, created, queued + 1) rest
        MatchExisting partyId confidence -> do
          setRowsArtist rows partyId "matched" confidence
          when (mode == "production") $ do
            let displayName = maybe "Artist"
                  (artistInventoryReferenceOriginalName . entityVal) (listToMaybe rows)
            existingProfile <- getBy (UniqueArtistProfile partyId)
            ensureArtistProfileForParty partyId displayName now
            ensureEnrichmentRow now partyId
            linkSocialProfiles partyId rows
            when (isNothing existingProfile) $
              recordFieldChange partyId Nothing "profile" Nothing (Just displayName)
                evidence confidence actor now
          go (matched + 1, created, queued) rest
    setRows rows disposition confidence = forM_ rows $ \(Entity rowId _) ->
      update rowId
        [ ArtistInventoryReferenceDisposition =. disposition
        , ArtistInventoryReferenceConfidence =. confidence
        , ArtistInventoryReferenceAliases =. aliasesFor rows
        ]
    setRowsArtist rows partyId disposition confidence = forM_ rows $ \(Entity rowId _) ->
      update rowId
        [ ArtistInventoryReferenceArtistPartyId =. Just partyId
        , ArtistInventoryReferenceDisposition =. disposition
        , ArtistInventoryReferenceConfidence =. Just confidence
        , ArtistInventoryReferenceAliases =. aliasesFor rows
        ]
    aliasesFor = Just . jsonText . nub
      . map (artistInventoryReferenceOriginalName . entityVal)

queueIdentity
  :: [Entity ArtistInventoryReference]
  -> Maybe PartyId
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> Text
  -> Double
  -> UTCTime
  -> SqlPersistT IO ()
queueIdentity [] _ _ _ _ _ _ _ = pure ()
queueIdentity (Entity inventoryId _ : _) mParty provider externalId candidateUrl evidence confidence now = do
  let idem = digestText (T.intercalate "|"
        [ intKeyText inventoryId
        , provider
        , fromMaybe "" externalId
        , fromMaybe "" candidateUrl
        , evidence
        ])
  _ <- upsert ArtistIdentityCandidate
    { artistIdentityCandidateInventoryReferenceId = inventoryId
    , artistIdentityCandidateArtistPartyId = mParty
    , artistIdentityCandidateProvider = provider
    , artistIdentityCandidateExternalId = externalId
    , artistIdentityCandidateCandidateUrl = candidateUrl
    , artistIdentityCandidateEvidence = evidence
    , artistIdentityCandidateConfidence = confidence
    , artistIdentityCandidateStatus = "pending"
    , artistIdentityCandidateIdempotencyKey = idem
    , artistIdentityCandidateCreatedAt = now
    , artistIdentityCandidateUpdatedAt = now
    , artistIdentityCandidateDecidedAt = Nothing
    , artistIdentityCandidateDecidedBy = Nothing
    , artistIdentityCandidateDecisionNote = Nothing
    }
    [ ArtistIdentityCandidateEvidence =. evidence
    , ArtistIdentityCandidateConfidence =. confidence
    , ArtistIdentityCandidateUpdatedAt =. now
    ]
  pure ()

auditProfileSlugs
  :: Text
  -> Text
  -> UTCTime
  -> [Entity ArtistProfile]
  -> SqlPersistT IO Int
auditProfileSlugs actor mode now profiles =
  fmap sum . forM profiles $ \(Entity _ profile) -> do
    let partyId = artistProfileArtistPartyId profile
    mParty <- get partyId
    case mParty of
      Nothing -> pure 0
      Just party -> do
        desired <- uniqueSlug partyId (partyDisplayName party)
        let current = artistProfileSlug profile
            invalid = maybe True (\value -> value /= slugifyArtistName value || T.null value) current
        if not invalid || current == Just desired
          then pure 0
          else do
            suggestion <- createArtistSuggestion now ArtistEnrichmentSuggestionCreate
              { aescArtistId = Just (fromSqlKey partyId)
              , aescInventoryReferenceId = Nothing
              , aescFieldName = "slug"
              , aescCurrentValue = current
              , aescProposedValue = Just desired
              , aescConfidence = 0.99
              , aescAutoPublish = Just True
              , aescEvidence = jsonText $ object
                  [ "signals" .= (["party_profile", "slug_validation"] :: [Text])
                  , "reason" .= ("missing_or_invalid_slug" :: Text)
                  ]
              }
            if mode == "production"
              then do
                suggestionEntity <- requireSuggestion (aesId suggestion)
                _ <- applySuggestion actor now Nothing suggestionEntity
                pure 1
              else pure 0

createArtistResearchSource
  :: UTCTime
  -> ArtistResearchSourceCreate
  -> SqlPersistT IO ArtistResearchSourceDTO
createArtistResearchSource now ArtistResearchSourceCreate{..} = do
  sourceUrl <- requirePublicUrl "sourceUrl" arscSourceUrl
  sourceType <- requireShortText "sourceType" 80 arscSourceType
  supported <- requireShortText "supportedFields" 1000 arscSupportedFields
  artistId <- traverse requirePartyId arscArtistId
  inventoryId <- traverse requireInventoryId arscInventoryReferenceId
  when (isNothing artistId && isNothing inventoryId) $
    liftIO (fail "research source requires artistId or inventoryReferenceId")
  let retrieved = fromMaybe now arscRetrievedAt
      idem = digestText (T.intercalate "|"
        [ maybe "" (T.pack . show . fromSqlKey) artistId
        , maybe "" (T.pack . show . fromSqlKey) inventoryId
        , sourceType
        , sourceUrl
        , supported
        ])
      row = ArtistResearchSource artistId inventoryId sourceUrl sourceType retrieved
        supported (cleanOptional arscAttribution) (cleanOptional arscContentHash) idem
  _ <- upsert row
    [ ArtistResearchSourceRetrievedAt =. retrieved
    , ArtistResearchSourceSupportedFields =. supported
    , ArtistResearchSourceAttribution =. cleanOptional arscAttribution
    , ArtistResearchSourceContentHash =. cleanOptional arscContentHash
    ]
  entity <- getBy (UniqueArtistResearchSource idem) >>= maybe
    (liftIO (fail "artist research source upsert failed")) pure
  pure (sourceEntityToDTO entity)

createArtistSuggestion
  :: UTCTime
  -> ArtistEnrichmentSuggestionCreate
  -> SqlPersistT IO ArtistEnrichmentSuggestionDTO
createArtistSuggestion now ArtistEnrichmentSuggestionCreate{..} = do
  fieldName <- validateFieldName aescFieldName
  confidence <- validateConfidence aescConfidence
  evidence <- requireShortText "evidence" 12000 aescEvidence
  artistId <- traverse requirePartyId aescArtistId
  inventoryId <- traverse requireInventoryId aescInventoryReferenceId
  when (isNothing artistId && isNothing inventoryId) $
    liftIO (fail "suggestion requires artistId or inventoryReferenceId")
  proposedValue <- validateFieldValue fieldName (fmap T.strip aescProposedValue)
  let currentValue = cleanOptional aescCurrentValue
      autoPublish = fromMaybe False aescAutoPublish
        && confidence >= 0.9
        && evidenceSignalCount evidence >= 2
      idem = digestText (T.intercalate "|"
        [ maybe "" (T.pack . show . fromSqlKey) artistId
        , maybe "" (T.pack . show . fromSqlKey) inventoryId
        , fieldName
        , fromMaybe "<null>" proposedValue
        ])
      row = ArtistEnrichmentSuggestion artistId inventoryId fieldName currentValue
        proposedValue confidence "pending" autoPublish evidence idem now now
        Nothing Nothing Nothing
  _ <- upsert row
    [ ArtistEnrichmentSuggestionCurrentValue =. currentValue
    , ArtistEnrichmentSuggestionProposedValue =. proposedValue
    , ArtistEnrichmentSuggestionConfidence =. confidence
    , ArtistEnrichmentSuggestionAutoPublish =. autoPublish
    , ArtistEnrichmentSuggestionEvidence =. evidence
    , ArtistEnrichmentSuggestionUpdatedAt =. now
    ]
  entity <- getBy (UniqueArtistEnrichmentSuggestion idem) >>= maybe
    (liftIO (fail "artist suggestion upsert failed")) pure
  suggestionEntityToDTO entity

decideArtistSuggestion
  :: Text
  -> PartyId
  -> Int64
  -> ArtistEnrichmentDecision
  -> SqlPersistT IO ArtistEnrichmentSuggestionDTO
decideArtistSuggestion actor decider rawSuggestionId decision = do
  now <- liftIO getCurrentTime
  entity <- requireSuggestion rawSuggestionId
  decideSuggestionEntity actor decider now decision entity

decideArtistSuggestionSet
  :: Text
  -> PartyId
  -> Int64
  -> ArtistEnrichmentDecision
  -> SqlPersistT IO [ArtistEnrichmentSuggestionDTO]
decideArtistSuggestionSet actor decider rawArtistId decision = do
  partyId <- requirePartyId rawArtistId
  now <- liftIO getCurrentTime
  rows <- selectList
    [ ArtistEnrichmentSuggestionArtistPartyId ==. Just partyId
    , ArtistEnrichmentSuggestionStatus ==. "pending"
    ] [Asc ArtistEnrichmentSuggestionId]
  mapM (decideSuggestionEntity actor decider now decision) rows

decideSuggestionEntity
  :: Text
  -> PartyId
  -> UTCTime
  -> ArtistEnrichmentDecision
  -> Entity ArtistEnrichmentSuggestion
  -> SqlPersistT IO ArtistEnrichmentSuggestionDTO
decideSuggestionEntity actor decider now ArtistEnrichmentDecision{..} entity@(Entity suggestionId suggestion) = do
  let decision = T.toLower (T.strip aedDecision)
  unless (decision `elem` ["approve", "reject"]) $
    liftIO (fail "decision must be approve or reject")
  if artistEnrichmentSuggestionStatus suggestion /= "pending"
    then suggestionEntityToDTO entity
    else if decision == "reject"
      then do
        update suggestionId
          [ ArtistEnrichmentSuggestionStatus =. "rejected"
          , ArtistEnrichmentSuggestionDecidedAt =. Just now
          , ArtistEnrichmentSuggestionDecidedBy =. Just decider
          , ArtistEnrichmentSuggestionDecisionNote =. cleanOptional aedNote
          , ArtistEnrichmentSuggestionUpdatedAt =. now
          ]
        updated <- getJust suggestionId
        suggestionEntityToDTO (Entity suggestionId updated)
      else do
        proposed <- validateFieldValue
          (artistEnrichmentSuggestionFieldName suggestion)
          (cleanOptional aedEditedValue <|> artistEnrichmentSuggestionProposedValue suggestion)
        applySuggestion actor now (Just decider) $ entity
          { entityVal = suggestion
              { artistEnrichmentSuggestionProposedValue = proposed
              , artistEnrichmentSuggestionDecisionNote = cleanOptional aedNote
              }
          }

applySuggestion
  :: Text
  -> UTCTime
  -> Maybe PartyId
  -> Entity ArtistEnrichmentSuggestion
  -> SqlPersistT IO ArtistEnrichmentSuggestionDTO
applySuggestion actor now mDecider (Entity suggestionId suggestion) = do
  partyId <- maybe (liftIO (fail "approved suggestion requires artistId")) pure
    (artistEnrichmentSuggestionArtistPartyId suggestion)
  -- A no-op update takes a row lock in PostgreSQL (and a write lock in the
  -- SQLite test backend). Competing approvals for this artist must therefore
  -- re-read the field after the first transaction commits instead of both
  -- accepting the same stale researched value.
  _ <- ensureProfile partyId now
  rawExecute
    "UPDATE artist_profile SET updated_at = updated_at WHERE artist_party_id = ?"
    [toPersistValue partyId]
  oldValue <- readArtistField partyId (artistEnrichmentSuggestionFieldName suggestion)
  let expectedValue = cleanOptional (artistEnrichmentSuggestionCurrentValue suggestion)
      newValue = artistEnrichmentSuggestionProposedValue suggestion
  if oldValue /= expectedValue
    then update suggestionId
      [ ArtistEnrichmentSuggestionStatus =. "superseded"
      , ArtistEnrichmentSuggestionDecidedAt =. Just now
      , ArtistEnrichmentSuggestionDecidedBy =. mDecider
      , ArtistEnrichmentSuggestionDecisionNote =. Just "Current value changed after research; rerun enrichment before approval."
      , ArtistEnrichmentSuggestionUpdatedAt =. now
      ]
    else do
      applyArtistField now partyId (artistEnrichmentSuggestionFieldName suggestion) newValue
      recordFieldChange partyId (Just suggestionId)
        (artistEnrichmentSuggestionFieldName suggestion) oldValue newValue
        (artistEnrichmentSuggestionEvidence suggestion)
        (artistEnrichmentSuggestionConfidence suggestion) actor now
      updateWhere
        [ ArtistEnrichmentSuggestionArtistPartyId ==. Just partyId
        , ArtistEnrichmentSuggestionFieldName ==. artistEnrichmentSuggestionFieldName suggestion
        , ArtistEnrichmentSuggestionStatus ==. "pending"
        , ArtistEnrichmentSuggestionId !=. suggestionId
        ]
        [ ArtistEnrichmentSuggestionStatus =. "superseded"
        , ArtistEnrichmentSuggestionUpdatedAt =. now
        ]
      let finalStatus = if artistEnrichmentSuggestionAutoPublish suggestion && isNothing mDecider
            then "auto_applied" else "approved"
      update suggestionId
        [ ArtistEnrichmentSuggestionProposedValue =. newValue
        , ArtistEnrichmentSuggestionStatus =. finalStatus
        , ArtistEnrichmentSuggestionDecidedAt =. Just now
        , ArtistEnrichmentSuggestionDecidedBy =. mDecider
        , ArtistEnrichmentSuggestionDecisionNote =. artistEnrichmentSuggestionDecisionNote suggestion
        , ArtistEnrichmentSuggestionUpdatedAt =. now
        ]
  updated <- getJust suggestionId
  suggestionEntityToDTO (Entity suggestionId updated)

readArtistField :: PartyId -> Text -> SqlPersistT IO (Maybe Text)
readArtistField partyId fieldName = do
  mParty <- get partyId
  mProfile <- getBy (UniqueArtistProfile partyId)
  mExtra <- getBy (UniqueArtistProfileEnrichment partyId)
  let profile = entityVal <$> mProfile
      extra = entityVal <$> mExtra
  pure $ case fieldName of
    "officialName" -> (extra >>= artistProfileEnrichmentOfficialName) <|> (partyDisplayName <$> mParty)
    "slug" -> profile >>= artistProfileSlug
    "bio" -> profile >>= artistProfileBio
    "city" -> profile >>= artistProfileCity
    "country" -> extra >>= artistProfileEnrichmentCountry
    "genres" -> profile >>= artistProfileGenres
    "heroImageUrl" -> profile >>= artistProfileHeroImageUrl
    "heroOriginalUrl" -> extra >>= artistProfileEnrichmentHeroOriginalUrl
    "heroSquareUrl" -> extra >>= artistProfileEnrichmentHeroSquareUrl
    "heroLandscapeUrl" -> extra >>= artistProfileEnrichmentHeroLandscapeUrl
    "heroResponsiveUrls" -> extra >>= artistProfileEnrichmentHeroResponsiveUrls
    "heroFocalPoint" -> extra >>= artistProfileEnrichmentHeroFocalPoint
    "lastVerifiedAt" -> T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"
      <$> (extra >>= artistProfileEnrichmentLastVerifiedAt)
    "confidence" -> T.pack . show <$> (extra >>= artistProfileEnrichmentConfidence)
    "reviewStatus" -> artistProfileEnrichmentReviewStatus <$> extra
    "spotifyArtistId" -> profile >>= artistProfileSpotifyArtistId
    "spotifyUrl" -> profile >>= artistProfileSpotifyUrl
    "youtubeChannelId" -> profile >>= artistProfileYoutubeChannelId
    "youtubeUrl" -> profile >>= artistProfileYoutubeUrl
    "instagramUrl" -> extra >>= artistProfileEnrichmentInstagramUrl
    "socialLinks" -> extra >>= artistProfileEnrichmentSocialLinks
    "websiteUrl" -> profile >>= artistProfileWebsiteUrl
    "featuredVideoUrl" -> profile >>= artistProfileFeaturedVideoUrl
    "highlights" -> profile >>= artistProfileHighlights
    "discography" -> extra >>= artistProfileEnrichmentDiscography
    "achievements" -> extra >>= artistProfileEnrichmentAchievements
    _ -> Nothing

applyArtistField :: UTCTime -> PartyId -> Text -> Maybe Text -> SqlPersistT IO ()
applyArtistField now partyId fieldName rawValue = do
  _ <- ensureProfile partyId now
  ensureEnrichmentRow now partyId
  let value = cleanOptional rawValue
      updateProfile field = updateWhere [ArtistProfileArtistPartyId ==. partyId]
        [field, ArtistProfileUpdatedAt =. Just now]
      updateExtra field = updateWhere [ArtistProfileEnrichmentArtistPartyId ==. partyId]
        [field, ArtistProfileEnrichmentUpdatedAt =. now]
  case fieldName of
    "officialName" -> do
      forM_ value (\name -> update partyId [PartyDisplayName =. name])
      updateExtra (ArtistProfileEnrichmentOfficialName =. value)
    "slug" -> updateProfile (ArtistProfileSlug =. value)
    "bio" -> updateProfile (ArtistProfileBio =. value)
    "city" -> updateProfile (ArtistProfileCity =. value)
    "country" -> updateExtra (ArtistProfileEnrichmentCountry =. value)
    "genres" -> updateProfile (ArtistProfileGenres =. value)
    "heroImageUrl" -> updateProfile (ArtistProfileHeroImageUrl =. value)
    "heroOriginalUrl" -> updateExtra (ArtistProfileEnrichmentHeroOriginalUrl =. value)
    "heroSquareUrl" -> updateExtra (ArtistProfileEnrichmentHeroSquareUrl =. value)
    "heroLandscapeUrl" -> updateExtra (ArtistProfileEnrichmentHeroLandscapeUrl =. value)
    "heroResponsiveUrls" -> updateExtra (ArtistProfileEnrichmentHeroResponsiveUrls =. value)
    "heroFocalPoint" -> updateExtra (ArtistProfileEnrichmentHeroFocalPoint =. value)
    "lastVerifiedAt" -> case value >>= parseUtcTimestamp of
      Nothing -> liftIO (fail "lastVerifiedAt must be an ISO-8601 UTC timestamp")
      Just timestamp -> updateExtra (ArtistProfileEnrichmentLastVerifiedAt =. Just timestamp)
    "confidence" -> case value >>= (readMaybe . T.unpack) of
      Just confidence | confidence >= 0 && confidence <= 1 ->
        updateExtra (ArtistProfileEnrichmentConfidence =. Just confidence)
      _ -> liftIO (fail "confidence must be between 0 and 1")
    "reviewStatus" -> case value of
      Just reviewStatus | reviewStatus `elem` ["unverified", "pending", "verified", "rejected", "ambiguous"] ->
        updateExtra (ArtistProfileEnrichmentReviewStatus =. reviewStatus)
      _ -> liftIO (fail "unsupported reviewStatus")
    "spotifyArtistId" -> updateProfile (ArtistProfileSpotifyArtistId =. value)
    "spotifyUrl" -> updateProfile (ArtistProfileSpotifyUrl =. value)
    "youtubeChannelId" -> updateProfile (ArtistProfileYoutubeChannelId =. value)
    "youtubeUrl" -> updateProfile (ArtistProfileYoutubeUrl =. value)
    "instagramUrl" -> updateExtra (ArtistProfileEnrichmentInstagramUrl =. value)
    "socialLinks" -> updateExtra (ArtistProfileEnrichmentSocialLinks =. value)
    "websiteUrl" -> updateProfile (ArtistProfileWebsiteUrl =. value)
    "featuredVideoUrl" -> updateProfile (ArtistProfileFeaturedVideoUrl =. value)
    "highlights" -> updateProfile (ArtistProfileHighlights =. value)
    "discography" -> updateExtra (ArtistProfileEnrichmentDiscography =. value)
    "achievements" -> updateExtra (ArtistProfileEnrichmentAchievements =. value)
    _ -> liftIO (fail "unsupported artist enrichment field")

parseUtcTimestamp :: Text -> Maybe UTCTime
parseUtcTimestamp = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" . T.unpack

ensureProfile :: PartyId -> UTCTime -> SqlPersistT IO (Entity ArtistProfile)
ensureProfile partyId now = do
  found <- getBy (UniqueArtistProfile partyId)
  case found of
    Just entity -> pure entity
    Nothing -> do
      mParty <- get partyId
      party <- maybe (liftIO (fail "artist party not found")) pure mParty
      slug <- uniqueSlug partyId (partyDisplayName party)
      rowId <- insert ArtistProfile
        { artistProfileArtistPartyId = partyId
        , artistProfileSlug = Just slug
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
      pure (Entity rowId ArtistProfile
        { artistProfileArtistPartyId = partyId
        , artistProfileSlug = Just slug
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
        })

recordFieldChange
  :: PartyId -> Maybe ArtistEnrichmentSuggestionId -> Text -> Maybe Text -> Maybe Text
  -> Text -> Double -> Text -> UTCTime -> SqlPersistT IO ()
recordFieldChange partyId suggestionId fieldName oldValue newValue evidence confidence actor now = do
  let idem = digestText (T.intercalate "|"
        [ T.pack (show (fromSqlKey partyId)), fieldName, fromMaybe "<null>" oldValue
        , fromMaybe "<null>" newValue, evidence
        ])
  _ <- insertUnique ArtistFieldChange
    { artistFieldChangeArtistPartyId = partyId
    , artistFieldChangeSuggestionId = suggestionId
    , artistFieldChangeFieldName = fieldName
    , artistFieldChangePreviousValue = oldValue
    , artistFieldChangeNewValue = newValue
    , artistFieldChangeEvidence = evidence
    , artistFieldChangeConfidence = confidence
    , artistFieldChangeActor = actor
    , artistFieldChangeChangedAt = now
    , artistFieldChangeIdempotencyKey = idem
    }
  pure ()

decideArtistIdentityCandidate
  :: PartyId
  -> Int64
  -> ArtistEnrichmentDecision
  -> SqlPersistT IO ArtistIdentityCandidateDTO
decideArtistIdentityCandidate decider rawCandidateId ArtistEnrichmentDecision{..} = do
  now <- liftIO getCurrentTime
  let candidateId = toSqlKey rawCandidateId
      decision = T.toLower (T.strip aedDecision)
  unless (decision `elem` ["approve", "reject"]) $
    liftIO (fail "decision must be approve or reject")
  candidate <- get candidateId >>= maybe (liftIO (fail "identity candidate not found")) pure
  if artistIdentityCandidateStatus candidate /= "pending"
    then pure (identityEntityToDTO (Entity candidateId candidate))
    else do
      inventory <- getJust (artistIdentityCandidateInventoryReferenceId candidate)
      groupRows <- selectList
        [ ArtistInventoryReferenceNormalizedName ==. artistInventoryReferenceNormalizedName inventory ] []
      groupCandidates <- selectList
        [ ArtistIdentityCandidateInventoryReferenceId <-. map entityKey groupRows ] []
      let approvedSiblings =
            [ siblingId
            | Entity siblingId sibling <- groupCandidates
            , siblingId /= candidateId
            , artistIdentityCandidateStatus sibling == "approved"
            ]
      case (decision, listToMaybe approvedSiblings) of
        ("approve", Just approvedSiblingId) ->
          liftIO (fail ("identity group already resolved by candidate "
            <> show (fromSqlKey approvedSiblingId)))
        _ -> pure ()
      if decision == "reject"
        then update candidateId
          [ ArtistIdentityCandidateStatus =. "rejected"
          , ArtistIdentityCandidateUpdatedAt =. now
          , ArtistIdentityCandidateDecidedAt =. Just now
          , ArtistIdentityCandidateDecidedBy =. Just decider
          , ArtistIdentityCandidateDecisionNote =. cleanOptional aedNote
          ]
        else do
          partyId <- case artistIdentityCandidateArtistPartyId candidate of
            Just existingPartyId -> pure existingPartyId
            Nothing -> resolveIdentityDecisionTarget inventory now (cleanOptional aedEditedValue)
          claimedRows <- claimIdentityGroup
            (artistInventoryReferenceNormalizedName inventory)
            (artistIdentityCandidateConfidence candidate)
            partyId
          ensureArtistProfileForParty partyId (artistInventoryReferenceOriginalName inventory) now
          ensureEnrichmentRow now partyId
          linkSocialProfiles partyId claimedRows
          update candidateId
            [ ArtistIdentityCandidateArtistPartyId =. Just partyId
            , ArtistIdentityCandidateStatus =. "approved"
            , ArtistIdentityCandidateUpdatedAt =. now
            , ArtistIdentityCandidateDecidedAt =. Just now
            , ArtistIdentityCandidateDecidedBy =. Just decider
            , ArtistIdentityCandidateDecisionNote =. cleanOptional aedNote
            ]
          let supersededNote = Just ("Superseded by approved identity candidate "
                <> T.pack (show (fromSqlKey candidateId)))
          forM_ groupCandidates $ \(Entity siblingId sibling) ->
            when (siblingId /= candidateId && artistIdentityCandidateStatus sibling == "pending") $
              update siblingId
                [ ArtistIdentityCandidateStatus =. "superseded"
                , ArtistIdentityCandidateUpdatedAt =. now
                , ArtistIdentityCandidateDecidedAt =. Just now
                , ArtistIdentityCandidateDecidedBy =. Just decider
                , ArtistIdentityCandidateDecisionNote =. supersededNote
                ]
          recordFieldChange partyId Nothing "profile" Nothing
            (Just (artistInventoryReferenceOriginalName inventory))
            (artistIdentityCandidateEvidence candidate)
            (artistIdentityCandidateConfidence candidate)
            ("admin:" <> T.pack (show (fromSqlKey decider))) now
      updated <- getJust candidateId
      pure (identityEntityToDTO (Entity candidateId updated))
  where
    claimIdentityGroup normalizedName confidence partyId = do
      updateWhere
        [ ArtistInventoryReferenceNormalizedName ==. normalizedName
        , ArtistInventoryReferenceArtistPartyId ==. Nothing
        ]
        [ ArtistInventoryReferenceArtistPartyId =. Just partyId
        , ArtistInventoryReferenceDisposition =. "matched_external"
        , ArtistInventoryReferenceConfidence =. Just confidence
        ]
      claimedRows <- selectList
        [ArtistInventoryReferenceNormalizedName ==. normalizedName] []
      let conflictingParties = nub
            [ conflictingPartyId
            | Entity _ row <- claimedRows
            , Just conflictingPartyId <- [artistInventoryReferenceArtistPartyId row]
            , conflictingPartyId /= partyId
            ]
      unless (null conflictingParties) $
        liftIO (fail "identity group was resolved by another decision")
      updateWhere
        [ ArtistInventoryReferenceNormalizedName ==. normalizedName
        , ArtistInventoryReferenceArtistPartyId ==. Just partyId
        ]
        [ ArtistInventoryReferenceDisposition =. "matched_external"
        , ArtistInventoryReferenceConfidence =. Just confidence
        ]
      pure claimedRows
    resolveIdentityDecisionTarget inventory now explicitTarget =
      case fmap T.toLower explicitTarget of
        Just "new" -> ensureCoreArtistProfile (artistInventoryReferenceOriginalName inventory) now
        Just rawTarget -> case readMaybe (T.unpack rawTarget) :: Maybe Int64 of
          Just rawPartyId | rawPartyId > 0 -> requirePartyId rawPartyId
          _ -> liftIO (fail "identity approval target must be an artist party id or 'new'")
        Nothing -> resolveOrCreateExternallyMatchedArtist inventory now
    resolveOrCreateExternallyMatchedArtist inventory now = do
      existing <- existingPartyNames
      case lookupExistingCandidates existing (artistInventoryReferenceNormalizedName inventory) of
        [] -> ensureCoreArtistProfile (artistInventoryReferenceOriginalName inventory) now
        _ -> liftIO (fail "identity approval requires explicitly selecting an existing artist or confirming a distinct new profile")

createArtistIdentityCandidate
  :: UTCTime
  -> ArtistIdentityCandidateCreate
  -> SqlPersistT IO ArtistIdentityCandidateDTO
createArtistIdentityCandidate now ArtistIdentityCandidateCreate{..} = do
  inventoryId <- requireInventoryId aiccInventoryReferenceId
  artistId <- traverse requirePartyId aiccArtistId
  provider <- requireShortText "provider" 80 aiccProvider
  externalId <- traverse (requireShortText "externalId" 500) (cleanOptional aiccExternalId)
  candidateUrl <- traverse (requirePublicUrl "candidateUrl") (cleanOptional aiccCandidateUrl)
  evidence <- requireShortText "evidence" 12000 aiccEvidence
  confidence <- validateConfidence aiccConfidence
  let idem = digestText (T.intercalate "|"
        [ T.pack (show (fromSqlKey inventoryId))
        , provider
        , fromMaybe "" externalId
        , fromMaybe "" candidateUrl
        ])
      row = ArtistIdentityCandidate inventoryId artistId provider externalId
        candidateUrl evidence confidence "pending" idem now now Nothing Nothing Nothing
  _ <- upsert row
    [ ArtistIdentityCandidateArtistPartyId =. artistId
    , ArtistIdentityCandidateEvidence =. evidence
    , ArtistIdentityCandidateConfidence =. confidence
    , ArtistIdentityCandidateUpdatedAt =. now
    ]
  entity <- getBy (UniqueArtistIdentityCandidate idem) >>= maybe
    (liftIO (fail "artist identity candidate upsert failed")) pure
  pure (identityEntityToDTO entity)

createArtistMediaAsset
  :: UTCTime
  -> ArtistMediaAssetCreate
  -> SqlPersistT IO ArtistMediaAssetDTO
createArtistMediaAsset now ArtistMediaAssetCreate{..} = do
  artistId <- requirePartyId amacArtistId
  assetKind <- requireShortText "assetKind" 80 amacAssetKind
  sourceUrl <- requirePublicUrl "sourceUrl" amacSourceUrl
  sourceAttribution <- requireShortText "sourceAttribution" 1000 amacSourceAttribution
  publicUrl <- requirePublicUrl "publicUrl" amacPublicUrl
  sourceContentHash <- requireHexHash amacSourceContentHash
  contentHash <- requireHexHash amacContentHash
  sourceMimeType <- requireSourceMimeType amacSourceMimeType
  mimeType <- requireMimeType amacMimeType
  rights <- requireRights amacRightsStatus
  driveId <- requireDriveId amacDriveFileId
  when (amacSourceWidth <= 0 || amacSourceHeight <= 0 || amacSourceByteSize <= 0
      || amacWidth <= 0 || amacHeight <= 0 || amacByteSize <= 0) $
    liftIO (fail "source and derivative dimensions and byte sizes must be positive")
  validateMediaBudget assetKind amacWidth amacHeight amacByteSize
  parentId <- traverse requireMediaId amacParentAssetId
  let idem = digestText (T.intercalate "|"
        [ T.pack (show amacArtistId), assetKind, contentHash, driveId ])
      row = ArtistMediaAsset artistId assetKind sourceUrl
        sourceAttribution (fromMaybe now amacRetrievedAt)
        sourceContentHash amacSourceWidth amacSourceHeight sourceMimeType amacSourceByteSize
        contentHash amacWidth amacHeight mimeType amacByteSize rights driveId
        publicUrl parentId (cleanOptional amacFocalPoint) idem now
  existingDrive <- getBy (UniqueArtistMediaDriveFile driveId)
  case existingDrive of
    Just entity
      | artistMediaAssetArtistPartyId (entityVal entity) == artistId
      , artistMediaAssetAssetKind (entityVal entity) == assetKind
      , artistMediaAssetContentHash (entityVal entity) == contentHash ->
          pure (mediaEntityToDTO entity)
      | otherwise -> liftIO (fail "driveFileId is already registered to another media asset")
    Nothing -> do
      _ <- insertUnique row
      entity <- getBy (UniqueArtistMediaAsset idem) >>= maybe
        (liftIO (fail "artist media upsert failed")) pure
      pure (mediaEntityToDTO entity)

loadArtistEnrichmentOverview
  :: Maybe Text
  -> Maybe Int64
  -> SqlPersistT IO ArtistEnrichmentOverviewDTO
loadArtistEnrichmentOverview mStatus mArtistId = do
  sourceRows <- selectList sourceFilters [Desc ArtistResearchSourceRetrievedAt, LimitTo 5000]
  profileRows <- selectList ([] :: [Filter ArtistProfile]) [Asc ArtistProfileArtistPartyId]
  profiles <- fmap catMaybes $ forM profileRows $ \(Entity _ profile) -> do
    let partyId = artistProfileArtistPartyId profile
    if maybe False (/= fromSqlKey partyId) mArtistId
      then pure Nothing
      else do
        mParty <- get partyId
        mExtra <- getBy (UniqueArtistProfileEnrichment partyId)
        pure (profileEnrichmentDTO profile <$> mParty <*> pure (entityVal <$> mExtra)
          <*> pure (brokenLinkFields partyId sourceRows))
  inventoryRows <- selectList inventoryFilters [Desc ArtistInventoryReferenceLastSeenAt, LimitTo 5000]
  suggestionRows <- selectList suggestionFilters [Desc ArtistEnrichmentSuggestionUpdatedAt, LimitTo 5000]
  suggestions <- mapM suggestionEntityToDTO suggestionRows
  changeRows <- selectList changeFilters [Desc ArtistFieldChangeChangedAt, LimitTo 5000]
  runRows <- selectList ([] :: [Filter ArtistEnrichmentRun]) [Desc ArtistEnrichmentRunStartedAt, LimitTo 100]
  identityRows <- selectList identityFilters [Desc ArtistIdentityCandidateUpdatedAt, LimitTo 5000]
  mediaRows <- selectList mediaFilters [Desc ArtistMediaAssetCreatedAt, LimitTo 5000]
  pure ArtistEnrichmentOverviewDTO
    { aeoProfiles = profiles
    , aeoInventory = map inventoryEntityToDTO inventoryRows
    , aeoSources = map sourceEntityToDTO sourceRows
    , aeoSuggestions = suggestions
    , aeoChanges = map changeEntityToDTO changeRows
    , aeoRuns = map runEntityToDTO runRows
    , aeoIdentityCandidates = map identityEntityToDTO identityRows
    , aeoMedia = map mediaEntityToDTO mediaRows
    }
  where
    mPartyKey = toSqlKey <$> mArtistId
    normalizedStatus = cleanOptional (T.toLower <$> mStatus)
    inventoryFilters = maybe [] (\partyId -> [ArtistInventoryReferenceArtistPartyId ==. Just partyId]) mPartyKey
    sourceFilters = maybe [] (\partyId -> [ArtistResearchSourceArtistPartyId ==. Just partyId]) mPartyKey
    suggestionFilters =
      maybe [] (\partyId -> [ArtistEnrichmentSuggestionArtistPartyId ==. Just partyId]) mPartyKey <>
      maybe [] (\status -> [ArtistEnrichmentSuggestionStatus ==. status]) normalizedStatus
    changeFilters = maybe [] (\partyId -> [ArtistFieldChangeArtistPartyId ==. partyId]) mPartyKey
    identityFilters = maybe [] (\status -> [ArtistIdentityCandidateStatus ==. status]) normalizedStatus
    mediaFilters = maybe [] (\partyId -> [ArtistMediaAssetArtistPartyId ==. partyId]) mPartyKey

profileEnrichmentDTO
  :: ArtistProfile -> Party -> Maybe ArtistProfileEnrichment -> [Text]
  -> ArtistProfileEnrichmentDTO
profileEnrichmentDTO profile party mExtra brokenFields = ArtistProfileEnrichmentDTO
  { apeArtistId = fromSqlKey (artistProfileArtistPartyId profile)
  , apeArtistName = partyDisplayName party
  , apeOfficialName = mExtra >>= artistProfileEnrichmentOfficialName
  , apeCountry = mExtra >>= artistProfileEnrichmentCountry
  , apeInstagramUrl = mExtra >>= artistProfileEnrichmentInstagramUrl
  , apeSocialLinks = mExtra >>= artistProfileEnrichmentSocialLinks
  , apeDiscography = mExtra >>= artistProfileEnrichmentDiscography
  , apeAchievements = mExtra >>= artistProfileEnrichmentAchievements
  , apeHeroOriginalUrl = mExtra >>= artistProfileEnrichmentHeroOriginalUrl
  , apeHeroSquareUrl = mExtra >>= artistProfileEnrichmentHeroSquareUrl
  , apeHeroLandscapeUrl = mExtra >>= artistProfileEnrichmentHeroLandscapeUrl
  , apeHeroResponsiveUrls = mExtra >>= artistProfileEnrichmentHeroResponsiveUrls
  , apeHeroFocalPoint = mExtra >>= artistProfileEnrichmentHeroFocalPoint
  , apeLastVerifiedAt = mExtra >>= artistProfileEnrichmentLastVerifiedAt
  , apeConfidence = mExtra >>= artistProfileEnrichmentConfidence
  , apeReviewStatus = maybe "unverified" artistProfileEnrichmentReviewStatus mExtra
  , apeMissingFields = missingFields
  , apeBrokenFields = brokenFields
  }
  where
    missingFields = map fst . filter (isNothing . snd) $
      [ ("slug", artistProfileSlug profile)
      , ("bio", artistProfileBio profile)
      , ("city", artistProfileCity profile)
      , ("country", mExtra >>= artistProfileEnrichmentCountry)
      , ("genres", artistProfileGenres profile)
      , ("heroImageUrl", artistProfileHeroImageUrl profile)
      , ("spotifyArtistId", artistProfileSpotifyArtistId profile)
      , ("spotifyUrl", artistProfileSpotifyUrl profile)
      , ("youtubeChannelId", artistProfileYoutubeChannelId profile)
      , ("youtubeUrl", artistProfileYoutubeUrl profile)
      , ("instagramUrl", mExtra >>= artistProfileEnrichmentInstagramUrl)
      , ("websiteUrl", artistProfileWebsiteUrl profile)
      , ("featuredVideoUrl", artistProfileFeaturedVideoUrl profile)
      , ("highlights", artistProfileHighlights profile)
      , ("discography", mExtra >>= artistProfileEnrichmentDiscography)
      , ("achievements", mExtra >>= artistProfileEnrichmentAchievements)
      ]

brokenLinkFields
  :: PartyId
  -> [Entity ArtistResearchSource]
  -> [Text]
brokenLinkFields partyId rows =
  [ field
  | (field, status) <- Map.toList latestByField
  , status == "link_validation_broken"
  ]
  where
    relevant =
      [ (field, artistResearchSourceSourceType row)
      | Entity _ row <- rows
      , artistResearchSourceArtistPartyId row == Just partyId
      , artistResearchSourceSourceType row `elem` ["link_validation_valid", "link_validation_broken"]
      , field <- filter (not . T.null) . map T.strip
          . T.splitOn "," $ artistResearchSourceSupportedFields row
      ]
    -- Source rows are newest-first, so retain the first observation per field.
    latestByField = foldl (\acc (field, status) -> Map.insertWith (\_ old -> old) field status acc)
      Map.empty relevant

inventoryEntityToDTO :: Entity ArtistInventoryReference -> ArtistInventoryReferenceDTO
inventoryEntityToDTO (Entity rowId row) = ArtistInventoryReferenceDTO
  (fromSqlKey rowId)
  (artistInventoryReferenceSourceType row)
  (artistInventoryReferenceSourceRecordId row)
  (artistInventoryReferenceOriginalName row)
  (artistInventoryReferenceNormalizedName row)
  (fromSqlKey <$> artistInventoryReferenceArtistPartyId row)
  (artistInventoryReferenceSocialArtistId row)
  (artistInventoryReferenceAliases row)
  (artistInventoryReferenceEvidence row)
  (artistInventoryReferenceConfidence row)
  (artistInventoryReferenceDisposition row)
  (artistInventoryReferenceFirstSeenAt row)
  (artistInventoryReferenceLastSeenAt row)

sourceEntityToDTO :: Entity ArtistResearchSource -> ArtistResearchSourceDTO
sourceEntityToDTO (Entity rowId row) = ArtistResearchSourceDTO
  (fromSqlKey rowId)
  (fromSqlKey <$> artistResearchSourceArtistPartyId row)
  (fromSqlKey <$> artistResearchSourceInventoryReferenceId row)
  (artistResearchSourceSourceUrl row)
  (artistResearchSourceSourceType row)
  (artistResearchSourceRetrievedAt row)
  (artistResearchSourceSupportedFields row)
  (artistResearchSourceAttribution row)
  (artistResearchSourceContentHash row)

suggestionEntityToDTO
  :: Entity ArtistEnrichmentSuggestion
  -> SqlPersistT IO ArtistEnrichmentSuggestionDTO
suggestionEntityToDTO (Entity rowId row) = do
  artistName <- case artistEnrichmentSuggestionArtistPartyId row of
    Nothing -> pure Nothing
    Just partyId -> fmap partyDisplayName <$> get partyId
  pure ArtistEnrichmentSuggestionDTO
    { aesId = fromSqlKey rowId
    , aesArtistId = fromSqlKey <$> artistEnrichmentSuggestionArtistPartyId row
    , aesInventoryReferenceId = fromSqlKey <$> artistEnrichmentSuggestionInventoryReferenceId row
    , aesArtistName = artistName
    , aesFieldName = artistEnrichmentSuggestionFieldName row
    , aesCurrentValue = artistEnrichmentSuggestionCurrentValue row
    , aesProposedValue = artistEnrichmentSuggestionProposedValue row
    , aesConfidence = artistEnrichmentSuggestionConfidence row
    , aesStatus = artistEnrichmentSuggestionStatus row
    , aesAutoPublish = artistEnrichmentSuggestionAutoPublish row
    , aesEvidence = artistEnrichmentSuggestionEvidence row
    , aesCreatedAt = artistEnrichmentSuggestionCreatedAt row
    , aesUpdatedAt = artistEnrichmentSuggestionUpdatedAt row
    , aesDecidedAt = artistEnrichmentSuggestionDecidedAt row
    , aesDecidedBy = fromSqlKey <$> artistEnrichmentSuggestionDecidedBy row
    , aesDecisionNote = artistEnrichmentSuggestionDecisionNote row
    }

changeEntityToDTO :: Entity ArtistFieldChange -> ArtistFieldChangeDTO
changeEntityToDTO (Entity rowId row) = ArtistFieldChangeDTO
  (fromSqlKey rowId)
  (fromSqlKey (artistFieldChangeArtistPartyId row))
  (fromSqlKey <$> artistFieldChangeSuggestionId row)
  (artistFieldChangeFieldName row)
  (artistFieldChangePreviousValue row)
  (artistFieldChangeNewValue row)
  (artistFieldChangeEvidence row)
  (artistFieldChangeConfidence row)
  (artistFieldChangeActor row)
  (artistFieldChangeChangedAt row)

runEntityToDTO :: Entity ArtistEnrichmentRun -> ArtistEnrichmentRunDTO
runEntityToDTO (Entity rowId row) = ArtistEnrichmentRunDTO
  (fromSqlKey rowId)
  (artistEnrichmentRunRunKey row)
  (artistEnrichmentRunMode row)
  (artistEnrichmentRunScope row)
  (fromSqlKey <$> artistEnrichmentRunRequestedArtistId row)
  (artistEnrichmentRunStatus row)
  (artistEnrichmentRunPhase row)
  (artistEnrichmentRunCheckpoint row)
  (artistEnrichmentRunCounters row)
  (artistEnrichmentRunErrorSummary row)
  (artistEnrichmentRunStartedAt row)
  (artistEnrichmentRunHeartbeatAt row)
  (artistEnrichmentRunFinishedAt row)

identityEntityToDTO :: Entity ArtistIdentityCandidate -> ArtistIdentityCandidateDTO
identityEntityToDTO (Entity rowId row) = ArtistIdentityCandidateDTO
  (fromSqlKey rowId)
  (fromSqlKey (artistIdentityCandidateInventoryReferenceId row))
  (fromSqlKey <$> artistIdentityCandidateArtistPartyId row)
  (artistIdentityCandidateProvider row)
  (artistIdentityCandidateExternalId row)
  (artistIdentityCandidateCandidateUrl row)
  (artistIdentityCandidateEvidence row)
  (artistIdentityCandidateConfidence row)
  (artistIdentityCandidateStatus row)
  (artistIdentityCandidateCreatedAt row)
  (artistIdentityCandidateUpdatedAt row)
  (artistIdentityCandidateDecidedAt row)
  (fromSqlKey <$> artistIdentityCandidateDecidedBy row)
  (artistIdentityCandidateDecisionNote row)

mediaEntityToDTO :: Entity ArtistMediaAsset -> ArtistMediaAssetDTO
mediaEntityToDTO (Entity rowId row) = ArtistMediaAssetDTO
  (fromSqlKey rowId)
  (fromSqlKey (artistMediaAssetArtistPartyId row))
  (artistMediaAssetAssetKind row)
  (artistMediaAssetSourceUrl row)
  (artistMediaAssetSourceAttribution row)
  (artistMediaAssetRetrievedAt row)
  (artistMediaAssetSourceContentHash row)
  (artistMediaAssetSourceWidth row)
  (artistMediaAssetSourceHeight row)
  (artistMediaAssetSourceMimeType row)
  (artistMediaAssetSourceByteSize row)
  (artistMediaAssetContentHash row)
  (artistMediaAssetWidth row)
  (artistMediaAssetHeight row)
  (artistMediaAssetMimeType row)
  (artistMediaAssetByteSize row)
  (artistMediaAssetRightsStatus row)
  (artistMediaAssetDriveFileId row)
  (artistMediaAssetPublicUrl row)
  (fromSqlKey <$> artistMediaAssetParentAssetId row)
  (artistMediaAssetFocalPoint row)
  (artistMediaAssetCreatedAt row)

requireSuggestion :: Int64 -> SqlPersistT IO (Entity ArtistEnrichmentSuggestion)
requireSuggestion rawId = do
  let rowId = toSqlKey rawId
  row <- get rowId >>= maybe (liftIO (fail "artist suggestion not found")) pure
  pure (Entity rowId row)

requirePartyId :: Int64 -> SqlPersistT IO PartyId
requirePartyId rawId
  | rawId <= 0 = liftIO (fail "artistId must be positive")
  | otherwise = do
      let partyId = toSqlKey rawId
      present <- get partyId
      maybe (liftIO (fail "artist party not found")) (const (pure partyId)) present

requireInventoryId :: Int64 -> SqlPersistT IO ArtistInventoryReferenceId
requireInventoryId rawId
  | rawId <= 0 = liftIO (fail "inventoryReferenceId must be positive")
  | otherwise = do
      let rowId = toSqlKey rawId
      present <- get rowId
      maybe (liftIO (fail "inventory reference not found")) (const (pure rowId)) present

requireMediaId :: Int64 -> SqlPersistT IO ArtistMediaAssetId
requireMediaId rawId
  | rawId <= 0 = liftIO (fail "parentAssetId must be positive")
  | otherwise = do
      let rowId = toSqlKey rawId
      present <- get rowId
      maybe (liftIO (fail "parent media asset not found")) (const (pure rowId)) present

validateFieldName :: Text -> SqlPersistT IO Text
validateFieldName raw = do
  let value = T.strip raw
      allowed =
        [ "officialName", "slug", "bio", "city", "country", "genres"
        , "heroImageUrl", "heroOriginalUrl", "heroSquareUrl", "heroLandscapeUrl"
        , "heroResponsiveUrls", "heroFocalPoint"
        , "lastVerifiedAt", "confidence", "reviewStatus"
        , "spotifyArtistId", "spotifyUrl", "youtubeChannelId"
        , "youtubeUrl", "instagramUrl", "socialLinks", "websiteUrl"
        , "featuredVideoUrl", "highlights", "discography", "achievements"
        ]
  unless (value `elem` allowed) (liftIO (fail "unsupported artist field"))
  pure value

evidenceSignalCount :: Text -> Int
evidenceSignalCount raw = length (nub parsedSignals)
  where
    parsedSignals :: [Text]
    parsedSignals = fromMaybe [] $ do
      value <- Aeson.decodeStrict' (TE.encodeUtf8 raw)
      parseMaybe (Aeson.withObject "artist evidence" (\obj -> obj .:? "signals" .!= [])) value

validateFieldValue :: Text -> Maybe Text -> SqlPersistT IO (Maybe Text)
validateFieldValue _ Nothing = pure Nothing
validateFieldValue fieldName (Just raw) = do
  let value = T.strip raw
      bounded maxChars = if T.length value <= maxChars
        then pure (Just value)
        else liftIO (fail (T.unpack fieldName <> " is too long"))
      publicUrl = Just <$> requirePublicUrl fieldName value
      validJson predicate message = case Aeson.eitherDecodeStrict' (TE.encodeUtf8 value) of
        Right decoded | predicate decoded -> pure (Just value)
        _ -> liftIO (fail message)
  case fieldName of
    "slug"
      | T.null value || T.length value > 96 || slugifyArtistName value /= value ->
          liftIO (fail "slug must be a non-empty lowercase URL slug up to 96 characters")
      | otherwise -> pure (Just value)
    "spotifyUrl" -> publicUrl
    "youtubeUrl" -> publicUrl
    "instagramUrl" -> publicUrl
    "websiteUrl" -> publicUrl
    "featuredVideoUrl" -> publicUrl
    "heroImageUrl" -> publicUrl
    "heroOriginalUrl" -> publicUrl
    "heroSquareUrl" -> publicUrl
    "heroLandscapeUrl" -> publicUrl
    "heroResponsiveUrls" -> validJson isObject "heroResponsiveUrls must be a JSON object"
    "socialLinks" -> validJson isObject "socialLinks must be a JSON object"
    "discography" -> validJson isArray "discography must be a JSON array"
    "achievements" -> validJson isArray "achievements must be a JSON array"
    "lastVerifiedAt" -> case parseUtcTimestamp value of
      Just _ -> pure (Just value)
      Nothing -> liftIO (fail "lastVerifiedAt must be an ISO-8601 UTC timestamp")
    "confidence" -> case readMaybe (T.unpack value) of
      Just confidence | confidence >= (0 :: Double) && confidence <= 1 -> pure (Just value)
      _ -> liftIO (fail "confidence must be between 0 and 1")
    "reviewStatus"
      | value `elem` ["unverified", "pending", "verified", "rejected", "ambiguous"] -> pure (Just value)
      | otherwise -> liftIO (fail "unsupported reviewStatus")
    "officialName" -> bounded 200
    "city" -> bounded 200
    "country" -> bounded 200
    "genres" -> bounded 1000
    "spotifyArtistId" -> bounded 500
    "youtubeChannelId" -> bounded 500
    "heroFocalPoint" -> bounded 100
    "bio" -> bounded 12000
    "highlights" -> bounded 12000
    _ -> bounded 12000
  where
    isObject (Aeson.Object _) = True
    isObject _ = False
    isArray (Aeson.Array _) = True
    isArray _ = False

validateConfidence :: Double -> SqlPersistT IO Double
validateConfidence value
  | isNaN value || isInfinite value || value < 0 || value > 1 =
      liftIO (fail "confidence must be between 0 and 1")
  | otherwise = pure value

requireShortText :: Text -> Int -> Text -> SqlPersistT IO Text
requireShortText fieldName maxChars raw =
  case nonBlank raw of
    Nothing -> liftIO (fail (T.unpack fieldName <> " is required"))
    Just value
      | T.length value > maxChars -> liftIO (fail (T.unpack fieldName <> " is too long"))
      | otherwise -> pure value

cleanOptional :: Maybe Text -> Maybe Text
cleanOptional = (>>= nonBlank)

requirePublicUrl :: Text -> Text -> SqlPersistT IO Text
requirePublicUrl fieldName raw = do
  value <- requireShortText fieldName 2048 raw
  if TrialsServer.isValidHttpUrl value
    then pure value
    else liftIO (fail (T.unpack fieldName <> " must be an absolute public http or https URL"))

requireHexHash :: Text -> SqlPersistT IO Text
requireHexHash raw = do
  value <- requireShortText "contentHash" 128 (T.toLower raw)
  if T.length value `elem` [64, 128] && T.all (\ch -> isDigit ch || ch `elem` ("abcdef" :: String)) value
    then pure value
    else liftIO (fail "contentHash must be a SHA-256 or SHA-512 hex digest")

requireMimeType :: Text -> SqlPersistT IO Text
requireMimeType raw = do
  value <- requireShortText "mimeType" 80 (T.toLower raw)
  if value `elem` ["image/avif", "image/webp"]
    then pure value
    else liftIO (fail "artist media must be image/avif or image/webp")

requireSourceMimeType :: Text -> SqlPersistT IO Text
requireSourceMimeType raw = do
  value <- requireShortText "sourceMimeType" 80 (T.toLower raw)
  if value `elem` ["image/jpeg", "image/png", "image/avif", "image/webp"]
    then pure value
    else liftIO (fail "artist image source must be JPEG, PNG, AVIF, or WebP")

requireRights :: Text -> SqlPersistT IO Text
requireRights raw = do
  value <- requireShortText "rightsStatus" 40 (T.toLower raw)
  if value `elem` ["authorized", "licensed"]
    then pure value
    else liftIO (fail "artist media requires authorized or licensed rightsStatus")

requireDriveId :: Text -> SqlPersistT IO Text
requireDriveId raw = do
  value <- requireShortText "driveFileId" 256 raw
  if T.all (\ch -> isAsciiLower ch || isAsciiUpper ch || isDigit ch || ch == '-' || ch == '_') value
    then pure value
    else liftIO (fail "driveFileId contains unsupported characters")

validateMediaBudget :: Text -> Int -> Int -> Int64 -> SqlPersistT IO ()
validateMediaBudget kind width height bytes = case kind of
  "original" -> do
    when (max width height > 2560) (liftIO (fail "optimized original long edge exceeds 2560px"))
    when (bytes > 2 * 1024 * 1024) (liftIO (fail "optimized original exceeds 2MB"))
  "square" -> do
    when (width /= 1024 || height /= 1024) (liftIO (fail "square derivative must be 1024x1024"))
    when (bytes > 400 * 1024) (liftIO (fail "square derivative exceeds 400KB"))
  "landscape" -> do
    when (width /= 1600 || height /= 900) (liftIO (fail "landscape derivative must be 1600x900"))
    when (bytes > 500 * 1024) (liftIO (fail "landscape derivative exceeds 500KB"))
  "responsive" -> when (bytes > 250 * 1024) (liftIO (fail "responsive derivative exceeds 250KB"))
  _ -> liftIO (fail "assetKind must be original, square, landscape, or responsive")

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.DDEX.MatchEngine
  ( -- * Match Engine
    detectConflicts
  , Conflict(..)
  , ConflictType(..)
  , MatchResult(..)
    -- * Query helpers
  , findExistingResourceByIsrc
  , findExistingReleaseByUpc
  , findExistingPartyByDpid
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe, catMaybes)
import Data.Int (Int64)
import Database.Persist
import Database.Persist.Sql (SqlPersistT, selectList, (==.))
import TDF.DDEX.ERN.V432.Normalize
import TDF.DDEX.Models

-- | Type of conflict detected
data ConflictType
  = ConflictDuplicateIsrc       -- ^ ISRC already exists in catalog
  | ConflictDuplicateUpc        -- ^ UPC already exists in catalog
  | ConflictDuplicateParty      -- ^ Party with same DPID/IPI/ISNI exists
  | ConflictDataMismatch        -- ^ Existing data differs from DDEX
  | ConflictMissingReference    -- ^ Referenced entity not found
  | ConflictAmbiguousMatch      -- ^ Multiple possible matches found
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | A detected conflict
data Conflict = Conflict
  { conflictType        :: ConflictType
  , conflictEntityType  :: Text        -- ^ "Resource", "Release", "Party", etc.
  , conflictIdentifier  :: Text        -- ^ ISRC, UPC, DPID, etc.
  , conflictDescription :: Text        -- ^ Human-readable description
  , conflictExistingId  :: Maybe Int64 -- ^ ID of existing entity if found
  , conflictSuggestedAction :: Text    -- ^ "UseExisting", "CreateNew", "Merge", "Review"
  } deriving (Show, Eq)

-- | Result of matching incoming data against catalog
data MatchResult = MatchResult
  { matchConflicts     :: [Conflict]
  , matchNewEntities   :: Int          -- ^ Count of entities to create
  , matchExistingMatches :: Int        -- ^ Count of entities matching existing
  , matchWarnings      :: [Text]       -- ^ Non-blocking warnings
  } deriving (Show, Eq)

-- | Detect conflicts between canonical import and existing catalog
detectConflicts :: CanonicalImport -> SqlPersistT IO MatchResult
detectConflicts ci = do
  -- Check for ISRC conflicts
  isrcConflicts <- detectIsrcConflicts ci

  -- Check for UPC conflicts
  upcConflicts <- detectUpcConflicts ci

  -- Check for party conflicts
  partyConflicts <- detectPartyConflicts ci

  -- Check for missing references
  refConflicts <- detectMissingReferences ci

  let allConflicts = isrcConflicts ++ upcConflicts ++ partyConflicts ++ refConflicts
      newEntities = countNewEntities ci
      existingMatches = countExistingMatches ci
      warnings = generateWarnings ci

  return MatchResult
    { matchConflicts = allConflicts
    , matchNewEntities = newEntities
    , matchExistingMatches = existingMatches
    , matchWarnings = warnings
    }

-- | Detect ISRC conflicts (sound recordings with existing ISRCs)
detectIsrcConflicts :: CanonicalImport -> SqlPersistT IO [Conflict]
detectIsrcConflicts ci = do
  let resourcesWithIsrc = filter hasIsrc (ciResources ci)
  conflicts <- mapM checkIsrc resourcesWithIsrc
  return (catMaybes conflicts)
  where
    hasIsrc r = case cresIsrc r of
      Just _ -> True
      Nothing -> False

    checkIsrc CanonicalResource{..} = case cresIsrc of
      Nothing -> return Nothing
      Just isrc -> do
        mExisting <- findExistingResourceByIsrc isrc
        case mExisting of
          Nothing -> return Nothing
          Just existingId -> return $ Just Conflict
            { conflictType = ConflictDuplicateIsrc
            , conflictEntityType = "Resource"
            , conflictIdentifier = isrc
            , conflictDescription = "Sound recording with ISRC " <> isrc <> " already exists"
            , conflictExistingId = Just existingId
            , conflictSuggestedAction = "Review"
            }

-- | Detect UPC conflicts (releases with existing UPCs)
detectUpcConflicts :: CanonicalImport -> SqlPersistT IO [Conflict]
detectUpcConflicts ci = do
  let releasesWithUpc = filter hasUpc (ciReleases ci)
  conflicts <- mapM checkUpc releasesWithUpc
  return (catMaybes conflicts)
  where
    hasUpc r = case crUpc r of
      Just _ -> True
      Nothing -> False

    checkUpc CanonicalRelease{..} = case crUpc of
      Nothing -> return Nothing
      Just upc -> do
        mExisting <- findExistingReleaseByUpc upc
        case mExisting of
          Nothing -> return Nothing
          Just existingId -> return $ Just Conflict
            { conflictType = ConflictDuplicateUpc
            , conflictEntityType = "Release"
            , conflictIdentifier = upc
            , conflictDescription = "Release with UPC " <> upc <> " already exists"
            , conflictExistingId = Just existingId
            , conflictSuggestedAction = "Review"
            }

-- | Detect party conflicts (parties with existing DPID/IPI/ISNI)
detectPartyConflicts :: CanonicalImport -> SqlPersistT IO [Conflict]
detectPartyConflicts ci = do
  conflicts <- mapM checkParty (ciParties ci)
  return (concat conflicts)
  where
    checkParty CanonicalParty{..} = do
      dpidConflicts <- case cpDPID of
        Nothing -> return []
        Just dpid -> do
          mExisting <- findExistingPartyByDpid dpid
          case mExisting of
            Nothing -> return []
            Just existingId -> return [Conflict
              { conflictType = ConflictDuplicateParty
              , conflictEntityType = "Party"
              , conflictIdentifier = "DPID:" <> dpid
              , conflictDescription = "Party with DPID " <> dpid <> " already exists"
              , conflictExistingId = Just existingId
              , conflictSuggestedAction = "UseExisting"
              }]
      return dpidConflicts

-- | Detect missing references (resource refs in releases that don't exist)
detectMissingReferences :: CanonicalImport -> SqlPersistT IO [Conflict]
detectMissingReferences ci = do
  let resourceRefs = map cresSourcePartyRef (ciResources ci)
      releaseRefs = concatMap crResourceRefs (ciReleases ci)
      missingRefs = filter (`notElem` resourceRefs) releaseRefs
  return $ map mkConflict missingRefs
  where
    mkConflict ref = Conflict
      { conflictType = ConflictMissingReference
      , conflictEntityType = "ResourceReference"
      , conflictIdentifier = ref
      , conflictDescription = "Release references non-existent resource: " <> ref
      , conflictExistingId = Nothing
      , conflictSuggestedAction = "Review"
      }

-- | Count entities that would be created (no existing match)
countNewEntities :: CanonicalImport -> Int
countNewEntities ci =
  length (ciResources ci) +
  length (ciReleases ci) +
  length (ciParties ci)

-- | Count entities that match existing catalog entries
countExistingMatches :: CanonicalImport -> Int
countExistingMatches _ci = 0  -- TODO: Implement after matching logic

-- | Generate warnings for the import
generateWarnings :: CanonicalImport -> [Text]
generateWarnings ci =
  let noCopyright = filter noCopyrightLine (ciReleases ci)
      noIsrc = filter noIsrcRecording (ciResources ci)
  in concat
    [ map (\r -> "Release missing copyright: " <> crTitle r) noCopyright
    , map (\r -> "Sound recording missing ISRC: " <> cresTitle r) noIsrc
    ]
  where
    noCopyrightLine r = case (crCopyrightLine r, crPhonographicCopyrightLine r) of
      (Nothing, Nothing) -> True
      _ -> False
    noIsrcRecording r = cresIsrc r == Nothing

-- | Find existing resource by ISRC
findExistingResourceByIsrc :: Text -> SqlPersistT IO (Maybe Int64)
findExistingResourceByIsrc _isrc = do
  -- TODO: Query catalog_resource joined with catalog_identifier
  -- For now, return Nothing
  return Nothing

-- | Find existing release by UPC
findExistingReleaseByUpc :: Text -> SqlPersistT IO (Maybe Int64)
findExistingReleaseByUpc _upc = do
  -- TODO: Query catalog_release joined with catalog_identifier
  -- For now, return Nothing
  return Nothing

-- | Find existing party by DPID
findExistingPartyByDpid :: Text -> SqlPersistT IO (Maybe Int64)
findExistingPartyByDpid _dpid = do
  -- TODO: Query party table with DPID identifier
  -- For now, return Nothing
  return Nothing

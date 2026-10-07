{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.MusicRelease.ContentValidation
  ( validateMusicReleaseContent
  , normalizeMusicIdentifier
  ) where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import TDF.API.MusicRelease
import TDF.MusicRelease.Domain
  ( IdentifierError(..)
  , IdentifierType(..)
  , validateIdentifier
  , validateMoney
  )

validateMusicReleaseContent :: MusicReleaseContentRequest -> Either Text ()
validateMusicReleaseContent MusicReleaseContentRequest{..} = do
  require (not (null musicContentTracks) && length musicContentTracks <= 500) "A release needs 1-500 tracks."
  require (length musicContentParties <= 1000 && length musicContentCredits <= 5000) "Catalog party or credit limit exceeded."
  requireUnique "track clientRef" (map musicTrackClientRef musicContentTracks)
  requireUnique "party clientRef" (map musicPartyClientRef musicContentParties)
  requireUnique "disc and track number" (map (\track -> T.pack (show (musicTrackDiscNumber track,musicTrackTrackNumber track))) musicContentTracks)
  requireUnique "recordingId" [T.pack (show value) | Just value <- map musicTrackRecordingId musicContentTracks]
  let trackRefs = Set.fromList (map musicTrackClientRef musicContentTracks)
      partyRefs = Set.fromList (map musicPartyClientRef musicContentParties)
  forM_ musicContentTracks validateTrack
  forM_ musicContentParties validateParty
  forM_ musicContentCredits (validateCredit trackRefs partyRefs)
  forM_ musicContentIdentifiers (validateCatalogIdentifier trackRefs)
  forM_ musicContentRightsDeclarations (validateRights trackRefs partyRefs)
  forM_ musicContentAvailability (validateAvailability trackRefs)
  requireUnique "credit" (map creditKey musicContentCredits)
  requireUnique "identifier" (map identifierKey musicContentIdentifiers)
  requireUnique "availability target" (map (fromMaybe "release" . musicAvailabilityTrackRef) musicContentAvailability)

normalizeMusicIdentifier :: Text -> Text -> Text
normalizeMusicIdentifier rawType rawValue = case identifierKind rawType of
  Right kind -> case validateIdentifier kind rawValue of
    Right value -> value
    Left _ -> T.strip rawValue
  Left _ -> T.strip rawValue

validateTrack :: MusicTrackDraft -> Either Text ()
validateTrack MusicTrackDraft{..} = do
  require (validClientRef musicTrackClientRef) "Each track clientRef must contain 1-100 safe characters."
  require (validRequiredText 500 (T.strip musicTrackTitle)) "Each track needs a title of at most 500 characters."
  require (validRequiredText 500 (T.strip musicTrackDisplayArtist)) "Each track needs a displayArtist."
  require (validLanguage musicTrackTitleLanguage) "Each track titleLanguage must be a supported BCP 47 language tag."
  require (musicTrackExplicitContent `elem` ["not_explicit","explicit","cleaned","unknown"]) "Track explicitContent is invalid."
  require (musicTrackDiscNumber > 0 && musicTrackTrackNumber > 0) "Disc and track numbers must be positive."
  require (maybe True (>=0) musicTrackPreviewStartMs) "previewStartMs cannot be negative."
  require (maybe True (>0) musicTrackPreviewDurationMs) "previewDurationMs must be positive."
  require (musicTrackPreviewStartMs == Nothing || musicTrackPreviewDurationMs /= Nothing) "previewStartMs requires previewDurationMs."

validateParty :: MusicPartyDraft -> Either Text ()
validateParty MusicPartyDraft{..} = do
  require (validClientRef musicPartyClientRef) "Each party clientRef must contain 1-100 safe characters."
  require (validRequiredText 500 (T.strip musicPartyDisplayName)) "Each credited party needs a displayName."
  require (musicPartyKind `elem` ["person","organization","unknown"]) "partyKind is invalid."
  require (musicPartyId == Nothing || musicPartyTdfPartyId == Nothing) "Use partyId or tdfPartyId, not both."
  requireUnique "party identifier" (map partyIdentifierKey musicPartyIdentifiers)
  forM_ musicPartyIdentifiers $ \MusicPartyIdentifierDraft{..} -> do
    identifierType <- partyIdentifierKind musicPartyIdentifierType
    case validateIdentifier identifierType musicPartyIdentifierValue of
      Left IdentifierError{..} -> Left identifierErrorMessage
      Right _ -> pure ()

validateCredit :: Set.Set Text -> Set.Set Text -> MusicCreditDraft -> Either Text ()
validateCredit trackRefs partyRefs MusicCreditDraft{..} = do
  require (musicCreditPartyRef `Set.member` partyRefs) ("Unknown credit partyRef: " <> musicCreditPartyRef)
  forM_ musicCreditTrackRef $ \ref -> require (ref `Set.member` trackRefs) ("Unknown credit trackRef: " <> ref)
  require (musicCreditRole `elem` creditRoles) ("Unsupported credit role: " <> musicCreditRole)
  require (musicCreditDisplayOrder >= 0) "Credit displayOrder cannot be negative."

validateCatalogIdentifier :: Set.Set Text -> MusicIdentifierDraft -> Either Text ()
validateCatalogIdentifier trackRefs MusicIdentifierDraft{..} = do
  forM_ musicIdentifierTrackRef $ \ref -> require (ref `Set.member` trackRefs) ("Unknown identifier trackRef: " <> ref)
  identifierType <- identifierKind musicIdentifierType
  case (musicIdentifierTrackRef,identifierType) of
    (Nothing,ISRC) -> Left "ISRC identifies a recording, not a release."
    (Just _,UPC) -> Left "UPC identifies a release, not a recording."
    (Just _,EAN) -> Left "EAN identifies a release, not a recording."
    (Just _,GRid) -> Left "GRid identifies a release, not a recording."
    (_,ISNI) -> Left "ISNI identifies a party, not a release or recording."
    (_,IPI) -> Left "IPI identifies a party, not a release or recording."
    (_,DPID) -> Left "DPID identifies a supply-chain party, not a release or recording."
    _ -> pure ()
  case validateIdentifier identifierType musicIdentifierValue of
    Left IdentifierError{..} -> Left identifierErrorMessage
    Right _ -> pure ()

validateRights :: Set.Set Text -> Set.Set Text -> MusicRightsDraft -> Either Text ()
validateRights trackRefs partyRefs MusicRightsDraft{..} = do
  forM_ musicRightsTrackRef $ \ref -> require (ref `Set.member` trackRefs) ("Unknown rights trackRef: " <> ref)
  require (musicRightsScope `elem` ["master","composition"]) "rightsScope must be master or composition."
  require (validRequiredText 1000 (T.strip musicRightsAuthorityBasis)) "Each rights declaration needs an authorityBasis."
  validateTerritories musicRightsTerritories
  require (maybe True (>=musicRightsStartsOn) musicRightsEndsOn) "Rights endsOn cannot precede startsOn."
  require (not (null musicRightsSplits) && sum (map musicSplitBasisPoints musicRightsSplits)==10000) "Each rights declaration must total exactly 10000 basis points."
  forM_ musicRightsSplits $ \MusicSplitDraft{..} -> do
    require (musicSplitPartyRef `Set.member` partyRefs) ("Unknown split partyRef: " <> musicSplitPartyRef)
    require (musicSplitBasisPoints > 0 && musicSplitBasisPoints <= 10000) "Split basisPoints must be between 1 and 10000."
    validateTerritories musicSplitTerritories
    require (maybe True (>=musicSplitStartsOn) musicSplitEndsOn) "Split endsOn cannot precede startsOn."

validateAvailability :: Set.Set Text -> MusicAvailabilityDraft -> Either Text ()
validateAvailability trackRefs MusicAvailabilityDraft{..} = do
  forM_ musicAvailabilityTrackRef $ \ref -> require (ref `Set.member` trackRefs) ("Unknown availability trackRef: " <> ref)
  require (musicAvailabilityTerritoryMode `elem` ["include","exclude"]) "territoryMode must be include or exclude."
  validateTerritories musicAvailabilityTerritories
  require (not (musicAvailabilityTerritoryMode=="exclude" && "Worldwide" `elem` musicAvailabilityTerritories)) "Worldwide cannot be used in an exclusion rule."
  require (maybe True id ((<) <$> musicAvailabilityStartsAt <*> musicAvailabilityEndsAt)) "Availability endsAt must follow startsAt."
  require (musicAvailabilityListeningPolicy `elem` ["none","preview","full"]) "listeningPolicy is invalid."
  require (musicAvailabilityDownloadPolicy `elem` ["none","free","purchase"]) "downloadPolicy is invalid."
  case musicAvailabilityDownloadPolicy of
    "none" -> require (musicAvailabilityDownloadableAssetId==Nothing && not musicAvailabilityPurchasable) "Non-downloadable content cannot be purchasable or name a download asset."
    "free" -> require (musicAvailabilityDownloadableAssetId/=Nothing && not musicAvailabilityPurchasable && musicAvailabilityPriceMinor==Nothing && musicAvailabilityCurrency==Nothing) "Free downloads require an asset and cannot have a price."
    "purchase" -> do
      require (musicAvailabilityDownloadableAssetId/=Nothing && musicAvailabilityPurchasable) "Purchase downloads require purchasable=true and a downloadable asset."
      case (musicAvailabilityCurrency,musicAvailabilityPriceMinor) of
        (Just currency,Just amount) -> case validateMoney ["USD"] True currency (fromIntegral amount) of
          Left message -> Left message
          Right _ -> pure ()
        _ -> Left "Purchase downloads require priceMinor and currency."
    _ -> pure ()

identifierKind :: Text -> Either Text IdentifierType
identifierKind raw = case T.toLower (T.strip raw) of
  "isrc" -> Right ISRC
  "upc" -> Right UPC
  "ean" -> Right EAN
  "grid" -> Right GRid
  "isni" -> Right ISNI
  "ipi" -> Right IPI
  "dpid" -> Right DPID
  "proprietary" -> Right Proprietary
  _ -> Left "Unsupported identifierType."

partyIdentifierKind :: Text -> Either Text IdentifierType
partyIdentifierKind raw = do
  kind <- identifierKind raw
  if kind `elem` [ISNI,IPI,DPID,Proprietary]
    then Right kind
    else Left "Party identifierType must be isni, ipi, dpid, or proprietary."

creditRoles :: [Text]
creditRoles =
  [ "main_artist","featured_artist","display_artist","composer","lyricist","performer"
  , "producer","engineer","mixer","mastering_engineer","publisher","label","other"
  ]

validateTerritories :: [Text] -> Either Text ()
validateTerritories values = do
  require (not (null values)) "At least one territory is required."
  require (all validTerritory values) "Territories must be Worldwide or uppercase ISO 3166-1 alpha-2 codes."
  requireUnique "territory" values

validTerritory :: Text -> Bool
validTerritory value = value=="Worldwide" || (T.length value==2 && T.all (\c -> c>='A' && c<='Z') value)

validLanguage :: Text -> Bool
validLanguage value = case T.splitOn "-" (T.strip value) of
  [] -> False
  primary : parts -> T.length value <= 35 && T.length primary `elem` [2,3]
    && all (not . T.null) (primary:parts) && T.all (\c -> isAsciiAlphaNum c || c=='-') value

validClientRef :: Text -> Bool
validClientRef value = validRequiredText 100 value && T.all (\c -> isAsciiAlphaNum c || c `elem` ("._:-" :: String)) value

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = (c>='a' && c<='z') || (c>='A' && c<='Z') || (c>='0' && c<='9')

requireUnique :: Text -> [Text] -> Either Text ()
requireUnique label values = require (Set.size (Set.fromList values)==length values) ("Duplicate " <> label <> ".")

creditKey :: MusicCreditDraft -> Text
creditKey MusicCreditDraft{..} = T.intercalate ":" [fromMaybe "release" musicCreditTrackRef,musicCreditPartyRef,musicCreditRole]

identifierKey :: MusicIdentifierDraft -> Text
identifierKey MusicIdentifierDraft{..} = T.intercalate ":" [fromMaybe "release" musicIdentifierTrackRef,T.toLower musicIdentifierType,T.toUpper (T.filter (`notElem` ['-',' ']) musicIdentifierValue)]

partyIdentifierKey :: MusicPartyIdentifierDraft -> Text
partyIdentifierKey MusicPartyIdentifierDraft{..} =
  T.toLower musicPartyIdentifierType <> ":" <> T.toUpper (T.filter (`notElem` ['-',' ']) musicPartyIdentifierValue)

require :: Bool -> Text -> Either Text ()
require True _ = Right ()
require False message = Left message

validRequiredText :: Int -> Text -> Bool
validRequiredText maxLength value = not (T.null (T.strip value)) && T.length value <= maxLength

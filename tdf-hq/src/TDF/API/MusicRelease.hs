{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.MusicRelease
  ( MusicReleasePublicAPI
  , MusicReleaseProtectedAPI
  , MusicReleaseCreateRequest(..)
  , MusicReleaseDraftSaveRequest(..)
  , MusicReleaseContentRequest(..)
  , MusicTrackDraft(..)
  , MusicPartyDraft(..)
  , MusicPartyIdentifierDraft(..)
  , MusicCreditDraft(..)
  , MusicIdentifierDraft(..)
  , MusicRightsDraft(..)
  , MusicSplitDraft(..)
  , MusicAvailabilityDraft(..)
  , MusicTermsAcceptanceRequest(..)
  , MusicReleaseTransitionRequest(..)
  , MusicReleaseCommentRequest(..)
  , MusicPlaybackEventRequest(..)
  , MusicFavoriteRequest(..)
  , MusicPlaylistCreateRequest(..)
  , MusicPlaylistItemRequest(..)
  , MusicPlaylistMoveRequest(..)
  , MusicInfringementReportRequest(..)
  , MusicInfringementActionRequest(..)
  , MusicPurchaseCreateRequest(..)
  , MusicDatafastStatusRequest(..)
  , MusicPaypalCaptureRequest(..)
  , MusicDownloadAuthorizeRequest(..)
  , MusicFreeDownloadRequest(..)
  , MusicDdexPartyRequest(..)
  , MusicDdexExportRequest(..)
  , MusicUploadCreateRequest(..)
  , MusicUploadProviderRequest(..)
  , MusicUploadPartRequest(..)
  , MusicUploadConfirmRequest(..)
  ) where

import Data.Aeson
  ( FromJSON(..)
  , Options
  , ToJSON(..)
  , Value
  , defaultOptions
  , fieldLabelModifier
  , genericParseJSON
  , genericToJSON
  , rejectUnknownFields
  )
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Servant
import TDF.UUIDInstances ()

type MusicReleasePublicAPI = "music" :>
  (    Header "CF-IPCountry" Text :> "releases"
         :> QueryParam "q" Text
         :> QueryParam "artistPartyId" Int64
         :> QueryParam "limit" Int
         :> QueryParam "offset" Int
         :> Get '[JSON] [Value]
  :<|> "releases" :> Capture "slug" Text :> Header "CF-IPCountry" Text :> Get '[JSON] Value
  :<|> "assets" :> Capture "assetId" UUID :> "access"
         :> Header "CF-IPCountry" Text :> Get '[JSON] Value
  :<|> "playback-events" :> Header "CF-IPCountry" Text
         :> ReqBody '[JSON] MusicPlaybackEventRequest :> Post '[JSON] NoContent
  )

type MusicReleaseProtectedAPI = "music" :>
  (    "studio" :> "releases" :> QueryParam "artistPartyId" Int64 :> Get '[JSON] [Value]
  :<|> "releases" :> Header "Idempotency-Key" Text :> ReqBody '[JSON] MusicReleaseCreateRequest :> PostCreated '[JSON] Value
  :<|> "releases" :> Capture "releaseId" UUID :> "versions" :> Capture "versionId" UUID :> Get '[JSON] Value
  :<|> "releases" :> Capture "releaseId" UUID :> "versions" :> Capture "versionId" UUID :> "corrections"
         :> Header "Idempotency-Key" Text :> PostCreated '[JSON] Value
  :<|> "releases" :> Capture "releaseId" UUID :> "versions" :> Capture "versionId" UUID :> "draft"
         :> ReqBody '[JSON] MusicReleaseDraftSaveRequest :> Put '[JSON] Value
  :<|> "releases" :> Capture "releaseId" UUID :> "versions" :> Capture "versionId" UUID :> "content"
         :> ReqBody '[JSON] MusicReleaseContentRequest :> Put '[JSON] Value
  :<|> "releases" :> Capture "releaseId" UUID :> "versions" :> Capture "versionId" UUID :> "terms"
         :> ReqBody '[JSON] MusicTermsAcceptanceRequest :> PostCreated '[JSON] Value
  :<|> "releases" :> Capture "releaseId" UUID :> "versions" :> Capture "versionId" UUID :> "validate"
         :> Post '[JSON] Value
  :<|> "releases" :> Capture "releaseId" UUID :> "versions" :> Capture "versionId" UUID :> "transition"
         :> Header "Idempotency-Key" Text :> ReqBody '[JSON] MusicReleaseTransitionRequest :> Post '[JSON] Value
  :<|> "releases" :> Capture "releaseId" UUID :> "versions" :> Capture "versionId" UUID :> "comments"
         :> ReqBody '[JSON] MusicReleaseCommentRequest :> PostCreated '[JSON] Value
  :<|> "releases" :> Capture "releaseId" UUID :> "versions" :> Capture "versionId" UUID :> "comments"
         :> Capture "commentId" UUID :> "resolve" :> Post '[JSON] Value
  :<|> "releases" :> Capture "releaseId" UUID :> "versions" :> Capture "versionId" UUID :> "uploads"
         :> Header "Idempotency-Key" Text :> ReqBody '[JSON] MusicUploadCreateRequest :> PostCreated '[JSON] Value
  :<|> "uploads" :> Capture "uploadId" UUID :> "provider"
         :> ReqBody '[JSON] MusicUploadProviderRequest :> Put '[JSON] Value
  :<|> "uploads" :> Capture "uploadId" UUID :> "parts" :> Capture "partNumber" Int :> Get '[JSON] Value
  :<|> "uploads" :> Capture "uploadId" UUID :> "parts" :> Capture "partNumber" Int
         :> ReqBody '[JSON] MusicUploadPartRequest :> Put '[JSON] Value
  :<|> "uploads" :> Capture "uploadId" UUID :> "completion" :> Get '[JSON] Value
  :<|> "uploads" :> Capture "uploadId" UUID :> "confirm"
         :> ReqBody '[JSON] MusicUploadConfirmRequest :> Post '[JSON] Value
  :<|> "uploads" :> Capture "uploadId" UUID :> Delete '[JSON] Value
  :<|> "favorites" :> Header "CF-IPCountry" Text :> Get '[JSON] [Value]
  :<|> "favorites" :> Header "CF-IPCountry" Text :> ReqBody '[JSON] MusicFavoriteRequest :> Put '[JSON] NoContent
  :<|> "favorites" :> Capture "recordingId" UUID :> Delete '[JSON] NoContent
  :<|> "playlists" :> Header "CF-IPCountry" Text :> Get '[JSON] [Value]
  :<|> "playlists" :> ReqBody '[JSON] MusicPlaylistCreateRequest :> PostCreated '[JSON] Value
  :<|> "playlists" :> Capture "playlistId" UUID :> ReqBody '[JSON] MusicPlaylistCreateRequest :> Put '[JSON] Value
  :<|> "playlists" :> Capture "playlistId" UUID :> Delete '[JSON] NoContent
  :<|> "playlists" :> Capture "playlistId" UUID :> "items" :> Header "CF-IPCountry" Text :> ReqBody '[JSON] MusicPlaylistItemRequest :> PostCreated '[JSON] Value
  :<|> "playlists" :> Capture "playlistId" UUID :> "items" :> Capture "itemId" UUID
         :> ReqBody '[JSON] MusicPlaylistMoveRequest :> Put '[JSON] Value
  :<|> "playlists" :> Capture "playlistId" UUID :> "items" :> Capture "itemId" UUID :> Delete '[JSON] NoContent
  :<|> "me" :> "playback-events" :> Header "CF-IPCountry" Text
         :> ReqBody '[JSON] MusicPlaybackEventRequest :> Post '[JSON] NoContent
  :<|> "history" :> Header "CF-IPCountry" Text :> Get '[JSON] [Value]
  :<|> "infringement-reports" :> Header "Idempotency-Key" Text
         :> ReqBody '[JSON] MusicInfringementReportRequest :> PostCreated '[JSON] Value
  :<|> "infringement-reports" :> QueryParam "releaseId" UUID :> Get '[JSON] [Value]
  :<|> "infringement-reports" :> Capture "reportId" UUID
         :> ReqBody '[JSON] MusicInfringementActionRequest :> Put '[JSON] Value
  :<|> "analytics" :> "releases" :> Capture "releaseId" UUID
         :> QueryParam "from" Day :> QueryParam "to" Day :> Get '[JSON] Value
  :<|> "purchases" :> Header "Idempotency-Key" Text :> Header "CF-IPCountry" Text
         :> ReqBody '[JSON] MusicPurchaseCreateRequest :> PostCreated '[JSON] Value
  :<|> "purchases" :> Get '[JSON] [Value]
  :<|> "purchases" :> Capture "purchaseId" UUID :> "datafast" :> "checkout"
         :> Header "Idempotency-Key" Text :> Post '[JSON] Value
  :<|> "purchases" :> Capture "purchaseId" UUID :> "datafast" :> "status"
         :> Header "Idempotency-Key" Text :> ReqBody '[JSON] MusicDatafastStatusRequest :> Post '[JSON] Value
  :<|> "purchases" :> Capture "purchaseId" UUID :> "paypal" :> "create"
         :> Header "Idempotency-Key" Text :> Post '[JSON] Value
  :<|> "purchases" :> Capture "purchaseId" UUID :> "paypal" :> "capture"
         :> Header "Idempotency-Key" Text :> ReqBody '[JSON] MusicPaypalCaptureRequest :> Post '[JSON] Value
  :<|> "entitlements" :> Get '[JSON] [Value]
  :<|> "entitlements" :> Capture "entitlementId" UUID :> "download"
         :> ReqBody '[JSON] MusicDownloadAuthorizeRequest :> Post '[JSON] Value
  :<|> "downloads" :> "free" :> Header "CF-IPCountry" Text
         :> ReqBody '[JSON] MusicFreeDownloadRequest :> Post '[JSON] Value
  :<|> "ddex" :> "parties" :> Get '[JSON] [Value]
  :<|> "ddex" :> "parties" :> ReqBody '[JSON] MusicDdexPartyRequest :> PostCreated '[JSON] Value
  :<|> "releases" :> Capture "releaseId" UUID :> "versions" :> Capture "versionId" UUID :> "ddex-exports"
         :> Get '[JSON] [Value]
  :<|> "releases" :> Capture "releaseId" UUID :> "versions" :> Capture "versionId" UUID :> "ddex-exports"
         :> Header "Idempotency-Key" Text :> ReqBody '[JSON] MusicDdexExportRequest :> PostCreated '[JSON] Value
  :<|> "ddex" :> "exports" :> Capture "exportId" UUID :> "download" :> Get '[JSON] Value
  )

data MusicReleaseCreateRequest = MusicReleaseCreateRequest
  { musicCreateArtistPartyId :: Int64
  , musicCreateCanonicalSlug :: Text
  , musicCreateReleaseKind :: Text
  , musicCreateTitle :: Text
  , musicCreateDisplayArtist :: Text
  , musicCreateTitleLanguage :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON MusicReleaseCreateRequest where
  parseJSON = genericParseJSON (musicOptions 11)
instance ToJSON MusicReleaseCreateRequest where
  toJSON = genericToJSON (musicOptions 11)

data MusicReleaseDraftSaveRequest = MusicReleaseDraftSaveRequest
  { musicDraftExpectedUpdatedAt :: UTCTime
  , musicDraftTitle :: Text
  , musicDraftSubtitle :: Maybe Text
  , musicDraftVersionTitle :: Maybe Text
  , musicDraftDisplayArtist :: Text
  , musicDraftTitleLanguage :: Text
  , musicDraftTitleScript :: Maybe Text
  , musicDraftPrimaryGenreId :: Maybe UUID
  , musicDraftSecondaryGenreId :: Maybe UUID
  , musicDraftExplicitContent :: Text
  , musicDraftOriginalReleaseDate :: Maybe Day
  , musicDraftReleaseAtUtc :: Maybe UTCTime
  , musicDraftReleaseTimezone :: Maybe Text
  , musicDraftEmbargoUntilUtc :: Maybe UTCTime
  , musicDraftLabelName :: Maybe Text
  , musicDraftCatalogNumber :: Maybe Text
  , musicDraftRecordingCopyrightText :: Maybe Text
  , musicDraftWorkCopyrightText :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON MusicReleaseDraftSaveRequest where
  parseJSON = genericParseJSON (musicOptions 10)
instance ToJSON MusicReleaseDraftSaveRequest where
  toJSON = genericToJSON (musicOptions 10)

-- | Full editable catalog payload. Client references are request-local stable
-- keys; server UUIDs remain the canonical, opaque identifiers.
data MusicReleaseContentRequest = MusicReleaseContentRequest
  { musicContentExpectedUpdatedAt :: UTCTime
  , musicContentTracks :: [MusicTrackDraft]
  , musicContentParties :: [MusicPartyDraft]
  , musicContentCredits :: [MusicCreditDraft]
  , musicContentIdentifiers :: [MusicIdentifierDraft]
  , musicContentRightsDeclarations :: [MusicRightsDraft]
  , musicContentAvailability :: [MusicAvailabilityDraft]
  } deriving (Eq, Show, Generic)

instance FromJSON MusicReleaseContentRequest where
  parseJSON = genericParseJSON (musicOptions 12)
instance ToJSON MusicReleaseContentRequest where
  toJSON = genericToJSON (musicOptions 12)

data MusicTrackDraft = MusicTrackDraft
  { musicTrackClientRef :: Text
  , musicTrackRecordingId :: Maybe UUID
  , musicTrackTitle :: Text
  , musicTrackSubtitle :: Maybe Text
  , musicTrackVersionTitle :: Maybe Text
  , musicTrackTitleLanguage :: Text
  , musicTrackTitleScript :: Maybe Text
  , musicTrackExplicitContent :: Text
  , musicTrackDiscNumber :: Int
  , musicTrackTrackNumber :: Int
  , musicTrackDisplayArtist :: Text
  , musicTrackIsPrimaryResource :: Bool
  , musicTrackPreviewStartMs :: Maybe Int64
  , musicTrackPreviewDurationMs :: Maybe Int64
  } deriving (Eq, Show, Generic)

instance FromJSON MusicTrackDraft where
  parseJSON = genericParseJSON (musicOptions 10)
instance ToJSON MusicTrackDraft where
  toJSON = genericToJSON (musicOptions 10)

data MusicPartyDraft = MusicPartyDraft
  { musicPartyClientRef :: Text
  , musicPartyId :: Maybe UUID
  , musicPartyTdfPartyId :: Maybe Int64
  , musicPartyDisplayName :: Text
  , musicPartyLegalName :: Maybe Text
  , musicPartyKind :: Text
  , musicPartyIdentifiers :: [MusicPartyIdentifierDraft]
  } deriving (Eq, Show, Generic)

instance FromJSON MusicPartyDraft where
  parseJSON = genericParseJSON musicPartyOptions
instance ToJSON MusicPartyDraft where
  toJSON = genericToJSON musicPartyOptions

data MusicPartyIdentifierDraft = MusicPartyIdentifierDraft
  { musicPartyIdentifierType :: Text
  , musicPartyIdentifierValue :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON MusicPartyIdentifierDraft where
  parseJSON = genericParseJSON (musicOptions 20)
instance ToJSON MusicPartyIdentifierDraft where
  toJSON = genericToJSON (musicOptions 20)

data MusicCreditDraft = MusicCreditDraft
  { musicCreditPartyRef :: Text
  , musicCreditTrackRef :: Maybe Text
  , musicCreditRole :: Text
  , musicCreditDisplayOrder :: Int
  , musicCreditNotes :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON MusicCreditDraft where
  parseJSON = genericParseJSON (musicOptions 11)
instance ToJSON MusicCreditDraft where
  toJSON = genericToJSON (musicOptions 11)

data MusicIdentifierDraft = MusicIdentifierDraft
  { musicIdentifierTrackRef :: Maybe Text
  , musicIdentifierType :: Text
  , musicIdentifierValue :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON MusicIdentifierDraft where
  parseJSON = genericParseJSON (musicOptions 15)
instance ToJSON MusicIdentifierDraft where
  toJSON = genericToJSON (musicOptions 15)

data MusicRightsDraft = MusicRightsDraft
  { musicRightsTrackRef :: Maybe Text
  , musicRightsScope :: Text
  , musicRightsAuthorityBasis :: Text
  , musicRightsTerritories :: [Text]
  , musicRightsStartsOn :: Day
  , musicRightsEndsOn :: Maybe Day
  , musicRightsSplits :: [MusicSplitDraft]
  } deriving (Eq, Show, Generic)

instance FromJSON MusicRightsDraft where
  parseJSON = genericParseJSON (musicOptions 11)
instance ToJSON MusicRightsDraft where
  toJSON = genericToJSON (musicOptions 11)

data MusicSplitDraft = MusicSplitDraft
  { musicSplitPartyRef :: Text
  , musicSplitBasisPoints :: Int
  , musicSplitTerritories :: [Text]
  , musicSplitStartsOn :: Day
  , musicSplitEndsOn :: Maybe Day
  } deriving (Eq, Show, Generic)

instance FromJSON MusicSplitDraft where
  parseJSON = genericParseJSON (musicOptions 10)
instance ToJSON MusicSplitDraft where
  toJSON = genericToJSON (musicOptions 10)

data MusicAvailabilityDraft = MusicAvailabilityDraft
  { musicAvailabilityTrackRef :: Maybe Text
  , musicAvailabilityTerritoryMode :: Text
  , musicAvailabilityTerritories :: [Text]
  , musicAvailabilityStartsAt :: Maybe UTCTime
  , musicAvailabilityEndsAt :: Maybe UTCTime
  , musicAvailabilityListeningPolicy :: Text
  , musicAvailabilityDownloadPolicy :: Text
  , musicAvailabilityPurchasable :: Bool
  , musicAvailabilityPriceMinor :: Maybe Int64
  , musicAvailabilityCurrency :: Maybe Text
  , musicAvailabilityDownloadableAssetId :: Maybe UUID
  } deriving (Eq, Show, Generic)

instance FromJSON MusicAvailabilityDraft where
  parseJSON = genericParseJSON (musicOptions 17)
instance ToJSON MusicAvailabilityDraft where
  toJSON = genericToJSON (musicOptions 17)

data MusicTermsAcceptanceRequest = MusicTermsAcceptanceRequest
  { musicTermsKind :: Text
  , musicTermsVersion :: Text
  , musicTermsAccepted :: Bool
  , musicTermsEvidence :: Value
  } deriving (Eq, Show, Generic)

instance FromJSON MusicTermsAcceptanceRequest where
  parseJSON = genericParseJSON (musicOptions 10)
instance ToJSON MusicTermsAcceptanceRequest where
  toJSON = genericToJSON (musicOptions 10)

data MusicReleaseTransitionRequest = MusicReleaseTransitionRequest
  { musicTransitionTargetState :: Text
  , musicTransitionReason :: Maybe Text
  , musicTransitionReleaseAtUtc :: Maybe UTCTime
  , musicTransitionReleaseTimezone :: Maybe Text
  , musicTransitionEmbargoUntilUtc :: Maybe UTCTime
  , musicTransitionTakedownAtUtc :: Maybe UTCTime
  , musicTransitionTakedownTimezone :: Maybe Text
  , musicTransitionSnapshot :: Maybe Value
  , musicTransitionSnapshotSha256 :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON MusicReleaseTransitionRequest where
  parseJSON = genericParseJSON (musicOptions 15)
instance ToJSON MusicReleaseTransitionRequest where
  toJSON = genericToJSON (musicOptions 15)

data MusicReleaseCommentRequest = MusicReleaseCommentRequest
  { musicCommentParentId :: Maybe UUID
  , musicCommentFieldPath :: Maybe Text
  , musicCommentBody :: Text
  , musicCommentStaffOnly :: Bool
  , musicCommentRequestChanges :: Bool
  } deriving (Eq, Show, Generic)

instance FromJSON MusicReleaseCommentRequest where
  parseJSON = genericParseJSON (musicOptions 12)
instance ToJSON MusicReleaseCommentRequest where
  toJSON = genericToJSON (musicOptions 12)

data MusicPlaybackEventRequest = MusicPlaybackEventRequest
  { musicEventId :: UUID
  , musicEventSessionId :: UUID
  , musicEventSequenceNumber :: Int
  , musicEventAnonymousId :: Maybe Text
  , musicEventReleaseVersionId :: UUID
  , musicEventRecordingId :: UUID
  , musicEventType :: Text
  , musicEventPositionMs :: Int64
  , musicEventListenedDeltaMs :: Int64
  , musicEventQuality :: Maybe Text
  , musicEventTerritoryCode :: Maybe Text
  , musicEventOccurredAt :: UTCTime
  , musicEventMetadata :: Value
  } deriving (Eq, Show, Generic)

instance FromJSON MusicPlaybackEventRequest where
  parseJSON = genericParseJSON musicEventOptions
instance ToJSON MusicPlaybackEventRequest where
  toJSON = genericToJSON musicEventOptions

data MusicFavoriteRequest = MusicFavoriteRequest
  { musicFavoriteRecordingId :: UUID
  } deriving (Eq, Show, Generic)

instance FromJSON MusicFavoriteRequest where
  parseJSON = genericParseJSON (musicOptions 13)
instance ToJSON MusicFavoriteRequest where
  toJSON = genericToJSON (musicOptions 13)

data MusicPlaylistCreateRequest = MusicPlaylistCreateRequest
  { musicPlaylistName :: Text
  , musicPlaylistVisibility :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON MusicPlaylistCreateRequest where
  parseJSON = genericParseJSON (musicOptions 13)
instance ToJSON MusicPlaylistCreateRequest where
  toJSON = genericToJSON (musicOptions 13)

data MusicPlaylistItemRequest = MusicPlaylistItemRequest
  { musicPlaylistRecordingId :: UUID
  , musicPlaylistPosition :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON MusicPlaylistItemRequest where
  parseJSON = genericParseJSON (musicOptions 13)
instance ToJSON MusicPlaylistItemRequest where
  toJSON = genericToJSON (musicOptions 13)

data MusicPlaylistMoveRequest = MusicPlaylistMoveRequest
  { musicPlaylistMovePosition :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON MusicPlaylistMoveRequest where
  parseJSON = genericParseJSON (musicOptions 17)
instance ToJSON MusicPlaylistMoveRequest where
  toJSON = genericToJSON (musicOptions 17)

data MusicInfringementReportRequest = MusicInfringementReportRequest
  { musicReportReleaseId :: UUID
  , musicReportReasonCode :: Text
  , musicReportDescription :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON MusicInfringementReportRequest where
  parseJSON = genericParseJSON (musicOptions 11)
instance ToJSON MusicInfringementReportRequest where
  toJSON = genericToJSON (musicOptions 11)

data MusicInfringementActionRequest = MusicInfringementActionRequest
  { musicReportStatus :: Text
  , musicReportNotes :: Text
  , musicReportSuspendVersionId :: Maybe UUID
  } deriving (Eq, Show, Generic)

instance FromJSON MusicInfringementActionRequest where
  parseJSON = genericParseJSON (musicOptions 11)
instance ToJSON MusicInfringementActionRequest where
  toJSON = genericToJSON (musicOptions 11)

data MusicPurchaseCreateRequest = MusicPurchaseCreateRequest
  { musicPurchaseAvailabilityRuleId :: UUID
  , musicPurchaseTerritoryCode :: Text
  , musicPurchaseTermsVersion :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON MusicPurchaseCreateRequest where
  parseJSON = genericParseJSON (musicOptions 13)
instance ToJSON MusicPurchaseCreateRequest where
  toJSON = genericToJSON (musicOptions 13)

data MusicDatafastStatusRequest = MusicDatafastStatusRequest
  { musicDatafastResourcePath :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON MusicDatafastStatusRequest where
  parseJSON = genericParseJSON (musicOptions 13)
instance ToJSON MusicDatafastStatusRequest where
  toJSON = genericToJSON (musicOptions 13)

data MusicPaypalCaptureRequest = MusicPaypalCaptureRequest
  { musicPaypalOrderId :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON MusicPaypalCaptureRequest where
  parseJSON = genericParseJSON (musicOptions 11)
instance ToJSON MusicPaypalCaptureRequest where
  toJSON = genericToJSON (musicOptions 11)

data MusicDownloadAuthorizeRequest = MusicDownloadAuthorizeRequest
  { musicDownloadRequestId :: UUID
  } deriving (Eq, Show, Generic)

instance FromJSON MusicDownloadAuthorizeRequest where
  parseJSON = genericParseJSON (musicOptions 13)
instance ToJSON MusicDownloadAuthorizeRequest where
  toJSON = genericToJSON (musicOptions 13)

data MusicFreeDownloadRequest = MusicFreeDownloadRequest
  { musicFreeAvailabilityRuleId :: UUID
  , musicFreeRequestId :: UUID
  } deriving (Eq, Show, Generic)

instance FromJSON MusicFreeDownloadRequest where
  parseJSON = genericParseJSON (musicOptions 9)
instance ToJSON MusicFreeDownloadRequest where
  toJSON = genericToJSON (musicOptions 9)

data MusicDdexPartyRequest = MusicDdexPartyRequest
  { musicDdexPartyName :: Text
  , musicDdexPartyDpid :: Text
  , musicDdexPartyRole :: Text
  , musicDdexVerificationAuthority :: Text
  , musicDdexVerificationEvidence :: Value
  } deriving (Eq, Show, Generic)

instance FromJSON MusicDdexPartyRequest where
  parseJSON = genericParseJSON (musicOptions 9)
instance ToJSON MusicDdexPartyRequest where
  toJSON = genericToJSON (musicOptions 9)

data MusicDdexExportRequest = MusicDdexExportRequest
  { musicDdexOperation :: Text
  , musicDdexSenderRegistryId :: UUID
  , musicDdexRecipientRegistryId :: UUID
  } deriving (Eq, Show, Generic)

instance FromJSON MusicDdexExportRequest where
  parseJSON = genericParseJSON (musicOptions 9)
instance ToJSON MusicDdexExportRequest where
  toJSON = genericToJSON (musicOptions 9)

data MusicUploadCreateRequest = MusicUploadCreateRequest
  { musicUploadRecordingId :: Maybe UUID
  , musicUploadAssetRole :: Text
  , musicUploadOriginalFilename :: Text
  , musicUploadExpectedMediaType :: Text
  , musicUploadExpectedSize :: Int64
  , musicUploadExpectedSha256 :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON MusicUploadCreateRequest where
  parseJSON = genericParseJSON (musicOptions 11)
instance ToJSON MusicUploadCreateRequest where
  toJSON = genericToJSON (musicOptions 11)

data MusicUploadProviderRequest = MusicUploadProviderRequest
  { musicUploadProviderUploadId :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON MusicUploadProviderRequest where
  parseJSON = genericParseJSON (musicOptions 11)
instance ToJSON MusicUploadProviderRequest where
  toJSON = genericToJSON (musicOptions 11)

data MusicUploadPartRequest = MusicUploadPartRequest
  { musicUploadPartByteSize :: Int64
  , musicUploadPartEtag :: Text
  , musicUploadPartSha256 :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON MusicUploadPartRequest where
  parseJSON = genericParseJSON (musicOptions 15)
instance ToJSON MusicUploadPartRequest where
  toJSON = genericToJSON (musicOptions 15)

data MusicUploadConfirmRequest = MusicUploadConfirmRequest
  { musicUploadConfirmEtag :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON MusicUploadConfirmRequest where
  parseJSON = genericParseJSON (musicOptions 18)
instance ToJSON MusicUploadConfirmRequest where
  toJSON = genericToJSON (musicOptions 18)

musicOptions :: Int -> Options
musicOptions prefixLength = defaultOptions
  { fieldLabelModifier = lowerFirst . drop prefixLength
  , rejectUnknownFields = True
  }
  where
    lowerFirst [] = []
    lowerFirst (first : rest)
      | first >= 'A' && first <= 'Z' = toEnum (fromEnum first + 32) : rest
      | otherwise = first : rest

-- The public request names retain the domain noun for fields that would
-- otherwise collapse to the ambiguous `id` and `kind`.  The Studio client has
-- always used partyId/partyKind, as do validation errors and response payloads.
musicPartyOptions :: Options
musicPartyOptions = (musicOptions 10)
  { fieldLabelModifier = \field -> case field of
      "musicPartyId" -> "partyId"
      "musicPartyKind" -> "partyKind"
      _ -> fieldLabelModifier (musicOptions 10) field
  }

musicEventOptions :: Options
musicEventOptions = (musicOptions 10)
  { fieldLabelModifier = \field -> case field of
      "musicEventId" -> "eventId"
      "musicEventType" -> "eventType"
      _ -> fieldLabelModifier (musicOptions 10) field
  }

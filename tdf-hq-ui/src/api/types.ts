import type { components } from './generated/types';

export interface PartyDTO {
  partyId: number;
  legalName?: string | null;
  displayName: string;
  isOrg: boolean;
  roles?: string[];
  taxId?: string | null;
  primaryEmail?: string | null;
  primaryPhone?: string | null;
  whatsapp?: string | null;
  instagram?: string | null;
  emergencyContact?: string | null;
  notes?: string | null;
  hasUserAccount?: boolean;
}

export interface SocialPartyProfileDTO {
  sppPartyId: number;
  sppDisplayName: string;
  sppAvatarUrl?: string | null;
  sppBio?: string | null;
  sppCity?: string | null;
}

export interface PartyCreate {
  cDisplayName: string;
  cIsOrg: boolean;
  cLegalName?: string | null;
  cPrimaryEmail?: string | null;
  cPrimaryPhone?: string | null;
  cWhatsapp?: string | null;
  cInstagram?: string | null;
  cTaxId?: string | null;
  cEmergencyContact?: string | null;
  cNotes?: string | null;
}

export interface PartyUpdate {
  uDisplayName?: string;
  uIsOrg?: boolean;
  uLegalName?: string | null;
  uPrimaryEmail?: string | null;
  uPrimaryPhone?: string | null;
  uWhatsapp?: string | null;
  uInstagram?: string | null;
  uTaxId?: string | null;
  uEmergencyContact?: string | null;
  uNotes?: string | null;
}

export interface PartyRelatedBookingDTO {
  prbBookingId: number;
  prbRole: string;
  prbTitle: string;
  prbServiceType?: string | null;
  prbStartsAt: string;
  prbEndsAt: string;
  prbStatus: string;
}

export interface PartyRelatedClassSessionDTO {
  prcClassSessionId: number;
  prcRole: string;
  prcSubjectId: number;
  prcSubjectName?: string | null;
  prcTeacherId: number;
  prcTeacherName?: string | null;
  prcStudentId: number;
  prcStudentName?: string | null;
  prcStartAt: string;
  prcEndAt: string;
  prcStatus: string;
  prcBookingId?: number | null;
}

export interface PartyRelatedLabelTrackDTO {
  prtId: string;
  prtTitle: string;
  prtStatus: string;
  prtCreatedAt: string;
  prtUpdatedAt: string;
}

export interface PartyRelatedDTO {
  prPartyId: number;
  prBookings: PartyRelatedBookingDTO[];
  prClassSessions: PartyRelatedClassSessionDTO[];
  prLabelTracks: PartyRelatedLabelTrackDTO[];
}

export interface DropdownOptionDTO {
  optionId: string;
  category: string;
  value: string;
  label?: string | null;
  active: boolean;
  sortOrder?: number | null;
}

export interface DropdownOptionCreate {
  docValue: string;
  docLabel?: string | null;
  docSortOrder?: number | null;
  docActive?: boolean | null;
}

export interface DropdownOptionUpdate {
  douValue?: string | null;
  douLabel?: string | null;
  douSortOrder?: number | null;
  douActive?: boolean | null;
}

export interface BandOptionsDTO {
  roles: DropdownOptionDTO[];
  genres: DropdownOptionDTO[];
}

export interface BookingResourceDTO {
  brRoomId: string;
  brRoomName: string;
  brRole: string;
}

export interface AssetDTO {
  assetId: string;
  name: string;
  category: string;
  status: string;
  condition?: string | null;
  brand?: string | null;
  model?: string | null;
  location?: string | null;
  qrToken?: string | null;
  photoUrl?: string | null;
  currentCheckoutKind?: string | null;
  currentCheckoutTarget?: string | null;
  currentCheckoutDisposition?: string | null;
  currentCheckoutHolderEmail?: string | null;
  currentCheckoutHolderPhone?: string | null;
  currentCheckoutAt?: string | null;
  currentCheckoutDueAt?: string | null;
  currentCheckoutPaymentType?: string | null;
  currentCheckoutPaymentInstallments?: number | null;
  currentCheckoutPaymentAmountCents?: number | null;
  currentCheckoutPaymentCurrency?: string | null;
  currentCheckoutPaymentOutstandingCents?: number | null;
  currentCheckoutPhotoUrl?: string | null;
}

export interface PageResponse<T> {
  items: T[];
  page: number;
  pageSize: number;
  total: number;
}

export interface AssetCreate {
  cName: string;
  cCategory: string;
  cPhotoUrl?: string | null;
}

export interface AssetUpdate {
  uName?: string;
  uCategory?: string;
  uStatus?: string;
  uLocationId?: string | null;
  uNotes?: string | null;
  uPhotoUrl?: string | null;
}

export interface MarketplaceItemDTO {
  miListingId: string;
  miAssetId: string;
  miPurpose: string;
  miTitle: string;
  miCategory: string;
  miBrand?: string | null;
  miModel?: string | null;
  miPhotoUrl?: string | null;
  miStatus?: string | null;
  miCondition?: string | null;
  miPriceUsdCents: number;
  miPriceDisplay: string;
  miMarkupPct: number;
  miCurrency: string;
  miRentalWeeklyPriceUsdCents?: number | null;
  miRentalWeeklyPriceDisplay?: string | null;
  miRentalSecurityDepositUsdCents?: number | null;
  miRentalSecurityDepositDisplay?: string | null;
  miRentalMinDays?: number | null;
  miRentalMaxDays?: number | null;
  miRentalLateFeeUsdCents?: number | null;
  miRentalLateFeeDisplay?: string | null;
  miRentalCancellationWindowHours?: number | null;
  miRentalTermsVersion?: string | null;
  miRentalTermsSummary?: string | null;
  miRentalTimezone?: string | null;
}

export interface MarketplaceCartItemDTO {
  mciListingId: string;
  mciTitle: string;
  mciCategory: string;
  mciBrand?: string | null;
  mciModel?: string | null;
  mciQuantity: number;
  mciUnitPriceUsdCents: number;
  mciSubtotalCents: number;
  mciUnitPriceDisplay: string;
  mciSubtotalDisplay: string;
  mciPurpose: 'sale' | 'rent';
  mciRentalStartDate?: string | null;
  mciRentalEndDate?: string | null;
  mciRentalDurationDays?: number | null;
  mciRentalChargeCents?: number | null;
  mciRentalChargeDisplay?: string | null;
  mciSecurityDepositCents?: number | null;
  mciSecurityDepositDisplay?: string | null;
}

export interface MarketplaceCartDTO {
  mcCartId: string;
  mcItems: MarketplaceCartItemDTO[];
  mcCurrency: string;
  mcSubtotalCents: number;
  mcSubtotalDisplay: string;
}

export interface MarketplaceOrderItemDTO {
  moiListingId: string;
  moiTitle: string;
  moiQuantity: number;
  moiUnitPriceUsdCents: number;
  moiSubtotalCents: number;
  moiUnitPriceDisplay: string;
  moiSubtotalDisplay: string;
}

export interface MarketplaceOrderDTO {
  moOrderId: string;
  moCartId?: string | null;
  moCurrency: string;
  moTotalUsdCents: number;
  moTotalDisplay: string;
  moStatus: string;
  moStatusHistory: [string, string][];
  moBuyerName: string;
  moBuyerEmail: string;
  moBuyerPhone?: string | null;
  moPaymentProvider?: string | null;
  moPaypalOrderId?: string | null;
  moPaypalPayerEmail?: string | null;
  moPaidAt?: string | null;
  moLookupToken?: string | null;
  moCheckoutStatus?: string | null;
  moManualPaymentStatus?: 'awaiting_evidence' | 'submitted' | 'under_review' | 'approved' | 'rejected' | 'requires_reconciliation' | null;
  moManualPaymentSubmittedAt?: string | null;
  moFulfillmentMethod?: string | null;
  moFulfillmentStatus?: string | null;
  moHoldExpiresAt?: string | null;
  moTrackingReference?: string | null;
  moFulfillmentHistory?: [string, string][];
  moOrderKind?: 'sale' | 'rental' | null;
  moRentalStartDate?: string | null;
  moRentalEndDate?: string | null;
  moRentalDurationDays?: number | null;
  moRentalChargeUsdCents?: number | null;
  moSecurityDepositUsdCents?: number | null;
  moDepositStatus?: string | null;
  moDepositDeductionUsdCents?: number | null;
  moRentalTermsVersion?: string | null;
  moRentalTimezone?: string | null;
  moConditionOut?: string | null;
  moConditionIn?: string | null;
  moCreatedAt: string;
  moUpdatedAt: string;
  moItems: MarketplaceOrderItemDTO[];
}

export interface MarketplaceManualEvidenceDTO {
  mmeEvidenceId: string;
  mmePaymentMethod: 'bank_transfer' | 'cash' | 'pos';
  mmeStatus: 'awaiting_evidence' | 'submitted' | 'under_review' | 'approved' | 'rejected';
  mmeCustomerReference?: string | null;
  mmeSubmittedAmountMinor?: number | null;
  mmeCurrency?: string | null;
  mmeSubmittedBy?: number | null;
  mmeSubmittedAt?: string | null;
  mmeReviewedBy?: number | null;
  mmeReviewedAt?: string | null;
  mmeReviewNotes?: string | null;
}

export interface MarketplaceCommerceDTO {
  mpcOrderId: string;
  mpcCheckoutId: string;
  mpcPaymentStatus: string;
  mpcHoldExpiresAt: string;
  mpcOrderKind: 'sale' | 'rental';
  mpcManualEvidence?: MarketplaceManualEvidenceDTO | null;
}

export type MarketplaceCustomerRequestType =
  | 'sale_cancellation'
  | 'sale_return'
  | 'rental_cancellation'
  | 'rental_extension'
  | 'rental_dispute';

export interface MarketplaceCustomerRequestSubmitPayload {
  mcrsRequestType: MarketplaceCustomerRequestType;
  mcrsReason: string;
  mcrsRequestedEndDate?: string;
  mcrsEvidenceUrl?: string;
}

export interface MarketplaceCustomerRequestDTO {
  mcrRequestId: string;
  mcrOrderId: string;
  mcrOrderKind: 'sale' | 'rental';
  mcrRequestType: MarketplaceCustomerRequestType;
  mcrStatus: 'submitted' | 'needs_quote' | 'approved' | 'rejected';
  mcrReason: string;
  mcrRequestedEndDate?: string | null;
  mcrEvidenceUrl?: string | null;
  mcrRequestedAt: string;
  mcrReviewedAt?: string | null;
  mcrReviewNotes?: string | null;
}

export interface MarketplaceDepositSettlementSubmitPayload {
  mdssSettlementMethod: 'bank_transfer' | 'cash' | 'pos' | 'forfeiture';
  mdssExternalReference: string;
  mdssEvidenceUrl: string;
}

export interface MarketplaceDepositSettlementDTO {
  mdsSettlementId: string;
  mdsOrderId: string;
  mdsCheckoutId: string;
  mdsCurrency: string;
  mdsDepositAmountMinor: number;
  mdsDeductionAmountMinor: number;
  mdsRefundAmountMinor: number;
  mdsSettlementMethod: 'bank_transfer' | 'cash' | 'pos' | 'forfeiture';
  mdsExternalReference: string;
  mdsEvidenceUrl: string;
  mdsStatus: 'submitted' | 'verified' | 'rejected' | 'requires_reconciliation';
  mdsSubmittedBy: number;
  mdsSubmittedAt: string;
  mdsReviewedBy?: number | null;
  mdsReviewedAt?: string | null;
  mdsReviewNotes?: string | null;
}

export interface MarketplaceOrderUpdatePayload {
  mouStatus?: string;
  mouPaymentProvider?: string | null;
  mouPaidAt?: string | null;
}

export interface DatafastCheckoutDTO {
  dcOrderId: string;
  dcCheckoutId: string;
  dcWidgetUrl: string;
  dcAmount: string;
  dcCurrency: string;
  dcLookupToken?: string | null;
}

export interface StripePaymentIntentDTO {
  spiClientSecret: string;
  spiPaymentIntentId?: string | null;
  spiOrderId: string;
  spiAmountCents: number;
  spiCurrency: string;
  spiPaymentSheet?: Record<string, unknown> | null;
  spiLookupToken?: string | null;
}

export interface PaypalCreateDTO {
  pcOrderId: string;
  pcPaypalOrderId: string;
  pcApprovalUrl?: string | null;
  pcLookupToken?: string | null;
}

export interface MarketplaceShippingAddress {
  msaAddressLine1: string;
  msaAddressLine2?: string;
  msaCity: string;
  msaProvince: string;
  msaPostalCode?: string;
  msaCountryCode: string;
}

export interface MarketplaceFulfillmentUpdatePayload {
  mfuStatus: string;
  mfuCarrier?: string;
  mfuTrackingReference?: string;
  mfuReasonCode?: string;
  mfuNotes?: string;
}

export interface MarketplaceRentalUpdatePayload {
  mruStatus: string;
  mruConditionOut?: string;
  mruConditionIn?: string;
  mruEvidenceUrl?: string;
  mruDepositDeductionUsdCents?: number;
  mruReasonCode?: string;
  mruNotes?: string;
}

export interface MarketplaceRentalTermsUpdatePayload {
  mrtuDailyRateUsdCents: number;
  mrtuWeeklyRateUsdCents: number | null;
  mrtuSecurityDepositUsdCents: number;
  mrtuLateFeeUsdCents: number;
  mrtuMinDays: number;
  mrtuMaxDays: number;
  mrtuCancellationWindowHours: number;
  mrtuTimezone: 'America/Guayaquil';
  mrtuTermsVersion: string;
  mrtuTermsSummary: string;
  mrtuActive: boolean;
}

export interface PaypalCaptureRequest {
  pcCaptureOrderId: string;
  pcCapturePaypalId: string;
}

export interface DriveUploadDTO {
  duFileId: string;
  duWebViewLink?: string | null;
  duWebContentLink?: string | null;
  duPublicUrl?: string | null;
}

export interface AssetUploadDTO {
  auFileName: string;
  auPath: string;
  auPublicUrl: string;
}

export interface LabelTrackDTO {
  ltId: string;
  ltTitle: string;
  ltNote?: string | null;
  ltStatus: string;
  ltOwnerId?: number | null;
  ltOwnerName?: string | null;
  ltCreatedAt: string;
  ltUpdatedAt: string;
}

export interface LabelProjectNoteDTO {
  lpnId: string;
  lpnText: string;
  lpnCompleted: boolean;
  lpnCreatedAt: string;
  lpnUpdatedAt: string;
  lpnVersion: number;
}

export interface AssetCheckoutDTO {
  checkoutId: string;
  assetId: string;
  targetKind: string;
  targetSessionId?: string | null;
  targetPartyRef?: string | null;
  targetRoomId?: string | null;
  disposition: string;
  termsAndConditions?: string | null;
  holderEmail?: string | null;
  holderPhone?: string | null;
  paymentType?: string | null;
  paymentInstallments?: number | null;
  paymentReference?: string | null;
  paymentAmountCents?: number | null;
  paymentCurrency?: string | null;
  paymentOutstandingCents?: number | null;
  checkedOutBy: string;
  checkedOutAt: string;
  dueAt?: string | null;
  conditionOut?: string | null;
  photoOutUrl?: string | null;
  conditionIn?: string | null;
  photoInUrl?: string | null;
  returnedAt?: string | null;
  notes?: string | null;
}

export interface BookingDTO {
  bookingId: number;
  title: string;
  startsAt: string; // ISO
  endsAt: string;   // ISO
  status: string;
  notes?: string | null;
  partyId?: number | null;
  engineerPartyId?: number | null;
  engineerName?: string | null;
  serviceOfferingId?: string | null;
  serviceType?: string | null;
  serviceOrderId?: number | null;
  serviceOrderTitle?: string | null;
  customerName?: string | null;
  partyDisplayName?: string | null;
  resources: BookingResourceDTO[];
  courseSlug?: string | null;
  coursePrice?: number | null;
  courseCurrency?: string | null;
  courseCapacity?: number | null;
  courseRemaining?: number | null;
  courseLocation?: string | null;
}

export interface VersionInfo {
  name: string;
  version: string;
  commit?: string | null;
  buildTime?: string | null;
}

type HealthState = 'ok' | 'degraded' | (string & Record<never, never>);

export interface HealthStatus {
  status: HealthState;
  version?: string | null;
}

export type ServiceCatalogDTO = components['schemas']['ServiceOffering'];
export type ServiceDefaultResourceDTO = components['schemas']['ServiceDefaultResource'];

export interface RoomDTO {
  roomId: string;
  rName: string;
  rBookable: boolean;
}

export interface RoomCreate {
  rcName: string;
}

export interface RoomUpdate {
  ruName?: string;
  ruIsBookable?: boolean;
}

export interface PipelineCardDTO {
  id: string;
  title: string;
  artist?: string | null;
  serviceOfferingId: string;
  serviceOfferingCode: string;
  workflowId: string;
  workflowStateId: string;
  workflowStateCode: string;
  workflowStateNameEs: string;
  workflowStateNameEn: string;
  sortOrder: number;
  notes?: string | null;
}

export interface PipelineStageDTO {
  id: string;
  code: string;
  nameEs: string;
  nameEn: string;
  sortOrder: number;
  terminal: boolean;
}

export interface PipelineServiceOfferingDTO {
  id: string;
  code: string;
  nameEs: string;
  nameEn: string;
}

export interface PipelineDefinitionDTO {
  workflowId: string;
  code: string;
  nameEs: string;
  nameEn: string;
  revision: number;
  serviceOfferings: PipelineServiceOfferingDTO[];
  stages: PipelineStageDTO[];
}

export type PipelineSnapshotDTO = components['schemas']['PipelineSnapshot'];
export type PipelineCardCreate = components['schemas']['PipelineCardCreate'];

export interface PipelineCardUpdate {
  title?: string;
  artist?: string | null;
  workflowStateId?: string;
  sortOrder?: number;
  notes?: string | null;
}

export interface ArtistProfileDTO {
  apArtistId: number;
  apDisplayName: string;
  apSlug?: string | null;
  apBio?: string | null;
  apCity?: string | null;
  apHeroImageUrl?: string | null;
  apSpotifyArtistId?: string | null;
  apSpotifyUrl?: string | null;
  apYoutubeChannelId?: string | null;
  apYoutubeUrl?: string | null;
  apWebsiteUrl?: string | null;
  apFeaturedVideoUrl?: string | null;
  /** Presentation-only labels resolved by the backend. */
  apGenres?: string | null;
  apGenreIds: string[];
  apHighlights?: string | null;
  apOfficialName?: string | null;
  apCountry?: string | null;
  apInstagramUrl?: string | null;
  apSocialLinks?: string | null;
  apDiscography?: string | null;
  apAchievements?: string | null;
  apHeroOriginalUrl?: string | null;
  apHeroSquareUrl?: string | null;
  apHeroLandscapeUrl?: string | null;
  apHeroResponsiveUrls?: string | null;
  apHeroFocalPoint?: string | null;
  apLastVerifiedAt?: string | null;
  apConfidence?: number | null;
  apReviewStatus?: string | null;
  apFollowerCount: number;
  apHasUserAccount?: boolean;
}

export interface ArtistReleaseDTO {
  arArtistId: number;
  arReleaseId: number;
  arTitle: string;
  arReleaseDate?: string | null;
  arDescription?: string | null;
  arCoverImageUrl?: string | null;
  arSpotifyUrl?: string | null;
  arYoutubeUrl?: string | null;
}

export interface ArtistReleaseUpsert {
  aruArtistId: number;
  aruTitle: string;
  aruReleaseDate?: string | null;
  aruDescription?: string | null;
  aruCoverImageUrl?: string | null;
  aruSpotifyUrl?: string | null;
  aruYoutubeUrl?: string | null;
}

export interface ArtistPromoSlotDTO {
  apsPromotionId: number;
  apsArtistId: number;
  apsDay: string;
  apsStartTime: string;
  apsMedium: string;
  apsProgram: string;
  apsInterviewerHost: string;
  apsBandMembers: string;
  apsStatus?: string | null;
  apsNotes?: string | null;
  apsCreatedAt: string;
  apsUpdatedAt: string;
}

export interface ArtistPromoSlotUpsert {
  apsuDay: string;
  apsuStartTime: string;
  apsuMedium: string;
  apsuProgram: string;
  apsuInterviewerHost: string;
  apsuBandMembers: string;
  apsuStatus?: string | null;
  apsuNotes?: string | null;
}

export interface ArtistPromoDayReportDTO {
  apdArtistId: number;
  apdArtistName: string;
  apdDay: string;
  apdTimezone: string;
  apdDayHeader: string;
  apdEntries: ArtistPromoSlotDTO[];
}

export interface FanProfileDTO {
  fpArtistId: number;
  fpDisplayName?: string | null;
  fpAvatarUrl?: string | null;
  fpFavoriteGenres?: string | null;
  fpFavoriteGenreIds: string[];
  fpBio?: string | null;
  fpCity?: string | null;
}

export interface FanProfileUpdate {
  fpuDisplayName?: string | null;
  fpuAvatarUrl?: string | null;
  fpuFavoriteGenreIds: string[];
  fpuBio?: string | null;
  fpuCity?: string | null;
}

export interface FanFollowDTO {
  ffArtistId: number;
  ffArtistName: string;
  ffHeroImageUrl?: string | null;
  ffSpotifyUrl?: string | null;
  ffYoutubeUrl?: string | null;
  ffStartedAt: string;
}

export interface ArtistFanDTO {
  afFanId: number;
  afDisplayName: string;
  afAvatarUrl?: string | null;
  afFollowedAt: string;
}

export interface ArtistFansResponse {
  items: ArtistFanDTO[];
  page: number;
  pageSize: number;
  total: number;
}

export interface PartyFollowDTO {
  pfFollowerId: number;
  pfFollowingId: number;
  pfViaNfc: boolean;
  pfStartedAt: string;
}

export interface SuggestedFriendDTO {
  sfPartyId: number;
  sfMutualCount: number;
}

export interface ChatThreadDTO {
  ctThreadId: number;
  ctOtherPartyId: number;
  ctOtherDisplayName: string;
  ctLastMessage?: string | null;
  ctLastMessageAt?: string | null;
  ctUpdatedAt: string;
}

export interface ChatMessageDTO {
  cmId: number;
  cmThreadId: number;
  cmSenderPartyId: number;
  cmBody: string;
  cmCreatedAt: string;
}

export interface RadioPresenceDTO {
  rpPartyId: number;
  rpStreamUrl: string;
  rpStationName?: string | null;
  rpStationId?: string | null;
  rpUpdatedAt: string;
}

export interface RadioPresenceUpsert {
  rpuStreamUrl: string;
  rpuStationName?: string | null;
  rpuStationId?: string | null;
}

export interface ArtistProfileUpsert {
  apuArtistId: number;
  apuDisplayName?: string | null;
  apuSlug?: string | null;
  apuBio?: string | null;
  apuCity?: string | null;
  apuHeroImageUrl?: string | null;
  apuSpotifyArtistId?: string | null;
  apuSpotifyUrl?: string | null;
  apuYoutubeChannelId?: string | null;
  apuYoutubeUrl?: string | null;
  apuWebsiteUrl?: string | null;
  apuFeaturedVideoUrl?: string | null;
  apuGenreIds: string[];
  apuHighlights?: string | null;
}

export interface ArtistProfilePhotoUpdate {
  apuHeroImageUrl: string;
}

export interface PaymentDTO {
  payId: number;
  payPartyId: number;
  payPartyDisplayName: string;
  payOrderId?: number | null;
  payInvoiceId?: number | null;
  payAmountCents: number;
  payCurrency: string;
  payMethod: string;
  payReference?: string | null;
  payPaidAt: string;
  payConcept: string;
  payPeriod?: string | null;
  payAttachment?: string | null;
}

export interface PaymentCreate {
  pcPartyId: number;
  pcOrderId?: number | null;
  pcInvoiceId?: number | null;
  pcAmountCents: number;
  pcCurrency: string;
  pcMethod: string;
  pcReference?: string | null;
  pcPaidAt: string;
  pcConcept: string;
  pcPeriod?: string | null;
  pcAttachmentUrl?: string | null;
}

export interface InternProfileDTO {
  ipPartyId: number;
  ipStartAt?: string | null;
  ipEndAt?: string | null;
  ipRequiredHours?: number | null;
  ipSkills?: string | null;
  ipAreas?: string | null;
  ipCreatedAt: string;
  ipUpdatedAt: string;
}

export interface InternProfileUpdate {
  ipuStartAt?: string | null;
  ipuEndAt?: string | null;
  ipuRequiredHours?: number | null;
  ipuSkills?: string | null;
  ipuAreas?: string | null;
}

export interface InternSummaryDTO {
  isPartyId: number;
  isName: string;
  isEmail?: string | null;
  isRoles: string[];
}

export interface InternProjectDTO {
  ipId: string;
  ipTitle: string;
  ipDescription?: string | null;
  ipStatus: string;
  ipActivationStatus?: string;
  ipStartAt?: string | null;
  ipDueAt?: string | null;
  ipCreatedAt: string;
  ipUpdatedAt: string;
}

export interface InternProjectCreate {
  ipcTitle: string;
  ipcDescription?: string;
  ipcStatus?: string;
  ipcActivationStatus?: string;
  ipcStartAt?: string;
  ipcDueAt?: string;
}

export interface InternProjectUpdate {
  ipuTitle?: string | null;
  ipuDescription?: string | null;
  ipuStatus?: string | null;
  ipuStartAt?: string | null;
  ipuDueAt?: string | null;
}

export interface InternTaskDTO {
  itId: string;
  itProjectId: string;
  itProjectName: string;
  itTitle: string;
  itDescription?: string | null;
  itStatus: string;
  itActivationStatus?: string;
  itProgress: number;
  itAssignedTo?: number | null;
  itAssignedName?: string | null;
  itProposedAssignee?: number | null;
  itDueAt?: string | null;
  itCreatedAt: string;
  itUpdatedAt: string;
}

export interface InternTaskCreate {
  itcProjectId: string;
  itcTitle: string;
  itcDescription?: string;
  itcAssignedTo?: number;
  itcProposedAssignee?: number;
  itcActivationStatus?: string;
  itcDueAt?: string;
}

export interface InternTaskUpdate {
  ituProjectId?: string | null;
  ituTitle?: string | null;
  ituDescription?: string | null;
  ituStatus?: string | null;
  ituProgress?: number | null;
  ituAssignedTo?: number | null;
  ituDueAt?: string | null;
}

export interface InternTodoDTO {
  itdId: string;
  itdText: string;
  itdDone: boolean;
  itdCreatedAt: string;
  itdUpdatedAt: string;
}

export interface InternTodoCreate {
  itdcText: string;
}

export interface InternTodoUpdate {
  itduText?: string | null;
  itduDone?: boolean | null;
}

export interface ClockInRequest {
  cirNotes?: string | null;
}

export interface ClockOutRequest {
  corNotes?: string | null;
}

export interface InternTimeEntryDTO {
  iteId: string;
  itePartyId: number;
  itePartyName: string;
  iteClockIn: string;
  iteClockOut?: string | null;
  iteDurationMinutes?: number | null;
  iteNotes?: string | null;
}

export interface InternPermissionDTO {
  iprId: string;
  iprPartyId: number;
  iprPartyName: string;
  iprCategory: string;
  iprReason?: string | null;
  iprStartAt: string;
  iprEndAt?: string | null;
  iprStatus: string;
  iprReviewedBy?: number | null;
  iprReviewedByName?: string | null;
  iprReviewedAt?: string | null;
  iprDecisionNotes?: string | null;
  iprCreatedAt: string;
  iprUpdatedAt: string;
}

export interface InternPermissionCreate {
  ipcCategory: string;
  ipcReason?: string | null;
  ipcStartAt: string;
  ipcEndAt?: string | null;
}

export interface InternPermissionUpdate {
  ipuStatus?: string | null;
  ipuDecisionNotes?: string | null;
}

export type InternExecutionStatus =
  | 'pending'
  | 'in_progress'
  | 'passed'
  | 'failed'
  | 'blocked'
  | 'not_applicable'
  | 'ready_for_retest'
  | 'verified';

export interface InternAuditPlanDTO {
  iapId: string;
  iapProjectId: string;
  iapTaskId: string;
  iapEnvironment: string;
  iapStatus: 'draft' | 'active' | 'completed' | 'cancelled';
  iapDurationDays: number;
  iapExpectedHoursMin: number;
  iapExpectedHoursMax: number;
  iapMidpointPercent: number;
  iapProposedAssignee?: number | null;
  iapFinalReviewRequired: boolean;
  iapCompletionJustification?: string | null;
  iapCompletionApprovedBy?: number | null;
  iapCompletionApprovedAt?: string | null;
  iapCaseCount: number;
  iapExecutedCaseCount: number;
  iapCriticalRemaining: number;
  iapOpenBlockerCount: number;
  iapFailedWithoutReport: number;
  iapEvidenceMissing: number;
  iapCalculatedProgress: number;
  iapCanComplete: boolean;
  iapCreatedAt: string;
  iapUpdatedAt: string;
}

export interface InternTestExecutionDTO {
  itexId: string;
  itexTestCaseId: string;
  itexExecutionNumber: number;
  itexExecutorPartyId: number;
  itexStatus: InternExecutionStatus;
  itexActualResult?: string | null;
  itexPersistedStateObserved?: string | null;
  itexSideEffectsObserved?: string | null;
  itexBlockerReason?: string | null;
  itexEvidenceSummary?: string | null;
  itexStartedAt?: string | null;
  itexCompletedAt?: string | null;
  itexCreatedAt: string;
  itexUpdatedAt: string;
}

export interface InternTestCaseDTO {
  itcId: string;
  itcPlanId: string;
  itcStableId: string;
  itcModuleName: string;
  itcFeatureName: string;
  itcUserRole: string;
  itcObjective: string;
  itcBusinessPurpose: string;
  itcPreconditions: string;
  itcRequiredTestData: string;
  itcEnvironment: string;
  itcPlatform: string;
  itcBrowserOrDevice: string;
  itcLanguage: string;
  itcDetailedSteps: string;
  itcExpectedResult: string;
  itcExpectedPersistedState: string;
  itcExpectedSideEffects: string;
  itcCleanupInstructions: string;
  itcCriticality: 'low' | 'medium' | 'high' | 'critical';
  itcEvidenceRequirement: 'light' | 'strong';
  itcExploratoryCharter?: string | null;
  itcApplicable: boolean;
  itcSortOrder: number;
  itcLatestExecution?: InternTestExecutionDTO | null;
}

export interface InternTestExecutionCreate {
  itecStatus: InternExecutionStatus;
  itecActualResult?: string | null;
  itecPersistedStateObserved?: string | null;
  itecSideEffectsObserved?: string | null;
  itecBlockerReason?: string | null;
  itecEvidenceSummary?: string | null;
}

export interface InternDailySummaryDTO {
  idsId: string;
  idsTaskId: string;
  idsAuthorPartyId: number;
  idsWorkDate: string;
  idsMinutesWorked: number;
  idsModulesTested: string;
  idsCasesCompleted: number;
  idsReportsCreated: number;
  idsBlockers?: string | null;
  idsNextStep: string;
  idsCreatedAt: string;
}

export interface InternDailySummaryCreate {
  idscWorkDate: string;
  idscMinutesWorked: number;
  idscModulesTested: string;
  idscCasesCompleted: number;
  idscReportsCreated: number;
  idscBlockers?: string | null;
  idscNextStep: string;
}

export interface InternFinalSummaryDTO {
  ifsId: string;
  ifsPlanId: string;
  ifsAuthorPartyId: number;
  ifsGeneratedSnapshot: string;
  ifsConclusions?: string | null;
  ifsSubmittedAt?: string | null;
  ifsApprovedBy?: number | null;
  ifsApprovedAt?: string | null;
  ifsCreatedAt: string;
  ifsUpdatedAt: string;
}

export type InternalReportType =
  | 'error'
  | 'suggestion'
  | 'idea'
  | 'question'
  | 'accessibility'
  | 'permissions'
  | 'performance'
  | 'content_translation';

export type InternalReportState =
  | 'draft'
  | 'submitted'
  | 'received'
  | 'needs_information'
  | 'confirmed'
  | 'prioritized'
  | 'in_progress'
  | 'ready_for_retest'
  | 'verified'
  | 'closed'
  | 'duplicate'
  | 'discarded';

export interface InternalFeedbackSummaryDTO {
  ifsId: string;
  ifsTitle: string;
  ifsReportType: InternalReportType;
  ifsState: InternalReportState;
  ifsModuleName: string;
  ifsFeatureName?: string | null;
  ifsEnvironment: string;
  ifsPlatform: string;
  ifsProposedSeverityId?: string | null;
  ifsAuthoritativeSeverityId?: string | null;
  ifsPriority?: string | null;
  ifsBlocking: boolean;
  ifsReporterPartyId: number;
  ifsReporterName: string;
  ifsInternshipProjectId?: string | null;
  ifsInternshipTaskId?: string | null;
  ifsTestCaseId?: string | null;
  ifsTestExecutionId?: string | null;
  ifsDuplicateOf?: string | null;
  ifsCreatedAt: string;
  ifsUpdatedAt: string;
}

export interface InternalFeedbackEvidenceDTO {
  ifeId: string;
  ifeKind: string;
  ifeOriginalFileName?: string | null;
  ifeContentType?: string | null;
  ifeSizeBytes?: number | null;
  ifeExternalUrl?: string | null;
  ifeCaption?: string | null;
  ifeUploadedBy: number;
  ifeCreatedAt: string;
}

export interface InternalFeedbackCommentDTO {
  ifcmId: string;
  ifcmAuthorPartyId: number;
  ifcmAuthorName: string;
  ifcmKind: string;
  ifcmBody: string;
  ifcmCreatedAt: string;
}

export interface InternalFeedbackHistoryDTO {
  ifhId: string;
  ifhActorPartyId: number;
  ifhActorName: string;
  ifhAction: string;
  ifhPreviousState?: string | null;
  ifhNewState?: string | null;
  ifhMetadata?: string | null;
  ifhCreatedAt: string;
}

export interface InternalFeedbackRetestDTO {
  ifrtId: string;
  ifrtExecutionId?: string | null;
  ifrtTesterPartyId: number;
  ifrtTesterName: string;
  ifrtResult: 'passed' | 'failed' | 'blocked';
  ifrtNotes?: string | null;
  ifrtEvidenceSummary?: string | null;
  ifrtCreatedAt: string;
}

export interface InternalFeedbackDTO {
  ifrSummary: InternalFeedbackSummaryDTO;
  ifrDescription: string;
  ifrCategoryId?: string | null;
  ifrUrlOrScreen?: string | null;
  ifrDevice?: string | null;
  ifrBrowser?: string | null;
  ifrLanguage: string;
  ifrAccountRole: string;
  ifrReproductionSteps?: string | null;
  ifrExpectedResult?: string | null;
  ifrActualResult?: string | null;
  ifrFrequency?: string | null;
  ifrAssignedTo?: number | null;
  ifrResolution?: string | null;
  ifrRetestResult?: string | null;
  ifrClosureReason?: string | null;
  ifrGithubIssueUrl?: string | null;
  ifrVideoLinks?: string | null;
  ifrSubmittedAt?: string | null;
  ifrClosedAt?: string | null;
  ifrAuditPlanMutable: boolean;
  ifrEvidence: InternalFeedbackEvidenceDTO[];
  ifrComments: InternalFeedbackCommentDTO[];
  ifrHistory: InternalFeedbackHistoryDTO[];
  ifrRetests: InternalFeedbackRetestDTO[];
  ifrPotentialDuplicates: InternalFeedbackSummaryDTO[];
}

export interface LegacyFeedbackDTO {
  lfdId: string;
  lfdTitle: string;
  lfdDescription: string;
  lfdCategoryId?: string | null;
  lfdSeverityId?: string | null;
  lfdContactEmail?: string | null;
  lfdConsent: boolean;
  lfdCreatedBy?: number | null;
  lfdHasAttachment: boolean;
  lfdCreatedAt: string;
}

export interface FanClubDTO {
  fcId: number;
  fcArtistId: number;
  fcName: string;
  fcDescription?: string | null;
  fcOfficers: FanClubOfficerDTO[];
  fcFollowerCount: number;
  fcArtistImageUrl?: string | null;
}

export interface FanClubOfficerDTO {
  fcoPartyId: number;
  fcoFanName: string;
  fcoAvatarUrl?: string | null;
  fcoRole: string;
  fcoElectedAt?: string | null;
  fcoTermEndsAt?: string | null;
}

export interface FanClubPostDTO {
  fcpId: number;
  fcpParentId?: number | null;
  fcpTitle?: string | null;
  fcpContent: string;
  fcpMediaUrls: string[];
  fcpAuthorId: number;
  fcpAuthorName: string;
  fcpAvatarUrl?: string | null;
  fcpIsPinned: boolean;
  fcpIsHidden: boolean;
  fcpReplies: number;
  fcpReactions: ReactionSummaryDTO;
  fcpCreatedAt: string;
  fcpUpdatedAt?: string | null;
}

export interface FanClubEventDTO {
  fceId: number;
  fceTitle: string;
  fceDescription?: string | null;
  fceStartsAt?: string | null;
  fceEndsAt?: string | null;
  fceLocation?: string | null;
  fceIsArtistConcert: boolean;
  fceCreatedBy?: number | null;
}

export interface FanClubElectionDTO {
  fceElectionId: number;
  fceYear: number;
  fceStatus: string;
  fceCandidacyStartsAt?: string | null;
  fceCandidacyEndsAt?: string | null;
  fceVotingStartsAt?: string | null;
  fceVotingEndsAt?: string | null;
  fceMyCandidacies: FanClubCandidacyDTO[];
  fceMyVotes: FanClubVoteDTO[];
}

export interface FanClubCandidacyDTO {
  fccCandidacyId: number;
  fccFanId: number;
  fccFanName: string;
  fccAvatarUrl?: string | null;
  fccRole: string;
  fccManifesto?: string | null;
  fccVoteCount: number;
}

export interface FanClubVoteDTO {
  fcvCandidacyId: number;
  fcvRole: string;
}

export interface FanClubCreatePostReq {
  fcpReqTitle?: string | null;
  fcpReqContent: string;
  fcpReqParentId?: number | null;
  fcpReqMediaUrls?: string[];
}

export interface FanClubCreateEventReq {
  fcevTitle: string;
  fcevDescription?: string | null;
  fcevStartsAt?: string | null;
  fcevEndsAt?: string | null;
  fcevLocation?: string | null;
}

export interface FanClubCreateElectionReq {
  fcelYear: number;
  fcelCandidacyStartsAt?: string | null;
  fcelCandidacyEndsAt?: string | null;
  fcelVotingStartsAt?: string | null;
  fcelVotingEndsAt?: string | null;
}

export interface FanClubCreateCandidacyReq {
  fccrRole: string;
  fccrManifesto?: string | null;
}

export interface FanClubVoteReq {
  fcvCandidacyIds: number[];
}

export interface FanClubMemberProfileDTO {
  fcmpId: number;
  fcmpPartyId: number;
  fcmpClubId: number;
  fcmpHandle?: string | null;
  fcmpBio?: string | null;
  fcmpAvatarUrl?: string | null;
  fcmpDisplayName: string;
  fcmpJoinedAt: string;
}

export interface FanClubMemoryDTO {
  fcmId: number;
  fcmMemberProfileId: number;
  fcmMemberName: string;
  fcmMemberAvatarUrl?: string | null;
  fcmTitle: string;
  fcmDescription?: string | null;
  fcmMediaUrls: string[];
  fcmIsHidden: boolean;
  fcmIsDeleted: boolean;
  fcmReactions: ReactionSummaryDTO;
  fcmCreatedAt: string;
}

export interface FanClubMemoryReportDTO {
  fcmrId: number;
  fcmrReporterId: number;
  fcmrMemoryId: number;
  fcmrReason: string;
  fcmrCreatedAt: string;
}

export interface ReactionSummaryDTO {
  rsItems: ReactionSummaryItemDTO[];
  rsTotal: number;
  rsMyReactionTypeId: string | null;
}

export interface ReactionSummaryItemDTO {
  rsiReactionTypeId: string;
  rsiCode: string;
  rsiNameEs: string;
  rsiNameEn: string;
  rsiDisplaySymbol: string;
  rsiCount: number;
}

export interface FanClubFeedItemDTO {
  fcfId: number;
  fcfKind: string;
  fcfTitle?: string | null;
  fcfContent: string;
  fcfAuthorId: number;
  fcfAuthorName: string;
  fcfAvatarUrl?: string | null;
  fcfMediaUrls: string[];
  fcfIsPinned: boolean;
  fcfIsOfficer: boolean;
  fcfIsHidden: boolean;
  fcfReactions: ReactionSummaryDTO;
  fcfCreatedAt: string;
}

export interface FanClubCreateMemoryReq {
  fcmReqTitle: string;
  fcmReqDescription?: string | null;
  fcmReqMediaUrls: string[];
}

export interface FanClubMemoryReportReq {
  fcmrReqReason: string;
}

export interface FanClubMemberProfileUpdate {
  fcmpuHandle?: string | null;
  fcmpuBio?: string | null;
  fcmpuAvatarUrl?: string | null;
}

export interface FanClubInboxMessageDTO {
  fcimId: number;
  fcimFanId: number;
  fcimFanName: string;
  fcimFanAvatarUrl?: string | null;
  fcimSubject?: string | null;
  fcimBody: string;
  fcimStatus: string;
  fcimOfficerId?: number | null;
  fcimOfficerName?: string | null;
  fcimReplyBody?: string | null;
  fcimCreatedAt: string;
  fcimUpdatedAt?: string | null;
}

export interface FanClubInboxSendReq {
  fcisReqSubject?: string | null;
  fcisReqBody: string;
}

export interface FanClubInboxReplyReq {
  fcirReqBody: string;
}

export interface FanClubInboxStatusReq {
  fcistReqStatus: string;
}

export interface ContentReactionReq {
  crrReactionTypeId: string;
}

export interface NotificationDTO {
  nId: number;
  nType: string;
  nTitle: string;
  nBody: string;
  nTargetType?: string | null;
  nTargetId?: number | null;
  nIsRead: boolean;
  nCreatedAt: string;
}

export interface NotificationCountDTO {
  ncUnread: number;
}

export interface LeaderboardEntryDTO {
  lbPartyId: number;
  lbDisplayName: string;
  lbAvatarUrl?: string | null;
  lbTotalReactions: number;
  lbBadges: CreatorBadgeDTO[];
  lbRank: number;
}

export interface CreatorBadgeDTO {
  cbBadgeTypeId: string;
  cbCode: string;
  cbNameEs: string;
  cbNameEn: string;
  cbAwardedAt: string;
  cbExpiresAt?: string | null;
}

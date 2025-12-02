export interface PartyDTO {
  partyId: number;
  legalName?: string | null;
  displayName: string;
  isOrg: boolean;
  taxId?: string | null;
  primaryEmail?: string | null;
  primaryPhone?: string | null;
  whatsapp?: string | null;
  instagram?: string | null;
  emergencyContact?: string | null;
  notes?: string | null;
  hasUserAccount?: boolean;
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
}

export interface AssetCheckoutDTO {
  checkoutId: string;
  assetId: string;
  targetKind: string;
  targetSessionId?: string | null;
  targetPartyRef?: string | null;
  targetRoomId?: string | null;
  checkedOutBy: string;
  checkedOutAt: string;
  dueAt?: string | null;
  conditionOut?: string | null;
  conditionIn?: string | null;
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
  serviceType?: string | null;
  serviceOrderId?: number | null;
  serviceOrderTitle?: string | null;
  customerName?: string | null;
  partyDisplayName?: string | null;
  resources: BookingResourceDTO[];
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
  pcId: string;
  pcTitle: string;
  pcArtist?: string | null;
  pcType: string;
  pcStage: string;
  pcSortOrder: number;
  pcNotes?: string | null;
}

export interface PipelineCardUpdate {
  pcuTitle?: string;
  pcuArtist?: string | null;
  pcuStage?: string;
  pcuSortOrder?: number;
  pcuNotes?: string | null;
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
  apGenres?: string | null;
  apHighlights?: string | null;
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

export interface FanProfileDTO {
  fpArtistId: number;
  fpDisplayName?: string | null;
  fpAvatarUrl?: string | null;
  fpFavoriteGenres?: string | null;
  fpBio?: string | null;
  fpCity?: string | null;
}

export interface FanProfileUpdate {
  fpuDisplayName?: string | null;
  fpuAvatarUrl?: string | null;
  fpuFavoriteGenres?: string | null;
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

export interface ArtistProfileUpsert {
  apuArtistId: number;
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
  apuGenres?: string | null;
  apuHighlights?: string | null;
}

export interface PaymentDTO {
  payId: number;
  payPartyId: number;
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

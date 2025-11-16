export type PartyDTO = {
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
};

export type PartyCreate = {
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
};

export type PartyUpdate = Partial<{
  uDisplayName: string;
  uIsOrg: boolean;
  uLegalName: string | null;
  uPrimaryEmail: string | null;
  uPrimaryPhone: string | null;
  uWhatsapp: string | null;
  uInstagram: string | null;
  uTaxId: string | null;
  uEmergencyContact: string | null;
  uNotes: string | null;
}>;

export type BookingResourceDTO = {
  brResourceId?: number | string | null;
  brRole?: string | null;
  brRoomName?: string | null;
};

export type BookingDTO = {
  bookingId: number;
  title?: string | null;
  startsAt?: string | null;
  endsAt?: string | null;
  status?: string | null;
  notes?: string | null;
  resources?: BookingResourceDTO[];
  partyId?: number | null;
  customerName?: string | null;
  partyDisplayName?: string | null;
  serviceOrderId?: number | null;
  serviceType?: string | null;
  serviceOrderTitle?: string | null;
};

export type VersionInfo = {
  name: string;
  version: string;
  commit?: string | null;
  buildTime?: string | null;
};

export type HealthStatus = {
  status: 'ok' | 'degraded' | string;
  version?: string | null;
};

export type ArtistProfileDTO = {
  apId?: number;
  apArtistId?: number;
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
  apFollowerCount?: number | null;
  apDisplayName?: string | null;
  apAvatarUrl?: string | null;
};

export type ArtistReleaseDTO = {
  arId?: number;
  arTitle?: string | null;
  arReleaseDate?: string | null;
  arCoverUrl?: string | null;
  arSpotifyUrl?: string | null;
  arYoutubeUrl?: string | null;
};

export type ArtistProfileUpsert = {
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
};

export type FanProfileDTO = {
  fpDisplayName?: string | null;
  fpBio?: string | null;
  fpCity?: string | null;
  fpFavoriteGenres?: string | null;
  fpAvatarUrl?: string | null;
};

export type FanProfileUpdate = {
  fpuDisplayName?: string | null;
  fpuBio?: string | null;
  fpuCity?: string | null;
  fpuFavoriteGenres?: string | null;
  fpuAvatarUrl?: string | null;
};

export type FanFollowDTO = {
  ffArtistId?: number;
  ffFollowerId?: number;
  ffArtistName?: string | null;
};

export type PipelineCardDTO = {
  pcId?: string | number;
  pcTitle?: string | null;
  pcArtist?: string | null;
  pcStage: string;
  pcSortOrder?: number;
};

export type PipelineCardUpdate = Partial<{
  pcuStage: string;
  pcuSortOrder: number;
}>;

export type RoomDTO = {
  roomId: string;
  rName: string;
  rBookable: boolean;
};

export type RoomCreate = {
  rcName: string;
};

export type RoomUpdate = Partial<{
  ruName: string;
  ruIsBookable: boolean;
}>;

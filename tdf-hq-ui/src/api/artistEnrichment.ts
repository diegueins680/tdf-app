import { get, patch, post } from './client';

export interface ArtistInventoryReference {
  airId: number;
  airSourceType: string;
  airSourceRecordId: string;
  airOriginalName: string;
  airNormalizedName: string;
  airArtistId?: number | null;
  airSocialArtistId?: number | null;
  airAliases?: string | null;
  airEvidence?: string | null;
  airConfidence?: number | null;
  airDisposition: string;
  airFirstSeenAt: string;
  airLastSeenAt: string;
}

export interface ArtistResearchSource {
  arsId: number;
  arsArtistId?: number | null;
  arsInventoryReferenceId?: number | null;
  arsSourceUrl: string;
  arsSourceType: string;
  arsRetrievedAt: string;
  arsSupportedFields: string;
  arsAttribution?: string | null;
  arsContentHash?: string | null;
}

export interface ArtistEnrichmentSuggestion {
  aesId: number;
  aesArtistId?: number | null;
  aesInventoryReferenceId?: number | null;
  aesArtistName?: string | null;
  aesFieldName: string;
  aesCurrentValue?: string | null;
  aesProposedValue?: string | null;
  aesConfidence: number;
  aesStatus: string;
  aesAutoPublish: boolean;
  aesEvidence: string;
  aesCreatedAt: string;
  aesUpdatedAt: string;
  aesDecidedAt?: string | null;
  aesDecidedBy?: number | null;
  aesDecisionNote?: string | null;
}

export interface ArtistFieldChange {
  afcId: number;
  afcArtistId: number;
  afcSuggestionId?: number | null;
  afcFieldName: string;
  afcPreviousValue?: string | null;
  afcNewValue?: string | null;
  afcEvidence: string;
  afcConfidence: number;
  afcActor: string;
  afcChangedAt: string;
}

export interface ArtistEnrichmentRun {
  aerId: number;
  aerRunKey: string;
  aerMode: string;
  aerScope: string;
  aerRequestedArtistId?: number | null;
  aerStatus: string;
  aerPhase: string;
  aerCheckpoint?: string | null;
  aerCounters?: string | null;
  aerErrorSummary?: string | null;
  aerStartedAt: string;
  aerHeartbeatAt: string;
  aerFinishedAt?: string | null;
}

export interface ArtistIdentityCandidate {
  aicId: number;
  aicInventoryReferenceId: number;
  aicArtistId?: number | null;
  aicProvider: string;
  aicExternalId?: string | null;
  aicCandidateUrl?: string | null;
  aicEvidence: string;
  aicConfidence: number;
  aicStatus: string;
  aicCreatedAt: string;
  aicUpdatedAt: string;
  aicDecidedAt?: string | null;
  aicDecidedBy?: number | null;
  aicDecisionNote?: string | null;
}

export interface ArtistMediaAsset {
  amaId: number;
  amaArtistId: number;
  amaAssetKind: string;
  amaSourceUrl: string;
  amaSourceAttribution: string;
  amaRetrievedAt: string;
  amaSourceContentHash: string;
  amaSourceWidth: number;
  amaSourceHeight: number;
  amaSourceMimeType: string;
  amaSourceByteSize: number;
  amaContentHash: string;
  amaWidth: number;
  amaHeight: number;
  amaMimeType: string;
  amaByteSize: number;
  amaRightsStatus: string;
  amaDriveFileId: string;
  amaPublicUrl: string;
  amaParentAssetId?: number | null;
  amaFocalPoint?: string | null;
  amaCreatedAt: string;
}

export interface ArtistProfileEnrichment {
  apeArtistId: number;
  apeArtistName: string;
  apeOfficialName?: string | null;
  apeCountry?: string | null;
  apeInstagramUrl?: string | null;
  apeSocialLinks?: string | null;
  apeDiscography?: string | null;
  apeAchievements?: string | null;
  apeHeroOriginalUrl?: string | null;
  apeHeroSquareUrl?: string | null;
  apeHeroLandscapeUrl?: string | null;
  apeHeroResponsiveUrls?: string | null;
  apeHeroFocalPoint?: string | null;
  apeLastVerifiedAt?: string | null;
  apeConfidence?: number | null;
  apeReviewStatus: string;
  apeMissingFields: string[];
  apeBrokenFields: string[];
}

export interface ArtistEnrichmentOverview {
  aeoProfiles: ArtistProfileEnrichment[];
  aeoInventory: ArtistInventoryReference[];
  aeoSources: ArtistResearchSource[];
  aeoSuggestions: ArtistEnrichmentSuggestion[];
  aeoChanges: ArtistFieldChange[];
  aeoRuns: ArtistEnrichmentRun[];
  aeoIdentityCandidates: ArtistIdentityCandidate[];
  aeoMedia: ArtistMediaAsset[];
}

export interface ArtistEnrichmentRunRequest {
  aerrMode: 'dry_run' | 'production';
  aerrArtistId?: number | null;
  aerrResumeRunKey?: string | null;
  aerrBatchSize?: number | null;
  aerrStaleDays?: number | null;
}

export interface ArtistEnrichmentRunUpdate {
  aeruStatus?: 'running' | 'completed' | 'failed' | 'cancelled' | 'blocked';
  aeruPhase?: string;
  aeruCheckpoint?: string;
  aeruCounters?: string;
  aeruErrorSummary?: string;
}

export interface ArtistEnrichmentDecision {
  aedDecision: 'approve' | 'reject';
  aedEditedValue?: string | null;
  aedNote?: string | null;
}

const overviewQuery = (status?: string, artistId?: number | null) => {
  const params = new URLSearchParams();
  if (status) params.set('status', status);
  if (artistId != null) params.set('artistId', String(artistId));
  const query = params.toString();
  return query ? `?${query}` : '';
};

export const ArtistEnrichment = {
  overview: (status?: string, artistId?: number | null) =>
    get<ArtistEnrichmentOverview>(`/admin/artists/enrichment/overview${overviewQuery(status, artistId)}`),
  run: (request: ArtistEnrichmentRunRequest) =>
    post<ArtistEnrichmentRun>('/admin/artists/enrichment/runs', request),
  updateRun: (runId: number, request: ArtistEnrichmentRunUpdate) =>
    patch<ArtistEnrichmentRun>(`/admin/artists/enrichment/runs/${runId}`, request),
  rerunArtist: (artistId: number, request: ArtistEnrichmentRunRequest) =>
    post<ArtistEnrichmentRun>(`/admin/artists/enrichment/artists/${artistId}/rerun`, request),
  decideSuggestion: (suggestionId: number, decision: ArtistEnrichmentDecision) =>
    patch<ArtistEnrichmentSuggestion>(`/admin/artists/enrichment/suggestions/${suggestionId}`, decision),
  decideSuggestionSet: (artistId: number, decision: ArtistEnrichmentDecision) =>
    patch<ArtistEnrichmentSuggestion[]>(`/admin/artists/enrichment/suggestion-sets/${artistId}`, decision),
  decideIdentity: (candidateId: number, decision: ArtistEnrichmentDecision) =>
    patch<ArtistIdentityCandidate>(`/admin/artists/enrichment/identity-candidates/${candidateId}`, decision),
};

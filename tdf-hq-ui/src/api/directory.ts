import { del, get, patch, post, put } from './client';
import type { components, operations } from './generated/types';

export type DirectoryEntityType = components['schemas']['DirectoryEntityType'];
export type DirectoryLocation = components['schemas']['PublicLocation'];
export type DirectorySearchItem = components['schemas']['DirectorySearchItem'];
export type DirectorySearchResponse = components['schemas']['DirectorySearchResponse'];
export type DirectorySearchQuery = NonNullable<operations['searchDirectory']['parameters']['query']>;
export type DirectoryTaxonomyItem = components['schemas']['TaxonomyItem'];
export type DirectoryTaxonomies = components['schemas']['DirectoryTaxonomies'];
export type ManagedDirectoryProfile = components['schemas']['ManagedDirectoryProfile'];
export type DirectoryProfileUpsert = components['schemas']['DirectoryProfileUpsert'];
export type DirectoryPortfolioItem = components['schemas']['DirectoryPortfolioItem'];
export type DirectoryProfileLink = components['schemas']['DirectoryProfileLink'];
export type ManagedClassified = components['schemas']['ManagedClassified'];
export type DirectoryInvitation = components['schemas']['DirectoryInvitation'];
export type DirectoryReviewPage = components['schemas']['DirectoryReviewPage'];
export type DirectoryReviewEligibility = components['schemas']['DirectoryReviewEligibility'];
export type DirectoryReview = components['schemas']['DirectoryReview'];
export type DirectoryFavorite = components['schemas']['DirectoryFavorite'];
export type PublicClassified = components['schemas']['PublicClassified'];
export type ClassifiedApplication = components['schemas']['ClassifiedApplication'];
export type OfferRound = components['schemas']['OfferRound'];
export type OfferCounterRequest = components['schemas']['OfferCounterRequest'];
export type OfferTerms = components['schemas']['OfferTerms'];
export type ClassifiedIntent = components['schemas']['ClassifiedIntent'];
export type PriceMode = components['schemas']['PriceMode'];
export type OfferSort = NonNullable<NonNullable<operations['listClassifiedApplications']['parameters']['query']>['sort']>;
export type MyOffersStatus = NonNullable<NonNullable<operations['listMyApplications']['parameters']['query']>['status']>;

const idempotencyHeaders = (key?: string) => ({ headers: { 'Idempotency-Key': key ?? crypto.randomUUID() } });

const append = (params: URLSearchParams, key: string, value: string | number | boolean | null | undefined) => {
  if (value !== undefined && value !== null && value !== '') params.set(key, `${value}`);
};

export const Directory = {
  notificationContext: (kind: string, id: string) => get<Record<string, unknown>>(`/directory/notification-context/${encodeURIComponent(kind)}/${encodeURIComponent(id)}`),
  search: (query: DirectorySearchQuery = {}, signal?: AbortSignal) => {
    const params = new URLSearchParams();
    Object.entries(query).forEach(([key, value]) => append(params, key, value));
    return get<DirectorySearchResponse>(`/directory/search?${params.toString()}`, { signal });
  },
  suggestions: (q: string, cityId?: string) => {
    const params = new URLSearchParams({ q });
    append(params, 'cityId', cityId);
    return get<{ label: string; canonicalQuery: string; suggestionKind: string; entityId?: string }[]>(
      `/directory/suggestions?${params.toString()}`,
    );
  },
  taxonomies: (locale = 'es') =>
    get<DirectoryTaxonomies>(`/directory/taxonomies?locale=${encodeURIComponent(locale)}`),
  profile: (slug: string) => get<components['schemas']['PublicDirectoryProfile']>(`/directory/profiles/${encodeURIComponent(slug)}`),
  profileByParty: (partyId: number) =>
    get<components['schemas']['PublicDirectoryProfile']>(`/directory/party-profiles/${partyId}`),
  prepareArtistClaim: (partyId: number) =>
    put<{ id: string; name: string }>(`/directory/artist-claim-targets/${partyId}`, {}),
  profileReviews: (slug: string, cursor?: string, limit = 20) => {
    const params = new URLSearchParams({ limit: String(limit) });
    append(params, 'cursor', cursor);
    return get<DirectoryReviewPage>(`/directory/profiles/${encodeURIComponent(slug)}/reviews?${params.toString()}`);
  },
  classified: (slug: string) => get<components['schemas']['PublicClassified']>(`/directory/classifieds/${encodeURIComponent(slug)}`),
  event: (id: string) => get<components['schemas']['PublicDirectoryEvent']>(`/directory/events/${encodeURIComponent(id)}`),
  venue: (id: string) => get<components['schemas']['PublicDirectoryVenue']>(`/directory/venues/${encodeURIComponent(id)}`),
  setAgeAssurance: (adultAttestation: boolean, guardianPartyId?: number) =>
    put<{ status: string; guardianConsentStatus?: string | null }>('/directory/age-assurance', { adultAttestation, guardianPartyId }),
  managedProfiles: () => get<ManagedDirectoryProfile[]>('/directory/profiles'),
  createProfile: (body: components['schemas']['DirectoryProfileUpsert'], idempotencyKey?: string) =>
    post<ManagedDirectoryProfile>('/directory/profiles', body, idempotencyHeaders(idempotencyKey)),
  updateProfile: (profileId: string, body: components['schemas']['DirectoryProfileUpsert']) =>
    put<ManagedDirectoryProfile>(`/directory/profiles/${encodeURIComponent(profileId)}`, body),
  setProfileStatus: (profileId: string, status: string) =>
    patch<ManagedDirectoryProfile>(`/directory/profiles/${encodeURIComponent(profileId)}/status`, { status }),
  managedClassifieds: () => get<ManagedClassified[]>('/directory/classifieds'),
  createClassified: (body: components['schemas']['ClassifiedCreate'], idempotencyKey?: string) =>
    post<ManagedClassified>('/directory/classifieds', body, idempotencyHeaders(idempotencyKey)),
  setClassifiedStatus: (classifiedId: string, status: string) =>
    patch<ManagedClassified>(`/directory/classifieds/${encodeURIComponent(classifiedId)}/status`, { status }),
  applications: (classifiedId: string, sort?: OfferSort) =>
    get<ClassifiedApplication[]>(`/directory/classifieds/${encodeURIComponent(classifiedId)}/applications${sort ? `?sort=${encodeURIComponent(sort)}` : ''}`),
  apply: (classifiedId: string, body: components['schemas']['ApplicationCreate'], idempotencyKey?: string) =>
    post<ClassifiedApplication>(`/directory/classifieds/${encodeURIComponent(classifiedId)}/applications`, body, idempotencyHeaders(idempotencyKey)),
  // ADR 0134: offers made through the caller's profiles, one offer with its rounds, and counter-offers.
  myApplications: (status?: MyOffersStatus) =>
    get<ClassifiedApplication[]>(`/directory/applications${status ? `?status=${encodeURIComponent(status)}` : ''}`),
  application: (applicationId: string) =>
    get<ClassifiedApplication>(`/directory/applications/${encodeURIComponent(applicationId)}`),
  counterOffer: (applicationId: string, body: OfferCounterRequest, idempotencyKey?: string) =>
    post<ClassifiedApplication>(`/directory/applications/${encodeURIComponent(applicationId)}/counter`, body, idempotencyHeaders(idempotencyKey)),
  /** expectedVersion binds the decision to the offer round the caller saw (409 when it moved on). */
  setApplicationStatus: (applicationId: string, status: string, reason?: string, expectedVersion?: number) =>
    patch<ClassifiedApplication>(`/directory/applications/${encodeURIComponent(applicationId)}/status`, {
      status,
      ...(reason ? { reason } : {}),
      ...(typeof expectedVersion === 'number' ? { expectedVersion } : {}),
    } satisfies components['schemas']['StatusRequest']),
  contact: (body: components['schemas']['DirectoryContact'], idempotencyKey?: string) =>
    post<Record<string, unknown>>('/directory/contact', body, idempotencyHeaders(idempotencyKey)),
  reviewEligibility: (authorProfileId?: string) =>
    get<DirectoryReviewEligibility[]>(`/directory/review-eligibility${authorProfileId ? `?authorProfileId=${encodeURIComponent(authorProfileId)}` : ''}`),
  createReview: (body: components['schemas']['DirectoryReviewCreate'], idempotencyKey?: string) =>
    post<DirectoryReview>('/directory/reviews', body, idempotencyHeaders(idempotencyKey)),
  invitations: () => get<components['schemas']['DirectoryInvitation'][]>('/directory/invitations'),
  invite: (body: components['schemas']['InvitationCreate'], idempotencyKey?: string) =>
    post<components['schemas']['DirectoryInvitation']>('/directory/invitations', body, idempotencyHeaders(idempotencyKey)),
  setInvitationStatus: (invitationId: string, status: string) =>
    patch<components['schemas']['DirectoryInvitation']>(`/directory/invitations/${encodeURIComponent(invitationId)}/status`, { status }),
  favorites: (targetKind?: DirectoryEntityType) =>
    get<components['schemas']['DirectoryFavorite'][]>(`/directory/favorites${targetKind ? `?targetKind=${encodeURIComponent(targetKind)}` : ''}`),
  addFavorite: (targetKind: DirectoryEntityType, targetId: string) =>
    put<void>(`/directory/favorites/${encodeURIComponent(targetKind)}/${encodeURIComponent(targetId)}`, {}),
  removeFavorite: (targetKind: DirectoryEntityType, targetId: string) =>
    del<void>(`/directory/favorites/${encodeURIComponent(targetKind)}/${encodeURIComponent(targetId)}`),
  savedSearches: () => get<components['schemas']['SavedDirectorySearch'][]>('/directory/saved-searches'),
  saveSearch: (body: components['schemas']['SavedSearchCreate'], idempotencyKey?: string) =>
    post<components['schemas']['SavedDirectorySearch']>('/directory/saved-searches', body, idempotencyHeaders(idempotencyKey)),
  claim: (body: components['schemas']['ClaimCreate'], idempotencyKey?: string) =>
    post<Record<string, unknown>>('/directory/claims', body, idempotencyHeaders(idempotencyKey)),
  requestVerification: (body: components['schemas']['VerificationCreate'], idempotencyKey?: string) =>
    post<Record<string, unknown>>('/directory/verifications', body, idempotencyHeaders(idempotencyKey)),
  report: (body: components['schemas']['ReportCreate'], idempotencyKey?: string) =>
    post<Record<string, unknown>>('/directory/reports', body, idempotencyHeaders(idempotencyKey)),
  adminClaims: () => get<Record<string, unknown>[]>('/directory/admin/claims'),
  setClaimStatus: (claimId: string, status: string, reason?: string) =>
    patch<Record<string, unknown>>(`/directory/admin/claims/${encodeURIComponent(claimId)}/status`, { status, reason }),
  adminVerifications: () => get<Record<string, unknown>[]>('/directory/admin/verifications'),
  setVerificationStatus: (verificationId: string, status: string, reason?: string) =>
    patch<Record<string, unknown>>(`/directory/admin/verifications/${encodeURIComponent(verificationId)}/status`, { status, reason }),
  moderationQueue: () => get<Record<string, unknown>[]>('/directory/admin/moderation'),
  decideModeration: (caseId: string, body: components['schemas']['ModerationDecision'], idempotencyKey?: string) =>
    post<Record<string, unknown>>(`/directory/admin/moderation/${encodeURIComponent(caseId)}/decisions`, body, idempotencyHeaders(idempotencyKey)),
  mergeProfiles: (body: components['schemas']['ProfileMerge'], idempotencyKey?: string) =>
    post<Record<string, unknown>>('/directory/admin/merges', body, idempotencyHeaders(idempotencyKey)),
};

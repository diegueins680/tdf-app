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
export type ManagedClassified = components['schemas']['ManagedClassified'];
export type DirectoryInvitation = components['schemas']['DirectoryInvitation'];

const idempotencyHeaders = (key?: string) => ({ headers: { 'Idempotency-Key': key ?? crypto.randomUUID() } });

const append = (params: URLSearchParams, key: string, value: string | number | boolean | null | undefined) => {
  if (value !== undefined && value !== null && value !== '') params.set(key, `${value}`);
};

export const Directory = {
  search: (query: DirectorySearchQuery = {}) => {
    const params = new URLSearchParams();
    Object.entries(query).forEach(([key, value]) => append(params, key, value));
    return get<DirectorySearchResponse>(`/directory/search?${params.toString()}`);
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
  classified: (slug: string) => get<components['schemas']['PublicClassified']>(`/directory/classifieds/${encodeURIComponent(slug)}`),
  event: (id: string) => get<components['schemas']['PublicDirectoryEvent']>(`/directory/events/${encodeURIComponent(id)}`),
  venue: (id: string) => get<components['schemas']['PublicDirectoryVenue']>(`/directory/venues/${encodeURIComponent(id)}`),
  setAgeAssurance: (adultAttestation: boolean, guardianPartyId?: number) =>
    put<{ status: string; guardianConsentStatus?: string | null }>('/directory/age-assurance', { adultAttestation, guardianPartyId }),
  managedProfiles: () => get<ManagedDirectoryProfile[]>('/directory/profiles'),
  createProfile: (body: components['schemas']['DirectoryProfileUpsert'], idempotencyKey?: string) =>
    post<ManagedDirectoryProfile>('/directory/profiles', body, idempotencyHeaders(idempotencyKey)),
  setProfileStatus: (profileId: string, status: string) =>
    patch<ManagedDirectoryProfile>(`/directory/profiles/${encodeURIComponent(profileId)}/status`, { status }),
  managedClassifieds: () => get<ManagedClassified[]>('/directory/classifieds'),
  createClassified: (body: components['schemas']['ClassifiedCreate'], idempotencyKey?: string) =>
    post<ManagedClassified>('/directory/classifieds', body, idempotencyHeaders(idempotencyKey)),
  setClassifiedStatus: (classifiedId: string, status: string) =>
    patch<ManagedClassified>(`/directory/classifieds/${encodeURIComponent(classifiedId)}/status`, { status }),
  applications: (classifiedId: string) =>
    get<Record<string, unknown>[]>(`/directory/classifieds/${encodeURIComponent(classifiedId)}/applications`),
  apply: (classifiedId: string, body: components['schemas']['ApplicationCreate'], idempotencyKey?: string) =>
    post<components['schemas']['ClassifiedApplication']>(`/directory/classifieds/${encodeURIComponent(classifiedId)}/applications`, body, idempotencyHeaders(idempotencyKey)),
  setApplicationStatus: (applicationId: string, status: string) =>
    patch<Record<string, unknown>>(`/directory/applications/${encodeURIComponent(applicationId)}/status`, { status }),
  contact: (body: components['schemas']['DirectoryContact'], idempotencyKey?: string) =>
    post<Record<string, unknown>>('/directory/contact', body, idempotencyHeaders(idempotencyKey)),
  invitations: () => get<components['schemas']['DirectoryInvitation'][]>('/directory/invitations'),
  invite: (body: components['schemas']['InvitationCreate'], idempotencyKey?: string) =>
    post<components['schemas']['DirectoryInvitation']>('/directory/invitations', body, idempotencyHeaders(idempotencyKey)),
  setInvitationStatus: (invitationId: string, status: string) =>
    patch<components['schemas']['DirectoryInvitation']>(`/directory/invitations/${encodeURIComponent(invitationId)}/status`, { status }),
  favorites: () => get<components['schemas']['DirectoryFavorite'][]>('/directory/favorites'),
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

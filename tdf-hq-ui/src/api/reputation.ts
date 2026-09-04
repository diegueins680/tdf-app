import { get, put } from './client';
import type { components } from './generated/types';

export type PublicReputation = components['schemas']['PublicReputation'];
export type ReputationCategory = components['schemas']['ReputationCategory'];
export type ReputationPreference = components['schemas']['ReputationPreference'];
export type ReputationPreferenceSave = components['schemas']['ReputationPreferenceSave'];

/** Public aggregate only: never use it to expose individual rankings. */
export const Reputation = {
  getPublic: (partyId: number) => get<PublicReputation>(`/reputation/profiles/${encodeURIComponent(String(partyId))}`),
  categories: (locale: 'es' | 'en' = 'es') => get<ReputationCategory[]>(`/reputation/categories?locale=${locale}`),
  getMyPreferences: (contextKind = 'general') => get<ReputationPreference>(
    `/reputation/preferences?contextKind=${encodeURIComponent(contextKind)}`,
  ),
  saveMyPreferences: (input: ReputationPreferenceSave, idempotencyKey: string) => put<ReputationPreference>(
    '/reputation/preferences',
    input,
    { headers: { 'Idempotency-Key': idempotencyKey } },
  ),
};

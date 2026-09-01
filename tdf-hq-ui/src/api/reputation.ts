import { get } from './client';
import type { components } from './generated/types';

export type PublicReputation = components['schemas']['PublicReputation'];

/** Public aggregate only: never use it to expose individual rankings. */
export const Reputation = {
  getPublic: (partyId: number) => get<PublicReputation>(`/reputation/profiles/${encodeURIComponent(String(partyId))}`),
};

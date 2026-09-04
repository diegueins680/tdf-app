import { get } from './client';
import type { operations } from './generated/types';

export type PartySelectorKind = 'any' | 'person' | 'organization';
export type PartySelectorContext = NonNullable<
  operations['searchPartiesForSelector']['parameters']['query']['context']
>;

export interface PartySelectorOption {
  partyId: number;
  partyType: 'person' | 'organization';
  displayName: string;
  username: string | null;
  avatarUrl: string | null;
  secondaryLabel: string | null;
  accountStatus: 'active' | 'inactive' | 'no-account';
}

export interface PartySelectorPage {
  items: PartySelectorOption[];
  nextCursor: number | null;
}

export interface PartySelectorSearchParams {
  query: string;
  context?: PartySelectorContext;
  kind?: PartySelectorKind;
  accountOnly?: boolean;
  excludedPartyIds?: number[];
  cursor?: number;
  limit?: number;
  signal?: AbortSignal;
}

export const searchPartiesForSelector = ({
  query,
  context = 'crm_assignment',
  kind = 'any',
  accountOnly = false,
  excludedPartyIds = [],
  cursor,
  limit = 15,
  signal,
}: PartySelectorSearchParams): Promise<PartySelectorPage> => {
  const params = new URLSearchParams({ q: query, context, kind, accountOnly: String(accountOnly), limit: String(limit) });
  if (cursor) params.set('cursor', String(cursor));
  excludedPartyIds.forEach((id) => params.append('excludePartyId', String(id)));
  return get<PartySelectorPage>(`/parties/search?${params.toString()}`, { signal });
};

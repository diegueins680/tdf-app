import { get, post, put } from './client';
import type { components } from './generated/types';

type Schemas = components['schemas'];
export type InteractionKind = Schemas['InteractionKind'];
export type InteractionSummary = Schemas['InteractionSummary'];
export type InteractionComment = Schemas['InteractionComment'];
export type InteractionMention = Schemas['InteractionMention'];
export type InteractionCommand = Schemas['InteractionCommand'];
export type InteractionPage = Schemas['InteractionPage'];
export type InteractionDestination = Schemas['InteractionDestination'];
export type InteractionCommentContext = Schemas['InteractionCommentContext'];
export type InteractionPreferences = Schemas['InteractionPreferences'];
export type InteractionSort = 'newest' | 'oldest' | 'relevant';
export interface InteractionIdentity { kind: InteractionKind; entityKey: string }
const base = (authenticated: boolean) => `${authenticated ? '' : '/public'}/interactions`;
const path = (identity: InteractionIdentity, authenticated: boolean) =>
  `${base(authenticated)}/targets/${encodeURIComponent(identity.kind)}/${encodeURIComponent(identity.entityKey)}`;
export const Interactions = {
  summary: (identity: InteractionIdentity, authenticated: boolean, signal?: AbortSignal) =>
    get<InteractionSummary>(path(identity, authenticated), { signal }),
  comments: (identity: InteractionIdentity, authenticated: boolean, sort: InteractionSort, root?: string, cursor?: string, signal?: AbortSignal) => {
    const params = new URLSearchParams({ sort, limit: '20' });
    if (root) params.set('root', root);
    if (cursor) params.set('cursor', cursor);
    return get<InteractionPage>(`${path(identity, authenticated)}/comments?${params}`, { signal });
  },
  context: (identity: InteractionIdentity, authenticated: boolean, id: string, signal?: AbortSignal) =>
    get<InteractionCommentContext>(`${path(identity, authenticated)}/comments/${encodeURIComponent(id)}`, { signal }),
  command: (targetId: string, command: InteractionCommand, requestKey: string) =>
    post<Schemas['InteractionCommandResult']>(`/interactions/targets/${encodeURIComponent(targetId)}/commands`, { requestKey, command }),
  destination: (kind: 'comment' | 'target', id: string, authenticated: boolean, signal?: AbortSignal) =>
    get<InteractionDestination>(`${base(authenticated)}/resolve/${kind}/${encodeURIComponent(id)}`, { signal }),
  reactors: (identity: InteractionIdentity, authenticated: boolean, cursor?: number, signal?: AbortSignal) =>
    get<Schemas['InteractionReactors']>(`${path(identity, authenticated)}/reactors?limit=20${cursor ? `&cursor=${cursor}` : ''}`, { signal }),
  blockedAccounts: (cursor?: number, signal?: AbortSignal) => get<Schemas['InteractionBlockedAccounts']>(`/interactions/blocked-accounts?limit=20${cursor ? `&cursor=${cursor}` : ''}`, { signal }),
  reports: (cursor?: string, signal?: AbortSignal) => get<InteractionPage>(`/interactions/reports?limit=20${cursor ? `&cursor=${cursor}` : ''}`, { signal }),
  blockState: (id: number) => get<Schemas['InteractionBlock']>(`/interactions/blocks/${id}`),
  block: (id: number, blocked: boolean, expectedVersion: number, blockRequestKey: string) =>
    put<Schemas['InteractionBlock']>(`/interactions/blocks/${id}`, { blocked, expectedVersion, blockRequestKey }),
  preferences: () => get<Schemas['InteractionPreferences']>('/interactions/preferences'),
  setPreferences: (preferences: Schemas['InteractionPreferences']) => put<Schemas['InteractionPreferences']>('/interactions/preferences', preferences),
  moderation: (targetId: string, cursor?: string, signal?: AbortSignal) =>
    get<InteractionPage>(`/interactions/moderation/${targetId}?limit=20${cursor ? `&cursor=${cursor}` : ''}`, { signal }),
};

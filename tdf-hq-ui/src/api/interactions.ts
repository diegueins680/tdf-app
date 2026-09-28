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
// Transports may return HTML from a stale deployment's SPA fallback with HTTP 200.
// Reject it before caching: a broken discussion must not crash its publication.
export function validateInteractionSummary(value: InteractionSummary): InteractionSummary {
  if (!value || typeof value !== 'object' || typeof value.id !== 'string'
      || !Array.isArray(value.reactions)
      || !Number.isSafeInteger(value.commentCount) || value.commentCount < 0
      || !Number.isSafeInteger(value.rootCount) || value.rootCount < 0
      || value.reactions.some((reaction) => !reaction || typeof reaction.id !== 'string'
        || typeof reaction.label !== 'string' || typeof reaction.emoji !== 'string'
        || !Number.isSafeInteger(reaction.count) || reaction.count < 0)) {
    throw new Error('Invalid interaction summary response');
  }
  return value;
}
export const Interactions = {
  summary: async (identity: InteractionIdentity, authenticated: boolean, signal?: AbortSignal) =>
    validateInteractionSummary(await get<InteractionSummary>(path(identity, authenticated), { signal })),
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

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
type JsonObject = Record<string, unknown>;
const object = (value: unknown): value is JsonObject => value !== null && typeof value === 'object' && !Array.isArray(value);
const text = (value: unknown): value is string => typeof value === 'string';
const nullableText = (value: unknown) => value === null || text(value);
const count = (value: unknown): value is number => typeof value === 'number' && Number.isSafeInteger(value) && value >= 0;
const positive = (value: unknown) => count(value) && value > 0;
const boolean = (value: unknown) => typeof value === 'boolean';
const nullableBoolean = (value: unknown) => value === null || boolean(value);
const author = (value: unknown) => object(value) && positive(value['id']) && text(value['displayName']) && nullableText(value['avatarUrl']);
const mention = (value: unknown) => object(value) && positive(value['partyId']) && count(value['start']) && count(value['end']) && value['end'] > value['start'];
const arrayOf = (value: unknown, valid: (item: unknown) => boolean) => Array.isArray(value) && value.every(valid);
const context = (value: unknown) => object(value) && text(value['kind']) && text(value['key'])
  && (value['ownerId'] === null || positive(value['ownerId'])) && text(value['title']) && text(value['route'])
  && value['route'].startsWith('/') && !value['route'].startsWith('//') && boolean(value['public'])
  && boolean(value['canManage']) && boolean(value['reactable']) && boolean(value['commentable']) && boolean(value['shareable']);
const comment = (value: unknown): boolean => object(value) && text(value['id']) && text(value['targetId'])
  && nullableText(value['parentId']) && text(value['rootId']) && count(value['depth']) && positive(value['version'])
  && text(value['createdAt']) && Number.isFinite(Date.parse(value['createdAt']))
  && (value['editedAt'] === null || (text(value['editedAt']) && Number.isFinite(Date.parse(value['editedAt']))))
  && text(value['state']) && text(value['body']) && (value['author'] === null || author(value['author']))
  && nullableBoolean(value['canEdit']) && nullableBoolean(value['canDelete']) && arrayOf(value['mentions'], mention)
  && (value['replyCount'] === undefined || count(value['replyCount']))
  && (value['openReports'] === undefined || count(value['openReports']))
  && (value['moderationBody'] === undefined || text(value['moderationBody']))
  && (value['reportReasons'] === undefined || arrayOf(value['reportReasons'], text))
  && (value['legacyPresentation'] == null || (object(value['legacyPresentation'])
    && (value['legacyPresentation']['title'] === undefined || nullableText(value['legacyPresentation']['title']))
    && (value['legacyPresentation']['mediaUrls'] === undefined || arrayOf(value['legacyPresentation']['mediaUrls'], text))));
const page = (value: unknown) => object(value) && arrayOf(value['items'], comment) && nullableText(value['nextCursor']);
const commentContext = (value: unknown) => object(value) && context(value['target'])
  && comment(value['root']) && comment(value['comment']) && (value['parent'] === null || comment(value['parent']))
  && arrayOf(value['surrounding'], comment);
const destination = (value: unknown) => object(value) && context(value) && text(value['targetId'])
  && nullableText(value['commentId']) && (value['context'] === null || commentContext(value['context']));
const reactors = (value: unknown) => object(value) && arrayOf(value['items'], (item) =>
  object(item) && author(item['author']) && text(item['reactionTypeId'])) && (value['nextCursor'] === null || positive(value['nextCursor']));
const block = (value: unknown) => object(value) && positive(value['partyId']) && boolean(value['blocked']) && count(value['version']);
const blockedAccounts = (value: unknown) => object(value) && arrayOf(value['items'], (item) =>
  object(item) && block(item) && text(item['displayName'])) && (value['nextCursor'] === null || positive(value['nextCursor']));
const preferences = (value: unknown) => object(value) && boolean(value['reactions']) && boolean(value['comments'])
  && boolean(value['replies']) && boolean(value['mentions']);
async function validated<T>(response: Promise<T>, valid: (value: unknown) => boolean, name: string): Promise<T> {
  const value = await response;
  if (!valid(value)) throw new Error(`Invalid interaction ${name} response`);
  return value;
}
export const Interactions = {
  summary: async (identity: InteractionIdentity, authenticated: boolean, signal?: AbortSignal) =>
    validateInteractionSummary(await get<InteractionSummary>(path(identity, authenticated), { signal })),
  comments: (identity: InteractionIdentity, authenticated: boolean, sort: InteractionSort, root?: string, cursor?: string, signal?: AbortSignal) => {
    const params = new URLSearchParams({ sort, limit: '20' });
    if (root) params.set('root', root);
    if (cursor) params.set('cursor', cursor);
    return validated(get<InteractionPage>(`${path(identity, authenticated)}/comments?${params}`, { signal }), page, 'comments');
  },
  context: (identity: InteractionIdentity, authenticated: boolean, id: string, signal?: AbortSignal) =>
    validated(get<InteractionCommentContext>(`${path(identity, authenticated)}/comments/${encodeURIComponent(id)}`, { signal }), commentContext, 'context'),
  command: (targetId: string, command: InteractionCommand, requestKey: string) =>
    post<Schemas['InteractionCommandResult']>(`/interactions/targets/${encodeURIComponent(targetId)}/commands`, { requestKey, command }),
  destination: (kind: 'comment' | 'target', id: string, authenticated: boolean, signal?: AbortSignal) =>
    validated(get<InteractionDestination>(`${base(authenticated)}/resolve/${kind}/${encodeURIComponent(id)}`, { signal }), destination, 'destination'),
  reactors: (identity: InteractionIdentity, authenticated: boolean, cursor?: number, signal?: AbortSignal) =>
    validated(get<Schemas['InteractionReactors']>(`${path(identity, authenticated)}/reactors?limit=20${cursor ? `&cursor=${cursor}` : ''}`, { signal }), reactors, 'reactors'),
  blockedAccounts: (cursor?: number, signal?: AbortSignal) => validated(get<Schemas['InteractionBlockedAccounts']>(`/interactions/blocked-accounts?limit=20${cursor ? `&cursor=${cursor}` : ''}`, { signal }), blockedAccounts, 'blocked accounts'),
  reports: (cursor?: string, signal?: AbortSignal) => validated(get<InteractionPage>(`/interactions/reports?limit=20${cursor ? `&cursor=${cursor}` : ''}`, { signal }), page, 'reports'),
  blockState: (id: number) => validated(get<Schemas['InteractionBlock']>(`/interactions/blocks/${id}`), block, 'block'),
  block: (id: number, blocked: boolean, expectedVersion: number, blockRequestKey: string) =>
    put<Schemas['InteractionBlock']>(`/interactions/blocks/${id}`, { blocked, expectedVersion, blockRequestKey }),
  preferences: () => validated(get<Schemas['InteractionPreferences']>('/interactions/preferences'), preferences, 'preferences'),
  setPreferences: (preferences: Schemas['InteractionPreferences']) => put<Schemas['InteractionPreferences']>('/interactions/preferences', preferences),
  moderation: (targetId: string, cursor?: string, signal?: AbortSignal) =>
    validated(get<InteractionPage>(`/interactions/moderation/${targetId}?limit=20${cursor ? `&cursor=${cursor}` : ''}`, { signal }), page, 'moderation'),
};

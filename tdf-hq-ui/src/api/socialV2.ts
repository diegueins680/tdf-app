import { z } from 'zod';
import type { components } from './generated/social-v2';
import { get, post, put } from './client';

const id = z.number().int().positive().safe();
const revision = z.number().int().nonnegative().safe();
const relationship = z.object({
  partyId: id, revision, following: z.boolean(), requested: z.boolean(),
  incoming: z.boolean(), connected: z.boolean(), blocked: z.boolean(),
  muted: z.boolean(), dismissed: z.boolean(),
});
const preferences = z.object({ discoverable: z.boolean(), personalized: z.boolean(), revision });
const me = preferences.extend({ relationships: z.array(relationship.extend({ displayName: z.string() })).max(50) });
const cursor = z.string().regex(/^[1-9]\d{0,18}$/);
const page = z.object({
  items: z.array(z.object({
    postId: id, position: cursor, publishedAt: z.string(), createdAt: z.string(),
    title: z.string().nullable(), content: z.string(), authorId: id,
    authorName: z.string(), artistId: id,
  })).max(50), nextCursor: cursor.nullable(),
});
const discovery = z.object({
  personalized: z.boolean(), items: z.array(z.object({
    partyId: id, displayName: z.string(), reason: z.enum(['shared_interests', 'public_profile']), relationship,
  })).max(50),
});
export type SocialRelationship = components['schemas']['SocialV2State'];
export type SocialMe = components['schemas']['SocialV2Me'];
export type SocialOperation = 'request' | 'accept' | 'disconnect' | 'follow' | 'unfollow'
  | 'block' | 'unblock' | 'mute' | 'unmute' | 'dismiss' | 'undismiss';
export const SocialV2 = {
  me: async () => me.parse(await get<unknown>('/social/v2/me')),
  relationship: async (partyId: number) => relationship.parse(
    await get<unknown>(`/social/v2/relationships/${id.parse(partyId)}`)),
  command: async (partyId: number, operation: SocialOperation, expectedRevision: number, requestKey: string) =>
    relationship.parse(await post<unknown>(`/social/v2/relationships/${id.parse(partyId)}`, {
      operation, expectedRevision: revision.parse(expectedRevision), requestKey: z.string().min(1).max(80).parse(requestKey),
    })),
  preferences: async (value: Pick<SocialMe, 'discoverable' | 'personalized'>, expectedRevision: number) =>
    preferences.parse(await put<unknown>('/social/v2/preferences', {
      ...value, expectedRevision: revision.parse(expectedRevision),
    })),
  following: async (before?: string) => page.parse(await get<unknown>(
    `/social/v2/following?limit=20${before ? `&cursor=${cursor.parse(before)}` : ''}`)),
  discover: async () => discovery.parse(await get<unknown>('/social/v2/discover?limit=20')),
};

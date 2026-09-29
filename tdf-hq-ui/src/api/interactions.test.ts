import { jest } from '@jest/globals';
const get = jest.fn<() => Promise<unknown>>();
jest.unstable_mockModule('./client', () => ({ get, post: jest.fn(), put: jest.fn() }));
const { Interactions } = await import('./interactions');
const identity = { kind: 'event' as const, entityKey: '42' };

test.each([undefined, null, '<!doctype html><html>SPA fallback</html>', {},
  { id: 'target', reactions: null },
  { id: 'target', reactions: [null], commentCount: 0, rootCount: 0 },
  { id: 'target', reactions: [], commentCount: -1, rootCount: 0 },
])('rejects malformed summaries before caching or rendering: %j', async (body) => {
  get.mockResolvedValue(body);
  await expect(Interactions.summary(identity, true)).rejects.toThrow('Invalid interaction summary response');
});

test('preserves authoritative counts and supports recovery on the next request', async () => {
  const summary = { id: 'target', reactions: [{ id: 'like', label: 'Like', emoji: '👍', count: 2 }], commentCount: 3, rootCount: 1 };
  get.mockResolvedValueOnce('<html>unavailable</html>').mockResolvedValueOnce(summary);
  await expect(Interactions.summary(identity, false)).rejects.toThrow();
  await expect(Interactions.summary(identity, false)).resolves.toBe(summary);
});

const publication = { kind: 'event', key: '42', ownerId: 7, title: 'Concert', route: '/eventos/42', public: true,
  canManage: false, reactable: true, commentable: true, shareable: true };
const validComment = { id: 'comment', targetId: 'target', rootId: 'comment', parentId: null, depth: 0, version: 1,
  createdAt: '2026-09-29T03:00:00Z', editedAt: null, state: 'visible', body: 'Hello',
  author: { id: 7, displayName: 'Ana', avatarUrl: null }, canEdit: false, canDelete: false, mentions: [], replyCount: 0 };
const validContext = { target: publication, root: validComment, comment: validComment, parent: null, surrounding: [validComment] };
const reads = [
  ['comments', () => Interactions.comments(identity, true, 'newest'), { items: [validComment], nextCursor: null }],
  ['context', () => Interactions.context(identity, true, 'comment'), validContext],
  ['destination', () => Interactions.destination('comment', 'comment', true), { ...publication, targetId: 'target', commentId: 'comment', context: validContext }],
  ['reactors', () => Interactions.reactors(identity, true), { items: [{ author: validComment.author, reactionTypeId: 'fire' }], nextCursor: null }],
  ['blocked accounts', () => Interactions.blockedAccounts(), { items: [{ partyId: 8, displayName: 'Luis', blocked: true, version: 1 }], nextCursor: null }],
  ['reports', () => Interactions.reports(), { items: [{ ...validComment, openReports: 2, reportReasons: ['Spam'] }], nextCursor: null }],
  ['block', () => Interactions.blockState(8), { partyId: 8, blocked: false, version: 0 }],
  ['preferences', () => Interactions.preferences(), { reactions: true, comments: true, replies: false, mentions: true }],
  ['moderation', () => Interactions.moderation('target'), { items: [{ ...validComment, state: 'hidden', moderationBody: 'Original' }], nextCursor: null }],
] as const;
test.each(reads)('%s rejects malformed transport data and accepts a valid retry', async (_name, read, valid) => {
  for (const malformed of ['<!doctype html>fallback', null, {}, { items: [null], nextCursor: null }]) {
    get.mockResolvedValueOnce(malformed);
    await expect(read()).rejects.toThrow(/Invalid interaction/);
  }
  get.mockResolvedValueOnce(valid);
  await expect(read()).resolves.toBe(valid);
});
test.each([
  { ...validComment, id: null }, { ...validComment, mentions: [null] },
  { ...validComment, author: { id: 7 } }, { ...validComment, createdAt: 'bad timestamp' },
  { ...validComment, reportReasons: [null] }, { ...validComment, legacyPresentation: { mediaUrls: [null] } },
])('rejects malformed nested comment data before rendering: %j', async (invalid) => {
  get.mockResolvedValueOnce({ items: [invalid], nextCursor: null });
  await expect(Interactions.comments(identity, true, 'newest')).rejects.toThrow('Invalid interaction comments');
});
test('accepts deleted-author tombstones and rejects broken deep-link ancestors', async () => {
  const tombstone = { ...validComment, state: 'deleted', author: null, body: '', canEdit: null, canDelete: null };
  get.mockResolvedValueOnce({ items: [tombstone], nextCursor: null });
  await expect(Interactions.comments(identity, false, 'oldest')).resolves.toEqual({ items: [tombstone], nextCursor: null });
  get.mockResolvedValueOnce({ ...validContext, root: null });
  await expect(Interactions.context(identity, true, 'comment')).rejects.toThrow('Invalid interaction context');
});

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

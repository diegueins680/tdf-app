import { jest } from '@jest/globals';
const get = jest.fn<() => Promise<unknown>>();
const post = jest.fn<() => Promise<unknown>>();
const put = jest.fn<() => Promise<unknown>>();
jest.unstable_mockModule('./client', () => ({ get, post, put }));
const { SocialV2 } = await import('./socialV2');
const state = { partyId: 2, revision: 1, following: false, requested: true, incoming: false,
  connected: false, blocked: false, muted: false, dismissed: false };
beforeEach(() => { jest.clearAllMocks(); });
it('sends only own authenticated intent and explicit revision/request identity', async () => {
  post.mockResolvedValue(state);
  expect(await SocialV2.command(2, 'request', 0, 'retry-key')).toEqual(state);
  expect(post).toHaveBeenCalledWith('/social/v2/relationships/2', {
    operation: 'request', expectedRevision: 0, requestKey: 'retry-key',
  });
});
it('rejects unsafe IDs and cursors before transport', async () => {
  await expect(SocialV2.relationship(Number.MAX_SAFE_INTEGER + 1)).rejects.toThrow();
  await expect(SocialV2.following('1&actor=3')).rejects.toThrow();
  expect(get).not.toHaveBeenCalled();
});
it('does not round the bigint cursor through JavaScript numbers', async () => {
  get.mockResolvedValue({ items: [], nextCursor: null });
  await SocialV2.following('9007199254740993');
  expect(get).toHaveBeenCalledWith('/social/v2/following?limit=20&cursor=9007199254740993');
});
it('rejects a malformed server permission state', async () => {
  get.mockResolvedValue({ ...state, connected: 'true' });
  await expect(SocialV2.relationship(2)).rejects.toThrow();
});
it('rejects an unknown explanation instead of displaying arbitrary server text', async () => {
  get.mockResolvedValue({ personalized: true, items: [{ partyId: 2, displayName: 'A', reason: 'private_purchase', relationship: state }] });
  await expect(SocialV2.discover()).rejects.toThrow();
});
it('sends opt-out independently of profile discoverability', async () => {
  put.mockResolvedValue({ discoverable: true, personalized: false, revision: 3 });
  await SocialV2.preferences({ discoverable: true, personalized: false }, 2);
  expect(put).toHaveBeenCalledWith('/social/v2/preferences', {
    discoverable: true, personalized: false, expectedRevision: 2,
  });
});

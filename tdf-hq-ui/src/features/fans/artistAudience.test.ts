import { jest } from '@jest/globals';
import { QueryClient } from '@tanstack/react-query';
const follow = jest.fn(async () => undefined), unfollow = jest.fn(async () => undefined);
jest.unstable_mockModule('../../api/fans', () => ({ Fans: { follow, unfollow } }));
const { invalidateArtistAudience, setArtistFollowing } = await import('./artistAudience');
beforeEach(() => { follow.mockClear(); unfollow.mockClear(); });
it('writes once through the existing authorized subscription boundary and rejects stale session intent', async () => {
  await setArtistFollowing({ artistId: 17, ownsSession: () => true }, true);
  expect(follow).toHaveBeenCalledTimes(1); expect(unfollow).not.toHaveBeenCalled();
  await expect(setArtistFollowing({ artistId: 17, ownsSession: () => false }, false)).rejects.toThrow();
  expect(unfollow).not.toHaveBeenCalled();
});
it('invalidates counts, list and profile aliases for the target without invalidating another account', async () => {
  const client = new QueryClient();
  const targetKeys = [['fan-follows', 42], ['artist-fans', 17, 1, 5], ['public-artist', 'slug'], ['public-artist', '17'], ['social-v2', 42, 'me']];
  for (const key of targetKeys) client.setQueryData(key, key[0] === 'public-artist' ? { apArtistId: 17 } : []);
  client.setQueryData(['fan-follows', 43], []);
  client.setQueryData(['public-artist', 'other'], { apArtistId: 18 });
  await invalidateArtistAudience(client, 42, 17);
  expect(targetKeys.every((key) => client.getQueryState(key)?.isInvalidated)).toBe(true);
  expect(client.getQueryState(['fan-follows', 43])?.isInvalidated).toBe(false);
  expect(client.getQueryState(['public-artist', 'other'])?.isInvalidated).toBe(false);
  client.clear();
});

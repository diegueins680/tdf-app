import { QueryClient } from '@tanstack/react-query';
import { invalidateArtistCatalog } from './catalogQueries';
it('refreshes the same release in profile, followed feed, label and music catalog without invalidating an unrelated feed', async () => {
  const client = new QueryClient();
  const keys = [['artist-release-catalog', 17], ['fan-release-feed', [17, 18], true], ['admin', 'artist-releases'], ['music-release-catalog', '', 1]];
  for (const key of keys) client.setQueryData(key, []);
  client.setQueryData(['fan-release-feed', [19], true], []);
  await invalidateArtistCatalog(client, 17);
  expect(keys.every((key) => client.getQueryState(key)?.isInvalidated)).toBe(true);
  expect(client.getQueryState(['fan-release-feed', [19], true])?.isInvalidated).toBe(false);
  client.clear();
});

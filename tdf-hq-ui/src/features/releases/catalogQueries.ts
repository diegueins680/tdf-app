import type { QueryClient } from '@tanstack/react-query';
import { canonicalArtistPartyId } from '../../utils/artistIdentity';

/** Refresh all consumers after a committed catalog change; never infer publication. */
export async function invalidateArtistCatalog(client: QueryClient, artistId: number): Promise<void> {
  if (canonicalArtistPartyId(artistId) === null) return;
  await Promise.all([
    client.invalidateQueries({ queryKey: ['artist-release-catalog', artistId] }),
    client.invalidateQueries({ queryKey: ['fan-release-feed'], predicate: (query) =>
      Array.isArray(query.queryKey[1]) && query.queryKey[1].includes(artistId) }),
    client.invalidateQueries({ queryKey: ['admin', 'artist-releases'] }),
    client.invalidateQueries({ queryKey: ['music-release-catalog'] }),
  ]);
}

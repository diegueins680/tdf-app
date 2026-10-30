import type { QueryClient } from '@tanstack/react-query';
import { Fans } from '../../api/fans';
import { canonicalArtistPartyId } from '../../utils/artistIdentity';
import type { FollowReadScope } from './reconcileFollowState';

/** Keep artist subscription writes on the existing authorized server boundary. */
export async function setArtistFollowing(scope: Pick<FollowReadScope, 'artistId' | 'ownsSession'>, following: boolean) {
  if (canonicalArtistPartyId(scope.artistId) === null || !scope.ownsSession()) throw new Error('Follow context changed');
  if (following) await Fans.follow(scope.artistId);
  else await Fans.unfollow(scope.artistId);
}

/** Refresh every projection of the same audience change, including cached slug aliases. */
export async function invalidateArtistAudience(client: QueryClient, viewerId: number, artistId: number): Promise<void> {
  if (canonicalArtistPartyId(viewerId) === null || canonicalArtistPartyId(artistId) === null) return;
  await Promise.all([
    client.invalidateQueries({ queryKey: ['fan-follows', viewerId] }),
    client.invalidateQueries({ queryKey: ['fan-artists'] }),
    client.invalidateQueries({ queryKey: ['artist-fans', artistId] }),
    client.invalidateQueries({ queryKey: ['public-artist'], predicate: (query) => {
      const profile = query.state.data as { apArtistId?: unknown } | undefined;
      return canonicalArtistPartyId(profile?.apArtistId) === artistId;
    } }),
    client.invalidateQueries({ queryKey: ['social-v2', viewerId] }),
  ]);
}

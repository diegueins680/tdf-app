import type { QueryClient } from '@tanstack/react-query';
import { Fans } from '../../api/fans';

export interface FollowReadScope {
  viewerId: number;
  artistId: number;
  ownsSession: () => boolean;
}

/** Read current server state after an ambiguous write. Never replay the write. */
export async function reconcileFollowState(
  client: QueryClient,
  scope: FollowReadScope,
): Promise<boolean | null> {
  if (!Number.isSafeInteger(scope.viewerId) || scope.viewerId <= 0
    || !Number.isSafeInteger(scope.artistId) || scope.artistId <= 0
    || !scope.ownsSession()) return null;
  const queryKey = ['fan-follows', scope.viewerId];
  try {
    await client.cancelQueries({ queryKey, exact: true });
    // Invalidate without a refetch so this explicit read cannot use a stale
    // pre-write result (including when the local clock moves backwards).
    await client.invalidateQueries({ queryKey, exact: true, refetchType: 'none' });
    if (!scope.ownsSession()) return null;
    const follows = await client.fetchQuery({
      queryKey,
      staleTime: 0,
      retry: false,
      queryFn: async () => {
        if (!scope.ownsSession()) throw new Error('Follow read context changed');
        const rows = await Fans.listFollows();
        if (!scope.ownsSession()) throw new Error('Follow read context changed');
        if (!Array.isArray(rows) || rows.some(row => !row
          || !Number.isSafeInteger(row.ffArtistId) || row.ffArtistId <= 0)) {
          throw new Error('Follow state was not confirmed');
        }
        return rows;
      },
    });
    return scope.ownsSession()
      ? follows.some(follow => follow.ffArtistId === scope.artistId)
      : null;
  } catch {
    return null;
  }
}

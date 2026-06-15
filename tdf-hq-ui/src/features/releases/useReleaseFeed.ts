import { useMemo } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Fans } from '../../api/fans';
import { compareReleaseDateValues } from '../../utils/releaseDate';
import type { ReleaseFeedItem } from './ReleasePlayerActions';

export interface ReleaseFeedArtist {
  id: number;
  name: string;
  spotifyUrl?: string | null;
  youtubeUrl?: string | null;
}

export function useReleaseFeed({
  enabled,
  targetArtists,
}: {
  enabled: boolean;
  targetArtists: ReleaseFeedArtist[];
}) {
  const releaseArtistIds = useMemo(
    () => targetArtists.map((artist) => artist.id).sort((a, b) => a - b),
    [targetArtists],
  );
  const query = useQuery({
    queryKey: ['fan-release-feed', releaseArtistIds, enabled],
    enabled: enabled && targetArtists.length > 0,
    queryFn: async () => {
      const perArtist = await Promise.all(
        targetArtists.map(async (artist) => {
          const releases = await Fans.getReleases(artist.id);
          return releases.map((release) => ({
            ...release,
            artistName: artist.name,
          }));
        }),
      );
      const flat = perArtist.flat() as ReleaseFeedItem[];
      return flat.sort((a, b) => compareReleaseDateValues(a.arReleaseDate, b.arReleaseDate, 'desc'));
    },
  });
  const releaseFeed = useMemo(() => query.data ?? [], [query.data]);

  return {
    hasReleaseTargets: targetArtists.length > 0,
    releaseArtistIds,
    releaseFeed,
    releaseFeedQuery: query,
  };
}

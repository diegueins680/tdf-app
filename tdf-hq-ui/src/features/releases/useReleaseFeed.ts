import { useMemo } from 'react';
import { useQuery } from '@tanstack/react-query';
import { loadArtistCatalogs } from '../../api/artistCatalog';
import type { MusicPublicReleaseSummary } from '../../api/musicReleases';
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
    retry: false,
    queryFn: ({ signal }) => loadArtistCatalogs(releaseArtistIds, signal),
  });
  const releaseFeed = useMemo(() => {
    const names = new Map(targetArtists.map((artist) => [artist.id, artist.name]));
    return (query.data ?? []).flatMap((catalog) => catalog.entries.flatMap((entry) => entry.source === 'legacy'
      ? [{ ...entry.release, artistName: names.get(catalog.artistId) ?? 'Artista TDF' }] : []))
      .sort((a, b) => compareReleaseDateValues(a.arReleaseDate, b.arReleaseDate, 'desc')) as ReleaseFeedItem[];
  }, [query.data, targetArtists]);
  const canonicalReleaseFeed = useMemo(() => {
    const unique = new Map<string, MusicPublicReleaseSummary>();
    (query.data ?? []).forEach((catalog) => catalog.entries.forEach((entry) => {
      if (entry.source === 'canonical') unique.set(entry.release.id, entry.release);
    }));
    return [...unique.values()].sort((a, b) => Date.parse(b.publishedAt) - Date.parse(a.publishedAt));
  }, [query.data]);
  const partialCatalog = query.data?.some((catalog) => catalog.unavailableSources.length > 0) ?? false;

  return {
    hasReleaseTargets: targetArtists.length > 0,
    releaseArtistIds,
    releaseFeed,
    canonicalReleaseFeed,
    partialCatalog,
    releaseFeedQuery: query,
  };
}

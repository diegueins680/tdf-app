import type { ArtistReleaseDTO } from '../../api/types';
import type { RecordsReleaseDTO } from '../../api/records';
import { extractYoutubeVideoId, normalizeSpotifyEmbed } from '../../utils/media';

type ArtworkRelease = ArtistReleaseDTO & {
  artistName: string;
  artistHeroImageUrl?: string | null;
};

const normalizeName = (value: string) => value.normalize('NFD')
  .replace(/\p{M}/gu, '').toLowerCase().replace(/[^\p{L}\p{N}]+/gu, ' ').trim();

const spotifyIdentity = (value?: string | null) => {
  const embed = normalizeSpotifyEmbed(value);
  return embed ? new URL(embed).pathname : null;
};

/** Presentation fallback only: never persists a guessed catalog relationship. */
export function getReleaseArtworkSources(release: ArtworkRelease, catalog: RecordsReleaseDTO[]): string[] {
  const spotify = spotifyIdentity(release.arSpotifyUrl);
  const youtube = extractYoutubeVideoId(release.arYoutubeUrl);
  const linked = catalog.filter((entry) => entry.resources.some((resource) =>
    (spotify !== null && resource.providerCode === 'spotify' && spotifyIdentity(resource.url) === spotify)
    || (youtube !== null && resource.providerCode === 'youtube' && extractYoutubeVideoId(resource.url) === youtube),
  ));
  // Legacy rows often have no platform links. Require a unique title AND credited
  // artist match; never substitute a different recording for an existing link.
  const hasPlatformLink = Boolean(release.arSpotifyUrl?.trim()) || Boolean(release.arYoutubeUrl?.trim());
  const candidates = hasPlatformLink ? linked : catalog.filter((entry) =>
    normalizeName(entry.title) === normalizeName(release.arTitle)
    && entry.contributors.some((credit) =>
      [credit.name, ...(credit.kind === 'credited-ensemble' ? credit.name.split(',') : [])]
        .some((name) => normalizeName(name) === normalizeName(release.artistName)),
    ),
  );
  const match = candidates.length === 1 ? candidates[0] : undefined;
  const covers = match?.resources
    .filter((resource) => resource.kind === 'audio-track')
    .sort((a, b) => Number(b.primary) - Number(a.primary) || a.sortOrder - b.sortOrder)
    .map((resource) => resource.thumbnailUrl) ?? [];
  return [...new Set([release.arCoverImageUrl, ...covers, release.artistHeroImageUrl]
    .map((url) => url?.trim()).filter((url): url is string => Boolean(url)))];
}

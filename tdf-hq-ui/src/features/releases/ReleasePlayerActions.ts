import type { ArtistReleaseDTO, ArtistReleaseUpsert } from '../../api/types';
import { normalizeStreamingSource } from '../../utils/media';
import { PLAYER_LOAD_TRACK_EVENT, type LoadPlayerTrackDetail } from '../../player/types';

export interface ReleaseFeedItem extends ArtistReleaseDTO {
  artistName: string;
}

export interface ReleaseStreamingFallback {
  spotify?: string | null;
  youtube?: string | null;
}

export interface ExternalReleasePlayback {
  kind: 'provider';
  provider: 'Spotify' | 'YouTube' | 'Google Drive';
  context: 'release' | 'artist';
  url: string;
}

export type ReleasePlaybackAction = { kind: 'native'; url: string } | ExternalReleasePlayback;

const providerHost = (host: string, domain: string) => host === domain || host.endsWith(`.${domain}`);

function classifyPlaybackUrl(raw: string | null | undefined, context: 'release' | 'artist'): ReleasePlaybackAction | null {
  const value = raw?.trim();
  if (!value) return null;
  let parsed: URL;
  const hasScheme = /^[a-zA-Z][a-zA-Z\d+.-]*:/.test(value);
  try {
    parsed = value.startsWith('/') ? new URL(value, 'https://tdfrecords.invalid')
      : new URL(hasScheme ? value : `https://${value}`);
  } catch { return null; }
  if (!['http:', 'https:'].includes(parsed.protocol) || parsed.username || parsed.password) return null;
  const host = parsed.hostname.toLowerCase().replace(/\.$/, '');
  // Reuse the established provider inference, including localized Spotify and
  // YouTube watch/shorts/live URLs. Non-embeddable profile/short-link pages still
  // require an external provider action rather than a native audio source.
  const normalized = normalizeStreamingSource({ url: parsed.href });
  const provider = normalized?.provider === 'spotify' || providerHost(host, 'spotify.com') || ['spotify.link', 'spoti.fi'].includes(host) ? 'Spotify'
    : normalized?.provider === 'youtube' || providerHost(host, 'youtube.com') || providerHost(host, 'youtu.be') || providerHost(host, 'youtube-nocookie.com') ? 'YouTube'
      : host === 'drive.google.com' && !(parsed.pathname === '/uc' && parsed.searchParams.get('id')) ? 'Google Drive' : null;
  if (provider) return { kind: 'provider', provider, context, url: parsed.href };
  // An artist profile fallback does not identify this selected recording.
  if (context === 'artist') return null;
  return { kind: 'native', url: hasScheme || value.startsWith('/') ? value : parsed.href };
}

export function resolveReleasePlaybackAction(
  release: ArtistReleaseDTO,
  releaseAudioMap: Record<number, string>,
  fallback?: ReleaseStreamingFallback,
): ReleasePlaybackAction | null {
  assertPositiveSafeInteger(release.arReleaseId, 'release.arReleaseId');
  for (const url of [releaseAudioMap[release.arReleaseId], release.arSpotifyUrl, release.arYoutubeUrl]) {
    const action = classifyPlaybackUrl(url, 'release');
    if (action) return action;
  }
  for (const url of [fallback?.spotify, fallback?.youtube]) {
    const action = classifyPlaybackUrl(url, 'artist');
    if (action) return action;
  }
  return null;
}

function assertPositiveSafeInteger(value: number, fieldName: string): void {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new RangeError(`${fieldName} must be a positive safe integer.`);
  }
}

function assertNonBlank(value: string, fieldName: string): void {
  if (!value.trim()) {
    throw new Error(`${fieldName} must be non-empty.`);
  }
}

function normalizeNonBlank(value: string, fieldName: string): string {
  const normalized = value.trim();
  if (!normalized) {
    throw new Error(`${fieldName} must be non-empty.`);
  }
  return normalized;
}

function assertReleaseIdentity(release: Pick<ArtistReleaseDTO, 'arReleaseId' | 'arTitle'>): void {
  assertPositiveSafeInteger(release.arReleaseId, 'release.arReleaseId');
  assertNonBlank(release.arTitle, 'release.arTitle');
}

/**
 * Contract:
 * - Precondition: `release.arArtistId` identifies a persisted artist and `release.arTitle` is non-empty.
 * - Precondition: `streamUrl` is non-empty after trimming.
 * - Postcondition: the returned upsert preserves release metadata and stores the normalized stream in the
 *   provider field selected by YouTube URL detection.
 */
export function buildReleaseStreamUpdatePayload(
  release: ArtistReleaseDTO,
  streamUrl: string,
): ArtistReleaseUpsert {
  assertPositiveSafeInteger(release.arArtistId, 'release.arArtistId');
  assertNonBlank(release.arTitle, 'release.arTitle');
  const normalized = normalizeNonBlank(streamUrl, 'streamUrl');
  const action = classifyPlaybackUrl(normalized, 'release');
  if (!action) throw new Error('streamUrl must be a safe HTTP(S) media or provider URL');
  const isYoutube = action.kind === 'provider' && action.provider === 'YouTube';

  return {
    aruArtistId: release.arArtistId,
    aruTitle: release.arTitle,
    aruReleaseDate: release.arReleaseDate ?? null,
    aruDescription: release.arDescription ?? null,
    aruCoverImageUrl: release.arCoverImageUrl ?? null,
    aruSpotifyUrl: isYoutube ? release.arSpotifyUrl ?? null : normalized,
    aruYoutubeUrl: isYoutube ? normalized : release.arYoutubeUrl ?? null,
  };
}

/**
 * Contract:
 * - Precondition: `release.arReleaseId` identifies a persisted release.
 * - Invariant: `releaseAudioMap` keys are release IDs; URLs are validated before selection.
 * - Postcondition: returns the first available URL by precedence: uploaded audio, release Spotify,
 *   release YouTube, contextual artist provider fallback, or `null`.
 */
export function resolveReleaseAudioUrl(
  release: ArtistReleaseDTO,
  releaseAudioMap: Record<number, string>,
  fallback?: ReleaseStreamingFallback,
): string | null {
  assertPositiveSafeInteger(release.arReleaseId, 'release.arReleaseId');

  return resolveReleasePlaybackAction(release, releaseAudioMap, fallback)?.url ?? null;

}

/**
 * Contract:
 * - Precondition: `release.arReleaseId` identifies a persisted release.
 * - Invariant: cached uploaded audio, when present, occupies the primary playback slot.
 * - Postcondition: returns a two-slot playback contract with nulls for unavailable providers.
 */
export function getReleasePlaybackUrls(
  release: ArtistReleaseDTO,
  releaseAudioMap: Record<number, string>,
  fallback?: ReleaseStreamingFallback,
): { spotifyUrl: string | null; youtubeUrl: string | null } {
  assertPositiveSafeInteger(release.arReleaseId, 'release.arReleaseId');

  const releaseUrl = (url: string | null | undefined) => classifyPlaybackUrl(url, 'release')?.url ?? null;
  const artistUrl = (url: string | null | undefined) => classifyPlaybackUrl(url, 'artist')?.url ?? null;
  return {
    spotifyUrl: releaseUrl(releaseAudioMap[release.arReleaseId]) ?? releaseUrl(release.arSpotifyUrl) ?? artistUrl(fallback?.spotify),
    youtubeUrl: releaseUrl(release.arYoutubeUrl) ?? artistUrl(fallback?.youtube),
  };
}

/**
 * Contract:
 * - Precondition: `release.arReleaseId` identifies a persisted release and `release.arTitle` is non-empty.
 * - Precondition: `streamUrl` is non-empty after trimming.
 * - Precondition: the URL identifies direct media, rather than a known provider page.
 * - Postcondition: dispatches exactly one global-player event with a stable legacy release/track ID.
 */
export function dispatchReleaseToRadio(
  release: ArtistReleaseDTO & { artistName?: string },
  streamUrl: string,
  target: Pick<Window, 'dispatchEvent'> = window,
): void {
  assertReleaseIdentity(release);
  const normalizedStreamUrl = normalizeNonBlank(streamUrl, 'streamUrl');
  const action = classifyPlaybackUrl(normalizedStreamUrl, 'release');
  if (action?.kind !== 'native') throw new Error('Native playback requires a direct HTTP(S) media URL; open provider pages on their platform.');

  target.dispatchEvent(
    new CustomEvent(PLAYER_LOAD_TRACK_EVENT, {
      detail: {
        track: {
          id: `legacy-release-${release.arReleaseId}`,
          releaseId: `legacy-release-${release.arReleaseId}`,
          title: release.arTitle,
          artist: release.artistName?.trim() ? release.artistName.trim() : 'Artista TDF',
          artworkUrl: release.arCoverImageUrl,
          sources: [{ url: action.url, quality: 'high', mediaType: 'audio/mpeg' }],
        },
        autoplay: true,
      } satisfies LoadPlayerTrackDetail,
    }),
  );
}

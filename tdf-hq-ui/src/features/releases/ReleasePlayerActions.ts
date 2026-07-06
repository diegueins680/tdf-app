import type { ArtistReleaseDTO, ArtistReleaseUpsert } from '../../api/types';

export interface ReleaseFeedItem extends ArtistReleaseDTO {
  artistName: string;
}

export interface ReleaseStreamingFallback {
  spotify?: string | null;
  youtube?: string | null;
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
  const isYoutube = /youtu\.?be|youtube\.com/.test(normalized.toLowerCase());

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
 * - Invariant: `releaseAudioMap` keys are release IDs and non-null values are playable URLs.
 * - Postcondition: returns the first available URL by precedence: uploaded audio, release Spotify,
 *   release YouTube, artist Spotify fallback, artist YouTube fallback, or `null`.
 */
export function resolveReleaseAudioUrl(
  release: ArtistReleaseDTO,
  releaseAudioMap: Record<number, string>,
  fallback?: ReleaseStreamingFallback,
): string | null {
  assertPositiveSafeInteger(release.arReleaseId, 'release.arReleaseId');

  return (
    releaseAudioMap[release.arReleaseId] ??
    release.arSpotifyUrl ??
    release.arYoutubeUrl ??
    fallback?.spotify ??
    fallback?.youtube ??
    null
  );
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

  return {
    spotifyUrl: releaseAudioMap[release.arReleaseId] ?? release.arSpotifyUrl ?? fallback?.spotify ?? null,
    youtubeUrl: release.arYoutubeUrl ?? fallback?.youtube ?? null,
  };
}

/**
 * Contract:
 * - Precondition: `release.arReleaseId` identifies a persisted release and `release.arTitle` is non-empty.
 * - Precondition: `streamUrl` is non-empty after trimming.
 * - Postcondition: dispatches exactly one `tdf-radio-load-stream` event with a stable `release-{id}` station ID.
 */
export function dispatchReleaseToRadio(
  release: ArtistReleaseDTO,
  streamUrl: string,
  target: Pick<Window, 'dispatchEvent'> = window,
): void {
  assertReleaseIdentity(release);
  const normalizedStreamUrl = normalizeNonBlank(streamUrl, 'streamUrl');

  target.dispatchEvent(
    new CustomEvent('tdf-radio-load-stream', {
      detail: {
        streamUrl: normalizedStreamUrl,
        stationName: release.arTitle,
        stationId: `release-${release.arReleaseId}`,
      },
    }),
  );
}

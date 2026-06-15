import type { ArtistReleaseDTO, ArtistReleaseUpsert } from '../../api/types';

export interface ReleaseFeedItem extends ArtistReleaseDTO {
  artistName: string;
}

export interface ReleaseStreamingFallback {
  spotify?: string | null;
  youtube?: string | null;
}

export function buildReleaseStreamUpdatePayload(
  release: ArtistReleaseDTO,
  streamUrl: string,
): ArtistReleaseUpsert {
  const normalized = streamUrl.trim();
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

export function resolveReleaseAudioUrl(
  release: ArtistReleaseDTO,
  releaseAudioMap: Record<number, string>,
  fallback?: ReleaseStreamingFallback,
): string | null {
  return (
    releaseAudioMap[release.arReleaseId] ??
    release.arSpotifyUrl ??
    release.arYoutubeUrl ??
    fallback?.spotify ??
    fallback?.youtube ??
    null
  );
}

export function getReleasePlaybackUrls(
  release: ArtistReleaseDTO,
  releaseAudioMap: Record<number, string>,
  fallback?: ReleaseStreamingFallback,
): { spotifyUrl: string | null; youtubeUrl: string | null } {
  return {
    spotifyUrl: releaseAudioMap[release.arReleaseId] ?? release.arSpotifyUrl ?? fallback?.spotify ?? null,
    youtubeUrl: release.arYoutubeUrl ?? fallback?.youtube ?? null,
  };
}

export function dispatchReleaseToRadio(
  release: ArtistReleaseDTO,
  streamUrl: string,
  target: Pick<Window, 'dispatchEvent'> = window,
): void {
  target.dispatchEvent(
    new CustomEvent('tdf-radio-load-stream', {
      detail: {
        streamUrl,
        stationName: release.arTitle,
        stationId: `release-${release.arReleaseId}`,
      },
    }),
  );
}

import type { MusicPublicRelease } from '../api/musicReleases';
import { musicSourceQuality } from './sourceMetadata';
import type { PlayerTrack } from './types';

type PublicTrack = Pick<MusicPublicRelease['tracks'][number],
  'trackId' | 'recordingId' | 'title' | 'displayArtist' | 'sources'> & { durationMs: number | null };

// Public metadata identifies the selected recording. Only the global engine
// resolves its ordered asset references, after an explicit listening gesture.
export function publicMusicTrack(track: PublicTrack & {
  releaseId?: string;
  releaseVersionId?: string;
  artworkUrl?: string | null;
}): PlayerTrack {
  return {
    id: track.trackId,
    recordingId: track.recordingId,
    releaseId: track.releaseId,
    releaseVersionId: track.releaseVersionId,
    title: track.title,
    artist: track.displayArtist,
    artworkUrl: track.artworkUrl ?? null,
    durationMs: track.durationMs,
    // The engine applies the cap only to the source actually resolved as preview.
    previewEndMs: track.sources.some((source) => source.role === 'preview_audio') ? 30000 : null,
    loudnessLufs: track.sources.map((source) => source.technicalMetadata['loudness_lufs'])
      .find((value): value is number => typeof value === 'number' && Number.isFinite(value)) ?? null,
    sources: track.sources.map((source) => ({
      url: '',
      assetId: source.assetId,
      quality: musicSourceQuality(source.technicalMetadata, source.mediaType),
      mediaType: source.mediaType,
      preview: source.role === 'preview_audio',
    })),
  };
}

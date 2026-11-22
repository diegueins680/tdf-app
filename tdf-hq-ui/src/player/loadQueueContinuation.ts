import { Fans } from '../api/fans';
import { musicReleases, type MusicPublicRelease, type MusicPublicReleaseSummary } from '../api/musicReleases';
import type { ArtistRecommendationDTO } from '../api/types';
import { publicMusicTrack } from './publicMusicTrack';
import {
  CONTINUATION_ARTIST_LIMIT,
  CONTINUATION_TRACK_LIMIT,
  planQueueContinuation,
  type ContinuationCandidate,
} from './queueContinuation';
import type { PlayerTrack } from './types';

/** The reads a continuation needs; injectable so tests do not touch the network. */
export interface ContinuationApi {
  listRecommendations: (limit: number) => Promise<ArtistRecommendationDTO[]>;
  listPublicReleases: (artistPartyId: number, signal?: AbortSignal) => Promise<MusicPublicReleaseSummary[]>;
  getPublicRelease: (slug: string) => Promise<MusicPublicRelease>;
}

const defaultApi: ContinuationApi = {
  listRecommendations: (limit) => Fans.listRecommendations(limit),
  listPublicReleases: (artistPartyId, signal) => musicReleases.listPublic('', artistPartyId, { limit: 1, signal }),
  getPublicRelease: (slug) => musicReleases.getPublic(slug),
};

const releaseTracks = (release: MusicPublicRelease): PlayerTrack[] =>
  release.tracks
    .filter((track) => track.sources.length > 0)
    .map((track) => publicMusicTrack({ ...track, releaseId: release.id, releaseVersionId: release.versionId }));

/**
 * Load the continuation for a queue that ended (MEDIA-MUSIC-CONTINUATION-001).
 * Artists are read in the server's order, one at a time, and reading stops as
 * soon as the plan is full. Any failure yields a shorter or empty plan.
 */
export async function loadQueueContinuation(
  queueTracks: readonly PlayerTrack[],
  options: { api?: ContinuationApi; signal?: AbortSignal; limit?: number } = {},
): Promise<PlayerTrack[]> {
  const api = options.api ?? defaultApi;
  const limit = options.limit ?? CONTINUATION_TRACK_LIMIT;
  let recommendations: ArtistRecommendationDTO[];
  try {
    recommendations = await api.listRecommendations(CONTINUATION_ARTIST_LIMIT);
  } catch {
    return [];
  }
  if (!Array.isArray(recommendations)) return [];
  const candidates: ContinuationCandidate[] = [];
  let planned: PlayerTrack[] = [];
  for (const recommendation of recommendations.slice(0, CONTINUATION_ARTIST_LIMIT)) {
    if (options.signal?.aborted) return [];
    if (planned.length >= limit) break;
    const artistId = recommendation?.arecArtistId;
    if (!Number.isSafeInteger(artistId) || artistId <= 0) continue;
    try {
      const [summary] = await api.listPublicReleases(artistId, options.signal);
      if (summary?.artistPartyId !== artistId) continue;
      const release = await api.getPublicRelease(summary.slug);
      candidates.push({ artistId, tracks: releaseTracks(release) });
    } catch {
      continue;
    }
    planned = planQueueContinuation(queueTracks, candidates, limit);
  }
  return options.signal?.aborted ? [] : planned;
}

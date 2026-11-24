import { nextQueueIndex, type QueueState } from './queue';
import type { PlayerTrack } from './types';

// MEDIA-MUSIC-CONTINUATION-001 (formal/system/queue-continuation.md).

/** Artists requested from GET /fans/me/recommendations for one continuation. */
export const CONTINUATION_ARTIST_LIMIT = 12;
/** Tracks appended by one continuation. */
export const CONTINUATION_TRACK_LIMIT = 5;

/** One recommended artist with the tracks of its newest public release, in release order. */
export interface ContinuationCandidate {
  artistId: number;
  tracks: PlayerTrack[];
}

/** The recording a track plays; legacy tracks without a recording are identified by their id. */
export const continuationTrackKey = (track: PlayerTrack): string => track.recordingId ?? track.id;

/**
 * The tracks to append when the queue ends. Candidates are in the order the
 * server ranked their artists. Total: every input yields a list.
 *
 * - the artists of the result are a subsequence of the candidate order;
 * - at most one track per artist and at most `limit` tracks;
 * - no result recording is in `queueTracks` or appears twice.
 */
export function planQueueContinuation(
  queueTracks: readonly PlayerTrack[],
  candidates: readonly ContinuationCandidate[],
  limit: number = CONTINUATION_TRACK_LIMIT,
): PlayerTrack[] {
  const bound = Number.isFinite(limit) ? Math.max(0, Math.floor(limit)) : 0;
  const taken = new Set(queueTracks.map(continuationTrackKey));
  const takenIds = new Set(queueTracks.map((track) => track.id));
  const seenArtists = new Set<number>();
  const planned: PlayerTrack[] = [];
  for (const candidate of candidates) {
    if (planned.length >= bound) break;
    if (seenArtists.has(candidate.artistId)) continue;
    seenArtists.add(candidate.artistId);
    const track = candidate.tracks.find((item) =>
      item.sources.length > 0 && !taken.has(continuationTrackKey(item)) && !takenIds.has(item.id));
    if (!track) continue;
    taken.add(continuationTrackKey(track));
    takenIds.add(track.id);
    planned.push(track);
  }
  return planned;
}

/**
 * Whether reaching the end of this queue may start a continuation: the feature
 * is on, a listener is signed in, and nothing else would play next (which
 * implies repeat is off and the current track is the last one).
 */
export function shouldContinueQueue(
  queue: QueueState,
  listenerPartyId: number | null | undefined,
  enabled: boolean,
): boolean {
  return enabled
    && typeof listenerPartyId === 'number' && Number.isSafeInteger(listenerPartyId) && listenerPartyId > 0
    && queue.tracks.length > 0
    && queue.currentIndex >= 0
    && queue.repeat === 'off'
    && nextQueueIndex(queue) < 0;
}

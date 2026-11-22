import { EMPTY_QUEUE, type QueueState } from './queue';
import {
  CONTINUATION_TRACK_LIMIT,
  continuationTrackKey,
  planQueueContinuation,
  shouldContinueQueue,
  type ContinuationCandidate,
} from './queueContinuation';
import { loadQueueContinuation, type ContinuationApi } from './loadQueueContinuation';
import { isQueueContinuationEnabled } from '../config/queueContinuationRollout';
import type { PlayerTrack } from './types';
import type { MusicPublicRelease, MusicPublicReleaseSummary } from '../api/musicReleases';
import type { ArtistRecommendationDTO } from '../api/types';

// MEDIA-MUSIC-CONTINUATION-001. The web workspace has no property-testing
// library; the random cases below come from a small seeded generator, so a
// failure is reproducible from its case number.

const track = (id: string, recordingId?: string, playable = true): PlayerTrack => ({
  id,
  recordingId,
  title: id,
  artist: 'Synthetic Artist',
  sources: playable ? [{ url: '', assetId: `asset-${id}`, quality: 'high', mediaType: 'audio/mp4' }] : [],
});

const mulberry32 = (seed: number) => () => {
  let t = (seed += 0x6d2b79f5);
  t = Math.imul(t ^ (t >>> 15), t | 1);
  t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
  return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
};

interface Case { queue: PlayerTrack[]; candidates: ContinuationCandidate[]; limit: number }

// Small id spaces on purpose: collisions between queue and candidates, repeated
// artists and repeated recordings are the interesting cases.
const generate = (seed: number): Case => {
  const random = mulberry32(seed);
  const pick = (n: number) => Math.floor(random() * n);
  const anyTrack = (): PlayerTrack => {
    const recording = pick(8);
    return track(`t${pick(10)}`, random() < 0.2 ? undefined : `r${recording}`, random() < 0.85);
  };
  return {
    queue: Array.from({ length: pick(5) }, anyTrack).filter((item) => item.sources.length > 0),
    candidates: Array.from({ length: pick(7) }, () => ({
      artistId: 1 + pick(5),
      tracks: Array.from({ length: pick(4) }, anyTrack),
    })),
    limit: pick(7),
  };
};

const CASES = Array.from({ length: 1000 }, (_, index) => index + 1);

const isSubsequence = (part: number[], whole: number[]): boolean => {
  let position = 0;
  for (const value of part) {
    position = whole.indexOf(value, position);
    if (position < 0) return false;
    position += 1;
  }
  return true;
};

/** The artist each planned track was taken from: the first candidate with that id that holds it. */
const artistsOf = (planned: PlayerTrack[], candidates: ContinuationCandidate[]): number[] =>
  planned.map((item) => candidates.find((candidate) => candidate.tracks.includes(item))?.artistId ?? -1);

describe('queue continuation plan', () => {
  it('appends only tracks of served artists, in the served order', () => {
    for (const seed of CASES) {
      const { queue, candidates, limit } = generate(seed);
      const planned = planQueueContinuation(queue, candidates, limit);
      const artists = artistsOf(planned, candidates);
      const served = candidates.map((candidate) => candidate.artistId)
        .filter((id, index, all) => all.indexOf(id) === index);
      expect({ seed, ok: !artists.includes(-1) && isSubsequence(artists, served) }).toEqual({ seed, ok: true });
    }
  });

  it('never repeats a queued or planned recording, and only plans playable tracks', () => {
    for (const seed of CASES) {
      const { queue, candidates, limit } = generate(seed);
      const planned = planQueueContinuation(queue, candidates, limit);
      const queued = new Set(queue.map(continuationTrackKey));
      const queuedIds = new Set(queue.map((item) => item.id));
      const keys = planned.map(continuationTrackKey);
      const ok = planned.every((item) => item.sources.length > 0
          && !queued.has(continuationTrackKey(item)) && !queuedIds.has(item.id))
        && new Set(keys).size === keys.length
        && new Set(planned.map((item) => item.id)).size === planned.length;
      expect({ seed, ok }).toEqual({ seed, ok: true });
    }
  });

  it('plans at most the limit and at most one track per artist', () => {
    for (const seed of CASES) {
      const { queue, candidates, limit } = generate(seed);
      const planned = planQueueContinuation(queue, candidates, limit);
      const artists = artistsOf(planned, candidates);
      expect({ seed, ok: planned.length <= limit && new Set(artists).size === artists.length })
        .toEqual({ seed, ok: true });
    }
  });

  it('is a function of its inputs and does not change them', () => {
    for (const seed of CASES) {
      const { queue, candidates, limit } = generate(seed);
      const before = JSON.stringify({ queue, candidates });
      const first = planQueueContinuation(queue, candidates, limit);
      const second = planQueueContinuation(queue, candidates, limit);
      expect({ seed, same: JSON.stringify(first) === JSON.stringify(second) }).toEqual({ seed, same: true });
      expect(JSON.stringify({ queue, candidates })).toBe(before);
    }
  });

  it('takes the first eligible track of each artist and skips an artist without one', () => {
    const queue = [track('q1', 'r1')];
    const candidates: ContinuationCandidate[] = [
      { artistId: 7, tracks: [track('a1', 'r1'), track('a2', 'r2')] },
      { artistId: 8, tracks: [track('b1', 'r3', false)] },
      { artistId: 7, tracks: [track('a3', 'r4')] },
      { artistId: 9, tracks: [track('c1', 'r2'), track('c2', 'r5')] },
    ];
    expect(planQueueContinuation(queue, candidates).map((item) => item.id)).toEqual(['a2', 'c2']);
  });

  it('yields nothing without candidates, with a zero or invalid limit', () => {
    const candidates = [{ artistId: 1, tracks: [track('a')] }];
    expect(planQueueContinuation([], [])).toEqual([]);
    expect(planQueueContinuation([], candidates, 0)).toEqual([]);
    expect(planQueueContinuation([], candidates, Number.NaN)).toEqual([]);
    expect(planQueueContinuation([], candidates, -3)).toEqual([]);
    expect(CONTINUATION_TRACK_LIMIT).toBe(5);
  });
});

describe('queue continuation decision', () => {
  const ended: QueueState = { tracks: [track('a'), track('b')], currentIndex: 1, shuffle: false, repeat: 'off' };

  it('continues only at the end of a non-repeating queue of a signed-in listener with the flag on', () => {
    expect(shouldContinueQueue(ended, 12, true)).toBe(true);
    expect(shouldContinueQueue(ended, 12, false)).toBe(false);
    expect(shouldContinueQueue(ended, null, true)).toBe(false);
    expect(shouldContinueQueue(ended, undefined, true)).toBe(false);
    expect(shouldContinueQueue(ended, 0, true)).toBe(false);
    expect(shouldContinueQueue({ ...ended, currentIndex: 0 }, 12, true)).toBe(false);
    expect(shouldContinueQueue({ ...ended, repeat: 'queue' }, 12, true)).toBe(false);
    expect(shouldContinueQueue({ ...ended, repeat: 'track' }, 12, true)).toBe(false);
    expect(shouldContinueQueue({ ...ended, repeat: 'release' }, 12, true)).toBe(false);
    expect(shouldContinueQueue({ ...ended, shuffle: true }, 12, true)).toBe(false);
    expect(shouldContinueQueue(EMPTY_QUEUE, 12, true)).toBe(false);
  });

  it('is off unless the build sets the flag to the exact string true', () => {
    expect(isQueueContinuationEnabled(undefined)).toBe(false);
    expect(isQueueContinuationEnabled('1')).toBe(false);
    expect(isQueueContinuationEnabled(true)).toBe(false);
    expect(isQueueContinuationEnabled('true')).toBe(true);
  });
});

describe('queue continuation loader', () => {
  const recommendation = (id: number): ArtistRecommendationDTO =>
    ({ arecArtistId: id, arecReason: 'similar_fans', arecScore: 1, arecSeedArtistIds: [] });
  const summary = (artistPartyId: number) =>
    ({ artistPartyId, slug: `release-${artistPartyId}` }) as MusicPublicReleaseSummary;
  const release = (artistPartyId: number, recordings: string[]) => ({
    id: `rel-${artistPartyId}`,
    versionId: `ver-${artistPartyId}`,
    tracks: recordings.map((recordingId, index) => ({
      trackId: `track-${artistPartyId}-${index}`,
      recordingId,
      title: recordingId,
      displayArtist: `Artist ${artistPartyId}`,
      durationMs: 1000,
      sources: [{ assetId: `asset-${recordingId}`, role: 'stream_audio', mediaType: 'audio/mp4', technicalMetadata: {} }],
    })),
  }) as unknown as MusicPublicRelease;

  const api = (overrides: Partial<ContinuationApi> = {}): ContinuationApi & { calls: string[] } => {
    const calls: string[] = [];
    return {
      calls,
      listRecommendations: (limit) => { calls.push(`recommendations:${limit}`); return Promise.resolve([1, 2, 3, 4, 5, 6, 7].map(recommendation)); },
      listPublicReleases: (id) => { calls.push(`list:${id}`); return Promise.resolve([summary(id)]); },
      getPublicRelease: (slug) => {
        calls.push(`get:${slug}`);
        const id = Number(slug.replace('release-', ''));
        return Promise.resolve(release(id, [`rec-${id}`]));
      },
      ...overrides,
    };
  };

  it('keeps the server order, tags tracks with their release and stops reading once the plan is full', async () => {
    const stub = api();
    const planned = await loadQueueContinuation([], { api: stub });
    expect(planned.map((item) => item.recordingId)).toEqual(['rec-1', 'rec-2', 'rec-3', 'rec-4', 'rec-5']);
    expect(planned[0]).toMatchObject({ releaseId: 'rel-1', releaseVersionId: 'ver-1' });
    expect(planned.every((item) => item.sources.every((source) => source.url === ''))).toBe(true);
    expect(stub.calls[0]).toBe('recommendations:12');
    expect(stub.calls).not.toContain('list:6');
  });

  it('skips an artist whose reads fail, has no release, or answers for another artist', async () => {
    const stub = api({
      listPublicReleases: (id) => id === 1 ? Promise.reject(new Error('offline'))
        : id === 2 ? Promise.resolve([])
          : id === 3 ? Promise.resolve([summary(99)])
            : Promise.resolve([summary(id)]),
    });
    const planned = await loadQueueContinuation([], { api: stub, limit: 2 });
    expect(planned.map((item) => item.recordingId)).toEqual(['rec-4', 'rec-5']);
  });

  it('does not repeat what is queued and yields nothing when the recommendation read fails or is aborted', async () => {
    const queued = [track('x', 'rec-1')];
    expect((await loadQueueContinuation(queued, { api: api(), limit: 1 })).map((item) => item.recordingId)).toEqual(['rec-2']);
    expect(await loadQueueContinuation([], { api: api({ listRecommendations: () => Promise.reject(new Error('403')) }) })).toEqual([]);
    const controller = new AbortController();
    controller.abort();
    expect(await loadQueueContinuation([], { api: api(), signal: controller.signal })).toEqual([]);
  });
});

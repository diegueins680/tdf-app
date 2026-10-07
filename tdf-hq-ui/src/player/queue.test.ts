import { EMPTY_QUEUE, nextQueueIndex, previousQueueIndex, queueReducer } from './queue';
import type { PlayerTrack } from './types';

const track = (id: string, releaseId = 'release-1'): PlayerTrack => ({
  id,
  releaseId,
  title: id,
  artist: 'Synthetic Artist',
  sources: [{ url: `https://media.invalid/${id}.m4a`, quality: 'high', mediaType: 'audio/mp4' }],
});

describe('global player queue', () => {
  const tracks = [track('one'), track('two'), track('three', 'release-2')];

  it('preserves the selected track while reordering', () => {
    const initial = queueReducer(EMPTY_QUEUE, { type: 'replace', tracks, currentTrackId: 'two' });
    const moved = queueReducer(initial, { type: 'move', from: 1, to: 0 });
    expect(moved.tracks.map(({ id }) => id)).toEqual(['two', 'one', 'three']);
    expect(moved.currentIndex).toBe(0);
  });

  it('updates the current index safely when a queued item is removed', () => {
    const initial = queueReducer(EMPTY_QUEUE, { type: 'replace', tracks, currentTrackId: 'two' });
    expect(queueReducer(initial, { type: 'remove', index: 0 }).currentIndex).toBe(0);
    expect(queueReducer(initial, { type: 'remove', index: 1 }).tracks[1]?.id).toBe('three');
  });

  it('supports queue repeat, release repeat, and deterministic shuffle', () => {
    const base = { tracks, currentIndex: 2, shuffle: false, repeat: 'queue' as const };
    expect(nextQueueIndex(base)).toBe(0);
    expect(previousQueueIndex({ ...base, currentIndex: 0 })).toBe(2);
    expect(nextQueueIndex({ ...base, currentIndex: 1, repeat: 'release' })).toBe(0);
    expect(nextQueueIndex({ ...base, currentIndex: 0, repeat: 'off', shuffle: true }, 0.99)).toBe(2);
  });
});

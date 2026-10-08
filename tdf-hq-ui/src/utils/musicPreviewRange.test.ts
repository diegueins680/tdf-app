import { musicPreviewRangeError } from './musicPreviewRange';

describe('music preview millisecond range', () => {
  it('accepts automatic, duration-only and exact-end ranges', () => {
    expect(musicPreviewRangeError(null, null, 2000)).toBeNull();
    expect(musicPreviewRangeError(null, 1000, 2000)).toBeNull();
    expect(musicPreviewRangeError(1500, 500, 2000)).toBeNull();
  });
  it('rejects missing duration, fractions, overflow and out-of-bounds selection', () => {
    for (const [start, duration] of [[0, null], [-1, 500], [0.5, 500], [0, 0], [0, 1.5], [NaN, 500], [2000, 500], [1500, 501], [Number.MAX_SAFE_INTEGER + 1, 1]]) {
      expect(musicPreviewRangeError(start ?? null, duration ?? null, 2000)).not.toBeNull();
    }
  });
  it('allows a draft before the master duration is known', () => {
    expect(musicPreviewRangeError(1500, 1250, null)).toBeNull();
  });
});

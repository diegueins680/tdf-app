import { musicSourceQuality } from './sourceMetadata';

describe('canonical worker source quality', () => {
  it('recognizes real FLAC output where zero means no configured lossy bitrate', () => {
    expect(musicSourceQuality({ pipeline: 'audio-v2', bitrate_kbps: 0, normalized: true }, 'audio/flac')).toBe('lossless');
  });
  it.each([[96, 'low'], [160, 'medium'], [256, 'high']] as const)('maps actual AAC %i to %s', (bitrate, quality) => {
    expect(musicSourceQuality({ bitrate_kbps: bitrate }, 'audio/mp4')).toBe(quality);
  });
  it('does not mistake a high lossy bitrate for a lossless codec', () => {
    expect(musicSourceQuality({ bitrate_kbps: 512 }, 'audio/mp4')).toBe('high');
  });
  it.each([0, -1, NaN, Infinity, undefined, '96'])('uses conservative high quality for unknown AAC bitrate %s', (bitrate) => {
    expect(musicSourceQuality({ bitrate_kbps: bitrate }, 'audio/mp4')).toBe('high');
  });
  it('accepts a parameterized FLAC media type without relying on filename or bitrate', () => {
    expect(musicSourceQuality({}, 'Audio/Flac; rate=48000')).toBe('lossless');
  });
});

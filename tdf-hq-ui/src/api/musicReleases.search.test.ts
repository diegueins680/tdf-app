import { jest } from '@jest/globals';
const get = jest.fn<(...args: unknown[]) => Promise<unknown>>();
jest.unstable_mockModule('./client', () => ({ get, post: jest.fn(), put: jest.fn(), del: jest.fn() }));
const { musicReleases } = await import('./musicReleases');
beforeEach(() => { get.mockReset().mockResolvedValue([]); });

it('encodes literal search text without release-detail or signing requests', async () => {
  await musicReleases.searchTracks('  Canción & 你好_%  ');
  expect(get).toHaveBeenCalledTimes(1);
  expect(get).toHaveBeenCalledWith('/music/search?q=Canci%C3%B3n+%26+%E4%BD%A0%E5%A5%BD_%25');
});
it('forwards page inputs and the exact cancellation signal', async () => {
  const signal = new AbortController().signal;
  await musicReleases.searchTracks(' Azul ', { limit: 13, offset: 12, signal });
  expect(get).toHaveBeenCalledWith('/music/search?q=Azul&limit=13&offset=12', { signal });
});
it('keeps service failure distinct from an empty catalog', async () => {
  const failure = new Error('Service unavailable');
  get.mockRejectedValue(failure);
  await expect(musicReleases.searchTracks('Azul')).rejects.toBe(failure);
});
it('preserves matched identity and deferred references returned by the server', async () => {
  const hits = [{ trackId: 'matched-B', recordingId: 'recording-B', releaseId: 'release-1',
    releaseVersionId: 'version-1', releaseSlug: 'azul', releaseTitle: 'Release', title: 'Canción Azul',
    displayArtist: 'Artist', durationMs: null,
    sources: [{ assetId: 'authorized-B', role: 'stream_audio', mediaType: 'audio/mp4', technicalMetadata: { bitrate_kbps: 256 } }] }];
  get.mockResolvedValue(hits);
  await expect(musicReleases.searchTracks('Azul')).resolves.toBe(hits);
  expect(get).toHaveBeenCalledTimes(1);
});
it('counts Unicode characters consistently with the server bound', async () => {
  await musicReleases.searchTracks('🎵'.repeat(200));
  expect(get).toHaveBeenCalledTimes(1);
  get.mockClear();
  expect(() => musicReleases.searchTracks('🎵'.repeat(201))).toThrow(RangeError);
  expect(get).not.toHaveBeenCalled();
});
it.each([{ limit: 0 }, { limit: 101 }, { limit: 1.5 }, { limit: Infinity },
  { offset: -1 }, { offset: NaN }, { offset: Infinity }, { offset: 0.5 }, { offset: Number.MAX_SAFE_INTEGER + 1 }])(
  'rejects invalid pagination before a request: %j', (page) => {
    expect(() => musicReleases.searchTracks('Azul', page)).toThrow(RangeError);
    expect(get).not.toHaveBeenCalled();
  },
);
it('rejects an overlong trimmed query before a request', () => {
  expect(() => musicReleases.searchTracks(` ${'a'.repeat(201)} `)).toThrow(RangeError);
  expect(get).not.toHaveBeenCalled();
});

import { jest } from '@jest/globals';
const get = jest.fn<(...args: unknown[]) => Promise<unknown>>();
jest.unstable_mockModule('./client', () => ({ get, post: jest.fn(), put: jest.fn(), del: jest.fn() }));
const { musicReleases } = await import('./musicReleases');
beforeEach(() => { get.mockReset().mockResolvedValue([]); });
const validHit = () => ({ trackId: '11111111-1111-4111-8111-111111111111', recordingId: '22222222-2222-4222-8222-222222222222',
  releaseId: '33333333-3333-4333-8333-333333333333', releaseVersionId: '44444444-4444-4444-8444-444444444444',
  releaseSlug: 'azul', releaseTitle: 'Release', title: 'Canción Azul', displayArtist: 'Artist', durationMs: null,
  sources: [{ assetId: '55555555-5555-4555-8555-555555555555', role: 'stream_audio', mediaType: 'audio/mp4', technicalMetadata: { bitrate_kbps: 256 } }] });

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
  const hits = [validHit()];
  get.mockResolvedValue(hits);
  await expect(musicReleases.searchTracks('Azul')).resolves.toBe(hits);
  expect(get).toHaveBeenCalledTimes(1);
});
it('accepts a valid empty result and finite full/preview metadata without changing identities', async () => {
  await expect(musicReleases.searchTracks('Missing')).resolves.toEqual([]);
  const hit = validHit();
  const hits = [{ ...hit, durationMs: 24000, sources: [{ ...hit.sources[0], role: 'preview_audio', technicalMetadata: { loudness_lufs: -14.3 } }] }];
  get.mockResolvedValue(hits);
  await expect(musicReleases.searchTracks('Azul')).resolves.toBe(hits);
});
it.each([
  ['SPA HTML', '<!doctype html><html>Private upstream debug sentinel</html>'],
  ['object', { items: [] }], ['null', null], ['number', 42], ['missing body', undefined],
  ['null hit', [null]], ['primitive hit', ['wrong']], ['unknown hit field', [{ ...validHit(), storageUrl: 'https://private.invalid/sentinel' }]],
  ['missing title', [{ ...validHit(), title: undefined }]], ['blank title', [{ ...validHit(), title: '  ' }]],
  ['blank artist', [{ ...validHit(), displayArtist: '' }]], ['null release context', [{ ...validHit(), releaseTitle: null }]],
  ['invalid track ID', [{ ...validHit(), trackId: 'wrong' }]], ['null recording ID', [{ ...validHit(), recordingId: null }]],
  ['invalid release ID', [{ ...validHit(), releaseId: 1 }]], ['invalid version ID', [{ ...validHit(), releaseVersionId: '' }]],
  ['blank slug', [{ ...validHit(), releaseSlug: ' ' }]], ['negative duration', [{ ...validHit(), durationMs: -1 }]],
  ['fractional duration', [{ ...validHit(), durationMs: .5 }]], ['unsafe duration', [{ ...validHit(), durationMs: Number.MAX_SAFE_INTEGER + 1 }]],
  ['non-finite duration', [{ ...validHit(), durationMs: Infinity }]], ['null source list', [{ ...validHit(), sources: null }]],
  ['empty source list', [{ ...validHit(), sources: [] }]], ['null source member', [{ ...validHit(), sources: [validHit().sources[0], null] }]],
  ['invalid asset ID', [{ ...validHit(), sources: [{ ...validHit().sources[0], assetId: 'wrong' }] }]],
  ['master source', [{ ...validHit(), sources: [{ ...validHit().sources[0], role: 'master_audio' }] }]],
  ['missing media type', [{ ...validHit(), sources: [{ ...validHit().sources[0], mediaType: null }] }]],
  ['blank media type', [{ ...validHit(), sources: [{ ...validHit().sources[0], mediaType: ' ' }] }]],
  ['source URL', [{ ...validHit(), sources: [{ ...validHit().sources[0], url: 'https://private.invalid/sentinel' }] }]],
  ['null metadata', [{ ...validHit(), sources: [{ ...validHit().sources[0], technicalMetadata: null }] }]],
  ['array metadata', [{ ...validHit(), sources: [{ ...validHit().sources[0], technicalMetadata: [] }] }]],
  ['string bitrate', [{ ...validHit(), sources: [{ ...validHit().sources[0], technicalMetadata: { bitrate_kbps: '256' } }] }]],
  ['non-finite loudness', [{ ...validHit(), sources: [{ ...validHit().sources[0], technicalMetadata: { loudness_lufs: NaN } }] }]],
  ['unknown metadata key', [{ ...validHit(), sources: [{ ...validHit().sources[0], technicalMetadata: { storageKey: 1 } }] }]],
  ['too many hits', Array.from({ length: 101 }, validHit)],
])('rejects malformed successful response: %s', async (_label, response) => {
  get.mockResolvedValue(response);
  await expect(musicReleases.searchTracks('Azul')).rejects.toThrow('Invalid music search response.');
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

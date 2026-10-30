import { jest } from '@jest/globals';
import type { ArtistReleaseDTO } from './types';
import type { MusicPublicReleaseSummary } from './musicReleases';
const listPublic = jest.fn<(...args: unknown[]) => Promise<MusicPublicReleaseSummary[]>>();
const getReleases = jest.fn<(...args: unknown[]) => Promise<ArtistReleaseDTO[]>>();
jest.unstable_mockModule('./fans', () => ({ Fans: { getReleases } }));
jest.unstable_mockModule('./musicReleases', () => ({ musicReleases: { listPublic } }));
const { loadArtistCatalog, loadArtistCatalogs, listCanonicalArtistReleases } = await import('./artistCatalog');
const canonical = (id: string, artistPartyId = 1) => ({ id, artistPartyId, title: 'Same title' }) as MusicPublicReleaseSummary;
const legacy = (arReleaseId: number, arArtistId = 1) => ({ arReleaseId, arArtistId, arTitle: 'Same title' }) as ArtistReleaseDTO;
beforeEach(() => { listPublic.mockReset().mockResolvedValue([]); getReleases.mockReset().mockResolvedValue([]); });

it('retains distinct legacy/canonical identities despite identical titles and deduplicates exact source IDs', async () => {
  listPublic.mockResolvedValue([canonical('1'), canonical('1')]);
  getReleases.mockResolvedValue([legacy(1), legacy(1)]);
  const catalog = await loadArtistCatalog(1);
  expect(catalog.entries.map((entry) => entry.key)).toEqual(['canonical:1', 'legacy:1']);
  expect(catalog.unavailableSources).toEqual([]);
});
it('reads past the default canonical page and forwards cancellation', async () => {
  const signal = new AbortController().signal;
  listPublic.mockResolvedValueOnce(Array.from({ length: 100 }, (_, i) => canonical(String(i))))
    .mockResolvedValueOnce([canonical('last')]);
  expect(await listCanonicalArtistReleases(1, signal)).toHaveLength(101);
  expect(listPublic).toHaveBeenNthCalledWith(2, '', 1, { limit: 100, offset: 100, signal });
});
it('rejects a repeated full page instead of looping or claiming the catalog is complete', async () => {
  listPublic.mockResolvedValue(Array.from({ length: 100 }, (_, i) => canonical(String(i))));
  await expect(listCanonicalArtistReleases(1)).rejects.toThrow('did not advance');
  expect(listPublic).toHaveBeenCalledTimes(2);
});
it.each(['canonical', 'legacy'] as const)('preserves the other source and reports a failed %s source', async (source) => {
  listPublic.mockResolvedValue([canonical('a')]); getReleases.mockResolvedValue([legacy(1)]);
  if (source === 'canonical') listPublic.mockRejectedValue(new Error('unavailable'));
  else getReleases.mockRejectedValue(new Error('unavailable'));
  const catalog = await loadArtistCatalog(1);
  expect(catalog.unavailableSources).toEqual([source]);
  expect(catalog.entries).toHaveLength(1);
});
it('reports total failure instead of a successful empty catalog', async () => {
  listPublic.mockRejectedValue(new Error('failed')); getReleases.mockRejectedValue(new Error('failed'));
  await expect(loadArtistCatalog(1)).rejects.toThrow('unavailable');
});
it('rejects cross-artist results from either source', async () => {
  listPublic.mockResolvedValue([canonical('other', 2)]); getReleases.mockResolvedValue([legacy(2, 2)]);
  await expect(loadArtistCatalog(1)).rejects.toThrow('unavailable');
});
it('does not replace cancellation with partial success', async () => {
  const controller = new AbortController();
  listPublic.mockImplementation(async () => { controller.abort(); return []; });
  await expect(loadArtistCatalog(1, controller.signal)).rejects.toMatchObject({ name: 'AbortError' });
});
it.each([0, -1, 1.5, Number.MAX_SAFE_INTEGER + 1])('rejects an invalid artist %s before network requests', async (id) => {
  await expect(loadArtistCatalog(id)).rejects.toThrow();
  expect(listPublic).not.toHaveBeenCalled(); expect(getReleases).not.toHaveBeenCalled();
});
it('bounds simultaneous artist reads and removes duplicate requested artists', async () => {
  let active = 0; let peak = 0;
  listPublic.mockImplementation(async () => {
    active++; peak = Math.max(peak, active);
    await new Promise((resolve) => setTimeout(resolve, 0)); active--; return [];
  });
  const catalogs = await loadArtistCatalogs([1, 2, 3, 4, 5, 6, 1]);
  expect(catalogs.map((catalog) => catalog.artistId)).toEqual([1, 2, 3, 4, 5, 6]);
  expect(peak).toBeLessThanOrEqual(4);
});
it('keeps available artists when another artist has no reachable source', async () => {
  listPublic.mockImplementation(async (_q, artist) => {
    if (artist === 1) throw new Error('offline'); return [canonical('available', 2)];
  });
  getReleases.mockRejectedValue(new Error('offline'));
  const catalogs = await loadArtistCatalogs([1, 2]);
  expect(catalogs[0]?.unavailableSources).toEqual(['canonical', 'legacy']);
  expect(catalogs[1]?.entries[0]?.key).toBe('canonical:available');
});

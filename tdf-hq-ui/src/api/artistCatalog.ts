import { Fans } from './fans';
import { musicReleases, type MusicPublicReleaseSummary } from './musicReleases';
import type { ArtistReleaseDTO } from './types';

export type CatalogSource = 'canonical' | 'legacy';
export type ArtistCatalogEntry =
  | { key: string; source: 'canonical'; release: MusicPublicReleaseSummary }
  | { key: string; source: 'legacy'; release: ArtistReleaseDTO };
export interface ArtistCatalog {
  artistId: number;
  entries: ArtistCatalogEntry[];
  unavailableSources: CatalogSource[];
}

const requireArtist = (artistId: number) => {
  if (!Number.isSafeInteger(artistId) || artistId <= 0) throw new RangeError('Artist ID must be a positive safe integer');
};
const checkCancellation = (signal?: AbortSignal) => {
  if (signal?.aborted) throw new DOMException('Catalog request aborted', 'AbortError');
};

/** Follow the public endpoint's pagination; never interpret a failed page as an empty catalog. */
export async function listCanonicalArtistReleases(artistId: number, signal?: AbortSignal): Promise<MusicPublicReleaseSummary[]> {
  requireArtist(artistId);
  const releases = new Map<string, MusicPublicReleaseSummary>();
  for (let offset = 0; offset < 10000; offset += 100) {
    checkCancellation(signal);
    const page = await musicReleases.listPublic('', artistId, { limit: 100, offset, signal });
    checkCancellation(signal);
    if (!Array.isArray(page) || page.length > 100 || page.some((release) => release?.artistPartyId !== artistId
      || typeof release.id !== 'string' || !release.id.trim())) throw new Error('Invalid artist catalog response');
    const previousSize = releases.size;
    page.forEach((release) => { if (!releases.has(release.id)) releases.set(release.id, release); });
    if (page.length < 100) return [...releases.values()];
    if (releases.size === previousSize) throw new Error('Artist catalog pagination did not advance');
  }
  throw new Error('Artist catalog exceeds the bounded pagination limit');
}

/**
 * One read boundary for both catalogs. Legacy references retain their own IDs
 * and never become published masters, streaming grants or canonical releases.
 * Only exact source identities are deduplicated; matching titles are insufficient.
 */
export async function loadArtistCatalog(artistId: number, signal?: AbortSignal): Promise<ArtistCatalog> {
  requireArtist(artistId);
  checkCancellation(signal);
  const results = await Promise.allSettled([
    listCanonicalArtistReleases(artistId, signal),
    Fans.getReleases(artistId, signal),
  ]);
  checkCancellation(signal);
  const entries: ArtistCatalogEntry[] = [];
  const unavailableSources: CatalogSource[] = [];
  const [canonical, legacy] = results;
  if (canonical.status === 'fulfilled') {
    entries.push(...canonical.value.map((release): ArtistCatalogEntry => ({ key: `canonical:${release.id}`, source: 'canonical', release })));
  } else unavailableSources.push('canonical');
  if (legacy.status === 'fulfilled' && Array.isArray(legacy.value)
    && legacy.value.every((release) => release?.arArtistId === artistId
      && Number.isSafeInteger(release.arReleaseId) && release.arReleaseId > 0)) {
    const unique = new Map(legacy.value.map((release) => [release.arReleaseId, release]));
    entries.push(...[...unique.values()].map((release): ArtistCatalogEntry => ({ key: `legacy:${release.arReleaseId}`, source: 'legacy', release })));
  } else unavailableSources.push('legacy');
  if (unavailableSources.length === 2) throw new Error('Artist release catalog is unavailable');
  return { artistId, entries, unavailableSources };
}

/** Bound artist-level fan-out as well as per-artist pagination. */
export async function loadArtistCatalogs(artistIds: number[], signal?: AbortSignal): Promise<ArtistCatalog[]> {
  checkCancellation(signal);
  const ids = [...new Set(artistIds)];
  ids.forEach(requireArtist);
  const result = new Array<ArtistCatalog>(ids.length);
  let next = 0;
  await Promise.all(Array.from({ length: Math.min(4, ids.length) }, async () => {
    while (next < ids.length) {
      const index = next++;
      try { result[index] = await loadArtistCatalog(ids[index]!, signal); }
      catch {
        checkCancellation(signal);
        result[index] = { artistId: ids[index]!, entries: [], unavailableSources: ['canonical', 'legacy'] };
      }
    }
  }));
  return result;
}

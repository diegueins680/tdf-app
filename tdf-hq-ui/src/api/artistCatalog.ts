import { ApiError } from './client';
import { Records, type RecordsFeedDTO } from './records';
import { nativeSessionsFromFeed, type NativeSession } from '../features/sessions/nativeSessions';
import { Fans } from './fans';
import { musicReleases, type MusicPublicReleaseSummary } from './musicReleases';
import type { ArtistReleaseDTO } from './types';

export type CatalogSource = 'canonical' | 'legacy' | 'records';
export type ArtistCatalogEntry =
  | { key: string; source: 'canonical'; release: MusicPublicReleaseSummary }
  | { key: string; source: 'legacy'; release: ArtistReleaseDTO };
export interface ArtistCatalog {
  artistId: number;
  entries: ArtistCatalogEntry[];
  unavailableSources: CatalogSource[];
  disabledSources?: CatalogSource[];
  nativeSessions?: NativeSession[];
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
export async function loadArtistCatalog(artistId: number, signal?: AbortSignal, recordsFeed?: Promise<RecordsFeedDTO>): Promise<ArtistCatalog> {
  requireArtist(artistId);
  checkCancellation(signal);
  const results = await Promise.allSettled([
    listCanonicalArtistReleases(artistId, signal),
    Fans.getReleases(artistId, signal),
    recordsFeed ?? Records.getFeed('es', signal),
  ]);
  checkCancellation(signal);
  const entries: ArtistCatalogEntry[] = [];
  const unavailableSources: CatalogSource[] = [];
  const disabledSources: CatalogSource[] = [];
  const [canonical, legacy, records] = results;
  let nativeSessions: NativeSession[] = [];
  if (records.status === 'fulfilled') {
    try { nativeSessions = nativeSessionsFromFeed(records.value, artistId); }
    catch { unavailableSources.push('records'); }
  } else unavailableSources.push('records');
  if (canonical.status === 'fulfilled') {
    entries.push(...canonical.value.map((release): ArtistCatalogEntry => ({ key: `canonical:${release.id}`, source: 'canonical', release })));
  } else if (canonical.reason instanceof ApiError && canonical.reason.status === 404) {
    // The collection endpoint deliberately returns 404 when public Music is disabled.
    // This is an unavailable feature, not a failed page of an enabled catalog.
    disabledSources.push('canonical');
  } else unavailableSources.push('canonical');
  if (legacy.status === 'fulfilled' && Array.isArray(legacy.value)
    && legacy.value.every((release) => release?.arArtistId === artistId
      && Number.isSafeInteger(release.arReleaseId) && release.arReleaseId > 0)) {
    const unique = new Map(legacy.value.map((release) => [release.arReleaseId, release]));
    entries.push(...[...unique.values()].map((release): ArtistCatalogEntry => ({ key: `legacy:${release.arReleaseId}`, source: 'legacy', release })));
  } else unavailableSources.push('legacy');
  if (unavailableSources.includes('legacy') && (unavailableSources.includes('canonical') || disabledSources.includes('canonical')) && nativeSessions.length === 0) throw new Error('Artist release catalog is unavailable');
  return { artistId, entries, unavailableSources, disabledSources, nativeSessions };
}

/** Bound artist-level fan-out as well as per-artist pagination. */
export async function loadArtistCatalogs(artistIds: number[], signal?: AbortSignal): Promise<ArtistCatalog[]> {
  checkCancellation(signal);
  const ids = [...new Set(artistIds)];
  ids.forEach(requireArtist);
  const recordsFeed = ids.length ? Records.getFeed('es', signal) : undefined;
  // A shared failed feed is still observed by every bounded per-artist read.
  void recordsFeed?.catch(() => undefined);
  const result = new Array<ArtistCatalog>(ids.length);
  let next = 0;
  await Promise.all(Array.from({ length: Math.min(4, ids.length) }, async () => {
    while (next < ids.length) {
      const index = next++;
      try { result[index] = await loadArtistCatalog(ids[index]!, signal, recordsFeed); }
      catch {
        checkCancellation(signal);
        result[index] = { artistId: ids[index]!, entries: [], unavailableSources: ['canonical', 'legacy'] };
      }
    }
  }));
  return result;
}

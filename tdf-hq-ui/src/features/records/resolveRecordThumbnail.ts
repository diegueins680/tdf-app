import type { RecordsResourceDTO } from '../../api/records';

export type ThumbnailResource = Pick<RecordsResourceDTO,
  'providerCode' | 'externalCode' | 'thumbnailUrl' | 'availability'>;

const youtubeImageHosts = new Set(['i.ytimg.com', 'img.youtube.com']);

/** Choose artwork only among resources belonging to this same catalog item. */
export function primaryRecordsResource(resources: RecordsResourceDTO[]): RecordsResourceDTO | undefined {
  const renderable = resources.filter(resource => recordThumbnailCandidates(resource).length > 0);
  return renderable.find(resource => resource.primary) ?? renderable[0]
    ?? resources.find(resource => resource.primary) ?? resources[0];
}

export function youtubeImageId(raw: string): string | null {
  try {
    const url = new URL(raw);
    return youtubeImageHosts.has(url.hostname)
      ? /^\/vi(?:_webp)?\/([^/]+)\//.exec(url.pathname)?.[1] ?? null : null;
  } catch { return null; }
}

/** An explicit editorial image wins. Provider images must belong to this video. */
export function recordThumbnailCandidates(resource: ThumbnailResource): string[] {
  if (resource.availability === 'unavailable') return [];
  const candidates: string[] = [];
  const explicit = resource.thumbnailUrl?.trim();
  if (explicit) {
    try {
      const url = new URL(explicit);
      const imageId = youtubeImageId(explicit);
      if (url.protocol === 'https:' && !url.username && !url.password
        && (!imageId || (resource.providerCode === 'youtube' && imageId === resource.externalCode))) {
        candidates.push(explicit);
      }
    } catch { /* Invalid metadata must not become an image request. */ }
  }
  if (resource.providerCode === 'youtube' && /^[A-Za-z0-9_-]{11}$/.test(resource.externalCode)) {
    // These are bounded recovery candidates, not evidence of availability.
    candidates.push(`https://i.ytimg.com/vi/${resource.externalCode}/hqdefault.jpg`);
    candidates.push(`https://i.ytimg.com/vi/${resource.externalCode}/mqdefault.jpg`);
  }
  return [...new Set(candidates)];
}

/** Missing YouTube images can decode as a 120x90 generic placeholder, even on 200. */
export function isProviderPlaceholder(src: string, width: number, height: number): boolean {
  if (!youtubeImageId(src)) return false;
  const path = new URL(src).pathname;
  return /\/(?:maxresdefault|sddefault|hqdefault|hq720|mqdefault)\./.test(path)
    && width <= 120 && height <= 90;
}

import type { RecordsResourceDTO } from '../../api/records';

export type ThumbnailResource = Pick<RecordsResourceDTO,
  'providerCode' | 'externalCode' | 'thumbnailUrl' | 'availability' | 'availabilityReason' | 'providerMetadata'>;

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
  let providerExplicit: string | undefined;
  const explicit = resource.thumbnailUrl?.trim();
  if (explicit) {
    try {
      const url = new URL(explicit);
      const imageId = youtubeImageId(explicit);
      if (url.protocol === 'https:' && !url.username && !url.password
        && (!youtubeImageHosts.has(url.hostname) || (resource.providerCode === 'youtube' && imageId === resource.externalCode))) {
        if (imageId) providerExplicit = explicit;
        else candidates.push(explicit);
      }
    } catch { /* Invalid metadata must not become an image request. */ }
  }
  const metadata = resource.providerMetadata;
  if (resource.providerCode === 'youtube' && metadata?.['id'] === resource.externalCode && Array.isArray(metadata['thumbnails'])) {
    for (const thumbnail of (metadata['thumbnails'] as unknown[]).slice(0, 5)) {
      if (thumbnail && typeof thumbnail === 'object' && 'url' in thumbnail && typeof thumbnail.url === 'string') {
        try {
          const url = new URL(thumbnail.url);
          if (url.protocol === 'https:' && !url.username && !url.password && youtubeImageId(url.href) === resource.externalCode) candidates.push(url.href);
        } catch { /* Ignore malformed provider metadata. */ }
      }
    }
  }
  if (providerExplicit) candidates.push(providerExplicit);
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

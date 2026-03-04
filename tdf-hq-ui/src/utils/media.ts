import type { ArtistReleaseDTO } from '../api/types';

export type StreamProvider = 'youtube' | 'spotify' | 'video' | 'audio';

export interface StreamingSource {
  url?: string | null;
  provider?: StreamProvider;
  label?: string;
  mimeType?: string;
  posterUrl?: string | null;
}

export interface NormalizedStreamingSource {
  url: string;
  provider: StreamProvider;
  label: string;
  mimeType?: string;
  posterUrl?: string | null;
}

const videoExtensions = ['.mp4', '.mov', '.webm', '.mkv'];
const audioExtensions = ['.mp3', '.aac', '.wav', '.ogg', '.m4a'];
const spotifyResourceTypes = new Set(['track', 'album', 'playlist', 'artist', 'episode', 'show']);

const isSpotifyLocaleSegment = (segment: string): boolean => /^intl-[a-z0-9-]+$/i.test(segment);

const hasScheme = (value: string) => /^[a-zA-Z][a-zA-Z\d+\-.]*:\/\//.test(value);

const parseUrl = (raw: string): URL | null => {
  const trimmed = raw.trim();
  if (!trimmed) return null;
  const candidate = hasScheme(trimmed) ? trimmed : `https://${trimmed}`;
  try {
    return new URL(candidate);
  } catch {
    return null;
  }
};

const hostMatches = (host: string, domain: string): boolean =>
  host === domain || host.endsWith(`.${domain}`);

const normalizeYoutubeVideoId = (candidate: string | null | undefined): string | null => {
  const value = candidate?.trim();
  if (!value) return null;
  return value;
};

export const normalizeYoutubeEmbed = (raw?: string | null): string | null => {
  if (!raw) return null;
  const url = parseUrl(raw);
  if (!url) return null;
  const host = url.hostname.toLowerCase();
  if (hostMatches(host, 'youtube.com')) {
    const v = normalizeYoutubeVideoId(url.searchParams.get('v'));
    if (v) return `https://www.youtube.com/embed/${v}`;
    const parts = url.pathname.split('/').filter(Boolean);
    const route = parts[0];
    const videoId =
      route === 'shorts' || route === 'embed' || route === 'live' || route === 'v'
        ? normalizeYoutubeVideoId(parts[1])
        : null;
    if (videoId) {
      return `https://www.youtube.com/embed/${videoId}`;
    }
    return null;
  }
  if (hostMatches(host, 'youtu.be')) {
    const [firstPathSegment] = url.pathname.split('/').filter(Boolean);
    const videoId = normalizeYoutubeVideoId(firstPathSegment);
    if (videoId) {
      return `https://www.youtube.com/embed/${videoId}`;
    }
  }
  return null;
};

const normalizeSpotifyPath = (pathname: string): string | null => {
  const segments = pathname.split('/').filter(Boolean);
  if (segments.length === 0) return null;
  const pathWithoutEmbed = segments[0] === 'embed' ? segments.slice(1) : segments;
  const resourceSegments = isSpotifyLocaleSegment(pathWithoutEmbed[0] ?? '')
    ? pathWithoutEmbed.slice(1)
    : pathWithoutEmbed;
  if (resourceSegments.length !== 2) return null;
  const [resourceType, resourceId] = resourceSegments;
  if (!resourceType || !resourceId) return null;
  const normalizedResourceType = resourceType.toLowerCase();
  if (!spotifyResourceTypes.has(normalizedResourceType)) return null;
  return `/embed/${normalizedResourceType}/${resourceId}`;
};

export const normalizeSpotifyEmbed = (raw?: string | null): string | null => {
  if (!raw) return null;
  const url = parseUrl(raw);
  if (!url) return null;
  const host = url.hostname.toLowerCase();
  if (!hostMatches(host, 'spotify.com')) return null;
  const normalizedPath = normalizeSpotifyPath(url.pathname);
  if (!normalizedPath) return null;

  const normalizedUrl = new URL(`https://${host}`);
  normalizedUrl.pathname = normalizedPath;
  normalizedUrl.search = url.search;
  return normalizedUrl.toString();
};

const inferProviderFromUrl = (rawUrl: string): StreamProvider => {
  const parsedUrl = parseUrl(rawUrl);
  const host = parsedUrl?.hostname.toLowerCase();
  const rawPath = rawUrl.trim().split(/[?#]/, 1)[0] ?? '';
  const pathCandidates = [parsedUrl?.pathname ?? '', rawPath].map((path) => path.toLowerCase());
  if (host && (hostMatches(host, 'youtube.com') || hostMatches(host, 'youtu.be'))) return 'youtube';
  if (host && hostMatches(host, 'spotify.com')) return 'spotify';
  if (pathCandidates.some((path) => videoExtensions.some((ext) => path.endsWith(ext)))) {
    return 'video';
  }
  if (pathCandidates.some((path) => audioExtensions.some((ext) => path.endsWith(ext)))) {
    return 'audio';
  }
  return 'audio';
};

export const normalizeStreamingSource = (
  source: StreamingSource,
): NormalizedStreamingSource | null => {
  if (!source.url) return null;
  const provider = source.provider ?? inferProviderFromUrl(source.url);
  if (provider === 'youtube') {
    const embed = normalizeYoutubeEmbed(source.url);
    if (!embed) return null;
    return {
      url: embed,
      provider,
      label: source.label ?? 'YouTube',
      posterUrl: source.posterUrl,
    };
  }
  if (provider === 'spotify') {
    const embed = normalizeSpotifyEmbed(source.url);
    if (!embed) return null;
    return {
      url: embed,
      provider,
      label: source.label ?? 'Spotify',
      posterUrl: source.posterUrl,
    };
  }
  return {
    url: source.url,
    provider,
    label: source.label ?? (provider === 'video' ? 'Video' : 'Audio'),
    mimeType: source.mimeType,
    posterUrl: source.posterUrl,
  };
};

export const buildReleaseStreamingSources = (release: ArtistReleaseDTO): StreamingSource[] => {
  const sources: StreamingSource[] = [];
  if (release.arYoutubeUrl) {
    sources.push({ url: release.arYoutubeUrl, provider: 'youtube', label: 'YouTube' });
  }
  if (release.arSpotifyUrl) {
    sources.push({ url: release.arSpotifyUrl, provider: 'spotify', label: 'Spotify' });
  }
  return sources;
};

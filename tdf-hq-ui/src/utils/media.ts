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

export const normalizeYoutubeEmbed = (raw?: string | null): string | null => {
  if (!raw) return null;
  try {
    const url = new URL(raw);
    const host = url.hostname.toLowerCase();
    if (host.includes('youtube.com')) {
      const v = url.searchParams.get('v');
      if (v) return `https://www.youtube.com/embed/${v}`;
      const parts = url.pathname.split('/').filter(Boolean);
      const shortsIdx = parts.findIndex((p) => p === 'shorts');
      if (shortsIdx >= 0 && parts[shortsIdx + 1]) {
        return `https://www.youtube.com/embed/${parts[shortsIdx + 1]}`;
      }
      const embedIdx = parts.findIndex((p) => p === 'embed');
      if (embedIdx >= 0 && parts[embedIdx + 1]) {
        return `https://www.youtube.com/embed/${parts[embedIdx + 1]}`;
      }
      const watchIdx = parts.findIndex((p) => p === 'watch');
      if (watchIdx >= 0 && parts[watchIdx + 1]) {
        return `https://www.youtube.com/embed/${parts[watchIdx + 1]}`;
      }
      if (parts.length > 0) {
        return `https://www.youtube.com/embed/${parts[parts.length - 1]}`;
      }
    }
    if (host.includes('youtu.be')) {
      const trimmed = url.pathname.replace(/^\//, '');
      if (trimmed.length > 0) {
        return `https://www.youtube.com/embed/${trimmed}`;
      }
    }
    return null;
  } catch {
    return null;
  }
};

export const normalizeSpotifyEmbed = (raw?: string | null): string | null => {
  if (!raw) return null;
  try {
    const url = new URL(raw);
    const host = url.hostname.toLowerCase();
    if (!host.includes('spotify.com')) return null;
    if (!url.pathname.startsWith('/embed')) {
      url.pathname = `/embed${url.pathname}`;
    }
    return url.toString();
  } catch {
    return null;
  }
};

const inferProviderFromUrl = (rawUrl: string): StreamProvider => {
  const lower = rawUrl.toLowerCase();
  if (lower.includes('youtube.com') || lower.includes('youtu.be')) return 'youtube';
  if (lower.includes('spotify.com')) return 'spotify';
  if (videoExtensions.some((ext) => lower.endsWith(ext))) return 'video';
  if (audioExtensions.some((ext) => lower.endsWith(ext))) return 'audio';
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

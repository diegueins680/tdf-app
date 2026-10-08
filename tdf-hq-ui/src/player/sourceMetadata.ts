import type { PlayerQuality } from './types';

// Use the server-authorized resource type, not a filename or a nominal bitrate:
// audio-v2 stores 0 for FLAC (no configured lossy bitrate), not "low quality".
export const musicSourceQuality = (metadata: Record<string, unknown>, mediaType?: string): PlayerQuality => {
  if (mediaType?.split(';')[0]?.trim().toLowerCase() === 'audio/flac') return 'lossless';
  const bitrate = metadata['bitrate_kbps'];
  if (typeof bitrate !== 'number' || !Number.isFinite(bitrate) || bitrate <= 0) return 'high';
  if (bitrate <= 96) return 'low';
  if (bitrate <= 160) return 'medium';
  return 'high';
};

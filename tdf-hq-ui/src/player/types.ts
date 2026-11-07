export type PlayerQuality = 'low' | 'medium' | 'high' | 'lossless';

export interface PlayerSource {
  url: string;
  assetId?: string;
  expiresAt?: string;
  quality: PlayerQuality;
  mediaType: string;
  bitrateKbps?: number;
  preview?: boolean;
}

export interface PlayerTrack {
  id: string;
  recordingId?: string;
  releaseId?: string;
  releaseVersionId?: string;
  title: string;
  artist: string;
  artworkUrl?: string | null;
  durationMs?: number | null;
  previewEndMs?: number | null;
  loudnessLufs?: number | null;
  sources: PlayerSource[];
}

export type RepeatMode = 'off' | 'track' | 'queue' | 'release';
export type PlaybackStatus = 'idle' | 'loading' | 'playing' | 'paused' | 'buffering' | 'error' | 'unavailable';

export interface LoadPlayerTrackDetail {
  track: PlayerTrack;
  queue?: PlayerTrack[];
  autoplay?: boolean;
  startPositionMs?: number;
}

export const PLAYER_LOAD_TRACK_EVENT = 'tdf-player-load-track';
export const PLAYER_TAKEOVER_EVENT = 'tdf-global-player-takeover';
export const RADIO_TAKEOVER_EVENT = 'tdf-radio-takeover';
export const PLAYER_ANALYTICS_EVENT = 'tdf-player-analytics';

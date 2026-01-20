import { del, get, post } from './client';
import type { RadioPresenceDTO, RadioPresenceUpsert } from './types';

export interface RadioStreamDTO {
  rsId: number;
  rsName?: string | null;
  rsStreamUrl: string;
  rsCountry?: string | null;
  rsGenre?: string | null;
  rsActive: boolean;
  rsLastCheckedAt?: string | null;
}

export interface RadioStreamUpsert {
  rsuStreamUrl: string;
  rsuName?: string | null;
  rsuCountry?: string | null;
  rsuGenre?: string | null;
}

export interface RadioImportRequest {
  rirSources?: string[] | null;
  rirLimit?: number | null;
}

export interface RadioImportResult {
  rirProcessed: number;
  rirInserted: number;
  rirUpdated: number;
  rirSources: string[];
  rirFailed: number;
  rirFailedSources: string[];
}

export interface RadioTransmissionRequest {
  name?: string | null;
  genre?: string | null;
  country?: string | null;
}

export interface RadioTransmissionInfo {
  rtiStreamId: number;
  rtiStreamUrl: string;
  rtiIngestUrl: string;
  rtiStreamKey: string;
  rtiWhipUrl: string;
}

export interface RadioNowPlayingRequest {
  rnpStreamUrl: string;
}

export interface RadioNowPlayingResult {
  rnpTitle?: string | null;
  rnpArtist?: string | null;
  rnpTrack?: string | null;
}

export const RadioAPI = {
  search: (params?: { country?: string; genre?: string }) => {
    const searchParams = new URLSearchParams();
    if (params?.country?.trim()) searchParams.set('country', params.country.trim());
    if (params?.genre?.trim()) searchParams.set('genre', params.genre.trim());
    const qs = searchParams.toString();
    return get<RadioStreamDTO[]>(`/radio/streams${qs ? `?${qs}` : ''}`);
  },
  upsertActive: (payload: RadioStreamUpsert) => post<RadioStreamDTO>('/radio/streams/active', payload),
  importSources: (payload: RadioImportRequest) => post<RadioImportResult>('/radio/streams/import', payload),
  nowPlaying: (payload: RadioNowPlayingRequest) => post<RadioNowPlayingResult>('/radio/streams/now-playing', payload),
  getPresence: (partyId?: number) =>
    partyId ? get<RadioPresenceDTO | null>(`/radio/presence/${partyId}`) : get<RadioPresenceDTO | null>('/radio/presence'),
  setPresence: (payload: RadioPresenceUpsert) => post<RadioPresenceDTO>('/radio/presence', payload),
  clearPresence: () => del<void>('/radio/presence'),
  createTransmission: (payload: RadioTransmissionRequest) =>
    post<RadioTransmissionInfo>('/radio/transmissions', {
      rtrName: payload.name,
      rtrGenre: payload.genre,
      rtrCountry: payload.country,
    }),
};

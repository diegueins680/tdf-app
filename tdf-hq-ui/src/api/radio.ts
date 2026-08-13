import { del, get, post } from './client';
import type { RadioPresenceDTO, RadioPresenceUpsert } from './types';

const requirePositiveInteger = (value: number, field: string): number => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${field} debe ser un entero positivo.`);
  }
  return value;
};

export interface RadioStreamDTO {
  rsId: number;
  rsName?: string | null;
  rsStreamUrl: string;
  rsCountryId?: string | null;
  rsCountry?: string | null;
  rsGenreId?: string | null;
  rsGenre?: string | null;
  rsActive: boolean;
  rsLastCheckedAt?: string | null;
}

export interface RadioAutoStopOption {
  id: string;
  code: string;
  label: string;
  description?: string | null;
  durationMinutes: number;
  defaultForBroadcast: boolean;
  version: number;
}

export interface RadioAutoStopOptions {
  catalogId: string;
  revision: number;
  options: RadioAutoStopOption[];
}

export interface RadioStreamUpsert {
  rsuStreamUrl: string;
  rsuName?: string;
  rsuCountryId?: string;
  rsuClearCountry?: boolean;
  rsuGenreId?: string;
  rsuClearGenre?: boolean;
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
  name?: string;
  genreId?: string;
  countryId?: string;
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
  listAutoStopOptions: (locale?: string) => {
    const params = new URLSearchParams();
    if (locale?.trim()) params.set('locale', locale.trim());
    const qs = params.toString();
    return get<RadioAutoStopOptions>(`/radio/auto-stop-options${qs ? `?${qs}` : ''}`);
  },
  search: (params?: { countryId?: string; genreId?: string }) => {
    const searchParams = new URLSearchParams();
    if (params?.countryId?.trim()) searchParams.set('countryId', params.countryId.trim());
    if (params?.genreId?.trim()) searchParams.set('genreId', params.genreId.trim());
    const qs = searchParams.toString();
    return get<RadioStreamDTO[]>(`/radio/streams${qs ? `?${qs}` : ''}`);
  },
  upsertActive: (payload: RadioStreamUpsert) => post<RadioStreamDTO>('/radio/streams/active', payload),
  importSources: (payload: RadioImportRequest) => post<RadioImportResult>('/radio/streams/import', payload),
  nowPlaying: (payload: RadioNowPlayingRequest) => post<RadioNowPlayingResult>('/radio/streams/now-playing', payload),
  getPresence: (partyId?: number) => {
    if (partyId == null) {
      return get<RadioPresenceDTO | null>('/radio/presence');
    }
    const normalizedPartyId = requirePositiveInteger(partyId, 'partyId');
    return get<RadioPresenceDTO | null>(`/radio/presence/${normalizedPartyId}`);
  },
  setPresence: (payload: RadioPresenceUpsert) => post<RadioPresenceDTO>('/radio/presence', payload),
  clearPresence: () => del<void>('/radio/presence'),
  createTransmission: (payload: RadioTransmissionRequest) =>
    post<RadioTransmissionInfo>('/radio/transmissions', {
      rtrName: payload.name,
      rtrGenreId: payload.genreId,
      rtrCountryId: payload.countryId,
    }),
};

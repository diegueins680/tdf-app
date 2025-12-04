import { get, post } from './client';

export interface AuthUrlResponse {
  url: string;
}

export interface TokenExchangeIn {
  code: string;
  redirectUri?: string;
  calendarId: string;
}

export interface CalendarConfigDTO {
  configId: number;
  calendarId: string;
  syncCursor?: string | null;
  syncedAt?: string | null;
}

export interface SyncRequest {
  calendarId: string;
  from?: string;
  to?: string;
}

export interface SyncResult {
  created: number;
  updated: number;
  deleted: number;
  cursor?: string | null;
}

export interface CalendarEventDTO {
  eventId: number;
  googleId: string;
  calendarId: string;
  status: string;
  summary?: string | null;
  description?: string | null;
  location?: string | null;
  startAt?: string | null;
  endAt?: string | null;
  updatedAt?: string | null;
  htmlLink?: string | null;
  rawPayload?: any;
}

export const CalendarApi = {
  getAuthUrl: () => post<AuthUrlResponse>('/calendar/v1/auth-url', {}),
  exchangeCode: (payload: TokenExchangeIn) => post<CalendarConfigDTO>('/calendar/v1/tokens', payload),
  getConfig: (calendarId?: string) => {
    const search = new URLSearchParams();
    if (calendarId) search.set('calendarId', calendarId);
    const qs = search.toString();
    return get<CalendarConfigDTO | null>(`/calendar/v1/config${qs ? `?${qs}` : ''}`);
  },
  sync: (payload: SyncRequest) => post<SyncResult>('/calendar/v1/sync', payload),
  listEvents: (params?: { calendarId?: string; from?: string; to?: string; status?: string }) => {
    const search = new URLSearchParams();
    if (params?.calendarId) search.set('calendarId', params.calendarId);
    if (params?.from) search.set('from', params.from);
    if (params?.to) search.set('to', params.to);
    if (params?.status) search.set('status', params.status);
    const qs = search.toString();
    return get<CalendarEventDTO[]>(`/calendar/v1/events${qs ? `?${qs}` : ''}`);
  },
};

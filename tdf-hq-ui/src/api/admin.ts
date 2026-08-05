import { get, post, put, del, patch, API_BASE_URL } from './client';
import { buildAuthorizationHeader } from './authHeader';
import type {
  ArtistProfileDTO,
  ArtistProfileUpsert,
  ArtistPromoDayReportDTO,
  ArtistPromoSlotDTO,
  ArtistPromoSlotUpsert,
  ArtistReleaseDTO,
  ArtistReleaseUpsert,
  DropdownOptionDTO,
  DropdownOptionCreate,
  DropdownOptionUpdate,
} from './types';
import type { Role } from './generated/client';

export interface CreateUserPayload {
  partyId: number;
  username?: string | null;
  roles?: (Role | (string & Record<never, never>))[];
}

export interface LogEntry {
  logTimestamp: string;
  logLevel: 'info' | 'warning' | 'error';
  logMessage: string;
}

export interface UserActivity {
  id: number;
  createdAt: string;
  actorPartyId?: number | null;
  actorName: string;
  actorUsernames: string[];
  actorRoles: string[];
  entity: string;
  entityId: string;
  action: string;
  metadata?: unknown;
}

export interface AdminUser {
  userId: number;
  partyId?: number | null;
  partyName: string;
  username: string;
  primaryEmail?: string | null;
  primaryPhone?: string | null;
  whatsapp?: string | null;
  active: boolean;
  roles: string[];
  modules: string[];
}

export interface WhatsAppMessageAdmin {
  id: number;
  externalId: string;
  partyId?: number | null;
  actorPartyId?: number | null;
  senderId: string;
  senderName?: string | null;
  phoneE164?: string | null;
  contactEmail?: string | null;
  text?: string | null;
  direction: string;
  replyStatus: string;
  replyError?: string | null;
  repliedAt?: string | null;
  replyText?: string | null;
  deliveryStatus: string;
  deliveryUpdatedAt?: string | null;
  deliveryError?: string | null;
  source?: string | null;
  resendOfMessageId?: number | null;
  createdAt: string;
}

export interface UserCommunicationHistory {
  userId: number;
  partyId: number;
  partyName: string;
  username: string;
  primaryEmail?: string | null;
  primaryPhone?: string | null;
  whatsapp?: string | null;
  messages: WhatsAppMessageAdmin[];
}

export interface AdminWhatsAppSendPayload {
  message: string;
  mode: 'reply' | 'notify';
  replyToMessageId?: number | null;
}

export interface AdminWhatsAppResendPayload {
  message?: string | null;
}

export interface AdminWhatsAppSendResponse {
  status: string;
  messageId?: number | null;
  deliveryStatus: string;
  message?: string | null;
}

const normalizeNetworkError = (err: unknown, fallbackMessage: string) => {
  const wrapped = new Error(fallbackMessage);
  (wrapped as Error & { cause?: unknown }).cause = err;
  return wrapped;
};

const joinApiUrl = (path: string) => {
  if (!API_BASE_URL) return path;
  const baseHasSlash = API_BASE_URL.endsWith('/');
  const pathHasSlash = path.startsWith('/');
  if (baseHasSlash && pathHasSlash) return `${API_BASE_URL}${path.slice(1)}`;
  if (!baseHasSlash && !pathHasSlash) return `${API_BASE_URL}/${path}`;
  return `${API_BASE_URL}${path}`;
};

const buildArtistPromoDayQuery = (day: string) => {
  const params = new URLSearchParams({ day });
  return `?${params.toString()}`;
};

async function getArtistPromoPdfBlob(artistId: number, day: string): Promise<Blob> {
  const authHeader = buildAuthorizationHeader();
  let res: Response;
  try {
    res = await fetch(
      joinApiUrl(`/admin/artists/${encodeURIComponent(String(artistId))}/promotions/report/pdf${buildArtistPromoDayQuery(day)}`),
      {
        method: 'GET',
        credentials: 'include',
        headers: {
          ...(authHeader ? { Authorization: authHeader } : {}),
        },
      },
    );
  } catch (err) {
    throw normalizeNetworkError(err, 'No se pudo contactar la API del reporte PDF de promoción.');
  }
  if (!res.ok) {
    const body = await res.text().catch(() => '');
    throw new Error(body.trim() || `No se pudo generar el PDF de promoción (${res.status})`);
  }
  return res.blob();
}

export const Admin = {
  listUsers: (includeInactive?: boolean) =>
    get<AdminUser[]>(`/admin/users${includeInactive ? '?includeInactive=true' : ''}`),
  getUser: (userId: number) => get<AdminUser>(`/admin/users/${userId}`),
  getUserCommunicationHistory: (userId: number, limit = 150) =>
    get<UserCommunicationHistory>(`/admin/users/${userId}/communications?limit=${limit}`),
  sendUserWhatsApp: (userId: number, payload: AdminWhatsAppSendPayload) =>
    post<AdminWhatsAppSendResponse>(`/admin/users/${userId}/communications/whatsapp`, {
      message: payload.message,
      mode: payload.mode,
      ...(payload.replyToMessageId ? { replyToMessageId: payload.replyToMessageId } : {}),
    }),
  resendWhatsAppMessage: (messageId: number, payload: AdminWhatsAppResendPayload = {}) =>
    post<AdminWhatsAppSendResponse>(`/admin/communications/whatsapp/${messageId}/resend`, {
      ...(payload.message ? { message: payload.message } : {}),
    }),
  createUser: (payload: CreateUserPayload) =>
    post('/admin/users', {
      uacPartyId: payload.partyId,
      uacUsername: payload.username ?? null,
      uacRoles: payload.roles,
    }),
  listArtistProfiles: () => get<ArtistProfileDTO[]>('/admin/artists/profiles'),
  upsertArtistProfile: (payload: ArtistProfileUpsert) =>
    post<ArtistProfileDTO>('/admin/artists/profiles', payload),
  listArtistPromoSlots: (artistId: number, day: string) =>
    get<ArtistPromoSlotDTO[]>(`/admin/artists/${encodeURIComponent(String(artistId))}/promotions${buildArtistPromoDayQuery(day)}`),
  createArtistPromoSlot: (artistId: number, payload: ArtistPromoSlotUpsert) =>
    post<ArtistPromoSlotDTO>(`/admin/artists/${encodeURIComponent(String(artistId))}/promotions`, payload),
  updateArtistPromoSlot: (artistId: number, promotionId: number, payload: ArtistPromoSlotUpsert) =>
    put<ArtistPromoSlotDTO>(
      `/admin/artists/${encodeURIComponent(String(artistId))}/promotions/${encodeURIComponent(String(promotionId))}`,
      payload,
    ),
  deleteArtistPromoSlot: (artistId: number, promotionId: number) =>
    del(`/admin/artists/${encodeURIComponent(String(artistId))}/promotions/${encodeURIComponent(String(promotionId))}`),
  getArtistPromoDayReport: (artistId: number, day: string) =>
    get<ArtistPromoDayReportDTO>(
      `/admin/artists/${encodeURIComponent(String(artistId))}/promotions/report${buildArtistPromoDayQuery(day)}`,
    ),
  getArtistPromoPdfBlob: (artistId: number, day: string) => getArtistPromoPdfBlob(artistId, day),
  createArtistRelease: (payload: ArtistReleaseUpsert) =>
    post<ArtistReleaseDTO>('/admin/artists/releases', payload),
  updateArtistRelease: (releaseId: number, payload: ArtistReleaseUpsert) =>
    put<ArtistReleaseDTO>(`/admin/artists/releases/${releaseId}`, payload),
  getLogs: (limit?: number): Promise<LogEntry[]> => {
    const params = limit ? `?limit=${limit}` : '';
    return get(`/admin/logs${params}`);
  },
  getActivity: (limit?: number): Promise<UserActivity[]> => {
    const params = limit ? `?limit=${limit}` : '';
    return get(`/admin/activity${params}`);
  },
  clearLogs: () => del('/admin/logs'),
  listDropdowns: (category: string, includeInactive?: boolean) =>
    get<DropdownOptionDTO[]>(`/admin/dropdowns/${category}${includeInactive ? '?includeInactive=true' : ''}`),
  createDropdown: (category: string, payload: DropdownOptionCreate) =>
    post<DropdownOptionDTO>(`/admin/dropdowns/${category}`, payload),
  updateDropdown: (category: string, optionId: string, payload: DropdownOptionUpdate) =>
    patch<DropdownOptionDTO>(`/admin/dropdowns/${category}/${optionId}`, payload),
};

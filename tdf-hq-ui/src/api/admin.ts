import { get, post, put, del, patch } from './client';
import type {
  ArtistProfileDTO,
  ArtistProfileUpsert,
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
  createArtistRelease: (payload: ArtistReleaseUpsert) =>
    post<ArtistReleaseDTO>('/admin/artists/releases', payload),
  updateArtistRelease: (releaseId: number, payload: ArtistReleaseUpsert) =>
    put<ArtistReleaseDTO>(`/admin/artists/releases/${releaseId}`, payload),
  getLogs: (limit?: number): Promise<LogEntry[]> => {
    const params = limit ? `?limit=${limit}` : '';
    return get(`/admin/logs${params}`);
  },
  clearLogs: () => del('/admin/logs'),
  listDropdowns: (category: string, includeInactive?: boolean) =>
    get<DropdownOptionDTO[]>(`/admin/dropdowns/${category}${includeInactive ? '?includeInactive=true' : ''}`),
  createDropdown: (category: string, payload: DropdownOptionCreate) =>
    post<DropdownOptionDTO>(`/admin/dropdowns/${category}`, payload),
  updateDropdown: (category: string, optionId: string, payload: DropdownOptionUpdate) =>
    patch<DropdownOptionDTO>(`/admin/dropdowns/${category}/${optionId}`, payload),
};

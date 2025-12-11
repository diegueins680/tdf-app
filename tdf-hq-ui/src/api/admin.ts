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
  partyId: number;
  partyName: string;
  username: string;
  active: boolean;
  roles: string[];
  modules: string[];
}

export const Admin = {
  listUsers: (includeInactive?: boolean) =>
    get<AdminUser[]>(`/admin/users${includeInactive ? '?includeInactive=true' : ''}`),
  getUser: (userId: number) => get<AdminUser>(`/admin/users/${userId}`),
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

import { get, post, del } from './client';
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

export const Admin = {
  createUser: (payload: CreateUserPayload) =>
    post('/admin/users', {
      uacPartyId: payload.partyId,
      uacUsername: payload.username ?? null,
      uacRoles: payload.roles,
    }),
  getLogs: (limit?: number): Promise<LogEntry[]> => {
    const params = limit ? `?limit=${limit}` : '';
    return get(`/admin/logs${params}`);
  },
  clearLogs: () => del('/admin/logs'),
};

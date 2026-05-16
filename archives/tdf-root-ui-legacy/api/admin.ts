import { get, post, put } from './client';
import type { AdminConsoleView, AdminUserDTO, AuditLogEntry, RoleKey } from './types';

export const AdminApi = {
  seed: () => post<void>('/admin/seed', {}),
  auditLogs: async () => {
    try {
      return await get<AuditLogEntry[]>('/admin/audit');
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error ?? '');
      if (message.includes('404')) {
        return [];
      }
      throw error;
    }
  },
  consolePreview: () => get<AdminConsoleView>('/stubs/admin/console'),
  listUsers: () => get<AdminUserDTO[]>('/admin/users'),
  updateUserRoles: (userId: number, roles: RoleKey[]) =>
    put<AdminUserDTO>(`/admin/users/${userId}/roles`, { roles }),
};

// API client for user role management
import type { components } from './types';
import { buildAuthorizationHeader } from '../authHeader';

export type Role = components['schemas']['Role'];
export type UserSummary = components['schemas']['UserRoleSummary'];
type RoleInput = Role | (string & Record<never, never>);
export type UserRoleUpdate = { roles: RoleInput[] };

const API_BASE = import.meta.env.VITE_API_BASE ?? 'http://localhost:8080';

export class ApiClient {
  private baseUrl: string;

  constructor(baseUrl: string = API_BASE) {
    this.baseUrl = baseUrl;
  }

  private async request<T>(
    endpoint: string,
    options?: RequestInit,
  ): Promise<T> {
    const url = `${this.baseUrl}${endpoint}`;
    const authHeader = buildAuthorizationHeader();
    const response = await fetch(url, {
      ...options,
      headers: {
        'Content-Type': 'application/json',
        ...(authHeader ? { Authorization: authHeader } : {}),
        ...options?.headers,
      },
    });

    if (!response.ok) {
      const message = await response.text().catch(() => '');
      const trimmedMessage = message.trim();
      const trimmedStatus = response.statusText.trim();
      const details = trimmedMessage !== '' ? trimmedMessage : trimmedStatus !== '' ? trimmedStatus : 'Request failed';
      throw new Error(`API error: ${response.status} ${details}`);
    }

    const raw = await response.text();
    if (!raw || raw.trim() === '') {
      return undefined as T;
    }
    return JSON.parse(raw) as T;
  }

  // Get all users with their roles
  async getUsers(): Promise<UserSummary[]> {
    return this.request<UserSummary[]>('/api/users');
  }

  // Get roles for a specific user
  async getUserRoles(userId: number): Promise<Role[]> {
    return this.request<Role[]>(`/api/users/${userId}/roles`);
  }

  // Update roles for a user
  async updateUserRoles(userId: number, roles: RoleInput[]): Promise<void> {
    const body: UserRoleUpdate = { roles };
    await this.request<void>(`/api/users/${userId}/roles`, {
      method: 'PUT',
      body: JSON.stringify(body),
    });
  }
}

export const apiClient = new ApiClient();

// API client for user role management
import type { components } from './types';
import { buildAuthorizationHeader } from '../authHeader';
import { env } from '../../utils/env';

export type Role = components['schemas']['Role'];
export type UserSummary = components['schemas']['UserRoleSummary'];
type RoleInput = Role | (string & Record<never, never>);
export type UserRoleUpdate = { roles: RoleInput[] };

const API_BASE = env.read('VITE_API_BASE') ?? 'http://localhost:8080';

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
    let response: Response;
    try {
      response = await fetch(url, {
        ...options,
        headers: {
          'Content-Type': 'application/json',
          ...(authHeader ? { Authorization: authHeader } : {}),
          ...options?.headers,
        },
      });
    } catch (err) {
      const wrapped = new Error('No se pudo contactar la API.');
      (wrapped as Error & { cause?: unknown }).cause = err;
      throw wrapped;
    }

    if (!response.ok) {
      const contentType = response.headers.get('content-type') ?? '';
      const message = await response.text().catch(() => '');
      let details = message.trim();
      if (details !== '' && contentType.includes('application/json')) {
        try {
          const json = JSON.parse(details) as Record<string, unknown>;
          const msg =
            typeof json['message'] === 'string'
              ? json['message']
              : typeof json['error'] === 'string'
                ? json['error']
                : typeof json['detail'] === 'string'
                  ? json['detail']
                  : null;
          if (msg && msg.trim()) details = msg.trim();
        } catch {
          // Keep original text when body is not valid JSON.
        }
      }
      const trimmedStatus = response.statusText.trim();
      if (details === '') {
        details = trimmedStatus !== '' ? trimmedStatus : 'Request failed';
      }
      throw new Error(`API error: ${response.status} ${details}`);
    }

    const raw = await response.text().catch(() => '');
    if (!raw || raw.trim() === '') {
      return undefined as T;
    }
    try {
      return JSON.parse(raw) as T;
    } catch {
      return raw as T;
    }
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

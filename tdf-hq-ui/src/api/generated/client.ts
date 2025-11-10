// API client for user role management
import type { components } from './types';
import { getStoredSessionToken } from '../../session/SessionContext';

export type User = components['schemas']['User'];
export type PartyRole = components['schemas']['PartyRole'];
export type PartyStatus = components['schemas']['PartyStatus'];
export type UserRoleUpdate = components['schemas']['UserRoleUpdate'];

const API_BASE = import.meta.env.VITE_API_BASE || 'http://localhost:8080';

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
    const token = getStoredSessionToken();
    const response = await fetch(url, {
      ...options,
      headers: {
        'Content-Type': 'application/json',
        ...(token ? { Authorization: token.toLowerCase().startsWith('bearer ') ? token : `Bearer ${token}` } : {}),
        ...options?.headers,
      },
    });

    if (!response.ok) {
      const message = await response.text().catch(() => '');
      const details = message || response.statusText || 'Request failed';
      throw new Error(`API error: ${response.status} ${details}`);
    }

    if (response.status === 204) {
      return undefined as T;
    }

    return response.json();
  }

  // Get all users with their roles
  async getUsers(): Promise<User[]> {
    return this.request<User[]>('/api/users');
  }

  // Get roles for a specific user
  async getUserRoles(userId: number): Promise<PartyRole[]> {
    return this.request<PartyRole[]>(`/api/users/${userId}/roles`);
  }

  // Update roles for a user
  async updateUserRoles(userId: number, roles: PartyRole[]): Promise<void> {
    const body: UserRoleUpdate = { roles };
    await this.request<void>(`/api/users/${userId}/roles`, {
      method: 'PUT',
      body: JSON.stringify(body),
    });
  }
}

export const apiClient = new ApiClient();

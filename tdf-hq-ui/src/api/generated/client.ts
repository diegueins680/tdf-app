// API client for user role management
import { User, PartyRole, UserRoleUpdate } from './types';

const API_BASE = import.meta.env.VITE_API_BASE || 'http://localhost:8080';

export class ApiClient {
  private baseUrl: string;

  constructor(baseUrl: string = API_BASE) {
    this.baseUrl = baseUrl;
  }

  private async request<T>(
    endpoint: string,
    options?: RequestInit
  ): Promise<T> {
    const url = `${this.baseUrl}${endpoint}`;
    const response = await fetch(url, {
      ...options,
      headers: {
        'Content-Type': 'application/json',
        ...options?.headers,
      },
    });

    if (!response.ok) {
      throw new Error(`API error: ${response.status} ${response.statusText}`);
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

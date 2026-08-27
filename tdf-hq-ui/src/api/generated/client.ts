// API client for user role management
import type { components } from './types';
import { buildAuthorizationHeader } from '../authHeader';
import { resolveApiBase } from '../../config/apiBase';

export type Role = components['schemas']['Role'];
export type UserSummary = components['schemas']['UserRoleSummary'];
export type SecurityRole = components['schemas']['SecurityRole'];
export type SecurityPartyRoleAssignment = components['schemas']['SecurityPartyRoleAssignment'];
export type SecurityGrantRevision = components['schemas']['SecurityGrantRevision'];

export interface PartyRoleGrantDraft {
  partyId: number;
  roleId: string;
  desiredActive: boolean;
  expectedVersion: number;
  reason: string;
  sourcePlatform: string;
  correlationId: string;
}

const API_BASE = resolveApiBase();
const ABSOLUTE_URL_PATTERN = /^[a-zA-Z][a-zA-Z\d+\-.]*:\/\//;

const isJsonContentType = (contentType: string): boolean => /[/+]json(?:;|$)/i.test(contentType);

const looksLikeJsonPayload = (value: string): boolean => {
  const trimmed = value.trim();
  return trimmed.startsWith('{') || trimmed.startsWith('[');
};

const joinRequestUrl = (base: string, endpoint: string): string => {
  const normalizedBase = base.trim();
  const normalizedEndpoint = endpoint.trim();
  if (!normalizedEndpoint) return normalizedBase;
  if (!normalizedBase || ABSOLUTE_URL_PATTERN.test(normalizedEndpoint)) return normalizedEndpoint;
  const baseHasSlash = normalizedBase.endsWith('/');
  const endpointHasSlash = normalizedEndpoint.startsWith('/');
  const endpointIsQueryOrHash =
    normalizedEndpoint.startsWith('?') || normalizedEndpoint.startsWith('#');
  if (!baseHasSlash && !endpointHasSlash && !endpointIsQueryOrHash) {
    return `${normalizedBase}/${normalizedEndpoint}`;
  }
  if (baseHasSlash && endpointHasSlash) return `${normalizedBase}${normalizedEndpoint.slice(1)}`;
  return `${normalizedBase}${normalizedEndpoint}`;
};

const isFormData = (value: unknown): value is FormData =>
  typeof FormData !== 'undefined' && value instanceof FormData;

const buildRequestHeaders = (initHeaders: HeadersInit | undefined, authHeader: string | undefined): Headers => {
  const headers = new Headers();
  if (authHeader) headers.set('Authorization', authHeader);
  if (initHeaders) {
    const extraHeaders = new Headers(initHeaders);
    extraHeaders.forEach((value, key) => headers.set(key, value));
  }
  return headers;
};

const shouldSetJsonContentType = (headers: Headers, body: BodyInit | null | undefined): boolean => {
  if (headers.has('Content-Type')) return false;
  if (body === undefined || body === null) return false;
  if (isFormData(body)) return false;
  return true;
};

const extractErrorDetails = (rawBody: string, contentType: string): string => {
  const trimmedBody = rawBody.trim();
  if (trimmedBody === '') return '';
  if (!isJsonContentType(contentType) && !looksLikeJsonPayload(trimmedBody)) return trimmedBody;

  try {
    const parsed = JSON.parse(trimmedBody) as unknown;
    if (typeof parsed === 'string') {
      const parsedMessage = parsed.trim();
      return parsedMessage === '' ? trimmedBody : parsedMessage;
    }
    if (parsed && typeof parsed === 'object') {
      const payload = parsed as Record<string, unknown>;
      const candidate =
        typeof payload['message'] === 'string'
          ? payload['message']
          : typeof payload['error'] === 'string'
            ? payload['error']
            : typeof payload['detail'] === 'string'
              ? payload['detail']
              : typeof payload['title'] === 'string'
                ? payload['title']
                : null;
      if (candidate && candidate.trim() !== '') return candidate.trim();
    }
  } catch {
    return trimmedBody;
  }

  return trimmedBody;
};

export class ApiClient {
  private baseUrl: string;

  constructor(baseUrl: string = API_BASE) {
    this.baseUrl = baseUrl;
  }

  private async request<T>(
    endpoint: string,
    options?: RequestInit,
  ): Promise<T> {
    const authHeader = buildAuthorizationHeader();
    const headers = buildRequestHeaders(options?.headers, authHeader);
    if (shouldSetJsonContentType(headers, options?.body)) {
      headers.set('Content-Type', 'application/json');
    }

    const url = joinRequestUrl(this.baseUrl, endpoint);
    let response: Response;
    try {
      response = await fetch(url, {
        ...options,
        credentials: 'include',
        headers,
      });
    } catch (err) {
      const wrapped = new Error('No se pudo contactar la API.');
      (wrapped as Error & { cause?: unknown }).cause = err;
      throw wrapped;
    }

    if (!response.ok) {
      const contentType = response.headers.get('content-type') ?? '';
      const message = await response.text().catch(() => '');
      let details = extractErrorDetails(message, contentType);
      const trimmedStatus = response.statusText.trim();
      if (details === '') {
        details = trimmedStatus !== '' ? trimmedStatus : 'Request failed';
      }
      throw new Error(`API error: ${response.status} ${details}`);
    }

    const raw = await response.text().catch(() => '');
    const trimmedRaw = raw.trim();
    if (!raw || trimmedRaw === '') {
      return undefined as T;
    }
    const contentType = response.headers.get('content-type') ?? '';
    if (!isJsonContentType(contentType) && !looksLikeJsonPayload(trimmedRaw)) {
      return raw as T;
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

  async getSecurityRoles(): Promise<SecurityRole[]> {
    return this.request<SecurityRole[]>('/catalog/security/roles');
  }

  async getPartyRoleAssignments(partyId: number): Promise<SecurityPartyRoleAssignment[]> {
    const query = new URLSearchParams({ partyId: String(partyId) });
    return this.request<SecurityPartyRoleAssignment[]>(`/catalog/security/party-role-assignments?${query}`);
  }

  async createPartyRoleRevision(payload: PartyRoleGrantDraft): Promise<SecurityGrantRevision> {
    return this.request<SecurityGrantRevision>('/catalog/security/party-role-revisions', {
      method: 'POST',
      body: JSON.stringify(payload),
    });
  }

  async submitSecurityRevision(revisionId: string): Promise<SecurityGrantRevision> {
    return this.request<SecurityGrantRevision>(`/catalog/security/revisions/${encodeURIComponent(revisionId)}/submit`, {
      method: 'POST',
    });
  }
}

export const apiClient = new ApiClient();

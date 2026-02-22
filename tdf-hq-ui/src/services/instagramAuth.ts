import { InstagramOAuthAPI, type InstagramOAuthExchangeResponse } from '../api/instagramOAuth';
import { env } from '../utils/env';

type InstagramOAuthProvider = 'facebook' | 'instagram';

const APP_ID =
  env.read('VITE_META_APP_ID') ??
  env.read('VITE_FACEBOOK_APP_ID') ??
  '';
const DEFAULT_FACEBOOK_SCOPES = 'instagram_basic,pages_show_list,pages_read_engagement';
const DEFAULT_INSTAGRAM_SCOPES = 'instagram_business_basic,instagram_business_content_publish';
const STATE_KEY = 'tdf-instagram-oauth-state';
const RESULT_KEY = 'tdf-instagram-oauth-result';
const DEFAULT_RETURN_TO = '/social/instagram';

const parseScopes = (raw: string) =>
  raw
    .split(/[\s,]+/)
    .map((scope) => scope.trim())
    .filter((scope) => scope.length > 0);

const uniqueScopes = (scopes: string[]) => Array.from(new Set(scopes));

const resolveOAuthProvider = (scopes: string[]): InstagramOAuthProvider => {
  const configured = env.read('VITE_INSTAGRAM_OAUTH_PROVIDER')?.trim().toLowerCase();
  if (configured === 'facebook' || configured === 'instagram') return configured;
  return scopes.some((scope) => scope.startsWith('instagram_business_')) ? 'instagram' : 'facebook';
};

const resolveScopes = (provider: InstagramOAuthProvider, scopes: string[]) => {
  if (provider === 'instagram') {
    const businessScopes = uniqueScopes(scopes.filter((scope) => scope.startsWith('instagram_business_')));
    if (businessScopes.length > 0) return businessScopes.join(',');
    return uniqueScopes(parseScopes(DEFAULT_INSTAGRAM_SCOPES)).join(',');
  }
  return uniqueScopes(scopes).join(',');
};

const rawScopes = env.read('VITE_INSTAGRAM_SCOPES')?.trim();
const requestedScopes = parseScopes(rawScopes && rawScopes.length > 0 ? rawScopes : DEFAULT_FACEBOOK_SCOPES);
const OAUTH_PROVIDER = resolveOAuthProvider(requestedScopes);
const SCOPES = resolveScopes(OAUTH_PROVIDER, requestedScopes);

const authUrlForProvider = (provider: InstagramOAuthProvider) =>
  provider === 'instagram'
    ? 'https://www.instagram.com/oauth/authorize'
    : 'https://www.facebook.com/v20.0/dialog/oauth';

export interface InstagramOAuthStateRecord {
  state: string;
  returnTo?: string;
  issuedAt: number;
}

const getRedirectUri = () => {
  const configured = env.read('VITE_INSTAGRAM_REDIRECT_URI');
  if (configured) return configured;
  if (typeof window === 'undefined') return '';
  return `${window.location.origin}/oauth/instagram/callback`;
};

const base64Encode = (value: string) => {
  try {
    if (typeof TextEncoder !== 'undefined') {
      const bytes = new TextEncoder().encode(value);
      let binary = '';
      bytes.forEach((b) => {
        binary += String.fromCharCode(b);
      });
      return btoa(binary);
    }
  } catch {
    // fall through to legacy path
  }
  const encoded = encodeURIComponent(value).replace(/%([0-9A-F]{2})/g, (_match, hex: string) =>
    String.fromCharCode(Number.parseInt(hex, 16)),
  );
  return btoa(encoded);
};

const base64Decode = (value: string) => {
  try {
    if (typeof TextDecoder !== 'undefined') {
      const binary = atob(value);
      const bytes = Uint8Array.from(binary, (c) => c.charCodeAt(0));
      return new TextDecoder().decode(bytes);
    }
  } catch {
    // fall through to legacy path
  }
  const binary = atob(value);
  const percentEncoded = Array.from(binary)
    .map((c) => `%${c.charCodeAt(0).toString(16).padStart(2, '0')}`)
    .join('');
  return decodeURIComponent(percentEncoded);
};

const encodeState = (payload: Record<string, unknown>) => {
  const raw = JSON.stringify(payload);
  return base64Encode(raw);
};

const decodeState = (value: string) => {
  try {
    const raw = base64Decode(value);
    return JSON.parse(raw) as Record<string, string | number | undefined>;
  } catch {
    return null;
  }
};

const sanitizeReturnTo = (value?: string | null) => {
  if (!value) return '';
  const trimmed = value.trim();
  if (!trimmed) return '';
  if (!trimmed.startsWith('/') || trimmed.startsWith('//')) return '';
  return trimmed;
};

export const resolveInstagramReturnTo = (value?: string | null) => {
  const safe = sanitizeReturnTo(value);
  return safe !== '' ? safe : DEFAULT_RETURN_TO;
};

const createNonce = () => {
  if (typeof crypto !== 'undefined' && typeof crypto.getRandomValues === 'function') {
    const bytes = new Uint8Array(16);
    crypto.getRandomValues(bytes);
    return Array.from(bytes)
      .map((b) => b.toString(16).padStart(2, '0'))
      .join('');
  }
  return Math.random().toString(16).slice(2);
};

export const instagramConfigError = () => {
  if (!APP_ID) return 'Falta VITE_META_APP_ID o VITE_FACEBOOK_APP_ID.';
  if (!getRedirectUri()) return 'Falta VITE_INSTAGRAM_REDIRECT_URI.';
  return null;
};

export const buildInstagramAuthUrl = (returnTo?: string) => {
  if (!APP_ID) throw new Error('Falta configurar VITE_META_APP_ID.');
  const redirectUri = getRedirectUri();
  if (!redirectUri) throw new Error('Falta configurar VITE_INSTAGRAM_REDIRECT_URI.');
  const issuedAt = Date.now();
  const safeReturnTo = sanitizeReturnTo(returnTo);
  const returnToValue = safeReturnTo !== '' ? safeReturnTo : undefined;
  const state = encodeState({ returnTo: returnToValue, nonce: createNonce(), issuedAt });
  const stored: InstagramOAuthStateRecord = {
    state,
    returnTo: returnToValue,
    issuedAt,
  };
  sessionStorage.setItem(STATE_KEY, JSON.stringify(stored));
  const params = new URLSearchParams({
    client_id: APP_ID,
    redirect_uri: redirectUri,
    scope: SCOPES,
    response_type: 'code',
    state,
  });
  return `${authUrlForProvider(OAUTH_PROVIDER)}?${params.toString()}`;
};

export const consumeInstagramState = (): InstagramOAuthStateRecord | null => {
  if (typeof window === 'undefined') return null;
  const raw = sessionStorage.getItem(STATE_KEY);
  if (raw) sessionStorage.removeItem(STATE_KEY);
  if (!raw) return null;
  try {
    return JSON.parse(raw) as InstagramOAuthStateRecord;
  } catch {
    return null;
  }
};

export const parseInstagramState = (state: string | null) => {
  if (!state) return null;
  return decodeState(state);
};

export const exchangeInstagramCode = async (code: string) => {
  const redirectUri = getRedirectUri();
  return InstagramOAuthAPI.exchange({ code, redirectUri });
};

export const storeInstagramResult = (result: InstagramOAuthExchangeResponse) => {
  if (typeof window === 'undefined') return;
  localStorage.setItem(RESULT_KEY, JSON.stringify(result));
};

export const getStoredInstagramResult = (): InstagramOAuthExchangeResponse | null => {
  if (typeof window === 'undefined') return null;
  const raw = localStorage.getItem(RESULT_KEY);
  if (!raw) return null;
  try {
    return JSON.parse(raw) as InstagramOAuthExchangeResponse;
  } catch {
    return null;
  }
};

export const clearInstagramResult = () => {
  if (typeof window === 'undefined') return;
  localStorage.removeItem(RESULT_KEY);
};

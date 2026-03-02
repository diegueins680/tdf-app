import { InstagramOAuthAPI, type InstagramOAuthExchangeResponse, type InstagramOAuthPage } from '../api/instagramOAuth';
import { env } from '../utils/env';

type InstagramOAuthProvider = 'facebook' | 'instagram';

const FACEBOOK_APP_ID =
  env.read('VITE_META_APP_ID') ??
  env.read('VITE_FACEBOOK_APP_ID') ??
  '';
const INSTAGRAM_APP_ID =
  env.read('VITE_INSTAGRAM_CLIENT_ID') ??
  env.read('VITE_INSTAGRAM_APP_ID') ??
  '';
const DEFAULT_FACEBOOK_SCOPES = 'instagram_basic,instagram_manage_messages,pages_show_list,pages_read_engagement';
const DEFAULT_INSTAGRAM_SCOPES = 'instagram_basic,instagram_manage_messages,instagram_business_basic,instagram_business_manage_messages';
const STATE_KEY = 'tdf-instagram-oauth-state';
const RESULT_KEY = 'tdf-instagram-oauth-result';
const REVIEW_ASSET_KEY = 'tdf-instagram-review-asset';
const DEFAULT_RETURN_TO = '/social/instagram';

const parseScopes = (raw: string) =>
  raw
    .split(/[\s,]+/)
    .map((scope) => scope.trim())
    .filter((scope) => scope.length > 0);

const uniqueScopes = (scopes: string[]) => Array.from(new Set(scopes));

const hasBusinessScopes = (scopes: string[]) => scopes.some((scope) => scope.startsWith('instagram_business_'));

const hasClassicInstagramScopes = (scopes: string[]) =>
  scopes.some((scope) => scope === 'instagram_basic' || scope === 'instagram_manage_messages');

const resolveOAuthProvider = (scopes: string[]): InstagramOAuthProvider => {
  const configured = env.read('VITE_INSTAGRAM_OAUTH_PROVIDER')?.trim().toLowerCase();
  if (configured === 'facebook') return 'facebook';
  if (configured === 'instagram') {
    // Instagram Login does not accept the classic facebook-login instagram_* scopes.
    if (hasClassicInstagramScopes(scopes)) return 'facebook';
    return INSTAGRAM_APP_ID ? 'instagram' : 'facebook';
  }
  // Default behavior: use Facebook Login for mixed/classic scopes; Instagram Login only for pure business scopes.
  if (hasBusinessScopes(scopes) && !hasClassicInstagramScopes(scopes) && INSTAGRAM_APP_ID) return 'instagram';
  return 'facebook';
};

const resolveScopes = (provider: InstagramOAuthProvider, scopes: string[]) => {
  if (provider === 'instagram') {
    const businessScopes = uniqueScopes(scopes.filter((scope) => scope.startsWith('instagram_business_')));
    if (businessScopes.length > 0) return businessScopes.join(',');
    return uniqueScopes(parseScopes(DEFAULT_INSTAGRAM_SCOPES)).join(',');
  }
  const facebookScopes = uniqueScopes(scopes.filter((scope) => !scope.startsWith('instagram_business_')));
  return facebookScopes.join(',');
};

const rawScopes = env.read('VITE_INSTAGRAM_SCOPES')?.trim();
const requestedScopes = parseScopes(rawScopes && rawScopes.length > 0 ? rawScopes : DEFAULT_FACEBOOK_SCOPES);
const OAUTH_PROVIDER = resolveOAuthProvider(requestedScopes);
const SCOPES = resolveScopes(OAUTH_PROVIDER, requestedScopes);

export interface MetaReviewAssetSelection {
  pageId: string;
  pageName: string;
  instagramUserId?: string | null;
  instagramUsername?: string | null;
  selectedAt: number;
}

const authUrlForProvider = (provider: InstagramOAuthProvider) =>
  provider === 'instagram'
    ? 'https://www.instagram.com/oauth/authorize'
    : 'https://www.facebook.com/v20.0/dialog/oauth';

const resolveClientId = (provider: InstagramOAuthProvider) =>
  provider === 'instagram'
    ? INSTAGRAM_APP_ID
    : FACEBOOK_APP_ID;

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
  if (!resolveClientId(OAUTH_PROVIDER)) {
    if (OAUTH_PROVIDER === 'instagram') {
      return 'Falta VITE_INSTAGRAM_CLIENT_ID (o VITE_INSTAGRAM_APP_ID) para Instagram Login.';
    }
    return 'Falta VITE_META_APP_ID o VITE_FACEBOOK_APP_ID.';
  }
  if (!getRedirectUri()) return 'Falta VITE_INSTAGRAM_REDIRECT_URI.';
  return null;
};

export const buildInstagramAuthUrl = (returnTo?: string) => {
  const clientId = resolveClientId(OAUTH_PROVIDER);
  if (!clientId) {
    if (OAUTH_PROVIDER === 'instagram') {
      throw new Error('Falta configurar VITE_INSTAGRAM_CLIENT_ID (o VITE_INSTAGRAM_APP_ID).');
    }
    throw new Error('Falta configurar VITE_META_APP_ID.');
  }
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
    client_id: clientId,
    redirect_uri: redirectUri,
    scope: SCOPES,
    response_type: 'code',
    state,
  });
  return `${authUrlForProvider(OAUTH_PROVIDER)}?${params.toString()}`;
};

export const getInstagramRequestedScopes = () => parseScopes(SCOPES);
export const getInstagramOAuthProvider = (): InstagramOAuthProvider => OAUTH_PROVIDER;

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

const toMetaReviewAssetSelection = (page: InstagramOAuthPage): MetaReviewAssetSelection => ({
  pageId: page.pageId,
  pageName: page.pageName,
  instagramUserId: page.instagramUserId ?? null,
  instagramUsername: page.instagramUsername ?? null,
  selectedAt: Date.now(),
});

const parseMetaReviewAssetSelection = (raw: string): MetaReviewAssetSelection | null => {
  try {
    const parsed = JSON.parse(raw) as Partial<MetaReviewAssetSelection>;
    if (!parsed || typeof parsed !== 'object') return null;
    if (typeof parsed.pageId !== 'string' || typeof parsed.pageName !== 'string') return null;
    return {
      pageId: parsed.pageId,
      pageName: parsed.pageName,
      instagramUserId: typeof parsed.instagramUserId === 'string' ? parsed.instagramUserId : null,
      instagramUsername: typeof parsed.instagramUsername === 'string' ? parsed.instagramUsername : null,
      selectedAt: typeof parsed.selectedAt === 'number' ? parsed.selectedAt : Date.now(),
    };
  } catch {
    return null;
  }
};

export const setMetaReviewAssetSelection = (page: InstagramOAuthPage | null) => {
  if (typeof window === 'undefined') return;
  if (!page) {
    localStorage.removeItem(REVIEW_ASSET_KEY);
    return;
  }
  localStorage.setItem(REVIEW_ASSET_KEY, JSON.stringify(toMetaReviewAssetSelection(page)));
};

export const getMetaReviewAssetSelection = (): MetaReviewAssetSelection | null => {
  if (typeof window === 'undefined') return null;
  const raw = localStorage.getItem(REVIEW_ASSET_KEY);
  if (!raw) return null;
  return parseMetaReviewAssetSelection(raw);
};

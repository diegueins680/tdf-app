import { InstagramOAuthAPI, type InstagramOAuthExchangeResponse } from '../api/instagramOAuth';
import { env } from '../utils/env';

const APP_ID =
  env.read('VITE_META_APP_ID') ??
  env.read('VITE_FACEBOOK_APP_ID') ??
  '';
const SCOPES =
  env.read('VITE_INSTAGRAM_SCOPES') ??
  'instagram_basic,pages_show_list,pages_read_engagement';
const STATE_KEY = 'tdf-instagram-oauth-state';
const RESULT_KEY = 'tdf-instagram-oauth-result';

const getRedirectUri = () => {
  const configured = env.read('VITE_INSTAGRAM_REDIRECT_URI');
  if (configured) return configured;
  if (typeof window === 'undefined') return '';
  return `${window.location.origin}/oauth/instagram/callback`;
};

const encodeState = (payload: Record<string, string | undefined>) => {
  const raw = JSON.stringify(payload);
  return btoa(raw);
};

const decodeState = (value: string) => {
  try {
    const raw = atob(value);
    return JSON.parse(raw) as Record<string, string | undefined>;
  } catch {
    return null;
  }
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
  const state = encodeState({ returnTo: returnTo ?? '' });
  sessionStorage.setItem(STATE_KEY, state);
  const params = new URLSearchParams({
    client_id: APP_ID,
    redirect_uri: redirectUri,
    scope: SCOPES,
    response_type: 'code',
    state,
  });
  return `https://www.facebook.com/v20.0/dialog/oauth?${params.toString()}`;
};

export const consumeInstagramState = () => {
  if (typeof window === 'undefined') return null;
  const stored = sessionStorage.getItem(STATE_KEY);
  if (stored) sessionStorage.removeItem(STATE_KEY);
  return stored;
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

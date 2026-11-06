import { del, post } from '../api/client';
import { getActiveSession } from '../session/SessionContext';
import { getAuthSessionEpoch } from '../session/authEvents';
import { UploadFailure } from '../api/uploadFailure';
import { isDriveConnectionHandle, retireLegacyDriveCredentials } from './storageCredentials';
export { isDriveConnectionHandle } from './storageCredentials';

const TOKEN_KEY = 'tdf-google-drive-token';
const CODE_VERIFIER_KEY = 'tdf-google-drive-code-verifier';
const STATE_KEY = 'tdf-google-drive-state';
const scopedTokenKey = () => { const owner = getActiveSession()?.partyId; return owner ? `${TOKEN_KEY}:${owner}` : null; };

interface StoredToken {
  ownerPartyId?: number;
  ownerEpoch?: number;
  accessToken: string;
  refreshToken?: string;
  expiresAt: number; // epoch ms
}

interface DriveTokenResponse {
  accessToken: string;
  refreshToken?: string;
  expiresIn: number;
  tokenType?: string;
}

interface DriveOAuthStatePayload {
  nonce: string;
  ownerPartyId?: number;
  expiresAt?: number;
  returnTo?: string;
}

export interface DriveFileInfo {
  id: string;
  name: string;
  webViewLink?: string;
  webContentLink?: string;
  publicUrl?: string;
}

const GOOGLE_AUTH_ENDPOINT = 'https://accounts.google.com/o/oauth2/v2/auth';

const getClientId = () => import.meta.env?.VITE_GOOGLE_DRIVE_CLIENT_ID;
const getRedirectUri = () => import.meta.env?.VITE_GOOGLE_DRIVE_REDIRECT_URI;
const createNonce = () => crypto.randomUUID().replace(/-/g, '');

const sanitizeReturnTo = (value?: string | null) => {
  if (!value) return undefined;
  const trimmed = value.trim();
  if (!trimmed || !trimmed.startsWith('/') || trimmed.startsWith('//') || [...trimmed].some((char) => char === '\\' || char.charCodeAt(0) < 32 || char.charCodeAt(0) === 127)) return undefined;
  return trimmed;
};

const buildDriveState = (returnTo?: string) =>
  encodeURIComponent(
    JSON.stringify({
      nonce: createNonce(),
      ownerPartyId: getActiveSession()?.partyId,
      expiresAt: Date.now() + 10 * 60_000,
      returnTo: sanitizeReturnTo(returnTo),
    } satisfies DriveOAuthStatePayload),
  );

export const parseDriveState = (value: string | null | undefined): DriveOAuthStatePayload | null => {
  if (!value) return null;

  try {
    const parsed = JSON.parse(decodeURIComponent(value)) as Partial<DriveOAuthStatePayload>;
    if (!parsed || typeof parsed !== 'object') return null;
    if (typeof parsed.nonce !== 'string' || !/^[a-f0-9]{32}$/.test(parsed.nonce)) return null;
    if (parsed.expiresAt !== undefined && (typeof parsed.expiresAt !== 'number' || parsed.expiresAt < Date.now())) return null;
    return {
      nonce: parsed.nonce,
      ownerPartyId: parsed.ownerPartyId,
      expiresAt: parsed.expiresAt,
      returnTo: sanitizeReturnTo(parsed.returnTo),
    };
  } catch {
    return null;
  }
};

export const driveConfigError = () => {
  const clientId = getClientId();
  const redirect = getRedirectUri();
  if (!clientId || !redirect) {
    return 'Faltan VITE_GOOGLE_DRIVE_CLIENT_ID o VITE_GOOGLE_DRIVE_REDIRECT_URI';
  }
  return null;
};

const base64UrlEncode = (buffer: ArrayBuffer | Uint8Array) => {
  const view = buffer instanceof Uint8Array ? buffer : new Uint8Array(buffer);
  return btoa(String.fromCharCode(...view))
    .replace(/\+/g, '-')
    .replace(/\//g, '_')
    .replace(/=+$/, '');
};

const sha256 = async (buffer: Uint8Array): Promise<ArrayBuffer> => {
  return crypto.subtle.digest('SHA-256', buffer as BufferSource);
};

export const generatePkce = async () => {
  const verifierBytes = crypto.getRandomValues(new Uint8Array(32));
  const verifier = base64UrlEncode(verifierBytes);
  const challengeBuffer = await sha256(new TextEncoder().encode(verifier));
  const challenge = base64UrlEncode(challengeBuffer);
  localStorage.setItem(CODE_VERIFIER_KEY, verifier);
  return { verifier, challenge };
};

const storeState = (val: string) => localStorage.setItem(STATE_KEY, val);
const consumeState = (expected?: string | null) => {
  const val = localStorage.getItem(STATE_KEY);
  if (expected !== undefined && val !== expected) return null;
  if (val) localStorage.removeItem(STATE_KEY);
  return val;
};

export const getStoredToken = (): StoredToken | null => {
  retireLegacyDriveCredentials();
  const invalidateStoredToken = () => {
    try {
      const key = scopedTokenKey();
      if (key) localStorage.removeItem(key);
    } catch {
      // ignore storage cleanup failures
    }
  };
  try {
    localStorage.removeItem(TOKEN_KEY); // retire the unbound legacy credential
    const key = scopedTokenKey();
    const raw = key ? localStorage.getItem(key) : null;
    if (!raw) return null;
    const parsed = JSON.parse(raw) as unknown;
    if (!parsed || typeof parsed !== 'object') {
      invalidateStoredToken();
      return null;
    }
    const value = parsed as Record<string, unknown>;
    const ownerPartyId = getActiveSession()?.partyId;
    if (!ownerPartyId || value['ownerPartyId'] !== ownerPartyId) { invalidateStoredToken(); return null; }
    const accessTokenRaw = value['accessToken'];
    const refreshTokenRaw = value['refreshToken'];
    const expiresAtRaw = value['expiresAt'];
    if (!isDriveConnectionHandle(accessTokenRaw) || (refreshTokenRaw !== undefined && refreshTokenRaw !== accessTokenRaw)) {
      invalidateStoredToken();
      return null;
    }
    if (typeof expiresAtRaw !== 'number' || !Number.isFinite(expiresAtRaw)) {
      invalidateStoredToken();
      return null;
    }
    return {
      ownerPartyId,
      ownerEpoch: getAuthSessionEpoch(),
      accessToken: accessTokenRaw,
      refreshToken: typeof refreshTokenRaw === 'string' ? refreshTokenRaw : undefined,
      expiresAt: expiresAtRaw,
    };
  } catch {
    invalidateStoredToken();
    return null;
  }
};

export const storeToken = (token: StoredToken) => {
  if (!isDriveConnectionHandle(token.accessToken) || (token.refreshToken !== undefined && token.refreshToken !== token.accessToken)) throw new UploadFailure('storage_reconnect_required', 409, 'reconnect');
  const ownerPartyId = getActiveSession()?.partyId;
  if (!ownerPartyId || (token.ownerPartyId !== undefined && token.ownerPartyId !== ownerPartyId) || (token.ownerEpoch !== undefined && token.ownerEpoch !== getAuthSessionEpoch())) throw new UploadFailure('storage_reconnect_required', 409, 'reconnect');
  const key = scopedTokenKey();
  if (!key) throw new UploadFailure('storage_reconnect_required', 409, 'reconnect');
  localStorage.setItem(key, JSON.stringify({ ownerPartyId, ownerEpoch: token.ownerEpoch,
    accessToken: token.accessToken, refreshToken: token.refreshToken, expiresAt: token.expiresAt }));
};

export const disconnectDrive = async (): Promise<void> => {
  const owner = getActiveSession()?.partyId;
  const epoch = getAuthSessionEpoch();
  await del<void>('/drive/token');
  if (getActiveSession()?.partyId === owner && getAuthSessionEpoch() === epoch) clearToken();
};

export const clearToken = () => { const key = scopedTokenKey(); if (key) localStorage.removeItem(key); };

export const buildAuthUrl = async (state?: string) => {
  const clientId = getClientId();
  const redirectUri = getRedirectUri();
  if (!clientId || !redirectUri) throw new Error('Faltan VITE_GOOGLE_DRIVE_CLIENT_ID o VITE_GOOGLE_DRIVE_REDIRECT_URI');
  const { challenge } = await generatePkce();
  const authState = buildDriveState(state);
  const params = new URLSearchParams({
    client_id: clientId,
    redirect_uri: redirectUri,
    response_type: 'code',
    access_type: 'offline',
    prompt: 'consent',
    scope: 'https://www.googleapis.com/auth/drive.file',
    code_challenge: challenge,
    code_challenge_method: 'S256',
    state: authState,
  });
  storeState(authState);
  return `${GOOGLE_AUTH_ENDPOINT}?${params.toString()}`;
};

export const exchangeCodeForToken = async (code: string) => {
  const ownerPartyId = getActiveSession()?.partyId;
  const ownerEpoch = getAuthSessionEpoch();
  const clientId = getClientId();
  const redirectUri = getRedirectUri();
  if (!clientId || !redirectUri) throw new Error('Faltan VITE_GOOGLE_DRIVE_CLIENT_ID o VITE_GOOGLE_DRIVE_REDIRECT_URI');
  const verifier = localStorage.getItem(CODE_VERIFIER_KEY);
  if (!verifier) throw new Error('No hay code_verifier para intercambio OAuth');
  localStorage.removeItem(CODE_VERIFIER_KEY);

  const data = await post<DriveTokenResponse>('/drive/token', {
    code,
    codeVerifier: verifier,
    redirectUri,
  });
  const expiresAt = Date.now() + data.expiresIn * 1000 - 60_000;
  const stored: StoredToken = {
    ownerPartyId, ownerEpoch,
    accessToken: data.accessToken,
    refreshToken: data.refreshToken,
    expiresAt,
  };
  storeToken(stored);
  return stored;
};

let refreshInFlight: { owner: number; epoch: number; promise: Promise<StoredToken> } | null = null;

export const refreshAccessToken = (refreshToken: string): Promise<StoredToken> => {
  const owner = getActiveSession()?.partyId;
  const epoch = getAuthSessionEpoch();
  if (!owner) return Promise.reject(new UploadFailure('storage_reconnect_required', 409, 'reconnect'));
  if (refreshInFlight?.owner === owner && refreshInFlight.epoch === epoch) return refreshInFlight.promise;
  const tokenKey = scopedTokenKey();
  const connectionSnapshot = tokenKey ? localStorage.getItem(tokenKey) : null;
  const promise = (async () => {
    const data = await post<DriveTokenResponse>('/drive/token/refresh', { refreshToken });
    if (getActiveSession()?.partyId !== owner || getAuthSessionEpoch() !== epoch || !tokenKey || localStorage.getItem(tokenKey) !== connectionSnapshot) {
      throw new UploadFailure('storage_reconnect_required', 409, 'reconnect');
    }
    const stored: StoredToken = {
      ownerPartyId: owner, ownerEpoch: epoch, accessToken: data.accessToken,
      refreshToken: data.refreshToken ?? refreshToken,
      expiresAt: Date.now() + data.expiresIn * 1000 - 60_000,
    };
    storeToken(stored);
    return stored;
  })();
  refreshInFlight = { owner, epoch, promise };
  void promise.finally(() => { if (refreshInFlight?.promise === promise) refreshInFlight = null; }).catch(() => undefined);
  return promise;
};

export const ensureAccessToken = async (): Promise<StoredToken> => {
  const stored = getStoredToken();
  if (stored && stored.expiresAt > Date.now()) return stored;
  if (stored?.refreshToken) {
    return refreshAccessToken(stored.refreshToken);
  }
  throw new Error('Necesitas iniciar sesión con Google Drive');
};

export type DriveExportMode = 'download' | 'view';

export const buildPublicContentUrl = (
  fileId: string,
  resourceKey?: string | null,
  mode: DriveExportMode = 'download',
) => {
  const params = new URLSearchParams({ export: mode, id: fileId });
  if (resourceKey) {
    params.set('resourcekey', resourceKey);
  }
  return `https://drive.google.com/uc?${params.toString()}`;
};

export const consumeDriveState = consumeState;

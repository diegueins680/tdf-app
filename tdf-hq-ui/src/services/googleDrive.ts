import { nanoid } from 'nanoid';

const TOKEN_KEY = 'tdf-google-drive-token';
const CODE_VERIFIER_KEY = 'tdf-google-drive-code-verifier';
const STATE_KEY = 'tdf-google-drive-state';

interface StoredToken {
  accessToken: string;
  refreshToken?: string;
  expiresAt: number; // epoch ms
}

export interface DriveFileInfo {
  id: string;
  name: string;
  webViewLink?: string;
  webContentLink?: string;
}

const GOOGLE_AUTH_ENDPOINT = 'https://accounts.google.com/o/oauth2/v2/auth';
const GOOGLE_TOKEN_ENDPOINT = 'https://oauth2.googleapis.com/token';
const GOOGLE_FILES_ENDPOINT = 'https://www.googleapis.com/drive/v3/files';
const GOOGLE_UPLOAD_ENDPOINT = 'https://www.googleapis.com/upload/drive/v3/files?uploadType=multipart';

const getClientId = () =>
  (import.meta.env as Record<string, string | undefined>)['VITE_GOOGLE_DRIVE_CLIENT_ID'];
const getRedirectUri = () =>
  (import.meta.env as Record<string, string | undefined>)['VITE_GOOGLE_DRIVE_REDIRECT_URI'];
const getFolderId = () => (import.meta.env as Record<string, string | undefined>)['VITE_GOOGLE_DRIVE_FOLDER_ID'];

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
const consumeState = () => {
  const val = localStorage.getItem(STATE_KEY);
  if (val) localStorage.removeItem(STATE_KEY);
  return val;
};

export const getStoredToken = (): StoredToken | null => {
  try {
    const raw = localStorage.getItem(TOKEN_KEY);
    if (!raw) return null;
    const parsed = JSON.parse(raw) as StoredToken;
    if (!parsed.accessToken || !parsed.expiresAt) return null;
    return parsed;
  } catch {
    return null;
  }
};

export const storeToken = (token: StoredToken) => {
  localStorage.setItem(TOKEN_KEY, JSON.stringify(token));
};

export const clearToken = () => localStorage.removeItem(TOKEN_KEY);

export const buildAuthUrl = async (state?: string) => {
  const clientId = getClientId();
  const redirectUri = getRedirectUri();
  if (!clientId || !redirectUri) throw new Error('Faltan VITE_GOOGLE_DRIVE_CLIENT_ID o VITE_GOOGLE_DRIVE_REDIRECT_URI');
  const { challenge } = await generatePkce();
  const params = new URLSearchParams({
    client_id: clientId,
    redirect_uri: redirectUri,
    response_type: 'code',
    access_type: 'offline',
    prompt: 'consent',
    scope: 'https://www.googleapis.com/auth/drive.file',
    code_challenge: challenge,
    code_challenge_method: 'S256',
  });
  if (state) {
    params.set('state', state);
    storeState(state);
  }
  return `${GOOGLE_AUTH_ENDPOINT}?${params.toString()}`;
};

export const exchangeCodeForToken = async (code: string) => {
  const clientId = getClientId();
  const redirectUri = getRedirectUri();
  if (!clientId || !redirectUri) throw new Error('Faltan VITE_GOOGLE_DRIVE_CLIENT_ID o VITE_GOOGLE_DRIVE_REDIRECT_URI');
  const verifier = localStorage.getItem(CODE_VERIFIER_KEY);
  if (!verifier) throw new Error('No hay code_verifier para intercambio OAuth');
  localStorage.removeItem(CODE_VERIFIER_KEY);

  const body = new URLSearchParams({
    client_id: clientId,
    grant_type: 'authorization_code',
    code,
    redirect_uri: redirectUri,
    code_verifier: verifier,
  });

  const res = await fetch(GOOGLE_TOKEN_ENDPOINT, {
    method: 'POST',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    body,
  });
  if (!res.ok) throw new Error('No se pudo obtener token de Google Drive');
  const data = (await res.json()) as {
    access_token: string;
    expires_in: number;
    refresh_token?: string;
    token_type: string;
  };
  const expiresAt = Date.now() + data.expires_in * 1000 - 60_000;
  const stored: StoredToken = {
    accessToken: data.access_token,
    refreshToken: data.refresh_token,
    expiresAt,
  };
  storeToken(stored);
  return stored;
};

export const refreshAccessToken = async (refreshToken: string) => {
  const clientId = getClientId();
  if (!clientId) throw new Error('Falta VITE_GOOGLE_DRIVE_CLIENT_ID');
  const body = new URLSearchParams({
    client_id: clientId,
    grant_type: 'refresh_token',
    refresh_token: refreshToken,
  });
  const res = await fetch(GOOGLE_TOKEN_ENDPOINT, {
    method: 'POST',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    body,
  });
  if (!res.ok) throw new Error('No se pudo refrescar token de Google Drive');
  const data = (await res.json()) as {
    access_token: string;
    expires_in: number;
  };
  const expiresAt = Date.now() + data.expires_in * 1000 - 60_000;
  const stored: StoredToken = { accessToken: data.access_token, refreshToken, expiresAt };
  storeToken(stored);
  return stored;
};

export const ensureAccessToken = async (): Promise<StoredToken> => {
  const stored = getStoredToken();
  if (stored && stored.expiresAt > Date.now()) return stored;
  if (stored?.refreshToken) {
    return refreshAccessToken(stored.refreshToken);
  }
  throw new Error('Necesitas iniciar sesión con Google Drive');
};

export const makeFilePublic = async (fileId: string): Promise<void> => {
  const token = await ensureAccessToken();
  await fetch(`https://www.googleapis.com/drive/v3/files/${fileId}/permissions`, {
    method: 'POST',
    headers: {
      Authorization: `Bearer ${token.accessToken}`,
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({ role: 'reader', type: 'anyone' }),
  }).catch(() => undefined);
};

export const buildPublicContentUrl = (fileId: string) =>
  `https://drive.google.com/uc?export=download&id=${fileId}`;

export const uploadToDrive = async (
  file: File,
  onProgress?: (pct: number) => void,
): Promise<DriveFileInfo> => {
  const token = await ensureAccessToken();
  const boundary = `-------tdf-${nanoid()}`;
  const metadata = {
    name: file.name,
    mimeType: file.type,
    ...(getFolderId() ? { parents: [getFolderId()] } : {}),
  };

  const bodyParts = [
    `--${boundary}`,
    'Content-Type: application/json; charset=UTF-8',
    '',
    JSON.stringify(metadata),
    `--${boundary}`,
    `Content-Type: ${file.type || 'application/octet-stream'}`,
    '',
  ];
  const ending = `\r\n--${boundary}--`;

  const reader = new FileReader();
  const fileContent = await new Promise<ArrayBuffer>((resolve, reject) => {
    reader.onerror = () => reject(new Error('No se pudo leer el archivo'));
    reader.onload = () => resolve(reader.result as ArrayBuffer);
    reader.readAsArrayBuffer(file);
  });

  const uint8Body = new Uint8Array([
    ...new TextEncoder().encode(bodyParts.join('\r\n') + '\r\n'),
    ...new Uint8Array(fileContent),
    ...new TextEncoder().encode(ending),
  ]);

  const xhr = new XMLHttpRequest();
  const promise = new Promise<DriveFileInfo>((resolve, reject) => {
    xhr.open('POST', GOOGLE_UPLOAD_ENDPOINT);
    xhr.setRequestHeader('Authorization', `Bearer ${token.accessToken}`);
    xhr.setRequestHeader('Content-Type', `multipart/related; boundary=${boundary}`);
    xhr.onreadystatechange = () => {
      if (xhr.readyState === XMLHttpRequest.DONE) {
        if (xhr.status >= 200 && xhr.status < 300) {
          try {
            const resp = JSON.parse(xhr.responseText);
            resolve(resp as DriveFileInfo);
          } catch (err) {
            reject(err instanceof Error ? err : new Error('No se pudo parsear la respuesta de Drive'));
          }
        } else {
          reject(new Error(`Upload failed (${xhr.status})`));
        }
      }
    };
    xhr.upload.onprogress = (evt) => {
      if (!evt.lengthComputable) return;
      const pct = Math.round((evt.loaded / evt.total) * 100);
      onProgress?.(pct);
    };
    xhr.send(uint8Body);
  });

  const fileInfo = await promise;

  // Make file link-readable
  try {
    await fetch(`${GOOGLE_FILES_ENDPOINT}/${fileInfo.id}/permissions`, {
      method: 'POST',
      headers: {
        Authorization: `Bearer ${token.accessToken}`,
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        role: 'reader',
        type: 'anyone',
      }),
    });
  } catch {
    // ignore permission failures; file remains private
  }
  return fileInfo;
};

export const consumeDriveState = consumeState;

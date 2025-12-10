import { useCallback, useEffect, useState } from 'react';
import {
  buildAuthUrl,
  clearToken,
  consumeDriveState,
  ensureAccessToken,
  exchangeCodeForToken,
  getStoredToken,
  storeToken,
} from '../services/googleDrive';

export type DriveAuthStatus = 'idle' | 'authenticating' | 'ready' | 'error';

export function useGoogleDriveAuth() {
  const [status, setStatus] = useState<DriveAuthStatus>(() => (getStoredToken() ? 'ready' : 'idle'));
  const [error, setError] = useState<string | null>(null);

  const startAuth = useCallback(async (returnTo?: string) => {
    setError(null);
    setStatus('authenticating');
    try {
      const state = returnTo ?? window.location.pathname + window.location.search;
      const url = await buildAuthUrl(state);
      window.location.assign(url);
    } catch (err) {
      setStatus('idle');
      setError(err instanceof Error ? err.message : 'No se pudo iniciar la autenticación con Drive.');
      throw err;
    }
  }, []);

  const ensureToken = useCallback(async () => {
    try {
      const token = await ensureAccessToken();
      storeToken(token);
      setStatus('ready');
      return token;
    } catch (err) {
      setStatus('idle');
      throw err;
    }
  }, []);

  const resetAuth = useCallback(() => {
    clearToken();
    setStatus('idle');
    setError(null);
  }, []);

  useEffect(() => {
    if (status === 'ready') return;
    const stored = getStoredToken();
    if (stored) {
      void ensureToken();
    }
  }, [ensureToken, status]);

  return { status, error, startAuth, ensureToken, resetAuth };
}

export function useGoogleDriveCallback() {
  const [result, setResult] = useState<{ ok: boolean; message?: string; state?: string }>({ ok: false });

  useEffect(() => {
    const search = new URLSearchParams(window.location.search);
    const code = search.get('code');
    const error = search.get('error');
    const state = search.get('state') ?? consumeDriveState() ?? undefined;
    if (error) {
      setResult({ ok: false, message: error, state });
      return;
    }
    if (!code) {
      setResult({ ok: false, message: 'No se recibió código de Google', state });
      return;
    }
    void (async () => {
      try {
        const token = await exchangeCodeForToken(code);
        storeToken(token);
        setResult({ ok: true, state });
      } catch (err) {
        setResult({ ok: false, message: err instanceof Error ? err.message : 'No se pudo guardar el token', state });
      }
    })();
  }, []);

  return result;
}

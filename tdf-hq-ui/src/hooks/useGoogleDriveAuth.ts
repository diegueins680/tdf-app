import { useCallback, useEffect, useState } from 'react';
import {
  buildAuthUrl,
  clearToken,
  driveConfigError,
  consumeDriveState,
  ensureAccessToken,
  exchangeCodeForToken,
  getStoredToken,
  storeToken,
} from '../services/googleDrive';

export type DriveAuthStatus = 'idle' | 'authenticating' | 'ready' | 'error';

interface DriveAuthOptions {
  enabled?: boolean;
}

export function useGoogleDriveAuth(options: DriveAuthOptions = {}) {
  const enabled = options.enabled ?? true;
  const [status, setStatus] = useState<DriveAuthStatus>(() => (enabled && getStoredToken() ? 'ready' : 'idle'));
  const [error, setError] = useState<string | null>(null);

  const startAuth = useCallback(async (returnTo?: string) => {
    if (!enabled) {
      throw new Error('La autenticación de Drive está deshabilitada.');
    }
    setError(null);
    const configWarning = driveConfigError();
    if (configWarning) {
      setStatus('idle');
      setError(configWarning);
      throw new Error(configWarning);
    }
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
  }, [enabled]);

  const ensureToken = useCallback(async () => {
    if (!enabled) {
      throw new Error('La autenticación de Drive está deshabilitada.');
    }
    try {
      const token = await ensureAccessToken();
      storeToken(token);
      setStatus('ready');
      return token;
    } catch (err) {
      setStatus('idle');
      throw err;
    }
  }, [enabled]);

  const resetAuth = useCallback(() => {
    if (!enabled) return;
    clearToken();
    setStatus('idle');
    setError(null);
  }, [enabled]);

  useEffect(() => {
    if (!enabled) {
      if (status !== 'idle') setStatus('idle');
      return;
    }
    if (status === 'ready') return;
    const stored = getStoredToken();
    if (stored) {
      void ensureToken();
    }
  }, [enabled, ensureToken, status]);

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

import { useCallback, useEffect, useState } from 'react';
import {
  buildAuthUrl,
  clearToken,
  driveConfigError,
  consumeDriveState,
  ensureAccessToken,
  exchangeCodeForToken,
  getStoredToken,
  parseDriveState,
  storeToken,
} from '../services/googleDrive';

export type DriveAuthStatus = 'idle' | 'authenticating' | 'ready' | 'error';

interface DriveAuthOptions {
  enabled?: boolean;
}

interface DriveCallbackResult {
  ok: boolean;
  message?: string;
  returnTo?: string;
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
  const [result, setResult] = useState<DriveCallbackResult>({ ok: false });

  useEffect(() => {
    const search = new URLSearchParams(window.location.search);
    const code = search.get('code');
    const error = search.get('error');
    const rawState = search.get('state');
    const storedState = consumeDriveState();
    const parsedState =
      storedState && rawState && rawState === storedState ? parseDriveState(storedState) : null;
    const returnTo = parsedState?.returnTo;

    if (!storedState || !rawState || rawState !== storedState || !parsedState) {
      setResult({ ok: false, message: 'Estado de OAuth inválido o expirado.' });
      return;
    }

    if (error) {
      setResult({ ok: false, message: error, returnTo });
      return;
    }
    if (!code) {
      setResult({ ok: false, message: 'No se recibió código de Google', returnTo });
      return;
    }
    void (async () => {
      try {
        const token = await exchangeCodeForToken(code);
        storeToken(token);
        setResult({ ok: true, returnTo });
      } catch (err) {
        setResult({
          ok: false,
          message: err instanceof Error ? err.message : 'No se pudo guardar el token',
          returnTo,
        });
      }
    })();
  }, []);

  return result;
}

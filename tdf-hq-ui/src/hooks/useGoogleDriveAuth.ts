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

const callbackFlowByQuery = new Map<string, Promise<DriveCallbackResult>>();

const buildCallbackKey = (code: string | null, error: string | null, rawState: string | null) =>
  JSON.stringify([code, error, rawState]);

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
    const callbackKey =
      code !== null || error !== null || rawState !== null
        ? buildCallbackKey(code, error, rawState)
        : null;

    if (callbackKey) {
      const cached = callbackFlowByQuery.get(callbackKey);
      if (cached) {
        void cached.then(setResult);
        return;
      }
    }

    const callbackFlow = async (): Promise<DriveCallbackResult> => {
      const storedState = consumeDriveState();
      const parsedState =
        storedState && rawState && rawState === storedState ? parseDriveState(storedState) : null;
      const returnTo = parsedState?.returnTo;

      if (!storedState || !rawState || rawState !== storedState || !parsedState) {
        return { ok: false, message: 'Estado de OAuth inválido o expirado.' };
      }

      if (error) {
        return { ok: false, message: error, returnTo };
      }
      if (!code) {
        return { ok: false, message: 'No se recibió código de Google', returnTo };
      }
      try {
        const token = await exchangeCodeForToken(code);
        storeToken(token);
        return { ok: true, returnTo };
      } catch (err) {
        return {
          ok: false,
          message: err instanceof Error ? err.message : 'No se pudo guardar el token',
          returnTo,
        };
      }
    };

    const flowPromise = callbackFlow();
    if (callbackKey) {
      const inflightPromise = flowPromise.finally(() => {
        if (callbackFlowByQuery.get(callbackKey) === inflightPromise) {
          callbackFlowByQuery.delete(callbackKey);
        }
      });
      callbackFlowByQuery.set(callbackKey, inflightPromise);
      void inflightPromise.then(setResult);
      return;
    }

    void flowPromise.then(setResult);
  }, []);

  return result;
}

import { useCallback, useEffect, useRef, useState } from 'react';
import { getActiveSession, useOptionalSession } from '../session/SessionContext';
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

  const popupCleanup = useRef<(() => void) | null>(null);
  useEffect(() => () => popupCleanup.current?.(), []);
  const startAuth = useCallback(async (returnTo?: string, options?: { popup?: boolean }) => {
    if (!enabled) {
      throw new Error('La autenticación de Drive está deshabilitada.');
    }
    if (popupCleanup.current) return;
    setError(null);
    const configWarning = driveConfigError();
    if (configWarning) {
      setStatus('idle');
      setError(configWarning);
      throw new Error(configWarning);
    }
    setStatus('authenticating');
    const popup = options?.popup ? window.open('about:blank', `tdf-drive-${crypto.randomUUID()}`, 'popup,width=560,height=720') : null;
    try {
      if (options?.popup && !popup) throw new Error('Permite abrir la ventana para conectar Drive; tus cambios se conservan.');
      const state = returnTo ?? window.location.pathname + window.location.search;
      const url = await buildAuthUrl(state);
      if (popup) {
        const expectedState = new URL(url).searchParams.get('state');
        await new Promise<void>((resolve, reject) => {
          const timer = window.setTimeout(() => { cleanup(); reject(new Error('No se completó la conexión. Tus cambios se conservan.')); }, 120_000);
          const cleanup = () => { window.clearTimeout(timer); window.removeEventListener('message', receive); window.removeEventListener('storage', receiveStorage); popupCleanup.current = null; };
          const receiveStorage = (event: StorageEvent) => {
            if (!event.key?.startsWith('tdf-google-drive-token:') || !getStoredToken()) return;
            cleanup(); setStatus('ready'); resolve();
          };
          const receive = (event: MessageEvent) => {
            if (event.origin !== window.location.origin || event.source !== popup || !event.data || typeof event.data !== 'object') return;
            const data = event.data as Record<string, unknown>;
            if (data['type'] !== 'tdf-drive-connected' || data['state'] !== expectedState || typeof data['ok'] !== 'boolean') return;
            cleanup();
            if (data['ok']) { setStatus('ready'); resolve(); }
            else reject(new Error('No se autorizó Drive. Tus cambios se conservan.'));
          };
          popupCleanup.current?.();
          popupCleanup.current = () => { cleanup(); popup.close(); reject(new Error('Conexión cancelada.')); };
          window.addEventListener('message', receive);
          window.addEventListener('storage', receiveStorage);
          popup.location.href = url;
        });
      } else window.location.assign(url);
    } catch (err) {
      popup?.close();
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
  const sessionLoading = useOptionalSession()?.loading ?? false;
  const [result, setResult] = useState<DriveCallbackResult>({ ok: false });

  useEffect(() => {
    // Bootstrap establishes the current owner/epoch. Do not consume one-use
    // OAuth state or start an exchange against the preliminary stored session.
    if (sessionLoading) return;
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
      const storedState = consumeDriveState(rawState);
      const parsedState =
        storedState && rawState && rawState === storedState ? parseDriveState(storedState) : null;
      const returnTo = parsedState?.returnTo;

      if (!storedState || !rawState || rawState !== storedState || !parsedState) {
        return { ok: false, message: 'Estado de OAuth inválido o expirado.' };
      }

      if (parsedState.ownerPartyId !== undefined && parsedState.ownerPartyId !== getActiveSession()?.partyId) {
        return { ok: false, message: 'La cuenta TDF cambió durante la conexión.' };
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
  }, [sessionLoading]);

  return result;
}

import { useCallback, useEffect, useState } from 'react';
import {
  buildInstagramAuthUrl,
  clearInstagramResult,
  consumeInstagramState,
  exchangeInstagramCode,
  getStoredInstagramResult,
  instagramConfigError,
  parseInstagramState,
  storeInstagramResult,
} from '../services/instagramAuth';

export type InstagramAuthStatus = 'idle' | 'authenticating' | 'ready' | 'error';
const STATE_TTL_MS = 10 * 60 * 1000;

export function useInstagramAuth() {
  const [status, setStatus] = useState<InstagramAuthStatus>(() =>
    getStoredInstagramResult() ? 'ready' : 'idle',
  );
  const [error, setError] = useState<string | null>(null);

  const startAuth = useCallback((returnTo?: string) => {
    setError(null);
    const configIssue = instagramConfigError();
    if (configIssue) {
      setStatus('error');
      setError(configIssue);
      return;
    }
    setStatus('authenticating');
    const state = returnTo ?? window.location.pathname + window.location.search;
    const url = buildInstagramAuthUrl(state);
    window.location.assign(url);
  }, []);

  const resetAuth = useCallback(() => {
    clearInstagramResult();
    setStatus('idle');
    setError(null);
  }, []);

  useEffect(() => {
    if (status === 'ready') return;
    if (getStoredInstagramResult()) {
      setStatus('ready');
    }
  }, [status]);

  return { status, error, startAuth, resetAuth };
}

export function useInstagramCallback() {
  const [result, setResult] = useState<{ ok: boolean; message?: string; returnTo?: string }>(
    { ok: false },
  );

  useEffect(() => {
    const search = new URLSearchParams(window.location.search);
    const code = search.get('code');
    const error = search.get('error');
    const rawState = search.get('state');
    const storedState = consumeInstagramState();
    const parsedState = rawState ? parseInstagramState(rawState) : null;
    const parsedReturnTo =
      typeof parsedState?.['returnTo'] === 'string' ? parsedState['returnTo'] : undefined;
    const parsedIssuedAt =
      typeof parsedState?.['issuedAt'] === 'number' ? parsedState['issuedAt'] : undefined;
    const returnTo = storedState?.returnTo ?? parsedReturnTo;
    const now = Date.now();

    if (storedState) {
      if (!rawState || rawState !== storedState.state) {
        setResult({ ok: false, message: 'Estado de OAuth inválido o expirado.', returnTo });
        return;
      }
    } else if (!rawState) {
      setResult({ ok: false, message: 'Estado de OAuth inválido o expirado.', returnTo });
      return;
    }

    const issuedAt = storedState?.issuedAt ?? parsedIssuedAt;
    if (issuedAt && now - issuedAt > STATE_TTL_MS) {
      setResult({ ok: false, message: 'Estado de OAuth expirado.', returnTo });
      return;
    }

    if (error) {
      setResult({ ok: false, message: error, returnTo });
      return;
    }
    if (!code) {
      setResult({ ok: false, message: 'No se recibió el código de autorización.', returnTo });
      return;
    }

    void (async () => {
      try {
        const response = await exchangeInstagramCode(code);
        storeInstagramResult(response);
        setResult({ ok: true, returnTo });
      } catch (err) {
        setResult({
          ok: false,
          message: err instanceof Error ? err.message : 'No se pudo completar la conexión.',
          returnTo,
        });
      }
    })();
  }, []);

  return result;
}

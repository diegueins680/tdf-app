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
const STATE_MAX_FUTURE_SKEW_MS = 60 * 1000;

interface InstagramCallbackResult {
  ok: boolean;
  message?: string;
  returnTo?: string;
}

interface ParsedInstagramState {
  returnTo?: string;
  issuedAt?: number;
}

const callbackFlowByQuery = new Map<string, Promise<InstagramCallbackResult>>();

const buildCallbackKey = (
  code: string | null,
  error: string | null,
  rawState: string | null,
  errorReason: string | null,
  errorDescription: string | null,
) => JSON.stringify([code, error, rawState, errorReason, errorDescription]);

const firstNonEmpty = (...values: (string | null | undefined)[]): string | undefined => {
  for (const value of values) {
    const trimmed = value?.trim();
    if (trimmed && trimmed.length > 0) return trimmed;
  }
  return undefined;
};

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
    try {
      setStatus('authenticating');
      const state = returnTo ?? window.location.pathname + window.location.search;
      const url = buildInstagramAuthUrl(state);
      window.location.assign(url);
    } catch (err) {
      setStatus('error');
      setError(err instanceof Error ? err.message : 'No se pudo iniciar la autorización de Instagram.');
    }
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
  const [result, setResult] = useState<InstagramCallbackResult>({ ok: false });

  useEffect(() => {
    const search = new URLSearchParams(window.location.search);
    const code = search.get('code');
    const error = search.get('error');
    const errorReason = search.get('error_reason');
    const errorDescription = search.get('error_description');
    const rawState = search.get('state');
    const callbackKey =
      code !== null || error !== null || rawState !== null || errorReason !== null || errorDescription !== null
        ? buildCallbackKey(code, error, rawState, errorReason, errorDescription)
        : null;

    if (callbackKey) {
      const cached = callbackFlowByQuery.get(callbackKey);
      if (cached) {
        void cached.then(setResult);
        return;
      }
    }

    const callbackFlow = async (): Promise<InstagramCallbackResult> => {
      const storedState = consumeInstagramState();
      const parsedState = rawState ? parseInstagramState(rawState) : null;
      const parsedStateRecord = parsedState as ParsedInstagramState | null;
      const parsedReturnTo =
        typeof parsedStateRecord?.returnTo === 'string' ? parsedStateRecord.returnTo : undefined;
      const parsedIssuedAt =
        typeof parsedStateRecord?.issuedAt === 'number' ? parsedStateRecord.issuedAt : undefined;
      const returnTo = storedState?.returnTo ?? parsedReturnTo;
      const now = Date.now();

      if (storedState) {
        if (!rawState || rawState !== storedState.state) {
          return { ok: false, message: 'Estado de OAuth inválido o expirado.', returnTo };
        }
      } else if (!rawState || !parsedStateRecord) {
        return { ok: false, message: 'Estado de OAuth inválido o expirado.', returnTo };
      }

      const issuedAt = storedState?.issuedAt ?? parsedIssuedAt;
      if (
        typeof issuedAt !== 'number' ||
        !Number.isFinite(issuedAt) ||
        issuedAt <= 0 ||
        issuedAt > now + STATE_MAX_FUTURE_SKEW_MS ||
        now - issuedAt > STATE_TTL_MS
      ) {
        return { ok: false, message: 'Estado de OAuth expirado.', returnTo };
      }

      const oauthError = firstNonEmpty(errorDescription, errorReason, error);
      if (oauthError) {
        return { ok: false, message: oauthError, returnTo };
      }
      if (!code) {
        return { ok: false, message: 'No se recibió el código de autorización.', returnTo };
      }

      try {
        const response = await exchangeInstagramCode(code);
        storeInstagramResult(response);
        return { ok: true, returnTo };
      } catch (err) {
        return {
          ok: false,
          message: err instanceof Error ? err.message : 'No se pudo completar la conexión.',
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

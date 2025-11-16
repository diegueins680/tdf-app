import type { ReactNode } from 'react';
import { createContext, useCallback, useContext, useEffect, useMemo, useState } from 'react';

export type SessionUser = {
  username: string;
  displayName: string;
  roles: string[];
  partyId?: number | null;
  modules?: string[];
};

export type SessionContextValue = {
  session: SessionUser | null;
  login: (user: SessionUser) => void;
  logout: () => void;
  apiToken: string | null;
  setApiToken: (token: string | null) => void;
};

const SESSION_STORAGE_KEY = 'tdf-hq-ui/session';
const TOKEN_STORAGE_KEY = 'tdf-hq-ui/api-token';
const envDemoToken = (import.meta.env.VITE_API_DEMO_TOKEN ?? '').trim();
export const DEFAULT_DEMO_TOKEN = envDemoToken;

const SessionContext = createContext<SessionContextValue | undefined>(undefined);

function readStoredSession(): SessionUser | null {
  if (typeof window === 'undefined') return null;
  try {
    const raw = window.localStorage.getItem(SESSION_STORAGE_KEY);
    return raw ? (JSON.parse(raw) as SessionUser) : null;
  } catch (error) {
    console.warn('Failed to parse stored session', error);
    return null;
  }
}

function persistSession(value: SessionUser | null) {
  if (typeof window === 'undefined') return;
  try {
    if (value) {
      window.localStorage.setItem(SESSION_STORAGE_KEY, JSON.stringify(value));
    } else {
      window.localStorage.removeItem(SESSION_STORAGE_KEY);
    }
  } catch (error) {
    console.warn('Failed to persist session', error);
  }
}

function readStoredApiToken(): string | null {
  if (typeof window === 'undefined') return null;
  try {
    const raw = window.localStorage.getItem(TOKEN_STORAGE_KEY);
    if (raw) {
      const trimmed = raw.trim();
      return trimmed === '' ? null : trimmed;
    }
  } catch (error) {
    console.warn('Failed to read stored API token', error);
  }
  return null;
}

function persistApiToken(token: string | null) {
  if (typeof window === 'undefined') return;
  try {
    if (token && token.trim() !== '') {
      window.localStorage.setItem(TOKEN_STORAGE_KEY, token);
    } else {
      window.localStorage.removeItem(TOKEN_STORAGE_KEY);
    }
  } catch (error) {
    console.warn('Failed to persist API token', error);
  }
}

export function getStoredSessionToken(): string | null {
  const stored = readStoredApiToken();
  if (stored) {
    return stored;
  }
  return DEFAULT_DEMO_TOKEN !== '' ? DEFAULT_DEMO_TOKEN : null;
}

export function SessionProvider({ children }: { children: ReactNode }) {
  const [session, setSession] = useState<SessionUser | null>(() => readStoredSession());
  const [apiToken, setApiTokenState] = useState<string | null>(() => readStoredApiToken() ?? (DEFAULT_DEMO_TOKEN || null));

  useEffect(() => {
    persistSession(session);
  }, [session]);

  useEffect(() => {
    persistApiToken(apiToken);
  }, [apiToken]);

  const login = useCallback((user: SessionUser) => {
    setSession(user);
  }, []);

  const logout = useCallback(() => {
    setSession(null);
    setApiTokenState(DEFAULT_DEMO_TOKEN ? DEFAULT_DEMO_TOKEN : null);
  }, []);

  const setApiToken = useCallback((next: string | null) => {
    const sanitized = next?.trim();
    setApiTokenState(sanitized && sanitized.length > 0 ? sanitized : null);
  }, []);

  const value = useMemo<SessionContextValue>(
    () => ({
      session,
      login,
      logout,
      apiToken,
      setApiToken,
    }),
    [session, login, logout, apiToken, setApiToken],
  );

  return <SessionContext.Provider value={value}>{children}</SessionContext.Provider>;
}

export function useSession(): SessionContextValue {
  const context = useContext(SessionContext);
  if (!context) {
    throw new Error('useSession must be used within a SessionProvider');
  }
  return context;
}

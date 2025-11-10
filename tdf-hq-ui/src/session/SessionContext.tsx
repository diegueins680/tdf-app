import type { ReactNode } from 'react';
import { createContext, useCallback, useContext, useEffect, useMemo, useState } from 'react';

export type SessionUser = {
  username: string;
  displayName: string;
  roles: string[];
  apiToken?: string | null;
  modules?: string[];
  partyId?: number;
};

export type LoginOptions = {
  remember?: boolean;
};

let currentSession: SessionUser | null = null;

export type SessionContextValue = {
  session: SessionUser | null;
  login: (user: SessionUser, options?: LoginOptions) => void;
  logout: () => void;
  setApiToken: (token: string | null) => void;
};

export const SESSION_STORAGE_KEY = 'tdf-hq-ui/session';
export const DEFAULT_DEMO_TOKEN = import.meta.env.VITE_API_DEMO_TOKEN ?? '';

const SessionContext = createContext<SessionContextValue | undefined>(undefined);

function readStoredSession(): SessionUser | null {
  if (typeof window === 'undefined') return null;
  try {
    const raw = window.localStorage.getItem(SESSION_STORAGE_KEY);
    const parsed = raw ? (JSON.parse(raw) as SessionUser) : null;
    currentSession = parsed;
    return parsed;
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

export function SessionProvider({ children }: { children: ReactNode }) {
  const [session, setSession] = useState<SessionUser | null>(() => readStoredSession());
  const [persistSessionEnabled, setPersistSessionEnabled] = useState<boolean>(true);

  useEffect(() => {
    if (session && persistSessionEnabled) {
      persistSession(session);
    } else {
      persistSession(null);
    }
  }, [session, persistSessionEnabled]);

  const login = useCallback((user: SessionUser, options?: LoginOptions) => {
    setPersistSessionEnabled(options?.remember ?? true);
    setSession(user);
    currentSession = user;
  }, []);

  const logout = useCallback(() => {
    setSession(null);
    currentSession = null;
  }, []);

  const setApiToken = useCallback((token: string | null) => {
    setSession((prev) => (prev ? { ...prev, apiToken: token || undefined } : prev));
    currentSession = currentSession ? { ...currentSession, apiToken: token || undefined } : currentSession;
  }, []);

  const value = useMemo<SessionContextValue>(
    () => ({ session, login, logout, setApiToken }),
    [session, login, logout, setApiToken],
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

export function getStoredSessionToken(): string | null {
  if (currentSession?.apiToken) {
    return currentSession.apiToken;
  }
  const stored = readStoredSession();
  return stored?.apiToken ?? null;
}

export function getActiveSession(): SessionUser | null {
  return currentSession;
}

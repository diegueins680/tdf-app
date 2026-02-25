import type { ReactNode } from 'react';
import { createContext, useCallback, useContext, useEffect, useMemo, useState } from 'react';
import { inferDemoToken } from '../config/appConfig';

export interface SessionUser {
  username: string;
  displayName: string;
  roles: string[];
  apiToken?: string | null;
  modules?: string[];
  partyId?: number;
}

export interface LoginOptions {
  remember?: boolean;
}

let currentSession: SessionUser | null = null;

export interface SessionContextValue {
  session: SessionUser | null;
  login: (user: SessionUser, options?: LoginOptions) => void;
  logout: () => void;
  setApiToken: (token: string | null) => void;
}

export const SESSION_STORAGE_KEY = 'tdf-hq-ui/session';

const inferredDemoToken = (() => {
  const host = typeof window === 'undefined' ? undefined : window.location.hostname;
  return inferDemoToken(host);
})();
export const DEFAULT_DEMO_TOKEN = inferredDemoToken;

const SessionContext = createContext<SessionContextValue | undefined>(undefined);

const parseStoredSession = (raw: string): SessionUser | null => {
  try {
    const parsed = JSON.parse(raw) as unknown;
    if (!parsed || typeof parsed !== 'object') return null;
    const value = parsed as Record<string, unknown>;
    const username = value['username'];
    if (typeof username !== 'string') return null;
    const rolesRaw = value['roles'];
    const roles = Array.isArray(rolesRaw)
      ? rolesRaw.filter((role): role is string => typeof role === 'string')
      : [];
    const modulesRaw = value['modules'];
    const modules = Array.isArray(modulesRaw)
      ? modulesRaw.filter((module): module is string => typeof module === 'string')
      : undefined;
    const apiTokenRaw = value['apiToken'];
    const apiToken =
      typeof apiTokenRaw === 'string'
        ? apiTokenRaw.trim() || null
        : apiTokenRaw == null
          ? undefined
          : null;
    const partyIdRaw = value['partyId'];
    const displayNameRaw = value['displayName'];
    const partyId =
      typeof partyIdRaw === 'number' && Number.isFinite(partyIdRaw)
        ? partyIdRaw
        : typeof partyIdRaw === 'string' && /^\d+$/.test(partyIdRaw.trim())
          ? Number.parseInt(partyIdRaw.trim(), 10)
          : undefined;

    return {
      username,
      displayName: typeof displayNameRaw === 'string' && displayNameRaw.trim() ? displayNameRaw : username,
      roles,
      ...(apiToken !== undefined ? { apiToken } : {}),
      ...(modules ? { modules } : {}),
      ...(partyId !== undefined ? { partyId } : {}),
    };
  } catch {
    return null;
  }
};

function readStoredSession(): SessionUser | null {
  if (typeof window === 'undefined') return null;
  try {
    const raw = window.localStorage.getItem(SESSION_STORAGE_KEY);
    if (!raw) {
      currentSession = null;
      return null;
    }
    const parsed = parseStoredSession(raw);
    if (!parsed) {
      currentSession = null;
      window.localStorage.removeItem(SESSION_STORAGE_KEY);
      return null;
    }
    currentSession = parsed;
    return parsed;
  } catch {
    currentSession = null;
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

interface SessionProviderProps {
  children: ReactNode;
}

export function SessionProvider({ children }: SessionProviderProps) {
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
    const normalized = token?.trim();
    const nextToken = normalized && normalized.length > 0 ? normalized : undefined;

    setSession((prev) => {
      if (!prev) {
        currentSession = prev;
        return prev;
      }
      const updatedSession = { ...prev, apiToken: nextToken ?? undefined };
      currentSession = updatedSession;
      return updatedSession;
    });
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
  return stored?.apiToken ?? (DEFAULT_DEMO_TOKEN || null);
}

export function getActiveSession(): SessionUser | null {
  return currentSession;
}

import type { ReactNode } from 'react';
import { createContext, useCallback, useContext, useEffect, useMemo, useState } from 'react';

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
let transientApiToken: string | null = null;

export interface SessionContextValue {
  session: SessionUser | null;
  login: (user: SessionUser, options?: LoginOptions) => void;
  logout: () => void;
  setApiToken: (token: string | null) => void;
}

export const SESSION_STORAGE_KEY = 'tdf-hq-ui/session';

const SessionContext = createContext<SessionContextValue | undefined>(undefined);

const normalizeNonEmptyString = (value: unknown): string | null => {
  if (typeof value !== 'string') return null;
  const trimmed = value.trim();
  return trimmed === '' ? null : trimmed;
};

interface NormalizeStringArrayOptions {
  lowerCase?: boolean;
  dedupeCaseInsensitive?: boolean;
}

const normalizeStringArray = (
  value: unknown,
  options: NormalizeStringArrayOptions = {},
): string[] => {
  const { lowerCase = false, dedupeCaseInsensitive = false } = options;
  if (!Array.isArray(value)) return [];
  const seen = new Set<string>();
  const normalized: string[] = [];
  value.forEach((entry) => {
    if (typeof entry !== 'string') return;
    const trimmed = entry.trim();
    if (trimmed === '') return;
    const normalizedEntry = lowerCase ? trimmed.toLowerCase() : trimmed;
    const dedupeKey = dedupeCaseInsensitive ? normalizedEntry.toLowerCase() : normalizedEntry;
    if (seen.has(dedupeKey)) return;
    seen.add(dedupeKey);
    normalized.push(normalizedEntry);
  });
  return normalized;
};

const normalizeApiToken = (value: string | null | undefined): string | null => {
  const trimmed = value?.trim();
  return trimmed ? trimmed : null;
};

const normalizePositivePartyId = (value: unknown): number | undefined => {
  if (typeof value === 'number') {
    return Number.isSafeInteger(value) && value > 0 ? value : undefined;
  }
  if (typeof value === 'string') {
    const trimmed = value.trim();
    if (!/^\d+$/.test(trimmed)) return undefined;
    const parsed = Number.parseInt(trimmed, 10);
    return Number.isSafeInteger(parsed) && parsed > 0 ? parsed : undefined;
  }
  return undefined;
};

export const parseStoredSession = (raw: string): SessionUser | null => {
  try {
    const parsed = JSON.parse(raw) as unknown;
    if (!parsed || typeof parsed !== 'object') return null;
    const value = parsed as Record<string, unknown>;
    const username = normalizeNonEmptyString(value['username']);
    if (!username) return null;
    const roles = normalizeStringArray(value['roles'], { dedupeCaseInsensitive: true });
    const modules = normalizeStringArray(value['modules'], {
      lowerCase: true,
      dedupeCaseInsensitive: true,
    });
    const apiToken = normalizeNonEmptyString(value['apiToken']) ?? undefined;
    const partyId = normalizePositivePartyId(value['partyId']);
    const displayName = normalizeNonEmptyString(value['displayName']) ?? username;

    return {
      username,
      displayName,
      roles,
      ...(apiToken ? { apiToken } : {}),
      ...(modules.length > 0 ? { modules } : {}),
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
    transientApiToken = null;
  }, []);

  const logout = useCallback(() => {
    setSession(null);
    currentSession = null;
    transientApiToken = null;
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
  return stored?.apiToken ?? transientApiToken;
}

export function getActiveSession(): SessionUser | null {
  return currentSession;
}

export function setTransientApiToken(token: string | null | undefined): void {
  transientApiToken = normalizeApiToken(token);
}

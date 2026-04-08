import type { ReactNode } from 'react';
import { createContext, useCallback, useContext, useEffect, useMemo, useRef, useState } from 'react';

import { loadSessionSnapshot, logoutSessionRequest } from '../api/session';

export type SessionUser = {
  username: string;
  displayName: string;
  roles: string[];
  partyId?: number | null;
  modules?: string[];
};

<<<<<<< HEAD
export interface LoginOptions {
  remember?: boolean;
}

type SessionStorageScope = 'local' | 'session';

let currentSession: SessionUser | null = null;
let transientApiToken: string | null = null;

export interface SessionContextValue {
  session: SessionUser | null;
  loading: boolean;
  login: (user: SessionUser, options?: LoginOptions) => void;
=======
export type SessionContextValue = {
  session: SessionUser | null;
  login: (user: SessionUser) => void;
>>>>>>> origin/problematicMain
  logout: () => void;
  apiToken: string | null;
  setApiToken: (token: string | null) => void;
};

<<<<<<< HEAD
export const SESSION_STORAGE_KEY = 'tdf-hq-ui/session';
=======
const SESSION_STORAGE_KEY = 'tdf-hq-ui/session';
const TOKEN_STORAGE_KEY = 'tdf-hq-ui/api-token';
const envDemoToken = (import.meta.env.VITE_API_DEMO_TOKEN ?? '').trim();
export const DEFAULT_DEMO_TOKEN = envDemoToken;
>>>>>>> origin/problematicMain

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

const normalizeApiToken = (value: unknown): string | null => normalizeNonEmptyString(value);

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

const normalizeSessionUser = (
  value: {
    username: unknown;
    displayName?: unknown;
    roles?: unknown;
    apiToken?: unknown;
    modules?: unknown;
    partyId?: unknown;
  },
): SessionUser => {
  const username = normalizeNonEmptyString(value.username) ?? 'usuario';
  const displayName = normalizeNonEmptyString(value.displayName) ?? username;
  const roles = normalizeStringArray(value.roles, { lowerCase: true, dedupeCaseInsensitive: true });
  const modules = normalizeStringArray(value.modules, { lowerCase: true, dedupeCaseInsensitive: true });
  const apiToken = normalizeApiToken(value.apiToken);
  const partyId = normalizePositivePartyId(value.partyId);

  return {
    username,
    displayName,
    roles,
    ...(apiToken ? { apiToken } : {}),
    ...(modules.length > 0 ? { modules } : {}),
    ...(partyId !== undefined ? { partyId } : {}),
  };
};

const sanitizeSessionForStorage = (value: SessionUser): Omit<SessionUser, 'apiToken'> => {
  const normalized = normalizeSessionUser(value);
  const { apiToken: _apiToken, ...persisted } = normalized;
  return persisted;
};

export const parseStoredSession = (raw: string): SessionUser | null => {
  try {
<<<<<<< HEAD
    const parsed = JSON.parse(raw) as unknown;
    if (!parsed || typeof parsed !== 'object') return null;
    const value = parsed as Record<string, unknown>;
    const username = normalizeNonEmptyString(value['username']);
    if (!username) return null;
    const displayName = normalizeNonEmptyString(value['displayName']) ?? username;
    return normalizeSessionUser({
      username,
      displayName,
      roles: value['roles'],
      modules: value['modules'],
      partyId: value['partyId'],
    });
  } catch {
    return null;
  }
};

function readStoredSessionFrom(storage: Storage | undefined): SessionUser | null {
  if (!storage) return null;
  try {
    const raw = storage.getItem(SESSION_STORAGE_KEY);
    if (!raw) return null;
    const parsed = parseStoredSession(raw);
    if (!parsed) {
      storage.removeItem(SESSION_STORAGE_KEY);
      return null;
    }
    return parsed;
  } catch {
=======
    const raw = window.localStorage.getItem(SESSION_STORAGE_KEY);
    return raw ? (JSON.parse(raw) as SessionUser) : null;
  } catch (error) {
    console.warn('Failed to parse stored session', error);
>>>>>>> origin/problematicMain
    return null;
  }
}

function readStoredSession(): { session: SessionUser | null; scope: SessionStorageScope } {
  if (typeof window === 'undefined') {
    currentSession = null;
    return { session: null, scope: 'local' };
  }

  const fromSession = readStoredSessionFrom(window.sessionStorage);
  if (fromSession) {
    currentSession = fromSession;
    return { session: fromSession, scope: 'session' };
  }

  const fromLocal = readStoredSessionFrom(window.localStorage);
  currentSession = fromLocal;
  return { session: fromLocal, scope: 'local' };
}

function persistSession(value: SessionUser | null, scope: SessionStorageScope) {
  if (typeof window === 'undefined') return;
  try {
    window.localStorage.removeItem(SESSION_STORAGE_KEY);
    window.sessionStorage.removeItem(SESSION_STORAGE_KEY);
    if (!value) {
      return;
    }

    const serialized = JSON.stringify(sanitizeSessionForStorage(value));
    const target = scope === 'session' ? window.sessionStorage : window.localStorage;
    target.setItem(SESSION_STORAGE_KEY, serialized);
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

<<<<<<< HEAD
export function SessionProvider({ children }: SessionProviderProps) {
  const [session, setSession] = useState<SessionUser | null>(() => readStoredSession().session);
  const [loading, setLoading] = useState(true);
  const [persistScope, setPersistScope] = useState<SessionStorageScope>(() => readStoredSession().scope);
  const sessionVersionRef = useRef(0);

  const updateSessionState = useCallback((next: SessionUser | null) => {
    currentSession = next;
    setSession(next);
  }, []);

  useEffect(() => {
    persistSession(session, persistScope);
  }, [session, persistScope]);

  useEffect(() => {
    let cancelled = false;
    const versionAtStart = sessionVersionRef.current;

    void (async () => {
      try {
        const snapshot = await loadSessionSnapshot();
        if (cancelled || versionAtStart !== sessionVersionRef.current) return;

        if (!snapshot) {
          updateSessionState(null);
          return;
        }

        setSession((prev) => {
          const next = normalizeSessionUser({
            username: snapshot.username,
            displayName: snapshot.displayName,
            roles: snapshot.roles,
            modules: snapshot.modules,
            partyId: snapshot.partyId,
            apiToken: prev?.apiToken,
          });
          currentSession = next;
          return next;
        });
      } catch (error) {
        if (cancelled || versionAtStart !== sessionVersionRef.current) return;
        console.warn('Failed to bootstrap session from server', error);
      } finally {
        if (!cancelled && versionAtStart === sessionVersionRef.current) {
          setLoading(false);
        }
      }
    })();

    return () => {
      cancelled = true;
    };
  }, [updateSessionState]);

  const login = useCallback((user: SessionUser, options?: LoginOptions) => {
    sessionVersionRef.current += 1;
    setPersistScope(options?.remember === false ? 'session' : 'local');
    setLoading(false);
    updateSessionState(normalizeSessionUser(user));
    transientApiToken = null;
  }, [updateSessionState]);

  const logout = useCallback(() => {
    sessionVersionRef.current += 1;
    setLoading(false);
    updateSessionState(null);
    transientApiToken = null;
    void logoutSessionRequest().catch((error) => {
      console.warn('Failed to clear server session', error);
    });
  }, [updateSessionState]);

  const setApiToken = useCallback((token: string | null) => {
    sessionVersionRef.current += 1;
    const normalized = normalizeApiToken(token);

    setSession((prev) => {
      if (!prev) {
        currentSession = prev;
        return prev;
      }
      const updatedSession = normalized
        ? { ...prev, apiToken: normalized }
        : { ...prev, apiToken: undefined };
      currentSession = updatedSession;
      return updatedSession;
    });
  }, []);

  const value = useMemo<SessionContextValue>(
    () => ({ session, loading, login, logout, setApiToken }),
    [session, loading, login, logout, setApiToken],
=======
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
    if (!apiToken && DEFAULT_DEMO_TOKEN) {
      setApiTokenState(DEFAULT_DEMO_TOKEN);
    }
  }, [apiToken]);

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
>>>>>>> origin/problematicMain
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
<<<<<<< HEAD

export function getStoredSessionToken(): string | null {
  if (currentSession?.apiToken) {
    return currentSession.apiToken;
  }
  return transientApiToken;
}

export function getActiveSession(): SessionUser | null {
  return currentSession;
}

export function setTransientApiToken(token: string | null | undefined): void {
  transientApiToken = normalizeApiToken(token);
}
=======
>>>>>>> origin/problematicMain

import { ReactNode, createContext, useCallback, useContext, useEffect, useMemo, useRef, useState } from 'react';
import { Session } from '../api/session';
import { Parties } from '../api/parties';
import { Bookings } from '../api/bookings';
import { PackagesApi } from '../api/packages';
import { Invoices } from '../api/invoices';
import { Inventory } from '../api/inventory';
import type { LoginResponse } from '../api/types';
import { getAuthToken, setAuthToken, setUnauthorizedHandler } from '../api/client';
import type { ModuleKey } from '../constants/modules';
import { ALL_MODULES } from '../constants/modules';

const STORAGE_KEY_PERSISTED = 'tdf-hq-auth';
const STORAGE_KEY_SESSION = 'tdf-hq-auth-session';
const INACTIVITY_LIMIT_MS = 30 * 60 * 1000; // 30 minutes sliding
const ROTATION_LIMIT_MS = 30 * 24 * 60 * 60 * 1000; // 30 days

const moduleProbes: Record<ModuleKey, () => Promise<void>> = {
  CRM: () => Parties.list().then(() => undefined),
  Scheduling: () => Bookings.list().then(() => undefined),
  Packages: () => PackagesApi.listProducts().then(() => undefined),
  Invoicing: () => Invoices.list().then(() => undefined),
  Admin: () => Inventory.list({ page: 1, pageSize: 1 }).then(() => undefined),
};

export type AuthUser = LoginResponse & {
  username: string;
  issuedAt: number;
  expiresAt: number;
  remember: boolean;
  rotateAt: number;
};

type AuthContextValue = {
  user: AuthUser | null;
  isAuthenticated: boolean;
  isLoading: boolean;
  login: (username: string, password: string, remember: boolean) => Promise<void>;
  loginWithToken: (token: string, displayName: string, remember: boolean) => Promise<void>;
  logout: () => void;
};

const AuthContext = createContext<AuthContextValue | undefined>(undefined);

function readFromStorage(): AuthUser | null {
  if (typeof window === 'undefined') {
    return null;
  }

  const sessionRaw = window.sessionStorage.getItem(STORAGE_KEY_SESSION);
  const persistedRaw = window.localStorage.getItem(STORAGE_KEY_PERSISTED);

  const raw = sessionRaw ?? persistedRaw;
  if (!raw) {
    setAuthToken(null);
    return null;
  }

  try {
    const parsed = JSON.parse(raw) as AuthUser;
    if (parsed.expiresAt && parsed.expiresAt < Date.now()) {
      window.sessionStorage.removeItem(STORAGE_KEY_SESSION);
      window.localStorage.removeItem(STORAGE_KEY_PERSISTED);
      setAuthToken(null);
      return null;
    }
    setAuthToken(parsed.token);
    return parsed;
  } catch (_err) {
    window.sessionStorage.removeItem(STORAGE_KEY_SESSION);
    window.localStorage.removeItem(STORAGE_KEY_PERSISTED);
    setAuthToken(null);
    return null;
  }
}

function persistUser(user: AuthUser | null) {
  if (typeof window === 'undefined') {
    return;
  }
  if (!user) {
    window.sessionStorage.removeItem(STORAGE_KEY_SESSION);
    window.localStorage.removeItem(STORAGE_KEY_PERSISTED);
    return;
  }
  const payload = JSON.stringify(user);
  if (user.remember) {
    window.localStorage.setItem(STORAGE_KEY_PERSISTED, payload);
    window.sessionStorage.removeItem(STORAGE_KEY_SESSION);
  } else {
    window.sessionStorage.setItem(STORAGE_KEY_SESSION, payload);
    window.localStorage.removeItem(STORAGE_KEY_PERSISTED);
  }
}

function buildAuthUser(base: LoginResponse, username: string, remember: boolean): AuthUser {
  const now = Date.now();
  return {
    ...base,
    username,
    remember,
    issuedAt: now,
    expiresAt: now + INACTIVITY_LIMIT_MS,
    rotateAt: now + ROTATION_LIMIT_MS,
  };
}

function isExpectedDenial(message: string) {
  const lower = message.toLowerCase();
  return lower.includes('missing access') || lower.includes('http 403') || lower.includes('http 404');
}

async function detectModules(): Promise<ModuleKey[]> {
  const detected: ModuleKey[] = [];
  for (const key of ALL_MODULES) {
    const probe = moduleProbes[key];
    if (!probe) continue;
    try {
      await probe();
      detected.push(key);
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error ?? '');
      if (isExpectedDenial(message)) {
        continue;
      }
      throw error;
    }
  }
  return detected;
}

export function AuthProvider({ children }: { children: ReactNode }) {
  const [user, setUser] = useState<AuthUser | null>(() => readFromStorage());
  const [isLoading, setIsLoading] = useState(false);
  const rotateWarningFired = useRef(false);

  const touchSession = useCallback((nextUser?: AuthUser | null) => {
    setUser(prev => {
      const updated = nextUser ?? (prev ? { ...prev, expiresAt: Date.now() + INACTIVITY_LIMIT_MS } : null);
      if (updated) {
        persistUser(updated);
        setAuthToken(updated.token);
      }
      return updated;
    });
  }, []);

  const applyDetectedModules = useCallback(async () => {
    try {
      const modules = await detectModules();
      setUser(prev => {
        if (!prev) return prev;
        if (modules.length === prev.modules.length && modules.every(m => prev.modules.includes(m))) {
          return prev;
        }
        const updated = { ...prev, modules } as AuthUser;
        persistUser(updated);
        return updated;
      });
    } catch (error) {
      console.error('Failed to detect module access', error);
    }
  }, []);

  const login = useCallback(async (username: string, password: string, remember: boolean) => {
    setIsLoading(true);
    try {
      const response = await Session.login({ username, password });
      const nextUser = buildAuthUser(response, username, remember);
      touchSession(nextUser);
      await applyDetectedModules();
    } finally {
      setIsLoading(false);
    }
  }, [touchSession, applyDetectedModules]);

  const loginWithToken = useCallback(async (token: string, displayName: string, remember: boolean) => {
    setIsLoading(true);
    const previous = getAuthToken();
    try {
      setAuthToken(token);
      const detectedModules = await detectModules();
      const nextResponse: LoginResponse = {
        token,
        partyId: -1,
        roles: [],
        modules: detectedModules,
      };
      const nextUser = buildAuthUser(nextResponse, displayName || 'API Token', remember);
      touchSession(nextUser);
    } catch (error) {
      setAuthToken(previous);
      throw error;
    } finally {
      setIsLoading(false);
    }
  }, [touchSession]);

  const logout = useCallback(() => {
    setUser(null);
    setAuthToken(null);
    persistUser(null);
    rotateWarningFired.current = false;
  }, []);

  useEffect(() => {
    setUnauthorizedHandler(logout);
    return () => {
      setUnauthorizedHandler(null);
    };
  }, [logout]);

  useEffect(() => {
    if (user && (!user.modules || user.modules.length === 0)) {
      void applyDetectedModules();
    }
  }, [user, applyDetectedModules]);

  useEffect(() => {
    if (!user) {
      return;
    }

    if (user.expiresAt <= Date.now()) {
      logout();
      return;
    }

    const handleActivity = () => {
      touchSession();
    };
    const events: Array<keyof WindowEventMap> = ['mousemove', 'keydown', 'click', 'touchstart'];
    events.forEach(eventName => window.addEventListener(eventName, handleActivity));

    const interval = window.setInterval(() => {
      if (user && user.expiresAt <= Date.now()) {
        logout();
      } else if (!rotateWarningFired.current && user && user.rotateAt <= Date.now()) {
        rotateWarningFired.current = true;
        window.dispatchEvent(new CustomEvent('tdf-session-rotation-due'));
      }
    }, 30_000);

    return () => {
      events.forEach(eventName => window.removeEventListener(eventName, handleActivity));
      window.clearInterval(interval);
    };
  }, [user, logout, touchSession]);

  const value = useMemo<AuthContextValue>(() => ({
    user,
    isAuthenticated: !!user?.token,
    isLoading,
    login,
    loginWithToken,
    logout,
  }), [user, isLoading, login, loginWithToken, logout]);

  return (
    <AuthContext.Provider value={value}>
      {children}
    </AuthContext.Provider>
  );
}

export function useAuth() {
  const ctx = useContext(AuthContext);
  if (!ctx) {
    throw new Error('useAuth must be used within AuthProvider');
  }
  return ctx;
}

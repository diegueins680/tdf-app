/** A local sign-out must survive reload even when server revocation fails.
 * Contains no identity, token or personal data. Server revocation is separate.
 */
export const LOGOUT_INTENT_KEY = 'tdf-hq-ui/logout-intent/v1';
const CHANGE_EVENT = 'tdf-hq-ui/logout-intent-changed';
export type LogoutStatus = 'cleared' | 'pending' | 'unconfirmed' | 'confirmed';
interface LogoutIntent { revision: string; status: LogoutStatus }
let volatileIntent: LogoutIntent | null = null;
let pendingRequest: Promise<void> | null = null;
let storageWritable: boolean | null = null;

const readIntent = (): LogoutIntent | null => {
  if (typeof window === 'undefined' || volatileIntent) return volatileIntent;
  try {
    if (storageWritable === null) {
      const probe = `${LOGOUT_INTENT_KEY}/probe`;
      window.localStorage.setItem(probe, '1');
      window.localStorage.removeItem(probe);
      storageWritable = true;
    }
    if (!storageWritable) return { revision: 'unavailable', status: 'unconfirmed' };
    const raw = window.localStorage.getItem(LOGOUT_INTENT_KEY);
    if (!raw) return volatileIntent;
    const value = JSON.parse(raw) as Partial<LogoutIntent>;
    if (typeof value.revision === 'string' && value.revision !== ''
      && ['cleared', 'pending', 'unconfirmed', 'confirmed'].includes(value.status ?? '')) {
      return value as LogoutIntent;
    }
  } catch {
    // Restricted/corrupt storage cannot safely authorize automatic cookie restore.
    return volatileIntent ?? { revision: 'unavailable', status: 'unconfirmed' };
  }
  return { revision: 'invalid', status: 'unconfirmed' };
};

export const getLogoutRevision = (): string => readIntent()?.revision ?? 'initial';
export const getLogoutStatus = (): LogoutStatus => {
  const status = readIntent()?.status ?? 'cleared';
  return status === 'pending' && !pendingRequest ? 'unconfirmed' : status;
};
export const hasLogoutIntent = (): boolean => getLogoutStatus() !== 'cleared';
export const ownsLogoutRevision = (revision: string): boolean => revision === getLogoutRevision();

const writeIntent = (intent: LogoutIntent): void => {
  volatileIntent = intent;
  if (typeof window === 'undefined') return;
  try {
    window.localStorage.setItem(LOGOUT_INTENT_KEY, JSON.stringify(intent));
    volatileIntent = null;
    storageWritable = true;
  } catch { storageWritable = false; /* memory-only until reload */ }
  window.dispatchEvent(new Event(CHANGE_EVENT));
};

const newRevision = (): string => Array.from(
  globalThis.crypto.getRandomValues(new Uint32Array(4)), (part) => part.toString(16),
).join('-');
export const beginLogoutIntent = (): string => {
  const revision = newRevision();
  writeIntent({ revision, status: 'pending' });
  return revision;
};
export const finishLogoutIntent = (revision: string, confirmed: boolean): void => {
  if (ownsLogoutRevision(revision)) writeIntent({ revision, status: confirmed ? 'confirmed' : 'unconfirmed' });
};
export const clearLogoutIntent = (): void => {
  // Keep a fresh revision: stale pre-logout auth callbacks must never own a new login.
  writeIntent({ revision: newRevision(), status: 'cleared' });
};
export const subscribeLogoutIntent = (listener: () => void): (() => void) => {
  const onStorage = (event: StorageEvent) => {
    if (event.key === LOGOUT_INTENT_KEY || event.key === null) {
      volatileIntent = null;
      listener();
    }
  };
  window.addEventListener(CHANGE_EVENT, listener);
  window.addEventListener('storage', onStorage);
  return () => {
    window.removeEventListener(CHANGE_EVENT, listener);
    window.removeEventListener('storage', onStorage);
  };
};
export const trackLogoutRequest = (request: Promise<void>): void => {
  pendingRequest = request;
  window.dispatchEvent(new Event(CHANGE_EVENT));
  void request.finally(() => {
    if (pendingRequest === request) {
      pendingRequest = null;
      window.dispatchEvent(new Event(CHANGE_EVENT));
    }
  }).catch(() => undefined);
};
export const waitForLogoutRequest = (): Promise<void> | null => pendingRequest?.catch(() => undefined) ?? null;
export const assertNoLogoutIntent = (): void => {
  if (hasLogoutIntent()) throw new Error('Inicia sesión para continuar.');
};

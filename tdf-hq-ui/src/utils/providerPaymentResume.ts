import type {
  HostedPaymentMethod,
  HostedPaymentProvider,
} from '../api/providerPaymentSessions';

export interface ProviderPaymentResume {
  version: 1;
  checkoutId: string;
  attemptId: string;
  provider: HostedPaymentProvider;
  paymentMethod: HostedPaymentMethod;
  lookupToken: string;
  returnPath: string;
  createdAt: number;
}

export interface ProviderPaymentPending {
  version: 1;
  checkoutId: string;
  provider: HostedPaymentProvider;
  paymentMethod: HostedPaymentMethod;
  lookupToken: string;
  returnPath: string;
  buyerPhone?: string;
  buyerCountryCode?: string;
  createdAt: number;
}

const ACTIVE_PAYMENT_KEY = 'tdf:provider-payment-session:active:v1';
const PENDING_PAYMENT_KEY = 'tdf:provider-payment-session:pending:v1';
const MAX_RESUME_AGE_MS = 24 * 60 * 60 * 1000;
const UUID_PATTERN = /^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;
const PROVIDERS = new Set<HostedPaymentProvider>(['placetopay', 'payphone']);
const METHODS = new Set<HostedPaymentMethod>([
  'card',
  'bank_redirect',
  'deuna_qr',
  'payphone_wallet',
]);

const storage = (): Storage | null => {
  try {
    return typeof window === 'undefined' ? null : window.sessionStorage;
  } catch {
    return null;
  }
};

export const safePaymentReturnPath = (value: string): string | null => {
  const normalized = value.trim();
  return normalized.startsWith('/')
      && !normalized.startsWith('//')
      && !/[\r\n]/.test(normalized)
    ? normalized
    : null;
};

const isResume = (value: unknown, now: number): value is ProviderPaymentResume => {
  if (!value || typeof value !== 'object') return false;
  const candidate = value as Partial<ProviderPaymentResume>;
  return candidate.version === 1
    && typeof candidate.checkoutId === 'string'
    && UUID_PATTERN.test(candidate.checkoutId)
    && typeof candidate.attemptId === 'string'
    && UUID_PATTERN.test(candidate.attemptId)
    && typeof candidate.provider === 'string'
    && PROVIDERS.has(candidate.provider)
    && typeof candidate.paymentMethod === 'string'
    && METHODS.has(candidate.paymentMethod)
    && typeof candidate.lookupToken === 'string'
    && candidate.lookupToken.trim().length >= 16
    && candidate.lookupToken.length <= 512
    && typeof candidate.returnPath === 'string'
    && safePaymentReturnPath(candidate.returnPath) !== null
    && typeof candidate.createdAt === 'number'
    && Number.isFinite(candidate.createdAt)
    && candidate.createdAt <= now + 60_000
    && now - candidate.createdAt <= MAX_RESUME_AGE_MS;
};

const isPending = (value: unknown, now: number): value is ProviderPaymentPending => {
  if (!value || typeof value !== 'object') return false;
  const candidate = value as Partial<ProviderPaymentPending>;
  return candidate.version === 1
    && typeof candidate.checkoutId === 'string'
    && UUID_PATTERN.test(candidate.checkoutId)
    && typeof candidate.provider === 'string'
    && PROVIDERS.has(candidate.provider)
    && typeof candidate.paymentMethod === 'string'
    && METHODS.has(candidate.paymentMethod)
    && typeof candidate.lookupToken === 'string'
    && candidate.lookupToken.trim().length >= 16
    && candidate.lookupToken.length <= 512
    && typeof candidate.returnPath === 'string'
    && safePaymentReturnPath(candidate.returnPath) !== null
    && (candidate.buyerPhone === undefined
      || (typeof candidate.buyerPhone === 'string' && /^\d{6,15}$/.test(candidate.buyerPhone)))
    && (candidate.buyerCountryCode === undefined
      || (typeof candidate.buyerCountryCode === 'string' && /^\d{1,3}$/.test(candidate.buyerCountryCode)))
    && typeof candidate.createdAt === 'number'
    && Number.isFinite(candidate.createdAt)
    && candidate.createdAt <= now + 60_000
    && now - candidate.createdAt <= MAX_RESUME_AGE_MS;
};

export const saveProviderPaymentResume = (resume: ProviderPaymentResume): boolean => {
  if (!isResume(resume, Date.now())) return false;
  try {
    const target = storage();
    if (!target) return false;
    target.setItem(ACTIVE_PAYMENT_KEY, JSON.stringify(resume));
    return target.getItem(ACTIVE_PAYMENT_KEY) !== null;
  } catch {
    return false;
  }
};

export const loadProviderPaymentResume = (
  expectedCheckoutId?: string,
): ProviderPaymentResume | null => {
  try {
    const raw = storage()?.getItem(ACTIVE_PAYMENT_KEY);
    if (!raw) return null;
    const parsed: unknown = JSON.parse(raw);
    if (!isResume(parsed, Date.now())) {
      storage()?.removeItem(ACTIVE_PAYMENT_KEY);
      return null;
    }
    if (expectedCheckoutId && parsed.checkoutId !== expectedCheckoutId) return null;
    return parsed;
  } catch {
    return null;
  }
};

export const clearProviderPaymentResume = (expectedCheckoutId?: string): void => {
  try {
    if (expectedCheckoutId) {
      const current = loadProviderPaymentResume();
      if (current?.checkoutId !== expectedCheckoutId) return;
    }
    storage()?.removeItem(ACTIVE_PAYMENT_KEY);
  } catch {
    // Storage is an optional recovery aid; the server remains authoritative.
  }
};

export const saveProviderPaymentPending = (pending: ProviderPaymentPending): boolean => {
  if (!isPending(pending, Date.now())) return false;
  try {
    const target = storage();
    if (!target) return false;
    target.setItem(PENDING_PAYMENT_KEY, JSON.stringify(pending));
    return target.getItem(PENDING_PAYMENT_KEY) !== null;
  } catch {
    return false;
  }
};

export const loadProviderPaymentPending = (
  expectedCheckoutId?: string,
): ProviderPaymentPending | null => {
  try {
    const raw = storage()?.getItem(PENDING_PAYMENT_KEY);
    if (!raw) return null;
    const parsed: unknown = JSON.parse(raw);
    if (!isPending(parsed, Date.now())) {
      storage()?.removeItem(PENDING_PAYMENT_KEY);
      return null;
    }
    if (expectedCheckoutId && parsed.checkoutId !== expectedCheckoutId) return null;
    return parsed;
  } catch {
    return null;
  }
};

export const clearProviderPaymentPending = (expectedCheckoutId?: string): void => {
  try {
    if (expectedCheckoutId) {
      const current = loadProviderPaymentPending();
      if (current?.checkoutId !== expectedCheckoutId) return;
    }
    storage()?.removeItem(PENDING_PAYMENT_KEY);
  } catch {
    // Storage is an optional recovery aid; the server remains authoritative.
  }
};

export const paymentIdempotencyStorageKey = (
  checkoutId: string,
  provider: HostedPaymentProvider,
  method: HostedPaymentMethod,
): string => `tdf:provider-payment-idempotency:v1:${checkoutId}:${provider}:${method}`;

export const loadExistingPaymentIdempotencyKey = (
  checkoutId: string,
  provider: HostedPaymentProvider,
  method: HostedPaymentMethod,
): string | null => {
  const key = paymentIdempotencyStorageKey(checkoutId, provider, method);
  try {
    const existing = storage()?.getItem(key)?.trim();
    if (existing && /^[A-Za-z0-9][A-Za-z0-9._:-]{15,127}$/.test(existing)) return existing;
  } catch {
    // Recovery must not replace an unavailable original key.
  }
  return null;
};

export const loadOrCreatePaymentIdempotencyKey = (
  checkoutId: string,
  provider: HostedPaymentProvider,
  method: HostedPaymentMethod,
): string => {
  const existing = loadExistingPaymentIdempotencyKey(checkoutId, provider, method);
  if (existing) return existing;
  const key = paymentIdempotencyStorageKey(checkoutId, provider, method);
  const entropy = globalThis.crypto?.randomUUID?.()
    ?? `${Date.now()}-${Math.random().toString(16).slice(2)}`;
  const created = `payment-session-${entropy}`;
  try {
    storage()?.setItem(key, created);
  } catch {
    // The caller still reuses this returned key for its current request.
  }
  return created;
};

export const clearPaymentIdempotencyKey = (
  checkoutId: string,
  provider: HostedPaymentProvider,
  method: HostedPaymentMethod,
): void => {
  try {
    storage()?.removeItem(paymentIdempotencyStorageKey(checkoutId, provider, method));
  } catch {
    // Optional browser recovery storage is unavailable.
  }
};

export const paymentAttemptCanBeReleased = (
  state: string | null | undefined,
  canRetryOrFallback: boolean | null | undefined,
): boolean => state === 'confirmed_no_charge'
  || (state === 'failed' && canRetryOrFallback === true);

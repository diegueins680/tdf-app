import type {
  HostedPaymentMethod,
  HostedPaymentProvider,
} from '../api/providerPaymentSessions';

export interface ProviderPaymentResume {
  version: 1;
  checkoutId: string;
  attemptId: string;
  completedAt?: number;
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

// Each checkout owns its recovery record. The old singleton is read only as a
// migration fallback; saving B must not destroy A's capability or safety lock.
const records = <T extends ProviderPaymentPending>(
  key: string, valid: (value: unknown, now: number) => value is T,
): T[] => {
  try {
    const target = storage();
    if (!target) return [];
    const found = new Map<string, T>();
    const keys = [key];
    for (let index = 0; index < target.length; index += 1) {
      const candidate = target.key(index);
      if (candidate?.startsWith(`${key}:checkout:`)) keys.push(candidate);
    }
    for (const candidate of keys) {
      const raw = target.getItem(candidate);
      if (!raw) continue;
      try {
        const parsed: unknown = JSON.parse(raw);
        if (valid(parsed, Date.now())) found.set(parsed.checkoutId, parsed);
      } catch { /* Preserve unreadable records for support; never select them. */ }
    }
    return [...found.values()].sort((a, b) => b.createdAt - a.createdAt);
  } catch { return []; }
};

const saveRecord = <T extends ProviderPaymentPending>(
  key: string, value: T, valid: (value: unknown, now: number) => value is T,
): boolean => {
  if (!valid(value, Date.now())) return false;
  try {
    const target = storage();
    if (!target) return false;
    const scopedKey = `${key}:checkout:${value.checkoutId}`;
    const serialized = JSON.stringify(value);
    target.setItem(scopedKey, serialized);
    return target.getItem(scopedKey) === serialized;
  } catch { return false; }
};

const selectRecord = <T extends ProviderPaymentPending>(
  values: T[], expectedCheckoutId?: string, returnPathPrefix?: string,
): T | null => {
  if (expectedCheckoutId !== undefined) return values.find((value) => value.checkoutId === expectedCheckoutId) ?? null;
  if (returnPathPrefix) return values.find((value) => value.returnPath.startsWith(returnPathPrefix)) ?? null;
  // An old unbound provider return must never silently choose another order.
  return values.length === 1 ? (values[0] ?? null) : null;
};

const clearRecord = <T extends ProviderPaymentPending>(
  key: string, valid: (value: unknown, now: number) => value is T, expectedCheckoutId?: string,
): void => {
  try {
    const target = storage();
    if (!target) return;
    const selected = selectRecord(records(key, valid), expectedCheckoutId);
    if (!selected) return;
    target.removeItem(`${key}:checkout:${selected.checkoutId}`);
    const legacy = target.getItem(key);
    if (legacy) {
      const parsed: unknown = JSON.parse(legacy);
      if (valid(parsed, Date.now()) && parsed.checkoutId === selected.checkoutId) target.removeItem(key);
    }
  } catch { /* A failed cleanup never authorizes another payment. */ }
};

export const saveProviderPaymentResume = (resume: ProviderPaymentResume): boolean =>
  saveRecord(ACTIVE_PAYMENT_KEY, resume, isResume);

export const loadProviderPaymentResume = (
  expectedCheckoutId?: string, returnPathPrefix?: string,
): ProviderPaymentResume | null => selectRecord(
  records(ACTIVE_PAYMENT_KEY, isResume).filter((value) =>
    !returnPathPrefix || expectedCheckoutId !== undefined
      || typeof value.completedAt !== 'number' || !Number.isFinite(value.completedAt)
      || value.completedAt < value.createdAt || value.completedAt > Date.now() + 60_000),
  expectedCheckoutId, returnPathPrefix,
);

// This is only a UI recovery hint from a verified server response. It does not
// release the original checkout or its idempotency key, or prove payment itself.
export const markProviderPaymentResumeCompleted = (checkoutId: string): boolean => {
  const resume = loadProviderPaymentResume(checkoutId);
  return resume ? saveProviderPaymentResume({ ...resume, completedAt: Math.max(Date.now(), resume.createdAt) }) : false;
};

export const clearProviderPaymentResume = (expectedCheckoutId?: string): void =>
  clearRecord(ACTIVE_PAYMENT_KEY, isResume, expectedCheckoutId);

export const saveProviderPaymentPending = (pending: ProviderPaymentPending): boolean =>
  saveRecord(PENDING_PAYMENT_KEY, pending, isPending);

export const loadProviderPaymentPending = (
  expectedCheckoutId?: string, returnPathPrefix?: string,
): ProviderPaymentPending | null => selectRecord(
  records(PENDING_PAYMENT_KEY, isPending), expectedCheckoutId, returnPathPrefix,
);

export const clearProviderPaymentPending = (expectedCheckoutId?: string): void =>
  clearRecord(PENDING_PAYMENT_KEY, isPending, expectedCheckoutId);

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

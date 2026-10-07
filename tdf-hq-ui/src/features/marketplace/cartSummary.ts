import { useSyncExternalStore } from 'react';
import {
  readOptionalBrowserStorage,
  removeOptionalBrowserPreference,
  writeOptionalBrowserPreference,
} from '../../utils/optionalBrowserStorage';

// Shared marketplace cart summary.
//
// The marketplace cart is anonymous: the backend keys it by a UUID that only
// lives in this browser's localStorage. That means it already survives
// login/logout on the same browser, so nothing here (or in the session code)
// should clear it on auth changes. Cross-device, per-user carts would need a
// backend migration (e.g. a nullable `cart.party_id`) and are out of scope.
//
// Storage keys are kept identical to the historical ones so carts saved before
// this module existed keep working.
export const CART_STORAGE_KEY = 'tdf-marketplace-cart-id';
export const CART_META_KEY = 'tdf-marketplace-cart-meta';
/** Fired on `window` whenever the stored cart meta changes in this tab. */
export const CART_EVENT = 'tdf-cart-updated';
/** Fired on `window` to ask a mounted MarketplacePage to open its cart drawer. */
export const CART_OPEN_EVENT = 'tdf-cart-open';
/** `/marketplace#carrito` opens the cart drawer when the page mounts. */
export const CART_HASH = 'carrito';
export const MARKETPLACE_CART_PATH = `/marketplace#${CART_HASH}`;

export interface MarketplaceCartPreviewItem {
  title: string;
  subtotal: string;
}

export interface MarketplaceCartMeta {
  cartId: string;
  count: number;
  preview?: MarketplaceCartPreviewItem[];
  updatedAt: number | null;
}

export interface MarketplaceCartSummary {
  cartId: string | null;
  count: number;
}

const EMPTY_SUMMARY: MarketplaceCartSummary = Object.freeze({ cartId: null, count: 0 });

export function parseCartMeta(raw: string | null | undefined): MarketplaceCartMeta | null {
  if (!raw) return null;
  let parsed: unknown;
  try {
    parsed = JSON.parse(raw);
  } catch {
    return null;
  }
  if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) return null;
  const record = parsed as Record<string, unknown>;
  const cartId = typeof record['cartId'] === 'string' ? record['cartId'].trim() : '';
  if (!cartId) return null;
  const rawCount = record['count'];
  const count = typeof rawCount === 'number' && Number.isFinite(rawCount) && rawCount > 0
    ? Math.floor(rawCount)
    : 0;
  const rawUpdatedAt = record['updatedAt'];
  const updatedAt = typeof rawUpdatedAt === 'number' && Number.isFinite(rawUpdatedAt) ? rawUpdatedAt : null;
  const preview = Array.isArray(record['preview'])
    ? (record['preview'] as unknown[]).flatMap((entry) => {
        if (!entry || typeof entry !== 'object') return [];
        const item = entry as Record<string, unknown>;
        return typeof item['title'] === 'string'
          ? [{ title: item['title'], subtotal: typeof item['subtotal'] === 'string' ? item['subtotal'] : '' }]
          : [];
      })
    : undefined;
  return { cartId, count, updatedAt, ...(preview ? { preview } : {}) };
}

export function readCartMeta(): MarketplaceCartMeta | null {
  if (typeof window === 'undefined') return null;
  return parseCartMeta(readOptionalBrowserStorage('local', CART_META_KEY));
}

let cachedRaw: string | null | undefined;
let cachedSummary: MarketplaceCartSummary = EMPTY_SUMMARY;

/** Stable snapshot for useSyncExternalStore (same object while storage is unchanged). */
export function readCartSummary(): MarketplaceCartSummary {
  if (typeof window === 'undefined') return EMPTY_SUMMARY;
  const raw = readOptionalBrowserStorage('local', CART_META_KEY);
  if (raw === cachedRaw) return cachedSummary;
  cachedRaw = raw;
  const meta = parseCartMeta(raw);
  cachedSummary = meta ? { cartId: meta.cartId, count: meta.count } : EMPTY_SUMMARY;
  return cachedSummary;
}

export function notifyCartChanged(): void {
  if (typeof window === 'undefined') return;
  window.dispatchEvent(new Event(CART_EVENT));
}

/** Persist (or remove, when `meta` is null or empty) the cart meta and notify listeners. */
export function writeCartMeta(meta: MarketplaceCartMeta | null): void {
  if (typeof window === 'undefined') return;
  if (!meta || meta.count <= 0) {
    removeOptionalBrowserPreference(CART_META_KEY);
  } else {
    writeOptionalBrowserPreference(CART_META_KEY, JSON.stringify(meta));
  }
  notifyCartChanged();
}

/** Forget the stored cart entirely (after a verified payment/checkout). */
export function clearStoredCart(): void {
  if (typeof window === 'undefined') return;
  removeOptionalBrowserPreference(CART_STORAGE_KEY);
  removeOptionalBrowserPreference(CART_META_KEY);
  notifyCartChanged();
}

export function subscribeCartSummary(onChange: () => void): () => void {
  if (typeof window === 'undefined') return () => undefined;
  const handleStorage = (event: StorageEvent) => {
    // `key === null` means storage.clear() in another tab.
    if (event.key === null || event.key === CART_META_KEY || event.key === CART_STORAGE_KEY) {
      onChange();
    }
  };
  window.addEventListener(CART_EVENT, onChange);
  window.addEventListener('storage', handleStorage);
  return () => {
    window.removeEventListener(CART_EVENT, onChange);
    window.removeEventListener('storage', handleStorage);
  };
}

const getServerSnapshot = () => EMPTY_SUMMARY;

export function useMarketplaceCartSummary(): MarketplaceCartSummary {
  return useSyncExternalStore(subscribeCartSummary, readCartSummary, getServerSnapshot);
}

export function requestOpenMarketplaceCart(): void {
  if (typeof window === 'undefined') return;
  window.dispatchEvent(new Event(CART_OPEN_EVENT));
}

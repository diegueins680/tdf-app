import { jest } from '@jest/globals';
import {
  CART_EVENT,
  CART_META_KEY,
  CART_OPEN_EVENT,
  CART_STORAGE_KEY,
  clearStoredCart,
  parseCartMeta,
  readCartSummary,
  requestOpenMarketplaceCart,
  subscribeCartSummary,
  writeCartMeta,
} from './cartSummary';

describe('marketplace cart summary store', () => {
  beforeEach(() => {
    window.localStorage.clear();
  });

  it('keeps the historical storage keys so saved carts survive the refactor', () => {
    expect(CART_STORAGE_KEY).toBe('tdf-marketplace-cart-id');
    expect(CART_META_KEY).toBe('tdf-marketplace-cart-meta');
    expect(CART_EVENT).toBe('tdf-cart-updated');
  });

  it('parses valid meta and rejects malformed payloads', () => {
    expect(parseCartMeta(JSON.stringify({ cartId: 'c1', count: 2, updatedAt: 5, preview: [{ title: 'Mic', subtotal: '$1' }] })))
      .toEqual({ cartId: 'c1', count: 2, updatedAt: 5, preview: [{ title: 'Mic', subtotal: '$1' }] });
    expect(parseCartMeta(null)).toBeNull();
    expect(parseCartMeta('not json')).toBeNull();
    expect(parseCartMeta('[]')).toBeNull();
    expect(parseCartMeta('"cart"')).toBeNull();
    expect(parseCartMeta(JSON.stringify({ count: 2 }))).toBeNull();
    expect(parseCartMeta(JSON.stringify({ cartId: '  ', count: 2 }))).toBeNull();
    expect(parseCartMeta(JSON.stringify({ cartId: 'c1', count: 'many' }))?.count).toBe(0);
    expect(parseCartMeta(JSON.stringify({ cartId: 'c1', count: -3 }))?.count).toBe(0);
    expect(parseCartMeta(JSON.stringify({ cartId: 'c1', count: Number.NaN }))?.count).toBe(0);
  });

  it('returns a stable snapshot while storage is unchanged', () => {
    window.localStorage.setItem(CART_META_KEY, JSON.stringify({ cartId: 'c1', count: 1 }));
    const first = readCartSummary();
    expect(first).toEqual({ cartId: 'c1', count: 1 });
    expect(readCartSummary()).toBe(first);
  });

  it('survives blocked storage', () => {
    const spy = jest.spyOn(Storage.prototype, 'getItem').mockImplementation(() => {
      throw new DOMException('Blocked', 'SecurityError');
    });
    try {
      expect(readCartSummary()).toEqual({ cartId: null, count: 0 });
    } finally {
      spy.mockRestore();
    }
  });

  it('notifies subscribers when this tab writes or clears the cart', () => {
    const listener = jest.fn();
    const unsubscribe = subscribeCartSummary(listener);
    writeCartMeta({ cartId: 'c1', count: 2, updatedAt: 1 });
    expect(listener).toHaveBeenCalledTimes(1);
    expect(readCartSummary()).toEqual({ cartId: 'c1', count: 2 });

    writeCartMeta({ cartId: 'c1', count: 0, updatedAt: 2 });
    expect(listener).toHaveBeenCalledTimes(2);
    expect(window.localStorage.getItem(CART_META_KEY)).toBeNull();
    expect(readCartSummary().count).toBe(0);

    window.localStorage.setItem(CART_STORAGE_KEY, 'c1');
    writeCartMeta({ cartId: 'c1', count: 1, updatedAt: 3 });
    clearStoredCart();
    expect(listener).toHaveBeenCalledTimes(4);
    expect(window.localStorage.getItem(CART_STORAGE_KEY)).toBeNull();
    expect(readCartSummary().count).toBe(0);

    unsubscribe();
    window.dispatchEvent(new Event(CART_EVENT));
    expect(listener).toHaveBeenCalledTimes(4);
  });

  it('reacts to cross-tab storage events for cart keys only', () => {
    const listener = jest.fn();
    const unsubscribe = subscribeCartSummary(listener);
    window.dispatchEvent(new StorageEvent('storage', { key: CART_META_KEY }));
    window.dispatchEvent(new StorageEvent('storage', { key: CART_STORAGE_KEY }));
    window.dispatchEvent(new StorageEvent('storage', { key: null }));
    window.dispatchEvent(new StorageEvent('storage', { key: 'unrelated' }));
    expect(listener).toHaveBeenCalledTimes(3);
    unsubscribe();
  });

  it('dispatches the open-cart request event', () => {
    const listener = jest.fn();
    window.addEventListener(CART_OPEN_EVENT, listener);
    requestOpenMarketplaceCart();
    window.removeEventListener(CART_OPEN_EVENT, listener);
    expect(listener).toHaveBeenCalledTimes(1);
  });
});

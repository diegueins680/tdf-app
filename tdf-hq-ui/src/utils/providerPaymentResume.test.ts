/** @jest-environment jsdom */
import {
  clearProviderPaymentPending,
  clearProviderPaymentResume,
  loadOrCreatePaymentIdempotencyKey,
  loadProviderPaymentPending,
  loadProviderPaymentResume,
  paymentAttemptCanBeReleased,
  safePaymentReturnPath,
  saveProviderPaymentPending,
  saveProviderPaymentResume,
} from './providerPaymentResume';

describe('provider payment redirect recovery', () => {
  const checkoutId = 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa';

  beforeEach(() => window.sessionStorage.clear());

  it('stores the lookup capability outside URLs and restores the exact attempt', () => {
    expect(saveProviderPaymentResume({
      version: 1,
      checkoutId,
      attemptId: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb',
      provider: 'placetopay',
      paymentMethod: 'card',
      lookupToken: 'secure-checkout-lookup-token',
      returnPath: '/eventos/41/orden/92',
      createdAt: Date.now(),
    })).toBe(true);

    expect(loadProviderPaymentResume(checkoutId)).toMatchObject({
      checkoutId,
      attemptId: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb',
      lookupToken: 'secure-checkout-lookup-token',
      returnPath: '/eventos/41/orden/92',
    });
    expect(window.location.href).not.toContain('secure-checkout-lookup-token');
  });

  it('rejects cross-origin, protocol-relative, expired, and malformed resume data', () => {
    expect(safePaymentReturnPath('/curso/demo/orden/7')).toBe('/curso/demo/orden/7');
    expect(safePaymentReturnPath('//evil.example/path')).toBeNull();
    expect(safePaymentReturnPath('https://evil.example/path')).toBeNull();
    expect(saveProviderPaymentResume({
      version: 1,
      checkoutId,
      attemptId: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb',
      provider: 'payphone',
      paymentMethod: 'payphone_wallet',
      lookupToken: 'secure-checkout-lookup-token',
      returnPath: '//evil.example',
      createdAt: Date.now(),
    })).toBe(false);
  });

  it('reuses one idempotency key until explicitly cleared', () => {
    const first = loadOrCreatePaymentIdempotencyKey(checkoutId, 'placetopay', 'card');
    const second = loadOrCreatePaymentIdempotencyKey(checkoutId, 'placetopay', 'card');
    expect(second).toBe(first);
    expect(first).toMatch(/^payment-session-/);
  });

  it('retains a pre-response attempt lock with no lookup capability in the URL', () => {
    expect(saveProviderPaymentPending({
      version: 1,
      checkoutId,
      provider: 'payphone',
      paymentMethod: 'payphone_wallet',
      lookupToken: 'secure-checkout-lookup-token',
      returnPath: '/reservas/orden/91',
      buyerPhone: '991234567',
      buyerCountryCode: '593',
      createdAt: Date.now(),
    })).toBe(true);
    expect(loadProviderPaymentPending(checkoutId)).toMatchObject({
      checkoutId,
      provider: 'payphone',
      paymentMethod: 'payphone_wallet',
      buyerPhone: '991234567',
    });
    expect(window.location.href).not.toContain('secure-checkout-lookup-token');
    clearProviderPaymentPending(checkoutId);
    expect(loadProviderPaymentPending(checkoutId)).toBeNull();
  });

  it('does not clear another checkout active in the tab', () => {
    saveProviderPaymentResume({
      version: 1,
      checkoutId,
      attemptId: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb',
      provider: 'placetopay',
      paymentMethod: 'card',
      lookupToken: 'secure-checkout-lookup-token',
      returnPath: '/return',
      createdAt: Date.now(),
    });
    clearProviderPaymentResume('cccccccc-cccc-4ccc-8ccc-cccccccccccc');
    expect(loadProviderPaymentResume(checkoutId)).not.toBeNull();
  });

  it('releases recovery only for authoritative no-charge outcomes', () => {
    expect(paymentAttemptCanBeReleased('succeeded', false)).toBe(false);
    expect(paymentAttemptCanBeReleased('confirmed_no_charge', true)).toBe(true);
    expect(paymentAttemptCanBeReleased('failed', true)).toBe(true);
    expect(paymentAttemptCanBeReleased('failed', false)).toBe(false);
    expect(paymentAttemptCanBeReleased('processing', false)).toBe(false);
    expect(paymentAttemptCanBeReleased('ambiguous', false)).toBe(false);
    expect(paymentAttemptCanBeReleased(undefined, undefined)).toBe(false);
  });
});

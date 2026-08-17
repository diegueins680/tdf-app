import { jest } from '@jest/globals';

const getMock = jest.fn<(path: string, init?: RequestInit) => Promise<unknown>>();
const postMock = jest.fn<(path: string, body: unknown, init?: RequestInit) => Promise<unknown>>();
const putMock = jest.fn<(path: string, body: unknown, init?: RequestInit) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: (path: string, init?: RequestInit) => getMock(path, init),
  post: (path: string, body: unknown, init?: RequestInit) => postMock(path, body, init),
  put: (path: string, body: unknown, init?: RequestInit) => putMock(path, body, init),
}));

const {
  Marketplace,
  getMarketplaceCheckoutIdempotencyKey,
  loadMarketplaceLookupToken,
  storeMarketplaceLookupToken,
} = await import('./marketplace');

describe('marketplace checkout API security contract', () => {
  beforeEach(() => {
    getMock.mockReset();
    postMock.mockReset();
    putMock.mockReset();
    window.localStorage.clear();
    window.sessionStorage.clear();
  });

  it('uses one stable checkout idempotency key when the customer switches payment rails', () => {
    const datafastKey = getMarketplaceCheckoutIdempotencyKey('cart-1', 'datafast');
    const paypalKey = getMarketplaceCheckoutIdempotencyKey('cart-1', 'paypal');
    const manualKey = getMarketplaceCheckoutIdempotencyKey('cart-1', 'bank_transfer');

    expect(datafastKey).toMatch(/^[0-9a-f-]{36}$/i);
    expect(paypalKey).toBe(datafastKey);
    expect(manualKey).toBe(datafastKey);
    expect(window.localStorage.getItem('tdf-marketplace-checkout-idempotency:cart-1')).toBe(datafastKey);
  });

  it('sends the immutable checkout key on every order-creation request', async () => {
    postMock.mockResolvedValue({});
    const payload = { mcrBuyerName: 'Ada', mcrBuyerEmail: 'ada@example.com' };

    await Marketplace.datafastCheckout('cart-1', payload, 'checkout-key');

    expect(postMock).toHaveBeenCalledWith(
      '/marketplace/cart/cart-1/datafast/checkout',
      payload,
      { headers: { 'Idempotency-Key': 'checkout-key' } },
    );
  });

  it('keeps guest lookup credentials out of URLs and sends them only as headers', async () => {
    getMock.mockResolvedValue({});
    storeMarketplaceLookupToken('order-1', 'lookup-secret');

    expect(loadMarketplaceLookupToken('order-1')).toBe('lookup-secret');
    await Marketplace.getOrder('order-1', 'lookup-secret');
    await Marketplace.confirmDatafastPayment('order-1', '/v1/checkouts/checkout-1/payment', 'lookup-secret');

    expect(getMock).toHaveBeenNthCalledWith(
      1,
      '/marketplace/orders/order-1',
      { headers: { 'X-Order-Lookup-Token': 'lookup-secret' } },
    );
    expect(getMock.mock.calls[1]?.[0]).not.toContain('lookup-secret');
    expect(getMock.mock.calls[1]?.[1]).toEqual({
      headers: { 'X-Order-Lookup-Token': 'lookup-secret' },
    });
  });

  it('submits manual evidence without a paid assertion and uses protected staff review routes', async () => {
    postMock.mockResolvedValue({});
    getMock.mockResolvedValue({});

    await Marketplace.submitManualEvidence('order-1', 'BANK-REFERENCE-1', 'lookup-secret');
    expect(postMock).toHaveBeenNthCalledWith(
      1,
      '/marketplace/orders/order-1/manual-payment/evidence',
      { mmesCustomerReference: 'BANK-REFERENCE-1' },
      { headers: { 'X-Order-Lookup-Token': 'lookup-secret' } },
    );
    expect(postMock.mock.calls[0]?.[0]).not.toContain('lookup-secret');

    await Marketplace.getCommerce('order-1');
    expect(getMock).toHaveBeenCalledWith('/marketplace/orders/order-1/commerce', undefined);

    await Marketplace.reviewManualPayment('order-1', 'approve', 'Matched bank statement.');
    expect(postMock).toHaveBeenNthCalledWith(
      2,
      '/marketplace/orders/order-1/manual-payment/review',
      { mmprAction: 'approve', mmprReviewNotes: 'Matched bank statement.' },
      undefined,
    );
  });
});

import { jest } from '@jest/globals';

const getMock = jest.fn();
const postMock = jest.fn();
const putMock = jest.fn();
const patchMock = jest.fn();
const deleteMock = jest.fn();
const postFormMock = jest.fn();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
  post: postMock,
  put: putMock,
  patch: patchMock,
  del: deleteMock,
  postForm: postFormMock,
}));

const {
  Merch,
  createMerchIdempotencyKey,
  readStoredMerchCart,
  readStoredMerchOrder,
  storeMerchCart,
  storeMerchOrder,
} = await import('./merch');
const { MerchReputation } = await import('./merchReputation');

describe('merch API capabilities and idempotency', () => {
  beforeEach(() => {
    sessionStorage.clear();
    jest.clearAllMocks();
  });

  it('keeps opaque cart and order capabilities in session storage only', () => {
    storeMerchCart('cementerio-de-elefantes', {
      id: 'cart-id', storeId: 'store-id', storeSlug: 'cementerio-de-elefantes',
      status: 'active', currency: 'USD', expiresAt: new Date().toISOString(),
      lookupToken: 'cart-secret', items: [], productSubtotalMinor: 0, totalMinor: 0,
    });
    storeMerchOrder({
      id: 'order-id', orderNumber: 'TDF-MERCH-1', storeId: 'store-id',
      storeName: 'Cementerio de Elefantes', storeSlug: 'cementerio-de-elefantes',
      currency: 'USD', productSubtotalMinor: 1000, discountMinor: 0, taxMinor: 0,
      shippingMinor: 0, totalMinor: 1000, commercialStatus: 'pending',
      paymentStatus: 'pending', fulfillmentStatus: 'pending', refundStatus: 'none',
      disputeStatus: 'none', lookupToken: 'order-secret', createdAt: new Date().toISOString(),
      updatedAt: new Date().toISOString(), lines: [], timeline: [],
    });

    expect(readStoredMerchCart('cementerio-de-elefantes')).toEqual({ id: 'cart-id', token: 'cart-secret' });
    expect(readStoredMerchOrder('order-id')).toEqual({ id: 'order-id', token: 'order-secret' });
    expect(localStorage.length).toBe(0);
  });

  it('rejects a cart response without its private lookup capability', () => {
    expect(() => storeMerchCart('artist', {
      id: 'cart-id', storeId: 'store-id', storeSlug: 'artist', status: 'active',
      currency: 'USD', expiresAt: new Date().toISOString(), items: [],
      productSubtotalMinor: 0, totalMinor: 0,
    })).toThrow(/capacidad privada/i);
  });

  it('sends checkout idempotency and capability headers without putting secrets in the URL', async () => {
    postMock.mockResolvedValueOnce({ id: 'order-id' });
    await Merch.checkout('cart/id', 'cart-secret', 'checkout-key-123', {
      recipient: {
        name: 'Paola', email: 'paola@example.test', phone: null, countryCode: 'EC',
        subdivision: 'P', city: 'Quito', addressLine1: 'Synthetic address',
        addressLine2: null, postalCode: null, deliveryNote: null,
      },
      shippingZoneId: 'zone-id', createAccount: false, locale: 'es',
    });

    expect(postMock).toHaveBeenCalledWith('/merch/carts/cart%2Fid/checkout', expect.any(Object), {
      headers: { 'X-Cart-Lookup-Token': 'cart-secret', 'Idempotency-Key': 'checkout-key-123' },
    });
  });

  it('cancels an unpaid order with both the private capability and a retry key', async () => {
    postMock.mockResolvedValueOnce({ id: 'order-id', commercialStatus: 'cancelled' });

    await Merch.cancelUnpaidOrder('order/id', 'order-secret', 'Changed my mind', 'cancel-key-123');

    expect(postMock).toHaveBeenCalledWith(
      '/merch/orders/order%2Fid/cancel',
      { reason: 'Changed my mind' },
      { headers: { 'X-Order-Lookup-Token': 'order-secret', 'Idempotency-Key': 'cancel-key-123' } },
    );
  });

  it('claims a guest order for reviews without placing its capability in the URL or body', async () => {
    putMock.mockResolvedValueOnce({ orderId: 'order/id', buyerLinked: true });

    await MerchReputation.claimBuyer('order/id', 'private-order-capability');

    expect(putMock).toHaveBeenCalledWith(
      '/merch/orders/order%2Fid/review-buyer-claim',
      undefined,
      { headers: { 'X-Order-Lookup-Token': 'private-order-capability' } },
    );
  });

  it('creates distinct, scoped idempotency keys', () => {
    const first = createMerchIdempotencyKey('product');
    const second = createMerchIdempotencyKey('product');
    expect(first).toMatch(/^product:/);
    expect(second).not.toBe(first);
  });

  it('updates only the scoped variant stock projection with an optimistic version', async () => {
    patchMock.mockResolvedValueOnce({ id: 'variant-id', stockOnHand: 12, version: 8 });
    await Merch.updateVariantStock('store/id', 'variant/id', {
      stockOnHand: 12,
      reorderThreshold: 3,
      active: true,
      version: 7,
    });

    expect(patchMock).toHaveBeenCalledWith(
      '/merch/seller/stores/store%2Fid/variants/variant%2Fid/stock',
      { stockOnHand: 12, reorderThreshold: 3, active: true, version: 7 },
    );
  });

  it('keeps seller and staff issue triage on distinct scoped endpoints', async () => {
    patchMock.mockResolvedValue({ id: 'issue-id', status: 'staff_review' });
    const body = { status: 'staff_review' as const, publicResponse: null, internalNotes: 'Synthetic escalation' };

    await Merch.updateSellerIssue('store/id', 'issue/id', body);
    await Merch.updateAdminIssue('issue/id', body);

    expect(patchMock).toHaveBeenNthCalledWith(1, '/merch/seller/stores/store%2Fid/issues/issue%2Fid', body);
    expect(patchMock).toHaveBeenNthCalledWith(2, '/merch/admin/issues/issue%2Fid', body);
  });
});

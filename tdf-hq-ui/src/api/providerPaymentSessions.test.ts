import { jest } from '@jest/globals';

const getMock = jest.fn<(path: string, init?: RequestInit) => Promise<unknown>>();
const postMock = jest.fn<(path: string, body: unknown, init?: RequestInit) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
  post: postMock,
}));

const {
  createProviderPaymentSession,
  getProviderPaymentSession,
  paymentMethodDefinition,
  safePlaceToPayRedirect,
} = await import('./providerPaymentSessions');

describe('provider payment-session client', () => {
  const checkoutId = 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa';
  const attemptId = 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb';
  const lookupToken = 'secure-checkout-lookup-token';

  beforeEach(() => {
    getMock.mockReset().mockResolvedValue({});
    postMock.mockReset().mockResolvedValue({});
  });

  it('sends lookup capability and exact idempotency key only in headers', async () => {
    await createProviderPaymentSession(
      checkoutId,
      lookupToken,
      'payment-session-11111111-1111-4111-8111-111111111111',
      { provider: 'placetopay', paymentMethod: 'bank_redirect' },
    );

    expect(postMock).toHaveBeenCalledWith(
      `/commerce/checkouts/${checkoutId}/payment-sessions`,
      { provider: 'placetopay', paymentMethod: 'bank_redirect' },
      { headers: {
        'X-Checkout-Lookup-Token': lookupToken,
        'Idempotency-Key': 'payment-session-11111111-1111-4111-8111-111111111111',
      } },
    );
    expect(JSON.stringify(postMock.mock.calls[0]?.[1])).not.toContain(lookupToken);
  });

  it('loads only the checkout-bound attempt with the lookup capability', async () => {
    await getProviderPaymentSession(checkoutId, attemptId, lookupToken);

    expect(getMock).toHaveBeenCalledWith(
      `/commerce/checkouts/${checkoutId}/payment-sessions/${attemptId}`,
      { headers: { 'X-Checkout-Lookup-Token': lookupToken } },
    );
  });

  it('rejects malformed identifiers before any request', () => {
    expect(() => createProviderPaymentSession(
      '../checkout',
      lookupToken,
      'payment-session-11111111-1111-4111-8111-111111111111',
      { provider: 'placetopay', paymentMethod: 'card' },
    )).toThrow('checkoutId must be a UUID');
    expect(postMock).not.toHaveBeenCalled();
  });

  it('maps only supported public labels to exact provider methods', () => {
    expect(paymentMethodDefinition('placetopay_deuna_qr')).toEqual({
      label: 'placetopay_deuna_qr',
      provider: 'placetopay',
      paymentMethod: 'deuna_qr',
    });
    expect(paymentMethodDefinition('unknown')).toBeNull();
  });

  it('allows only exact HTTPS PlaceToPay Ecuador checkout hosts', () => {
    expect(safePlaceToPayRedirect('https://checkout-test.placetopay.ec/session/42/token'))
      .toBe('https://checkout-test.placetopay.ec/session/42/token');
    expect(safePlaceToPayRedirect('https://checkout.placetopay.ec/session/42'))
      .toBe('https://checkout.placetopay.ec/session/42');
    expect(safePlaceToPayRedirect('https://checkout.placetopay.ec.attacker.example/session/42'))
      .toBeNull();
    expect(safePlaceToPayRedirect('https://checkout.placetopay.ec@attacker.example/session/42'))
      .toBeNull();
    expect(safePlaceToPayRedirect('http://checkout.placetopay.ec/session/42'))
      .toBeNull();
  });
});

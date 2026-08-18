import { jest } from '@jest/globals';

const getMock = jest.fn<(path: string, init?: RequestInit) => Promise<unknown>>();
const postMock = jest.fn<(path: string, body: unknown, init?: RequestInit) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
  post: postMock,
}));

const { EventTickets } = await import('./eventTickets');

describe('EventTickets public API contract', () => {
  beforeEach(() => {
    getMock.mockReset();
    postMock.mockReset();
  });

  it('creates a guest checkout with server pricing and an idempotency key', async () => {
    postMock.mockResolvedValue({ orderId: 11 });
    const payload = {
      tierId: 7,
      quantity: 2,
      buyerName: 'Ana Rivera',
      buyerEmail: 'ana@example.com',
      termsAccepted: true as const,
    };

    await EventTickets.createCheckout(4, payload, 'ticket-checkout-key');

    expect(postMock).toHaveBeenCalledWith(
      '/public/events/4/ticket-orders',
      payload,
      { headers: { 'Idempotency-Key': 'ticket-checkout-key' } },
    );
  });

  it('protects tracking and provider calls with the lookup token', async () => {
    getMock.mockResolvedValue({ orderId: 11 });

    await EventTickets.confirmDatafastStatus(
      4,
      11,
      '/v1/checkouts/provider-checkout/payment',
      'lookup-secret',
    );

    expect(getMock).toHaveBeenCalledWith(
      '/public/events/4/ticket-orders/11/datafast/status?resourcePath=%2Fv1%2Fcheckouts%2Fprovider-checkout%2Fpayment',
      { headers: { 'X-Order-Lookup-Token': 'lookup-secret' } },
    );
  });

  it('fails locally on invalid public identifiers', () => {
    expect(() => EventTickets.getStorefront(0)).toThrow('eventId');
    expect(() => EventTickets.getCheckout(4, -1, 'lookup')).toThrow('orderId');
  });
});

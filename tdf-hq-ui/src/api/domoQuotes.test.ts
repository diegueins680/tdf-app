import { jest } from '@jest/globals';

const getMock = jest.fn();
const postMock = jest.fn();

jest.unstable_mockModule('./client', () => ({ get: getMock, post: postMock }));

const { DomoQuotes } = await import('./domoQuotes');

describe('DomoQuotes client', () => {
  const quoteId = 'd1000000-0000-4000-8000-000000000001';

  beforeEach(() => {
    getMock.mockReset();
    postMock.mockReset();
  });

  it('creates a quote with an idempotency key', async () => {
    const payload = {
      customerName: 'Ana',
      customerEmail: 'ana@example.com',
      eventType: 'wedding',
      guests: 80,
      startsAt: '2030-01-10T15:00:00Z',
      durationHours: 8,
      setupHours: 2,
      catering: true,
      production: true,
      transport: false,
    };
    await DomoQuotes.createQuote(payload, 'domo-checkout-idempotency-1');
    expect(postMock).toHaveBeenCalledWith('/public/domo/quotes', payload, {
      headers: { 'Idempotency-Key': 'domo-checkout-idempotency-1' },
    });
  });

  it('encodes the Datafast resource and sends the secure lookup token', async () => {
    await DomoQuotes.confirmDatafastStatus(
      quoteId,
      '/v1/checkouts/provider-resource/payment',
      'secure-token',
    );
    expect(getMock).toHaveBeenCalledWith(
      `/public/domo/quotes/${quoteId}/datafast/status?resourcePath=%2Fv1%2Fcheckouts%2Fprovider-resource%2Fpayment`,
      { headers: { 'X-Order-Lookup-Token': 'secure-token' } },
    );
  });

  it('rejects malformed quote identifiers before a request is sent', () => {
    expect(() => DomoQuotes.getQuote('../admin', 'secure-token')).toThrow('quoteId must be a UUID.');
    expect(getMock).not.toHaveBeenCalled();
  });
});

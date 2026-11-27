import { jest } from '@jest/globals';

const getCourse = jest.fn<(...args: unknown[]) => Promise<Record<string, unknown>>>();
const getTickets = jest.fn<(...args: unknown[]) => Promise<Record<string, unknown>>>();
const getBooking = jest.fn<(...args: unknown[]) => Promise<Record<string, unknown>>>();
const getDomo = jest.fn<(...args: unknown[]) => Promise<Record<string, unknown>>>();
const getService = jest.fn<(...args: unknown[]) => Promise<Record<string, unknown>>>();
const getMethods = jest.fn<(...args: unknown[]) => Promise<Record<string, unknown>>>();
jest.unstable_mockModule('./courses', () => ({ Courses: { getCheckout: getCourse } }));
jest.unstable_mockModule('./eventTickets', () => ({ EventTickets: { getCheckout: getTickets } }));
jest.unstable_mockModule('./bookings', () => ({ Bookings: { getPublicCheckout: getBooking } }));
jest.unstable_mockModule('./domoQuotes', () => ({ DomoQuotes: { getQuote: getDomo } }));
jest.unstable_mockModule('./serviceStorefront', () => ({ ServiceStorefront: { getOrder: getService } }));
jest.unstable_mockModule('./paymentCapabilities', () => ({ loadAvailableCheckoutMethods: getMethods }));

const { loadCheckoutPaymentAlternatives } = await import('./checkoutPaymentAlternatives');
const reference = { checkoutId: 'checkout-1', lookupToken: 'private-original-token', returnPath: '/curso/produccion/orden/26' };
beforeEach(() => jest.resetAllMocks());

it('reads the original course and only offers its current enabled methods', async () => {
  getCourse.mockResolvedValue({ checkoutId: 'checkout-1', checkoutAvailable: true, paymentStatus: 'failed', paymentMethods: ['paypal', 'payphone_wallet'] });
  expect(await loadCheckoutPaymentAlternatives(reference)).toEqual(['paypal', 'payphone_wallet']);
  expect(getCourse).toHaveBeenCalledWith('produccion', 26, reference.lookupToken);
});

it.each([
  { checkoutId: 'other-checkout', checkoutAvailable: true, paymentStatus: 'failed' },
  { checkoutId: 'checkout-1', checkoutAvailable: false, paymentStatus: 'failed' },
  { checkoutId: 'checkout-1', checkoutAvailable: true, paymentStatus: 'paid' },
  { checkoutId: 'checkout-1', checkoutAvailable: true, paymentStatus: 'expired' },
])('does not offer methods for closed, unavailable or different orders: %j', async (order) => {
  getCourse.mockResolvedValue({ ...order, paymentMethods: ['paypal'] });
  expect(await loadCheckoutPaymentAlternatives(reference)).toEqual([]);
});

it('preserves a failed lookup as an error instead of inferring availability', async () => {
  getTickets.mockRejectedValue(new Error('network unavailable'));
  await expect(loadCheckoutPaymentAlternatives({ ...reference, returnPath: '/eventos/7/orden/9' })).rejects.toThrow('network unavailable');
});

it('only lists booking methods executable on its original tracking screen', async () => {
  getBooking.mockResolvedValue({ checkoutId: 'checkout-1', paymentStatus: 'failed', paymentMethods: ['paypal', 'datafast', 'placetopay_card', 'payphone_wallet'] });
  expect(await loadCheckoutPaymentAlternatives({ ...reference, returnPath: '/reservas/orden/12' })).toEqual(['placetopay_card', 'payphone_wallet']);
});

it('reads ticket and Domo alternatives without preparing another checkout', async () => {
  getTickets.mockResolvedValue({ checkoutId: 'checkout-1', paymentStatus: 'failed', paymentMethods: ['bank_transfer', 'paypal'] });
  expect(await loadCheckoutPaymentAlternatives({ ...reference, returnPath: '/eventos/7/orden/9' })).toEqual(['bank_transfer', 'paypal']);
  getDomo.mockResolvedValue({ checkoutId: 'checkout-1', paymentStatus: 'failed', quoteStatus: 'deposit_due', paymentMethods: ['payphone_card'] });
  expect(await loadCheckoutPaymentAlternatives({ ...reference, returnPath: '/domo-del-pululahua/cotizaciones/quote-1' })).toEqual(['payphone_card']);
});

it('checks capabilities for the immutable service amount without inventing buyer country', async () => {
  getService.mockResolvedValue({ ssoCheckoutId: 'checkout-1', ssoStatus: 'payment_failed', ssoCurrency: 'USD', ssoPriceUsdCents: 50000 });
  getMethods.mockResolvedValue({ placeToPayCard: false, payPhoneCard: true, payPhoneWallet: true, paypal: true });
  expect(await loadCheckoutPaymentAlternatives({ ...reference, returnPath: '/mezcla-mastering/pedido/ORDER-1' })).toEqual(['payphone_card', 'payphone_wallet']);
  expect(getMethods).toHaveBeenCalledWith({ currency: 'USD', amountMinor: 50000, productFlow: 'professional_service' });
});

it.each(['//outside.invalid/order', '/\\outside.invalid/order', '/unknown/order'])('never guesses an order for %s', async (returnPath) => {
  expect(await loadCheckoutPaymentAlternatives({ ...reference, returnPath })).toEqual([]);
  expect(getCourse).not.toHaveBeenCalled();
  expect(getTickets).not.toHaveBeenCalled();
  expect(getBooking).not.toHaveBeenCalled();
  expect(getDomo).not.toHaveBeenCalled();
  expect(getService).not.toHaveBeenCalled();
});

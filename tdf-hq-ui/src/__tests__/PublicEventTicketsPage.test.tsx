/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter, Route, Routes } from 'react-router-dom';

const getStorefrontMock = jest.fn<(eventId: number) => Promise<unknown>>();
const getCheckoutMock = jest.fn<(eventId: number, orderId: number, token: string) => Promise<unknown>>();
const confirmDatafastStatusMock = jest.fn<(
  eventId: number,
  orderId: number,
  resourcePath: string,
  token: string,
) => Promise<unknown>>();

jest.unstable_mockModule('../api/eventTickets', () => ({
  EventTickets: {
    getStorefront: (eventId: number) => getStorefrontMock(eventId),
    getCheckout: (eventId: number, orderId: number, token: string) =>
      getCheckoutMock(eventId, orderId, token),
    confirmDatafastStatus: (
      eventId: number,
      orderId: number,
      resourcePath: string,
      token: string,
    ) => confirmDatafastStatusMock(eventId, orderId, resourcePath, token),
    createCheckout: jest.fn(),
    createDatafastCheckout: jest.fn(),
    createPaypalOrder: jest.fn(),
    capturePaypalOrder: jest.fn(),
  },
}));

jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({
  useLocalePreferences: () => ({ locale: 'es-EC', timezone: 'America/Guayaquil' }),
}));

jest.unstable_mockModule('../hooks/useMetaTags', () => ({
  useMetaTags: jest.fn(),
}));

const { default: PublicEventTicketsPage } = await import('../pages/PublicEventTicketsPage');

const storefrontFixture = {
  eventId: 41,
  title: 'Festival TDF',
  description: 'Evento público',
  startsAt: '2030-08-20T22:00:00Z',
  endsAt: '2030-08-21T02:00:00Z',
  timezone: 'America/Guayaquil',
  venueName: 'Domo',
  venueAddress: null,
  checkoutAvailable: true,
  unavailableReason: null,
  tiers: [{
    tierId: 8,
    code: 'GENERAL',
    name: 'General',
    description: null,
    unitPriceMinor: 2000,
    currency: 'USD',
    remaining: 25,
    salesStart: null,
    salesEnd: null,
    transfersAllowed: true,
  }],
};

const checkoutFixture = (overrides: Record<string, unknown> = {}) => ({
  orderId: 92,
  eventId: 41,
  checkoutId: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
  lookupToken: null,
  paymentStatus: 'processing',
  fulfillmentStatus: 'seat_held',
  holdExpiresAt: '2030-08-20T21:00:00Z',
  quote: {
    policyVersion: 'owned-event-v1',
    currency: 'USD',
    quantity: 1,
    unitPriceMinor: 2000,
    grossFaceValueMinor: 2000,
    discountMinor: 0,
    netFaceValueMinor: 2000,
    buyerPlatformFeeMinor: 40,
    organizerPlatformFeeMinor: 40,
    taxMinor: 0,
    checkoutTotalMinor: 2040,
    organizerPayableMinor: 1960,
    platformFeeMinor: 80,
    termsVersion: 'event-ticket-terms-v1',
  },
  paymentMethods: ['datafast', 'paypal'],
  tickets: [],
  ...overrides,
});

const flush = async () => {
  await act(async () => {
    await new Promise((resolve) => setTimeout(resolve, 0));
  });
};

const waitForExpectation = async (assertion: () => void, attempts = 12) => {
  let lastError: unknown;
  for (let index = 0; index < attempts; index += 1) {
    try {
      assertion();
      return;
    } catch (error) {
      lastError = error;
      await flush();
    }
  }
  throw lastError;
};

describe('PublicEventTicketsPage verified payment boundary', () => {
  let container: HTMLDivElement;
  let root: Root;
  let queryClient: QueryClient;

  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    window.localStorage.clear();
    window.localStorage.setItem('tdf:event-ticket-checkout:41:92', 'secure-lookup-token');
    getStorefrontMock.mockReset().mockResolvedValue(storefrontFixture);
    getCheckoutMock.mockReset();
    confirmDatafastStatusMock.mockReset();
    queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    container = document.createElement('div');
    document.body.appendChild(container);
    root = createRoot(container);
  });

  afterEach(async () => {
    await act(async () => root.unmount());
    queryClient.clear();
    container.remove();
  });

  const renderTracking = async (route: string) => {
    await act(async () => {
      root.render(
        <MemoryRouter initialEntries={[route]}>
          <QueryClientProvider client={queryClient}>
            <Routes>
              <Route path="/eventos/:eventId/orden/:orderId" element={<PublicEventTicketsPage />} />
            </Routes>
          </QueryClientProvider>
        </MemoryRouter>,
      );
    });
    await waitForExpectation(() => expect(getStorefrontMock).toHaveBeenCalledWith(41));
  };

  it('treats a Datafast browser return as processing until server verification finishes', async () => {
    confirmDatafastStatusMock.mockResolvedValue(checkoutFixture());
    getCheckoutMock.mockResolvedValue(checkoutFixture());
    await renderTracking('/eventos/41/orden/92?resourcePath=%2Fv1%2Fcheckouts%2Fprovider-1%2Fpayment');

    await waitForExpectation(() => expect(confirmDatafastStatusMock).toHaveBeenCalledWith(
      41,
      92,
      '/v1/checkouts/provider-1/payment',
      'secure-lookup-token',
    ));
    expect(container.textContent).toContain('La orden no está pagada');
    expect(container.textContent).toContain('Pago: processing');
    expect(container.textContent).not.toContain('Pago verificado por el servidor');
    expect(container.textContent).not.toContain('TICKET-');
  });

  it('reports a failed provider verification without fabricating payment or tickets', async () => {
    confirmDatafastStatusMock.mockRejectedValue(new Error('provider unavailable'));
    await renderTracking('/eventos/41/orden/92?resourcePath=%2Fv1%2Fcheckouts%2Fprovider-1%2Fpayment');

    await waitForExpectation(() => expect(container.textContent).toContain(
      'El servidor no pudo verificar esta orden. No mostramos ningún pago como exitoso.',
    ));
    expect(container.textContent).not.toContain('El servidor verificó el pago');
    expect(container.textContent).not.toContain('TICKET-');
  });

  it('shows ticket codes only after the server returns paid and issued states', async () => {
    getCheckoutMock.mockResolvedValue(checkoutFixture({
      paymentStatus: 'paid',
      fulfillmentStatus: 'issued',
      paymentMethods: [],
      tickets: [{
        ticketId: 501,
        ticketCode: 'TICKET-VERIFIED-501',
        status: 'valid',
        holderName: 'Comprador',
      }],
    }));
    await renderTracking('/eventos/41/orden/92');

    await waitForExpectation(() => expect(getCheckoutMock).toHaveBeenCalledWith(
      41,
      92,
      'secure-lookup-token',
    ));
    expect(container.textContent).toContain('El servidor verificó el pago y emitió las entradas.');
    expect(container.textContent).toContain('TICKET-VERIFIED-501');
    expect(container.textContent).toContain('Pago: paid');
    expect(container.textContent).toContain('Cumplimiento: issued');
  });
});

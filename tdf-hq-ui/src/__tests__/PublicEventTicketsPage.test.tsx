/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import { fireEvent } from '@testing-library/react';

const getStorefrontMock = jest.fn<(eventId: number) => Promise<unknown>>();
const createCheckoutMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
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
    createCheckout: createCheckoutMock,
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

const qrCanvasMock = jest.fn<() => Promise<void>>().mockResolvedValue(undefined);
jest.unstable_mockModule('qrcode', () => ({ default: { toCanvas: qrCanvasMock } }));
jest.unstable_mockModule('../mobile/MobilePromo', () => ({ default: () => null }));

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
  policy: {
    policyVersion: 'owned-event-v1',
    currency: 'USD',
    buyerFeeBps: 200,
    organizerFeeBps: 200,
    taxBps: 0,
    holdMinutes: 15,
    termsVersion: 'event-ticket-terms-v1',
    termsSummary: 'Aceptas el precio, las tarifas y las condiciones mostradas.',
    refundPolicy: 'Reembolso total.',
    transferAllowed: true,
  },
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
    window.sessionStorage.clear();
    window.localStorage.setItem('tdf:event-ticket-checkout:41:92', 'secure-lookup-token');
    getStorefrontMock.mockReset().mockResolvedValue(storefrontFixture);
    createCheckoutMock.mockReset().mockResolvedValue(checkoutFixture());
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
              <Route path="/eventos/:eventId/entradas" element={<PublicEventTicketsPage />} />
              <Route path="/eventos/:eventId/orden/:orderId" element={<PublicEventTicketsPage />} />
            </Routes>
          </QueryClientProvider>
        </MemoryRouter>,
      );
    });
    await waitForExpectation(() => expect(getStorefrontMock).toHaveBeenCalledWith(41));
  };

  it('shows the approved ticket policy before consent and checkout', async () => {
    await renderTracking('/eventos/41/entradas');

    await waitForExpectation(() => expect(container.textContent).toContain(
      'Aceptas el precio, las tarifas y las condiciones mostradas.',
    ));
    expect(container.textContent).toContain('Política de reembolso: Reembolso total.');
    expect(container.textContent).toContain('Tarifa al comprador: 2%');
    expect(container.textContent).toContain('Tarifa al organizador (descontada del pago): 2%');
    expect(container.textContent).toContain('Retención temporal de inventario: 15 minutos');
    expect(container.textContent).toContain('Transferencias: permitidas');
    expect(container.textContent).toContain('Versión de términos: event-ticket-terms-v1');
  });

  it('shows the server policy limit and rejects an oversized quantity even when HTML validation is bypassed', async () => {
    getStorefrontMock.mockResolvedValue({
      ...storefrontFixture,
      policy: { ...storefrontFixture.policy, maxTicketsPerOrder: 4 },
    });
    await renderTracking('/eventos/41/entradas?tierId=8&quantity=5');
    await waitForExpectation(() => expect(container.textContent).toContain('Hasta 4 entradas por orden.'));
    expect(container.querySelector<HTMLInputElement>('input[type="number"]')?.max).toBe('4');
    await act(async () => {
      fireEvent.click(container.querySelector<HTMLInputElement>('input[type="checkbox"]')!);
    });
    await act(async () => {
      fireEvent.submit(container.querySelector('form')!);
    });
    expect(container.textContent).toContain('Puedes comprar hasta 4 entradas por orden.');
    expect(createCheckoutMock).not.toHaveBeenCalled();
  });

  it('permits the exact policy boundary and retains the actual remaining stock display', async () => {
    getStorefrontMock.mockResolvedValue({
      ...storefrontFixture,
      policy: { ...storefrontFixture.policy, maxTicketsPerOrder: 4 },
    });
    await renderTracking('/eventos/41/entradas?tierId=8&quantity=4');
    await waitForExpectation(() => expect(container.textContent).toContain('Hasta 4 entradas por orden.'));
    expect(container.textContent).toContain('25 disponibles');
    await act(async () => {
      fireEvent.change(container.querySelector<HTMLInputElement>('input[maxlength="160"]')!,
        { target: { value: 'Comprador de prueba' } });
      fireEvent.change(container.querySelector<HTMLInputElement>('input[type="email"]')!,
        { target: { value: 'buyer@example.invalid' } });
      fireEvent.click(container.querySelector<HTMLInputElement>('input[type="checkbox"]')!);
    });
    await act(async () => {
      fireEvent.submit(container.querySelector('form')!);
    });
    expect(createCheckoutMock).toHaveBeenCalledWith(41,
      expect.objectContaining({ quantity: 4, buyerEmail: 'buyer@example.invalid' }), expect.any(String));
  });

  it('keeps the legacy policy fallback bounded by remaining inventory', async () => {
    await renderTracking('/eventos/41/entradas?tierId=8');
    await waitForExpectation(() => expect(container.textContent).toContain('Hasta 100 entradas por orden.'));
    expect(container.querySelector<HTMLInputElement>('input[type="number"]')?.max).toBe('25');
  });

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

  it('renders only the exact hosted method labels supplied by the server', async () => {
    getCheckoutMock.mockResolvedValue(checkoutFixture({
      paymentStatus: 'awaiting_payment',
      paymentMethods: ['placetopay_deuna_qr'],
    }));
    await renderTracking('/eventos/41/orden/92');

    await waitForExpectation(() => expect(container.textContent).toContain('QR DeUna! · PlaceToPay'));
    expect(container.textContent).not.toContain('Datafast');
    expect(container.textContent).not.toContain('PayPhone');
  });

  it('shows ticket codes only after the server returns paid and issued states', async () => {
    getCheckoutMock.mockResolvedValue(checkoutFixture({
      paymentStatus: 'paid',
      fulfillmentStatus: 'issued',
      paymentMethods: [],
      tickets: [{
        ticketId: 501,
        ticketCode: 'TICKET-VERIFIED-501',
        status: 'issued',
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
  it.each(['checked_in', 'refunded', 'cancelled'])('never displays a QR for a %s ticket', async (status) => {
    qrCanvasMock.mockClear();
    getCheckoutMock.mockResolvedValue(checkoutFixture({
      paymentStatus: 'paid', fulfillmentStatus: 'issued', paymentMethods: [],
      tickets: [{ ticketId: 502, ticketCode: 'PRIVATE-REVOKED-CODE', status, holderName: 'Titular' }],
    }));
    await renderTracking('/eventos/41/orden/92');
    await waitForExpectation(() => expect(container.textContent).toContain('Titular'));
    expect(container.textContent).not.toContain('PRIVATE-REVOKED-CODE');
    expect(qrCanvasMock).not.toHaveBeenCalled();
  });

  it('does not present a ticket before fulfillment even when payment is paid', async () => {
    getCheckoutMock.mockResolvedValue(checkoutFixture({
      paymentStatus: 'paid', fulfillmentStatus: 'seat_held', paymentMethods: [],
      tickets: [{ ticketId: 503, ticketCode: 'NOT-YET-ISSUED', status: 'issued', holderName: 'Titular' }],
    }));
    await renderTracking('/eventos/41/orden/92');
    await waitForExpectation(() => expect(container.textContent).toContain('Pago: paid'));
    expect(container.textContent).not.toContain('NOT-YET-ISSUED');
    expect(container.querySelector('canvas')).toBeNull();
  });

});

/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { Link, MemoryRouter, Route, Routes } from 'react-router-dom';
import { fireEvent } from '@testing-library/react';

const getStorefrontMock = jest.fn<(eventId: number) => Promise<unknown>>();
const createCheckoutMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const createPaypalOrderMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const capturePaypalOrderMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const selectBankTransferMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const submitBankTransferEvidenceMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const readEnvMock = jest.fn<(key: string) => string | undefined>();
const funnelCaptureMock = jest.fn();
const analytics = { ready: true, capture: funnelCaptureMock };
jest.unstable_mockModule('../analytics/useAnalytics', () => ({ useAnalytics: () => analytics }));
jest.unstable_mockModule('../utils/env', () => ({ env: { read: readEnvMock } }));
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
    createPaypalOrder: createPaypalOrderMock,
    capturePaypalOrder: capturePaypalOrderMock,
    selectBankTransfer: selectBankTransferMock,
    submitBankTransferEvidence: submitBankTransferEvidenceMock,
  },
}));

jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({
  useLocalePreferences: () => ({ locale: 'es-EC', timezone: 'America/Guayaquil' }),
}));

const metaTagsMock = jest.fn<(metadata: Record<string, unknown>) => void>();
jest.unstable_mockModule('../hooks/useMetaTags', () => ({ useMetaTags: metaTagsMock }));

const qrCanvasMock = jest.fn<() => Promise<void>>().mockResolvedValue(undefined);
jest.unstable_mockModule('qrcode', () => ({ default: { toCanvas: qrCanvasMock } }));
jest.unstable_mockModule('../mobile/MobilePromo', () => ({ default: () => null }));

const hostedCreateMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const hostedGetMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const hostedApi = await import('../api/providerPaymentSessions');
jest.unstable_mockModule('../api/providerPaymentSessions', () => ({
  ...hostedApi,
  createProviderPaymentSession: hostedCreateMock,
  getProviderPaymentSession: hostedGetMock,
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

// Compare primitive identities, not digit substrings in timestamps. Traverse
// arrays and objects so an identifier cannot hide in a nested analytics field.
const expectNoPrivateTicketData = (value: unknown): void => {
  expect(value).not.toBe(92);
  expect(value).not.toBe('92');
  expect(value).not.toBe(501);
  expect(value).not.toBe('501');
  if (typeof value === 'string') {
    expect(value).not.toMatch(/private-capability|orden|stale|TICKET-VERIFIED|secure-lookup|Comprador/);
  } else if (Array.isArray(value)) {
    value.forEach(expectNoPrivateTicketData);
  } else if (value && typeof value === 'object') {
    Object.entries(value).forEach(expectNoPrivateTicketData);
  }
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
    createPaypalOrderMock.mockReset();
    capturePaypalOrderMock.mockReset();
    selectBankTransferMock.mockReset();
    submitBankTransferEvidenceMock.mockReset();
    readEnvMock.mockReset();
    funnelCaptureMock.mockClear();
    analytics.ready = true;
    hostedCreateMock.mockReset();
    hostedGetMock.mockReset();
    delete window.paypal;
    getCheckoutMock.mockReset();
    confirmDatafastStatusMock.mockReset();
    metaTagsMock.mockReset();
    queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    container = document.createElement('div');
    document.body.appendChild(container);
    root = createRoot(container);
  });

  afterEach(async () => {
    await act(async () => root.unmount());
    queryClient.clear();
    container.remove();
    delete window.paypal;
    jest.restoreAllMocks();
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

  it('retrieves issued tickets when public ticket sales are closed', async () => {
    getStorefrontMock.mockRejectedValue(new Error('404: Tickets are not on sale'));
    getCheckoutMock.mockResolvedValue(checkoutFixture({
      paymentStatus: 'paid', fulfillmentStatus: 'issued', paymentMethods: [],
      tickets: [{ ticketId: 501, ticketCode: 'TICKET-VERIFIED', status: 'issued', holderName: 'Comprador' }],
    }));
    await renderTracking('/eventos/41/orden/92');
    await waitForExpectation(() => expect(container.textContent).toContain('Entradas emitidas'));
    expect(getCheckoutMock).toHaveBeenCalledWith(41, 92, 'secure-lookup-token');
    expect(container.textContent).not.toContain('Elige tus entradas');
  });

  it('does not wait for a slow storefront before displaying an authorized order', async () => {
    getStorefrontMock.mockImplementation(() => new Promise(() => {}));
    getCheckoutMock.mockResolvedValue(checkoutFixture({ paymentStatus: 'paid', fulfillmentStatus: 'issued' }));
    await renderTracking('/eventos/41/orden/92');
    await waitForExpectation(() => expect(container.textContent).toContain('El servidor verificó el pago'));
  });

  it('retries a failed order lookup without creating a second purchase', async () => {
    getCheckoutMock.mockRejectedValueOnce(new Error('Temporary outage'))
      .mockResolvedValueOnce(checkoutFixture({ paymentStatus: 'paid', fulfillmentStatus: 'issued' }));
    await renderTracking('/eventos/41/orden/92');
    await waitForExpectation(() => expect(container.textContent).toContain('No vuelvas a comprar ni a pagar'));
    expect(container.textContent).not.toContain('Elige tus entradas');
    const retry = Array.from(container.querySelectorAll('button')).find((button) => button.textContent === 'Intentar de nuevo')!;
    await act(async () => { fireEvent.click(retry); });
    await waitForExpectation(() => expect(container.textContent).toContain('El servidor verificó el pago'));
    expect(getCheckoutMock).toHaveBeenCalledTimes(2);
    expect(createCheckoutMock).not.toHaveBeenCalled();
  });

  it('explains missing order access without offering another purchase', async () => {
    window.localStorage.clear();
    await renderTracking('/eventos/41/orden/92');
    await waitForExpectation(() => expect(container.textContent).toContain('navegador donde compraste'));
    expect(container.textContent).not.toContain('Elige tus entradas');
    expect(getCheckoutMock).not.toHaveBeenCalled();
  });

  it('ignores a late receipt after navigating to an order without its capability', async () => {
    let resolveFirst!: (value: unknown) => void;
    getCheckoutMock.mockImplementation(() => new Promise((resolve) => { resolveFirst = resolve; }));
    await act(async () => {
      root.render(<MemoryRouter initialEntries={['/eventos/41/orden/92']}>
        <QueryClientProvider client={queryClient}>
          <Link to="/eventos/41/orden/93">Otra orden</Link>
          <Routes><Route path="/eventos/:eventId/orden/:orderId" element={<PublicEventTicketsPage />} /></Routes>
        </QueryClientProvider>
      </MemoryRouter>);
    });
    await waitForExpectation(() => expect(getCheckoutMock).toHaveBeenCalledTimes(1));
    expect(container.textContent).not.toContain('Elige tus entradas');
    await act(async () => { fireEvent.click(container.querySelector('a')!); });
    await waitForExpectation(() => expect(container.textContent).toContain('navegador donde compraste'));
    await act(async () => {
      resolveFirst(checkoutFixture({ paymentStatus: 'paid', fulfillmentStatus: 'issued',
        tickets: [{ ticketId: 501, ticketCode: 'TICKET-VERIFIED', status: 'issued' }] }));
    });
    expect(container.textContent).not.toContain('TICKET-VERIFIED');
    expect(container.textContent).not.toContain('Entradas emitidas');
  });

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

  it('shows the event artwork above the checkout', async () => {
    getStorefrontMock.mockResolvedValue({ ...storefrontFixture, imageUrl: 'https://api.example.invalid/flyer.png' });
    await renderTracking('/eventos/41/entradas');
    await waitForExpectation(() => expect(container.querySelector('img[alt="Arte de Festival TDF"]')).toBeTruthy());
    expect(container.querySelector('img')?.getAttribute('src')).toBe('https://api.example.invalid/flyer.png');
  });

  it('uses the event canonical and does not advertise a disabled checkout as a priced offer', async () => {
    getStorefrontMock.mockResolvedValue({ ...storefrontFixture, checkoutAvailable: false });
    await renderTracking('/eventos/41/entradas?utm_source=artist');
    await waitForExpectation(() => expect(container.textContent).toContain('Festival TDF'));
    const metadata = metaTagsMock.mock.calls.at(-1)?.[0];
    expect(metadata).toMatchObject({
      title: 'Festival TDF',
      canonical: `${window.location.origin}/eventos/41`,
      robots: 'noindex,follow',
    });
    expect(metadata).not.toHaveProperty('structuredData');
  });

  it('reports a bank transfer as pending staff verification, never as paid', async () => {
    const bankTransfer = {
      instructions: 'Banco Internacional ahorros 440781141',
      paymentReference: 'TDF-92',
      amountMinor: 2040,
      currency: 'USD',
      evidenceStatus: 'awaiting_evidence',
      customerReference: null,
      reviewNotes: null,
    };
    getCheckoutMock.mockResolvedValue(checkoutFixture({
      paymentStatus: 'awaiting_payment', paymentMethods: ['paypal', 'bank_transfer'],
    }));
    selectBankTransferMock.mockResolvedValue(checkoutFixture({
      paymentStatus: 'awaiting_payment', paymentMethods: ['paypal', 'bank_transfer'], bankTransfer,
    }));
    submitBankTransferEvidenceMock.mockResolvedValue(checkoutFixture({
      paymentStatus: 'awaiting_payment', paymentMethods: ['paypal', 'bank_transfer'],
      bankTransfer: { ...bankTransfer, evidenceStatus: 'submitted', customerReference: 'COMP-55' },
    }));
    await renderTracking('/eventos/41/orden/92');
    const button = await (async () => {
      let found: HTMLButtonElement | undefined;
      await waitForExpectation(() => {
        found = Array.from(container.querySelectorAll('button'))
          .find((candidate) => candidate.textContent === 'Transferencia bancaria');
        expect(found).toBeTruthy();
      });
      return found!;
    })();
    await act(async () => { fireEvent.click(button); });
    await waitForExpectation(() => expect(container.textContent).toContain('TDF-92'));
    expect(selectBankTransferMock).toHaveBeenCalledWith(41, 92, 'secure-lookup-token');
    expect(container.textContent).toContain('Banco Internacional ahorros 440781141');

    const input = container.querySelector<HTMLInputElement>('input[maxlength="120"]')!;
    await act(async () => { fireEvent.change(input, { target: { value: 'COMP-55' } }); });
    const submit = Array.from(container.querySelectorAll('button'))
      .find((candidate) => candidate.textContent === 'Ya transferí')!;
    await act(async () => { fireEvent.click(submit); });
    await waitForExpectation(() => expect(container.textContent).toContain('Estamos verificando el depósito'));
    expect(submitBankTransferEvidenceMock).toHaveBeenCalledWith(41, 92, 'COMP-55', 'secure-lookup-token');
    expect(container.textContent).not.toContain('El servidor verificó el pago');
    const providers = funnelCaptureMock.mock.calls.map((call) => call[1]);
    expect(providers).toEqual(expect.arrayContaining([expect.objectContaining({ provider: 'bank_transfer' })]));
    providers.forEach(expectNoPrivateTicketData);
  });

  it('keeps a receipt out of search and its order reference out of canonical metadata', async () => {
    getCheckoutMock.mockResolvedValue(checkoutFixture());
    await renderTracking('/eventos/41/orden/92');
    await waitForExpectation(() => expect(container.textContent).toContain('Estado de la orden'));
    const metadata = metaTagsMock.mock.calls.at(-1)?.[0];
    expect(metadata).toMatchObject({ canonical: `${window.location.origin}/eventos/41`, robots: 'noindex,follow' });
    expect(metadata).not.toHaveProperty('structuredData');
    expect(JSON.stringify(metadata)).not.toContain('secure-lookup-token');
  });

  it('mounts already-loaded PayPal buttons on the first portal opening and captures only the bound order', async () => {
    readEnvMock.mockReturnValue('synthetic-public-client');
    getCheckoutMock.mockResolvedValue(checkoutFixture({ paymentStatus: 'awaiting_payment' }));
    createPaypalOrderMock.mockResolvedValue({ pcPaypalOrderId: 'BOUND-PAYPAL-ORDER' });
    capturePaypalOrderMock.mockResolvedValue(checkoutFixture());
    const render = jest.fn<(target: string | HTMLElement) => Promise<void>>().mockResolvedValue(undefined);
    const close = jest.fn<() => void>();
    const buttons = jest.fn<NonNullable<typeof window.paypal>['Buttons']>(() => ({ render, close }));
    window.paypal = { Buttons: buttons };
    await renderTracking('/eventos/41/orden/92');
    const paypalButton = () => Array.from(container.querySelectorAll('button'))
      .find((button) => button.textContent === 'PayPal')!;
    await waitForExpectation(() => expect(paypalButton()?.disabled).toBe(false));
    await act(async () => fireEvent.click(paypalButton()));
    await waitForExpectation(() => expect(render).toHaveBeenCalledTimes(1));
    expect(render.mock.calls[0]?.[0]).toBeInstanceOf(HTMLElement);
    expect((render.mock.calls[0]?.[0] as HTMLElement).isConnected).toBe(true);
    const options = buttons.mock.calls[0]![0];
    expect(options.createOrder?.()).toBe('BOUND-PAYPAL-ORDER');
    await act(async () => options.onApprove?.({ orderID: 'OTHER-ORDER' }));
    expect(capturePaypalOrderMock).not.toHaveBeenCalled();
    await act(async () => options.onApprove?.({ orderID: 'BOUND-PAYPAL-ORDER' }));
    expect(capturePaypalOrderMock).toHaveBeenCalledWith(41, 92, 'BOUND-PAYPAL-ORDER', 'secure-lookup-token');
    expect(container.textContent).toContain('La orden no está pagada');
    expect(container.querySelector('canvas')).toBeNull();
    expect(close).toHaveBeenCalled();
    expect(funnelCaptureMock).toHaveBeenCalledWith('ticketing_payment_initiated', expect.objectContaining({ provider: 'paypal' }));
    expect(funnelCaptureMock.mock.calls.map(([phase]) => phase)).not.toContain('ticketing_payment_completed');
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
    expect(funnelCaptureMock).not.toHaveBeenCalled();
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
    expect(funnelCaptureMock).toHaveBeenCalledWith('ticketing_checkout_started', expect.objectContaining({ event_id: 41, quantity: 4 }));
    expect(JSON.stringify(funnelCaptureMock.mock.calls)).not.toMatch(/buyer@example|Comprador|secure-lookup/);
  });

  it.each([[true], [false]])('sends invoice identification only when the policy issues invoices (%s)', async (invoiced) => {
    getStorefrontMock.mockResolvedValue({
      ...storefrontFixture,
      policy: { ...storefrontFixture.policy, maxTicketsPerOrder: 4, taxInvoiceIssued: invoiced },
    });
    await renderTracking('/eventos/41/entradas?tierId=8&quantity=1');
    await waitForExpectation(() => expect(container.textContent).toContain('Hasta 4 entradas por orden.'));
    expect(container.textContent?.includes('Datos para tu factura electrónica')).toBe(invoiced);
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
    const payload = createCheckoutMock.mock.calls.at(-1)?.[1] as Record<string, unknown>;
    if (invoiced) {
      expect(payload).toMatchObject({ billingIdType: 'consumidor_final' });
    } else {
      expect(payload).not.toHaveProperty('billingIdType');
    }
    expect(payload).not.toHaveProperty('billingIdNumber');
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
    expect(container.textContent).not.toContain('processing');
    expect(container.textContent).not.toContain('Pago verificado por el servidor');
    expect(container.textContent).not.toContain('TICKET-');
    expect(funnelCaptureMock.mock.calls.map(([phase]) => phase)).not.toContain('ticketing_payment_completed');
  });

  it('reports a failed provider verification without fabricating payment or tickets', async () => {
    confirmDatafastStatusMock.mockRejectedValue(new Error('provider unavailable'));
    await renderTracking('/eventos/41/orden/92?resourcePath=%2Fv1%2Fcheckouts%2Fprovider-1%2Fpayment');

    await waitForExpectation(() => expect(container.textContent).toContain(
      'No pudimos cargar tu orden. Revisa tu conexión e inténtalo de nuevo. No vuelvas a comprar ni a pagar.',
    ));
    expect(container.textContent).not.toContain('El servidor verificó el pago');
    expect(container.textContent).not.toContain('TICKET-');
    expect(funnelCaptureMock).not.toHaveBeenCalled();
  });

  it.each(['prepared', 'failed', 'confirmed_no_charge'])('records a %s hosted initiation once without claiming payment', async (state) => {
    const session = {
      checkoutId: checkoutFixture().checkoutId,
      attemptId: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb',
      operationId: 'cccccccc-cccc-4ccc-8ccc-cccccccccccc',
      provider: 'placetopay', state, externalId: 'request-7', redirectUrl: null,
      outcomeCertainty: state === 'prepared' ? 'ambiguous' : 'confirmed_no_charge',
      canRetryOrFallback: state !== 'prepared',
    };
    hostedCreateMock.mockResolvedValue(session);
    hostedGetMock.mockResolvedValue(session);
    getCheckoutMock.mockResolvedValue(checkoutFixture({
      paymentStatus: 'awaiting_payment', paymentMethods: ['placetopay_card'],
    }));
    await renderTracking('/eventos/41/orden/92');
    await waitForExpectation(() => expect(container.textContent).toContain('Tarjeta · PlaceToPay'));
    const button = [...container.querySelectorAll('button')].find((item) => item.textContent?.includes('Tarjeta · PlaceToPay'));
    if (!button) throw new Error('Hosted payment button missing');
    await act(async () => fireEvent.click(button));
    await waitForExpectation(() => expect(hostedCreateMock).toHaveBeenCalledTimes(1));
    await waitForExpectation(() => expect(funnelCaptureMock).toHaveBeenCalledWith(
      'ticketing_payment_initiated', expect.objectContaining({ event_id: 41, provider: 'placetopay' }),
    ));
    await renderTracking('/eventos/41/orden/92');
    expect(funnelCaptureMock.mock.calls.filter(([phase]) => phase === 'ticketing_payment_initiated')).toHaveLength(1);
    expect(funnelCaptureMock.mock.calls.map(([phase]) => phase)).not.toContain('ticketing_payment_completed');
  });

  it.each([false, true])('captures current landing attribution and replaces stale campaign (previous=%s)', async (previous) => {
    jest.spyOn(Date.prototype, 'toISOString').mockReturnValue('2026-10-06T05:36:12.592Z');
    if (previous) window.localStorage.setItem('tdf:growth-attribution:v1', JSON.stringify({
      source: 'stale', campaign: 'old', landingPath: '/old', capturedAt: '2026-01-01T00:00:00Z',
    }));
    getCheckoutMock.mockResolvedValue(checkoutFixture({ paymentStatus: 'paid', fulfillmentStatus: 'seat_held' }));
    await renderTracking('/eventos/41/orden/92?utm_source=instagram&utm_medium=social&utm_campaign=patch&lookup=private-capability');
    await waitForExpectation(() => expect(funnelCaptureMock).toHaveBeenCalledWith('ticketing_payment_completed', expect.objectContaining({
      attribution_source: 'instagram', attribution_medium: 'social', attribution_campaign: 'patch',
    })));
    const persisted = window.localStorage.getItem('tdf:growth-attribution:v1');
    expect(JSON.parse(persisted ?? '{}')).toEqual(expect.objectContaining({ landingPath: '/eventos/41', campaign: 'patch' }));
    expectNoPrivateTicketData(JSON.parse(persisted ?? '{}'));
    expectNoPrivateTicketData(funnelCaptureMock.mock.calls);
  });

  it('updates a revisited landing campaign even when its payment observation is deduplicated', async () => {
    getCheckoutMock.mockResolvedValue(checkoutFixture({ paymentStatus: 'paid', fulfillmentStatus: 'seat_held' }));
    await renderTracking('/eventos/41/orden/92?utm_source=old&utm_campaign=previous');
    await waitForExpectation(() => expect(funnelCaptureMock).toHaveBeenCalledTimes(1));
    await act(async () => root.unmount());
    root = createRoot(container);
    funnelCaptureMock.mockClear();
    await renderTracking('/eventos/41/orden/92?utm_source=instagram&utm_campaign=patch');
    await waitForExpectation(() => expect(JSON.parse(window.localStorage.getItem('tdf:growth-attribution:v1') ?? '{}'))
      .toEqual(expect.objectContaining({ source: 'instagram', campaign: 'patch', landingPath: '/eventos/41' })));
    expect(funnelCaptureMock).not.toHaveBeenCalled();
  });

  it('does not persist campaign attribution when analytics is disabled', async () => {
    analytics.ready = false;
    getCheckoutMock.mockResolvedValue(checkoutFixture({ paymentStatus: 'paid', fulfillmentStatus: 'seat_held' }));
    await renderTracking('/eventos/41/orden/92?utm_source=instagram&utm_campaign=patch');
    await waitForExpectation(() => expect(container.textContent).toContain('La emisión de entradas todavía está pendiente.'));
    expect(funnelCaptureMock).not.toHaveBeenCalled();
    expect(window.localStorage.getItem('tdf:growth-attribution:v1')).toBeNull();
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
    expect(container.textContent).toContain('Entradas emitidas');
    expect(container.querySelector('canvas')).not.toBeNull();
    expect(funnelCaptureMock.mock.calls.map(([phase]) => phase)).toEqual([
      'ticketing_payment_completed', 'ticketing_ticket_issued', 'ticketing_ticket_opened',
    ]);
    expectNoPrivateTicketData(funnelCaptureMock.mock.calls);
    await renderTracking('/eventos/41/orden/92');
    expect(funnelCaptureMock).toHaveBeenCalledTimes(3);
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
    expect(funnelCaptureMock.mock.calls.map(([phase]) => phase)).not.toContain('ticketing_ticket_opened');
  });

  it('does not present a ticket before fulfillment even when payment is paid', async () => {
    getCheckoutMock.mockResolvedValue(checkoutFixture({
      paymentStatus: 'paid', fulfillmentStatus: 'seat_held', paymentMethods: [],
      tickets: [{ ticketId: 503, ticketCode: 'NOT-YET-ISSUED', status: 'issued', holderName: 'Titular' }],
    }));
    await renderTracking('/eventos/41/orden/92');
    await waitForExpectation(() => expect(container.textContent).toContain('La emisión de entradas todavía está pendiente.'));
    expect(container.textContent).not.toContain('NOT-YET-ISSUED');
    expect(container.querySelector('canvas')).toBeNull();
    expect(funnelCaptureMock.mock.calls.map(([phase]) => phase)).toEqual(['ticketing_payment_completed']);
  });

  it('labels included tax without adding it again to the server total', async () => {
    getCheckoutMock.mockResolvedValue(checkoutFixture({
      quote: { ...checkoutFixture().quote, quantity: 4, grossFaceValueMinor: 8000,
        netFaceValueMinor: 8000, buyerPlatformFeeMinor: 0, organizerPlatformFeeMinor: 0,
        taxIncluded: true, taxMinor: 1043, checkoutTotalMinor: 8000,
        organizerPayableMinor: 6957, platformFeeMinor: 0 },
    }));
    await renderTracking('/eventos/41/orden/92');
    await waitForExpectation(() => expect(container.textContent).toContain('Impuesto incluido'));
    expect(container.textContent).toMatch(/80[,.]00/);
    expect(container.textContent).not.toMatch(/90[,.]43/);
  });

});

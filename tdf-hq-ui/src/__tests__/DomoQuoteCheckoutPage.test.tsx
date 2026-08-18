/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter, Route, Routes } from 'react-router-dom';

const getQuoteMock = jest.fn<(quoteId: string, token: string) => Promise<unknown>>();
const confirmDatafastStatusMock = jest.fn<(
  quoteId: string,
  resourcePath: string,
  token: string,
) => Promise<unknown>>();

jest.unstable_mockModule('../api/domoQuotes', () => ({
  DomoQuotes: {
    getQuote: (quoteId: string, token: string) => getQuoteMock(quoteId, token),
    confirmDatafastStatus: (quoteId: string, resourcePath: string, token: string) =>
      confirmDatafastStatusMock(quoteId, resourcePath, token),
    acceptQuote: jest.fn(),
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

const { default: DomoQuoteCheckoutPage } = await import('../pages/DomoQuoteCheckoutPage');

const quoteId = 'd1000000-0000-4000-8000-000000000001';
const quoteFixture = (overrides: Record<string, unknown> = {}) => ({
  quoteId,
  checkoutId: 'd3000000-0000-4000-8000-000000000001',
  lookupToken: null,
  quoteStatus: 'deposit_due',
  paymentStatus: 'processing',
  fulfillmentStatus: 'date_held',
  rateCardVersion: 'approved-v1',
  currency: 'USD',
  eventType: 'wedding',
  guests: 80,
  startsAt: '2030-01-10T15:00:00Z',
  endsAt: '2030-01-10T23:00:00Z',
  setupStartsAt: '2030-01-10T13:00:00Z',
  lines: [{
    code: 'venue_base',
    description: 'Domo event base',
    quantity: 1,
    unitAmountMinor: 180000,
    subtotalMinor: 180000,
  }],
  subtotalMinor: 448000,
  taxMinor: 53760,
  totalMinor: 501760,
  depositMinor: 200704,
  balanceMinor: 301056,
  timezone: 'America/Guayaquil',
  termsVersion: 'domo-terms-v1',
  holdExpiresAt: '2030-01-10T14:15:00Z',
  termsAcceptedAt: '2030-01-01T10:00:00Z',
  depositPaidAt: null,
  paymentMethods: ['datafast', 'paypal'],
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

describe('DomoQuoteCheckoutPage verified deposit boundary', () => {
  let container: HTMLDivElement;
  let root: Root;

  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    window.localStorage.clear();
    window.localStorage.setItem(`tdf:domo-quote:${quoteId}`, 'secure-lookup-token');
    getQuoteMock.mockReset();
    confirmDatafastStatusMock.mockReset();
    container = document.createElement('div');
    document.body.appendChild(container);
    root = createRoot(container);
  });

  afterEach(async () => {
    await act(async () => root.unmount());
    container.remove();
  });

  const renderTracking = async (route: string) => {
    await act(async () => {
      root.render(
        <MemoryRouter initialEntries={[route]}>
          <Routes>
            <Route path="/domo-del-pululahua/cotizaciones/:quoteId" element={<DomoQuoteCheckoutPage />} />
          </Routes>
        </MemoryRouter>,
      );
    });
  };

  it('keeps a provider-returned quote unpaid and the date only held', async () => {
    confirmDatafastStatusMock.mockResolvedValue(quoteFixture());
    getQuoteMock.mockResolvedValue(quoteFixture());
    await renderTracking(`/domo-del-pululahua/cotizaciones/${quoteId}?resourcePath=%2Fv1%2Fcheckouts%2Fdomo%2Fpayment`);

    await waitForExpectation(() => expect(confirmDatafastStatusMock).toHaveBeenCalledWith(
      quoteId,
      '/v1/checkouts/domo/payment',
      'secure-lookup-token',
    ));
    expect(container.textContent).toContain('No está reservada y el depósito no está pagado');
    expect(container.textContent).toContain('Pago: processing');
    expect(container.textContent).toContain('Cumplimiento del espacio: date_held');
    expect(container.textContent).not.toContain('reservó la fecha');
  });

  it('does not fabricate a deposit or reservation when server verification fails', async () => {
    confirmDatafastStatusMock.mockRejectedValue(new Error('provider unavailable'));
    await renderTracking(`/domo-del-pululahua/cotizaciones/${quoteId}?resourcePath=%2Fv1%2Fcheckouts%2Fdomo%2Fpayment`);

    await waitForExpectation(() => expect(container.textContent).toContain(
      'El servidor no pudo verificar esta cotización. No mostramos fecha ni pago como confirmados.',
    ));
    expect(container.textContent).not.toContain('El servidor verificó el depósito');
    expect(container.textContent).not.toContain('date_reserved');
  });

  it('shows a verified deposit while keeping event completion separate', async () => {
    getQuoteMock.mockResolvedValue(quoteFixture({
      quoteStatus: 'deposit_paid',
      paymentStatus: 'paid',
      fulfillmentStatus: 'date_reserved',
      depositPaidAt: '2030-01-01T10:05:00Z',
      paymentMethods: [],
    }));
    await renderTracking(`/domo-del-pululahua/cotizaciones/${quoteId}`);

    await waitForExpectation(() => expect(getQuoteMock).toHaveBeenCalledWith(
      quoteId,
      'secure-lookup-token',
    ));
    expect(container.textContent).toContain('El servidor verificó el depósito y reservó la fecha');
    expect(container.textContent).toContain('Esto no significa que el evento esté completado');
    expect(container.textContent).toContain('Saldo restante');
    expect(container.textContent).toContain('Cumplimiento del espacio: date_reserved');
  });
});

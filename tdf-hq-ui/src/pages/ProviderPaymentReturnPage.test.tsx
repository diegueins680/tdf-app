/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { act, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { onlineManager, QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, Route, Routes } from 'react-router-dom';

import {
  loadOrCreatePaymentIdempotencyKey,
  loadProviderPaymentResume,
  paymentIdempotencyStorageKey,
  saveProviderPaymentResume,
} from '../utils/providerPaymentResume';

const checkoutId = 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa';
const attemptId = 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb';
const getMock = jest.fn();
const returnMock = jest.fn(async () => ({ Response: true, ErrorCode: '000' }));
const alternativesMock = jest.fn<() => Promise<string[]>>();
let paypalClientId = '';

jest.unstable_mockModule('../api/checkoutPaymentAlternatives', () => ({
  loadCheckoutPaymentAlternatives: alternativesMock,
}));
jest.unstable_mockModule('../utils/env', () => ({
  env: { read: (key: string) => key === 'VITE_PAYPAL_CLIENT_ID' ? paypalClientId : undefined },
}));

jest.unstable_mockModule('../api/providerPaymentSessions', () => ({
  getProviderPaymentSession: getMock,
  submitPayPhoneButtonReturn: returnMock,
  safeProviderRedirect: (_provider: string, redirect: string | null) => redirect,
}));

jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({
  useLocalePreferences: () => ({ locale: 'es-EC' }),
}));

const { default: ProviderPaymentReturnPage } = await import('./ProviderPaymentReturnPage');

const resume = () => saveProviderPaymentResume({
  version: 1,
  checkoutId,
  attemptId,
  provider: 'placetopay',
  paymentMethod: 'card',
  lookupToken: 'secure-checkout-lookup-token',
  returnPath: '/orden',
  createdAt: Date.now(),
});

const session = (state: string, canRetryOrFallback: boolean) => ({
  checkoutId,
  attemptId,
  operationId: 'cccccccc-cccc-4ccc-8ccc-cccccccccccc',
  provider: 'placetopay',
  state,
  externalId: 'request-7',
  redirectUrl: null,
  outcomeCertainty: canRetryOrFallback ? 'confirmed_no_charge' : 'ambiguous',
  canRetryOrFallback,
});

const renderPage = (
  entry = '/pagos/retorno',
  client = new QueryClient({ defaultOptions: { queries: { retry: false } } }),
) => {
  return render(
    <QueryClientProvider client={client}>
      <MemoryRouter initialEntries={[entry]}>
        <Routes>
          <Route path="/pagos/retorno" element={<ProviderPaymentReturnPage />} />
          <Route path="/orden" element={<div>Orden</div>} />
        </Routes>
      </MemoryRouter>
    </QueryClientProvider>,
  );
};

describe('ProviderPaymentReturnPage', () => {
  beforeEach(() => {
    returnMock.mockClear();
    window.sessionStorage.clear();
    getMock.mockReset();
    alternativesMock.mockReset().mockResolvedValue([]);
    paypalClientId = '';
    Object.defineProperty(window, 'paypal', { configurable: true, writable: true, value: undefined });
    resume();
  });

  it('retries delivery of the same return after a network error without starting a new checkout', async () => {
    window.sessionStorage.clear();
    returnMock.mockRejectedValueOnce(new Error('offline'));
    renderPage('/pagos/retorno?id=123&clientTransactionId=TDFC-order');
    fireEvent.click(await screen.findByRole('button', { name: 'Reintentar' }));
    await waitFor(() => expect(returnMock).toHaveBeenCalledTimes(2));
    expect(returnMock).toHaveBeenNthCalledWith(2, 123, 'TDFC-order');
    await waitFor(() => expect(screen.queryByRole('button', { name: 'Reintentar' })).toBeNull());
    expect(getMock).not.toHaveBeenCalled();
  });

  it.each(['&', '?'])('forwards a Button return with separator %s even without local recovery', async (separator) => {
    window.sessionStorage.clear();
    renderPage('/pagos/retorno?tdf_checkout=' + checkoutId + separator + 'id=12345&clientTransactionId=TDFC-fixture');
    await waitFor(() => expect(returnMock).toHaveBeenCalledWith(12345, 'TDFC-fixture'));
    expect(getMock).not.toHaveBeenCalled();
  });

  it('ignores ambiguous provider return parameters', () => {
    renderPage('/pagos/retorno?id=12345&id=12346&clientTransactionId=TDFC-fixture');
    expect(returnMock).not.toHaveBeenCalled();
  });

  it('uses the server-bound checkout selector even after another checkout starts', async () => {
    saveProviderPaymentResume({ version: 1,
      checkoutId: 'dddddddd-dddd-4ddd-8ddd-dddddddddddd', attemptId,
      provider: 'placetopay', paymentMethod: 'card', lookupToken: 'other-private-lookup',
      returnPath: '/other-order', createdAt: Date.now() });
    getMock.mockResolvedValue(session('succeeded', false));
    renderPage(`/pagos/retorno?tdf_checkout=${checkoutId}`);
    await screen.findByText(/Pago confirmado/);
    expect(getMock).toHaveBeenCalledWith(checkoutId, attemptId, 'secure-checkout-lookup-token');
    expect(screen.getByRole('link', { name: 'Volver a la orden' }).getAttribute('href')).toBe('/orden');
  });

  it.each([
    { clientId: '', sdk: true, visible: false, unchecked: false },
    { clientId: 'configured-public-client-id', sdk: false, visible: true, unchecked: true },
    { clientId: 'configured-public-client-id', sdk: true, visible: true, unchecked: false },
  ])('distinguishes unavailable, unchecked and ready PayPal: %j', async ({ clientId, sdk, visible, unchecked }) => {
    paypalClientId = clientId;
    Object.defineProperty(window, 'paypal', { configurable: true, writable: true,
      value: sdk ? { Buttons: jest.fn() } : undefined });
    getMock.mockResolvedValue(session('failed', true));
    alternativesMock.mockResolvedValue(['paypal', 'bank_transfer']);
    renderPage();
    await screen.findByText('Otros medios de pago para esta compra');
    expect(Boolean(screen.queryByText('PayPal', { exact: true }))).toBe(visible);
    const pending = screen.queryByRole('region', { name: 'Opciones por comprobar al volver a tu orden' });
    expect(Boolean(pending)).toBe(unchecked);
    if (pending) expect(within(pending).getByText('PayPal', { exact: true })).toBeTruthy();
    expect(screen.getByText('Transferencia bancaria', { exact: true })).toBeTruthy();
    expect(getMock).toHaveBeenCalledTimes(1);
  });

  it('does not claim no alternative exists when only PayPal remains after a full-page return', async () => {
    paypalClientId = 'configured-public-client-id';
    getMock.mockResolvedValue(session('failed', true));
    alternativesMock.mockResolvedValue(['paypal']);
    renderPage();
    await screen.findByRole('region', { name: 'Opciones por comprobar al volver a tu orden' });
    expect(screen.queryByText(/No hay otro medio habilitado/)).toBeNull();
    expect(screen.getByText('PayPal', { exact: true })).toBeTruthy();
    expect(screen.getByRole('link', { name: 'Volver a la orden' }).getAttribute('href')).toBe('/orden');
    expect(getMock).toHaveBeenCalledTimes(1);
  });

  it.each(['', '?tdf_checkout=', '?tdf_checkout=unknown'])('never guesses a checkout for an ambiguous or invalid return %s', (query) => {
    saveProviderPaymentResume({ version: 1,
      checkoutId: 'dddddddd-dddd-4ddd-8ddd-dddddddddddd', attemptId,
      provider: 'placetopay', paymentMethod: 'card', lookupToken: 'other-private-lookup',
      returnPath: '/other-order', createdAt: Date.now() });
    renderPage(`/pagos/retorno${query}`);
    expect(screen.getByText(/no podemos consultar el pago/)).toBeTruthy();
    expect(getMock).not.toHaveBeenCalled();
  });

  it.each([
    ['payphone', 'https://pay.payphonetodoesposible.com/Anonymous/Index', 'origin'],
    ['placetopay', 'https://checkout-test.placetopay.ec/session/test', 'no-referrer'],
  ])('uses the appropriate referrer policy when continuing a %s session', async (provider, redirectUrl, policy) => {
    getMock.mockResolvedValue({ ...session('requires_customer_action', false), provider, redirectUrl });
    renderPage();
    const link = await screen.findByRole('link', { name: 'Continuar con el pago' });
    expect(link.getAttribute('href')).toBe(redirectUrl);
    expect(link.getAttribute('referrerpolicy')).toBe(policy);
  });

  it('keeps a claimable prepared session locked when an older server permits fallback', async () => {
    getMock.mockResolvedValue(session('prepared', true));
    renderPage();
    await screen.findByText(/No pagues otra vez/);
    expect(alternativesMock).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('link', { name: 'Volver a la orden' }));
    expect(loadProviderPaymentResume(checkoutId)).not.toBeNull();
  });

  it('retains the private recovery lock when the result is ambiguous', async () => {
    getMock.mockResolvedValue(session('ambiguous', false));
    renderPage();

    fireEvent.click(await screen.findByRole('link', { name: 'Volver a la orden' }));

    expect(await screen.findByText('Orden')).toBeTruthy();
    expect(loadProviderPaymentResume(checkoutId)).not.toBeNull();
  });

  it('keeps the original attempt when the provider denies retry even for a no-charge state', async () => {
    const key = loadOrCreatePaymentIdempotencyKey(checkoutId, 'placetopay', 'card');
    getMock.mockResolvedValue(session('confirmed_no_charge', false));
    renderPage();
    await screen.findByText(/No pagues otra vez/);
    expect(screen.queryByText(/Vuelve a la orden para elegir/)).toBeNull();
    expect(alternativesMock).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('link', { name: 'Volver a la orden' }));
    await screen.findByText('Orden');
    expect(loadProviderPaymentResume(checkoutId)).not.toBeNull();
    expect(window.sessionStorage.getItem(paymentIdempotencyStorageKey(checkoutId, 'placetopay', 'card'))).toBe(key);
  });

  it('checks a cached no-charge result again when the buyer returns to the page', async () => {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false, staleTime: 30_000 } } });
    client.setQueryData(['provider-payment-session-return', checkoutId, attemptId], session('failed', true));
    let complete!: (result: ReturnType<typeof session>) => void;
    getMock.mockImplementation(() => new Promise(resolve => { complete = resolve; }));
    renderPage('/pagos/retorno', client);
    expect(getMock).toHaveBeenCalledWith(checkoutId, attemptId, 'secure-checkout-lookup-token');
    expect(screen.queryByText(/No se realizó ningún cobro/)).toBeNull();
    expect(alternativesMock).not.toHaveBeenCalled();
    await act(async () => { complete(session('processing', false)); });
    expect(await screen.findByText(/No pagues otra vez/)).toBeTruthy();
    expect(loadProviderPaymentResume(checkoutId)).not.toBeNull();
  });

  it('keeps the private attempt locked when a return status check pauses offline', () => {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false, staleTime: 30_000 } } });
    client.setQueryData(['provider-payment-session-return', checkoutId, attemptId], session('failed', true));
    const originalKey = loadOrCreatePaymentIdempotencyKey(checkoutId, 'placetopay', 'card');
    onlineManager.setOnline(false);
    try {
      renderPage('/pagos/retorno', client);
      expect(screen.getByText(/cuando vuelva la conexión/)).toBeTruthy();
      expect(screen.queryByText(/No se realizó ningún cobro/)).toBeNull();
      expect(getMock).not.toHaveBeenCalled();
      expect(alternativesMock).not.toHaveBeenCalled();
      fireEvent.click(screen.getByRole('link', { name: 'Volver a la orden' }));
      expect(loadProviderPaymentResume(checkoutId)).not.toBeNull();
      expect(loadOrCreatePaymentIdempotencyKey(checkoutId, 'placetopay', 'card')).toBe(originalKey);
    } finally {
      client.clear();
      onlineManager.setOnline(true);
    }
  });

  it('clears recovery and the reusable key only after authoritative no-charge evidence', async () => {
    const key = loadOrCreatePaymentIdempotencyKey(checkoutId, 'placetopay', 'card');
    expect(key).toBeTruthy();
    getMock.mockResolvedValue(session('confirmed_no_charge', true));
    renderPage();

    await screen.findByText(/No se realizó ningún cobro/);
    fireEvent.click(screen.getByRole('link', { name: 'Volver a la orden' }));

    expect(await screen.findByText('Orden')).toBeTruthy();
    expect(loadProviderPaymentResume(checkoutId)).toBeNull();
    expect(window.sessionStorage.getItem(
      paymentIdempotencyStorageKey(checkoutId, 'placetopay', 'card'),
    )).toBeNull();
  });

  it('presents a verified PayPhone cancellation neutrally and releases only its recovery key', async () => {
    saveProviderPaymentResume({
      version: 1,
      checkoutId,
      attemptId,
      provider: 'payphone',
      paymentMethod: 'payphone_wallet',
      lookupToken: 'secure-checkout-lookup-token',
      returnPath: '/orden',
      createdAt: Date.now(),
    });
    loadOrCreatePaymentIdempotencyKey(checkoutId, 'payphone', 'payphone_wallet');
    const otherKey = loadOrCreatePaymentIdempotencyKey(checkoutId, 'placetopay', 'card');
    getMock.mockResolvedValue({ ...session('confirmed_no_charge', true), provider: 'payphone' });
    renderPage();

    await screen.findByText(/No se realizó ningún cobro/);
    expect(screen.queryByText(/rechazado|cancelaste|rechazo del banco/i)).toBeNull();
    fireEvent.click(screen.getByRole('link', { name: 'Volver a la orden' }));

    expect(await screen.findByText('Orden')).toBeTruthy();
    expect(loadProviderPaymentResume(checkoutId)).toBeNull();
    expect(window.sessionStorage.getItem(
      paymentIdempotencyStorageKey(checkoutId, 'payphone', 'payphone_wallet'),
    )).toBeNull();
    expect(window.sessionStorage.getItem(
      paymentIdempotencyStorageKey(checkoutId, 'placetopay', 'card'),
    )).toBe(otherKey);
  });
});

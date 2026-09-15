/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { fireEvent, render, screen } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
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

jest.unstable_mockModule('../api/providerPaymentSessions', () => ({
  getProviderPaymentSession: getMock,
  safePlaceToPayRedirect: () => null,
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

const renderPage = () => {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  return render(
    <QueryClientProvider client={client}>
      <MemoryRouter initialEntries={['/pagos/retorno']}>
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
    window.sessionStorage.clear();
    getMock.mockReset();
    resume();
  });

  it('retains the private recovery lock when the result is ambiguous', async () => {
    getMock.mockResolvedValue(session('ambiguous', false));
    renderPage();

    fireEvent.click(await screen.findByRole('link', { name: 'Volver a la orden' }));

    expect(await screen.findByText('Orden')).toBeTruthy();
    expect(loadProviderPaymentResume(checkoutId)).not.toBeNull();
  });

  it('clears recovery and the reusable key only after authoritative no-charge evidence', async () => {
    const key = loadOrCreatePaymentIdempotencyKey(checkoutId, 'placetopay', 'card');
    expect(key).toBeTruthy();
    getMock.mockResolvedValue(session('confirmed_no_charge', true));
    renderPage();

    await screen.findByText(/confirmó que no se completó un cobro/);
    fireEvent.click(screen.getByRole('link', { name: 'Volver a la orden' }));

    expect(await screen.findByText('Orden')).toBeTruthy();
    expect(loadProviderPaymentResume(checkoutId)).toBeNull();
    expect(window.sessionStorage.getItem(
      paymentIdempotencyStorageKey(checkoutId, 'placetopay', 'card'),
    )).toBeNull();
  });
});

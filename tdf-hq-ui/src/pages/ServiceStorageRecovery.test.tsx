import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot } from 'react-dom/client';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import { waitFor } from '@testing-library/react';
import type { ServiceStorefrontOrderDTO } from '../api/serviceStorefront';

const order: ServiceStorefrontOrderDTO = {
  ssoId: 'synthetic-id', ssoOrderNumber: 'synthetic-order', ssoBuyerName: 'Synthetic Buyer',
  ssoBuyerEmail: 'synthetic@example.test', ssoPackageId: 'synthetic-package', ssoServiceKind: 'mixing',
  ssoTier: 'standard', ssoPriceUsdCents: 1000, ssoCurrency: 'USD', ssoStatus: 'awaiting_payment',
  ssoSongCount: 1, ssoCreatedAt: '2026-09-18T00:00:00Z', ssoUpdatedAt: '2026-09-18T00:00:00Z',
};
const getOrder = jest.fn(async () => order);
const confirmDatafastPayment = jest.fn(async () => ({ ...order, ssoStatus: 'datafast_pending' }));
jest.unstable_mockModule('../api/serviceStorefront', () => ({ ServiceStorefront: { getOrder, confirmDatafastPayment } }));
jest.unstable_mockModule('../components/reviews/ExperienceReviews', () => ({ default: () => null }));
const { default: Tracking } = await import('./ServiceOrderTrackingPage');
const { default: PaymentReturn } = await import('./ServiceDatafastReturnPage');
(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

beforeEach(() => { jest.clearAllMocks(); window.sessionStorage.clear(); });

for (const page of ['tracking', 'return'] as const) {
  it.each(['getter', 'getItem', 'allowed-empty'] as const)(`${page} offers recovery without making unauthorized calls under %s`, async (operation) => {
    const descriptor = Object.getOwnPropertyDescriptor(window, 'sessionStorage')!;
    const deny = () => { throw new DOMException('Denied', 'SecurityError'); };
    const spy = operation === 'getItem' ? jest.spyOn(Storage.prototype, 'getItem').mockImplementation(deny) : undefined;
    if (operation === 'getter') Object.defineProperty(window, 'sessionStorage', { configurable: true, get: deny });
    const path = page === 'tracking' ? '/mezcla-mastering/pedido/synthetic-order' : '/mezcla-mastering/pago-datafast?orderId=synthetic-order&resourcePath=synthetic-provider-path';
    window.history.replaceState(null, '', path);
    const container = document.createElement('div'); document.body.appendChild(container);
    const root = createRoot(container);
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    try {
      await act(async () => { root.render(<QueryClientProvider client={client}><MemoryRouter initialEntries={[path]}><Routes>
        <Route path="/mezcla-mastering/pedido/:orderNumber" element={<Tracking />} />
        <Route path="/mezcla-mastering/pago-datafast" element={<PaymentReturn />} />
      </Routes></MemoryRouter></QueryClientProvider>); });
      await waitFor(() => expect(container.textContent).toContain(page === 'tracking' ? 'Usa el enlace original o solicita ayuda a TDF.' : 'Faltan los datos privados necesarios'));
      expect(getOrder).not.toHaveBeenCalled();
      expect(confirmDatafastPayment).not.toHaveBeenCalled();
      expect(container.textContent).not.toContain('El pago está confirmado');
    } finally {
      await act(async () => root.unmount()); container.remove(); client.clear();
      Object.defineProperty(window, 'sessionStorage', descriptor); spy?.mockRestore(); window.history.replaceState(null, '', '/');
    }
  });
}

it('retains a valid private fragment when the optional session cache is unavailable', async () => {
  const descriptor = Object.getOwnPropertyDescriptor(window, 'sessionStorage')!;
  Object.defineProperty(window, 'sessionStorage', { configurable: true, get: () => { throw new DOMException('Denied', 'SecurityError'); } });
  const path = '/mezcla-mastering/pedido/synthetic-order#access=synthetic-private-key';
  window.history.replaceState(null, '', path);
  const container = document.createElement('div'); document.body.appendChild(container);
  const root = createRoot(container);
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  try {
    await act(async () => { root.render(<QueryClientProvider client={client}><MemoryRouter initialEntries={[path]}><Routes><Route path="/mezcla-mastering/pedido/:orderNumber" element={<Tracking />} /></Routes></MemoryRouter></QueryClientProvider>); });
    await waitFor(() => expect(getOrder).toHaveBeenCalledWith('synthetic-order', 'synthetic-private-key'));
    expect(confirmDatafastPayment).not.toHaveBeenCalled();
    expect(container.textContent).not.toContain('El pago está confirmado');
  } finally {
    await act(async () => root.unmount()); container.remove(); client.clear();
    Object.defineProperty(window, 'sessionStorage', descriptor); window.history.replaceState(null, '', '/');
  }
});

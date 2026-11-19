import { jest } from '@jest/globals';
import { act, render, screen } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import type { AvailableCheckoutMethods } from '../api/paymentCapabilities';

const getOrder = jest.fn<() => Promise<Record<string, unknown>>>();
const loadMethods = jest.fn<() => Promise<AvailableCheckoutMethods>>();
jest.unstable_mockModule('../api/serviceStorefront', () => ({ ServiceStorefront: { getOrder } }));
jest.unstable_mockModule('../api/paymentCapabilities', () => ({ loadAvailableCheckoutMethods: loadMethods }));
jest.unstable_mockModule('../components/reviews/ExperienceReviews', () => ({ default: () => null }));
const { default: ServiceOrderTrackingPage } = await import('./ServiceOrderTrackingPage');

const noMethods: AvailableCheckoutMethods = {
  datafast: false, paypal: false, placeToPayCard: false, placeToPayBankRedirect: false,
  placeToPayDeunaQr: false, payPhoneWallet: false, bankTransfer: false,
};
const order = { ssoOrderNumber: 'SERVICE-1', ssoCheckoutId: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
  ssoStatus: 'payment_failed', ssoPriceUsdCents: 50000, ssoCurrency: 'USD', ssoServiceKind: 'Mezcla',
  ssoTier: 'Pro', ssoSongCount: 1, ssoPackageId: 'package-1' };

beforeEach(() => {
  window.sessionStorage.clear();
  window.sessionStorage.setItem('tdf-service-order:SERVICE-1', 'private-test-lookup-token');
  getOrder.mockReset().mockResolvedValue(order);
  loadMethods.mockReset();
});

function renderPage() {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  const view = render(<QueryClientProvider client={client}><MemoryRouter initialEntries={['/mezcla-mastering/pedido/SERVICE-1']}>
    <Routes><Route path="/mezcla-mastering/pedido/:orderNumber" element={<ServiceOrderTrackingPage />} /></Routes>
  </MemoryRouter></QueryClientProvider>);
  return { client, cleanup: () => { view.unmount(); client.clear(); } };
}

it('distinguishes loading and failed capability lookup from a confirmed empty list', async () => {
  let reject!: (error: Error) => void;
  loadMethods.mockImplementationOnce(() => new Promise((_, fail) => { reject = fail; }));
  const { client, cleanup } = renderPage();
  try {
    await screen.findByText(/No pudimos consultar otros medios de pago/);
    expect(screen.queryByText(/No hay otro medio habilitado/)).toBeNull();
    await act(async () => { reject(new Error('temporarily offline')); });
    expect(screen.getByText(/No pudimos consultar otros medios de pago/)).toBeTruthy();
    expect(screen.queryByText(/No hay otro medio habilitado/)).toBeNull();
    loadMethods.mockResolvedValue(noMethods);
    await act(async () => { await client.invalidateQueries({ queryKey: ['service-storefront-order-payment-methods'] }); });
    await screen.findByText(/No hay otro medio habilitado/);
    expect(screen.queryByText(/No pudimos consultar otros medios de pago/)).toBeNull();
  } finally { cleanup(); }
});

it('directs a failed service order to support rather than advertising its disabled executors', async () => {
  loadMethods.mockResolvedValue({ ...noMethods, payPhoneWallet: true });
  const { cleanup } = renderPage();
  try {
    await screen.findByText(/No hay otro medio habilitado/);
    expect(screen.queryByText('App PayPhone', { exact: true })).toBeNull();
    expect(screen.queryByRole('button', { name: /Pagar con.*PayPhone/ })).toBeNull();
    expect(screen.queryByText(/No pudimos consultar otros medios de pago/)).toBeNull();
    expect(loadMethods).toHaveBeenCalledTimes(1);
  } finally { cleanup(); }
});

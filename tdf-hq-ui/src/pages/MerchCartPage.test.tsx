import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import type { MerchCheckoutRequest, MerchOrder } from '../api/merch';

const checkout = jest.fn<(id: string, token: string, key: string, payload: MerchCheckoutRequest) => Promise<MerchOrder>>();
const remove = jest.fn<() => Promise<unknown>>();
const capture = jest.fn();
const remember = jest.fn();
let enabled = true, available = true, hasZone = true;
let session: { displayName: string; username: string } | null = null;
const capabilities = () => ({ features: { publicCatalog: true, checkout: enabled } });
const storefront = () => ({ id: 'store-a', displayName: 'Synthetic store', shippingZones: hasZone ? [{ id: 'zone-a', name: 'Synthetic delivery', rateMinor: 500 }] : [] });
const cart = () => ({ id: 'cart-a', storeId: 'store-a', productSubtotalMinor: 2500, currency: 'USD', items: [{ variantId: 'variant-a', productName: 'Synthetic product', variantName: 'Size M', sku: 'SYNTHETIC', quantity: 1, subtotalMinor: 2500, available }] });
jest.unstable_mockModule('../api/merch', () => ({
  Merch: { capabilities: async () => capabilities(), storefront: async () => storefront(), cart: async () => cart(), checkout, deleteCartItem: remove },
  createMerchIdempotencyKey: () => 'synthetic-checkout-key',
  readStoredMerchCart: () => ({ id: 'cart-a', token: 'synthetic-private-token' }), storeMerchOrder: remember,
}));
jest.unstable_mockModule('../api/merchReputation', () => ({ MerchReputation: { store: async () => null } }));
jest.unstable_mockModule('../components/merch/MerchReputationSummary', () => ({ MerchReputationSummary: () => null }));
jest.unstable_mockModule('../analytics/posthog', () => ({ getAnalyticsClient: () => ({ capture }) }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session }), getStoredSessionToken: () => null, getActiveSession: () => null }));
jest.unstable_mockModule('react-i18next', () => ({ useTranslation: () => ({ i18n: { resolvedLanguage: 'es' } }) }));
const { default: Cart } = await import('./MerchCartPage');
let client: QueryClient;
const field = (name: string) => screen.getByRole<HTMLInputElement>('textbox', { name, exact: true });
const edit = (name: string, value: string) => fireEvent.change(field(name), { target: { value } });
const submit = () => fireEvent.submit(screen.getByRole('form', { name: 'Datos de entrega' }));
const button = () => screen.getByRole<HTMLButtonElement>('button', { name: 'Revisar y continuar al pago' });
const fillValid = () => { edit('Nombre completo', '  Ada Sintética  '); edit('Email', 'ada@example.test'); edit('Ciudad', 'Quito'); edit('Dirección', 'Dirección sintética 123'); };
async function mount() {
  client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  const tree = () => <QueryClientProvider client={client}><MemoryRouter initialEntries={['/tienda/synthetic/carrito']}><Routes><Route path="/tienda/:storeSlug/carrito" element={<Cart />} /><Route path="/tienda/orden/:orderId" element={<div>Orden sintética creada</div>} /></Routes></MemoryRouter></QueryClientProvider>;
  const view = render(tree()); await screen.findByRole('form', { name: 'Datos de entrega' });
  await waitFor(() => expect(button().disabled).toBe(!enabled || !available || !hasZone));
  return () => view.rerender(tree());
}
beforeEach(() => { enabled = true; available = true; hasZone = true; session = null; checkout.mockReset(); remove.mockReset(); capture.mockReset(); remember.mockReset(); window.sessionStorage.clear(); });
afterEach(() => { cleanup(); client.clear(); });

it('shows errors after blur/submit, moves focus only on submit and never sends invalid contact', async () => {
  await mount(); expect(field('Nombre completo').getAttribute('aria-invalid')).toBe('false');
  act(() => field('Email').focus()); fireEvent.blur(field('Nombre completo'));
  expect(field('Nombre completo').getAttribute('aria-invalid')).toBe('true'); expect(document.activeElement).toBe(field('Email'));
  submit(); expect(document.activeElement).toBe(field('Nombre completo')); expect(checkout).not.toHaveBeenCalled(); expect(capture).not.toHaveBeenCalled();
  edit('Nombre completo', 'Ada'); edit('Email', 'invalid'); submit(); expect(document.activeElement).toBe(field('Email'));
  expect(field('Email').getAttribute('aria-invalid')).toBe('true');
  const helper = field('Email').getAttribute('aria-describedby'); expect(document.getElementById(helper!)?.textContent).toContain('Ingresa un correo válido.');
  expect(field('Email').value).toBe('invalid'); expect(checkout).not.toHaveBeenCalled();
});

it.each([
  ['Nombre completo', 'a'.repeat(201)], ['Email', 'a'.repeat(310) + '@example.test'],
  ['Ciudad', 'c'.repeat(121)], ['Dirección', 'd'.repeat(301)],
  ['Teléfono (opcional)', '1'.repeat(501)], ['Provincia', 'p'.repeat(501)],
  ['Referencia (opcional)', 'r'.repeat(501)], ['Nota de entrega (opcional)', 'una línea\notra línea'],
  ['Nombre completo', 'A\u0085B'], ['Nota de entrega (opcional)', 'n'.repeat(501)],
])('retains invalid %s input and blocks programmatic checkout', async (name, value) => {
  await mount(); fillValid(); edit(name, value); submit();
  expect(field(name).value).toBe(value); expect(field(name).getAttribute('aria-invalid')).toBe('true');
  expect(document.activeElement).toBe(field(name)); expect(checkout).not.toHaveBeenCalled(); expect(capture).not.toHaveBeenCalled();
});

it('prefills editable account contact without restoring a deliberately cleared value', async () => {
  session = { displayName: 'Cuenta sintética', username: 'account@example.test' };
  const rerender = await mount(); expect(field('Nombre completo').value).toBe('Cuenta sintética'); expect(field('Email').value).toBe('account@example.test');
  edit('Email', ''); session = { ...session, username: 'changed@example.test' }; rerender();
  expect(field('Email').value).toBe(''); submit(); expect(document.activeElement).toBe(field('Email')); expect(checkout).not.toHaveBeenCalled();
});

it.each(['capability', 'stock', 'zone'])('blocks a direct submit when %s makes checkout unavailable', async condition => {
  enabled = condition !== 'capability'; available = condition !== 'stock'; hasZone = condition !== 'zone';
  await mount(); fillValid(); expect(button().disabled).toBe(true); submit(); expect(checkout).not.toHaveBeenCalled(); expect(capture).not.toHaveBeenCalled();
});

it('blocks checkout during a pending cart removal even through direct submission', async () => {
  remove.mockImplementationOnce(() => new Promise(() => {})); await mount(); fillValid();
  fireEvent.click(screen.getByRole('button', { name: 'Quitar Synthetic product' }));
  await waitFor(() => expect(button().disabled).toBe(true)); submit(); expect(checkout).not.toHaveBeenCalled();
});

it('keeps drafts, locks pending inputs and retries the same payload/key after rejection', async () => {
  let reject!: (reason: Error) => void;
  checkout.mockImplementationOnce(() => new Promise((_resolve, fail) => { reject = fail; }));
  await mount(); fillValid(); edit('Teléfono (opcional)', ' 0991234567 '); edit('Provincia', ' Pichincha '); edit('Nota de entrega (opcional)', ' Nota sintética ');
  fireEvent.click(button()); await waitFor(() => expect(field('Nombre completo').disabled).toBe(true));
  expect(screen.getByRole<HTMLInputElement>('checkbox').disabled).toBe(true);
  expect(screen.getByRole('combobox').getAttribute('aria-disabled')).toBe('true');
  expect(screen.getByRole<HTMLButtonElement>('button', { name: 'Quitar Synthetic product' }).disabled).toBe(true);
  submit(); expect(checkout).toHaveBeenCalledTimes(1);
  await act(async () => reject(new Error('Rechazo sintético')));
  await screen.findByText('Rechazo sintético'); expect(field('Nombre completo').value).toBe('  Ada Sintética  ');
  checkout.mockResolvedValueOnce({ id: 'synthetic-order', paymentStatus: 'pending' } as MerchOrder);
  fireEvent.click(button()); await screen.findByText('Orden sintética creada'); expect(checkout).toHaveBeenCalledTimes(2);
  expect(checkout.mock.calls[1]).toEqual(checkout.mock.calls[0]);
  expect(checkout.mock.calls[0]).toEqual(['cart-a', 'synthetic-private-token', 'synthetic-checkout-key', {
    recipient: { name: 'Ada Sintética', email: 'ada@example.test', phone: '0991234567', countryCode: 'EC', subdivision: 'Pichincha', city: 'Quito', addressLine1: 'Dirección sintética 123', addressLine2: null, postalCode: null, deliveryNote: 'Nota sintética' },
    shippingZoneId: 'zone-a', createAccount: false, locale: 'es',
  }]);
  expect(remember).toHaveBeenCalledWith(expect.objectContaining({ id: 'synthetic-order', paymentStatus: 'pending' }), 'synthetic-private-token');
  expect(screen.queryByText('Total calculado por el servidor')).toBeNull();
});

it('allows the API text boundaries including Unicode characters without truncation', async () => {
  await mount(); fillValid(); edit('Nombre completo', '🎵'.repeat(200)); edit('Ciudad', 'c'.repeat(120)); edit('Dirección', 'd'.repeat(300)); edit('Nota de entrega (opcional)', 'n'.repeat(500));
  checkout.mockRejectedValueOnce(new Error('Rechazo sintético')); submit(); await waitFor(() => expect(checkout).toHaveBeenCalledTimes(1));
  expect(checkout.mock.calls[0]?.[3].recipient.name).toBe('🎵'.repeat(200));
});

it('blocks checkout during background reads until availability is refreshed', async () => {
  await mount(); fillValid();
  await act(async () => client.cancelQueries({ queryKey: ['merch-cart', 'cart-a'] }));
  client.setQueryDefaults(['merch-cart', 'cart-a'], { queryFn: () => new Promise(() => {}) });
  await act(async () => { void client.fetchQuery({ queryKey: ['merch-cart', 'cart-a'], queryFn: () => new Promise(() => {}) }).catch(() => {}); });
  await screen.findByText('Actualizando tu carrito…'); expect(button().disabled).toBe(true); submit(); expect(checkout).not.toHaveBeenCalled();
});

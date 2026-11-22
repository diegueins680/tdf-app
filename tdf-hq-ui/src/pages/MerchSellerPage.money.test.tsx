import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { MerchProduct, MerchProductRequest, MerchShippingZoneRequest } from '../api/merch';

const updateStock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
let managedProducts: MerchProduct[] = [];
const createProduct = jest.fn<(id: string, payload: MerchProductRequest, key: string) => Promise<unknown>>();
const createShippingZone = jest.fn<(id: string, payload: MerchShippingZoneRequest) => Promise<unknown>>();
const empty = async () => [];
jest.unstable_mockModule('../api/merch', () => ({
  createMerchIdempotencyKey: (scope: string) => `synthetic-${scope}`,
  Merch: {
    capabilities: async () => ({ features: {} }),
    sellerStores: async () => [{ id: 'synthetic-store', displayName: 'Synthetic store', operationalStatus: 'active', permissions: { catalog: true, settings: true, stock: true } }],
    sellerProducts: async () => managedProducts, updateVariantStock: updateStock, sellerOrders: empty, sellerIssues: empty, members: empty,
    createProduct, createShippingZone,
  },
}));
jest.unstable_mockModule('../api/directory', () => ({ Directory: { managedProfiles: empty } }));
jest.unstable_mockModule('../components/party-selector/PartySelector', () => ({ PartySelector: () => null }));
jest.unstable_mockModule('react-i18next', () => ({ useTranslation: () => ({ i18n: { resolvedLanguage: 'es' } }) }));
const { default: MerchSellerPage } = await import('./MerchSellerPage');
let client: QueryClient;
const change = (input: HTMLElement, value: string) => fireEvent.change(input, { target: { value } });
async function mount() {
  client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  render(<QueryClientProvider client={client}><MerchSellerPage /></QueryClientProvider>);
  await screen.findByText('Nuevo borrador de producto');
}
beforeEach(() => { managedProducts = []; updateStock.mockReset(); createProduct.mockReset().mockResolvedValue({}); createShippingZone.mockReset().mockResolvedValue({}); });
afterEach(() => { cleanup(); client.clear(); });

it('converts each variant independently to cents and leaves stock and weight in their existing units', async () => {
  await mount();
  change(screen.getByLabelText(/^Nombre del producto/), 'Camiseta');
  change(screen.getByLabelText(/^Slug/), 'camiseta');
  change(screen.getByLabelText(/^Descripción/, { exact: true }), 'Descripción sintética');
  fireEvent.click(screen.getByRole('button', { name: 'Agregar variante' }));
  screen.getAllByLabelText(/^SKU/).forEach((input, index) => change(input, `SKU-${index}`));
  screen.getAllByLabelText(/^Nombre de variante/).forEach((input, index) => change(input, `Variante ${index + 1}`));
  const prices = screen.getAllByLabelText(/^Precio \(USD\)/);
  change(prices[0]!, '25,50'); change(prices[1]!, '17.09');
  change(screen.getAllByLabelText(/^Stock/)[0]!, '12');
  change(screen.getAllByLabelText(/^Peso \(gramos\)/)[0]!, '350');
  fireEvent.click(screen.getByRole('button', { name: 'Guardar borrador' }));
  await waitFor(() => expect(createProduct).toHaveBeenCalledTimes(1));
  const [store, payload, key] = createProduct.mock.calls[0]!;
  expect([store, key]).toEqual(['synthetic-store', 'synthetic-product']);
  expect(payload.variants[0]).toEqual(expect.objectContaining({ priceMinor: 2550, stockOnHand: 12, weightGrams: 350 }));
  expect(payload.variants[1]?.priceMinor).toBe(1709);
  expect(payload.variants.every(variant => !('price' in variant))).toBe(true);
});

it.each(['', '0', '-1', '25.505', '1e2', '90071992547409.92'])('blocks product amount %s even when native submit validation is bypassed', async value => {
  await mount();
  const input = screen.getByLabelText(/^Precio \(USD\)/);
  change(input, value);
  expect(input.getAttribute('aria-invalid')).toBe('true');
  expect(screen.getByRole<HTMLButtonElement>('button', { name: 'Guardar borrador' }).disabled).toBe(true);
  fireEvent.submit(input.closest('form')!);
  await screen.findByRole('alert');
  expect(createProduct).not.toHaveBeenCalled();
  expect((input as HTMLInputElement).value).toBe(value);
});

it('converts delivery fees exactly and allows explicit free delivery', async () => {
  await mount();
  fireEvent.click(screen.getByRole('tab', { name: 'Políticas y entrega' }));
  const rate = screen.getByLabelText(/^Tarifa \(USD\)/);
  change(rate, '5,25');
  fireEvent.click(screen.getByRole('button', { name: 'Agregar zona' }));
  await waitFor(() => expect(createShippingZone).toHaveBeenCalledWith('synthetic-store', expect.objectContaining({ rateMinor: 525, freeShippingMinMinor: null })));
  change(rate, '0');
  fireEvent.click(screen.getByRole('button', { name: 'Agregar zona' }));
  await waitFor(() => expect(createShippingZone).toHaveBeenLastCalledWith('synthetic-store', expect.objectContaining({ rateMinor: 0 })));
});

it('blocks invalid delivery amounts, keeps the input, and supports correction', async () => {
  await mount();
  fireEvent.click(screen.getByRole('tab', { name: 'Políticas y entrega' }));
  const rate = screen.getByLabelText(/^Tarifa \(USD\)/);
  change(rate, '5,255');
  expect(rate.getAttribute('aria-invalid')).toBe('true');
  fireEvent.submit(rate.closest('form')!);
  await waitFor(() => expect(createShippingZone).not.toHaveBeenCalled());
  change(rate, '5,25');
  fireEvent.click(screen.getByRole('button', { name: 'Agregar zona' }));
  await waitFor(() => expect(createShippingZone).toHaveBeenCalledWith('synthetic-store', expect.objectContaining({ rateMinor: 525 })));
});

it.each([['Stock', ''], ['Stock', '-1'], ['Stock', '1.5'], ['Stock', '1e3'], ['Stock', '2147483648'], ['Stock', '21474836470'],
  ['Peso (gramos)', ''], ['Peso (gramos)', '0'], ['Peso (gramos)', '100001'], ['Peso (gramos)', '1000001']])(
  'retains invalid %s text %s and blocks draft creation even on programmatic submit', async (label, raw) => {
    await mount();
    const input = screen.getByLabelText(new RegExp('^'+label.replace(/[()]/g, '\\$&')));
    change(input, raw);
    expect((input as HTMLInputElement).value).toBe(raw);
    expect(input.getAttribute('aria-invalid')).toBe('true');
    expect(document.getElementById(input.getAttribute('aria-describedby')!)?.textContent).toBeTruthy();
    expect((screen.getByRole('button', { name: 'Guardar borrador' })).disabled).toBe(true);
    fireEvent.submit(input.closest('form')!);
    await waitFor(() => expect(screen.getByRole('alert')).toBeTruthy());
    expect(createProduct).not.toHaveBeenCalled();
  },
);
it('keeps explicit zero stock valid, enforces the database boundary and sends integers with the original idempotency key', async () => {
  await mount();
  change(screen.getByLabelText(/^Stock/), '2147483647');
  change(screen.getByLabelText(/^Peso/), '100000');
  fireEvent.submit(screen.getByLabelText(/^Stock/).closest('form')!);
  await waitFor(() => expect(createProduct).toHaveBeenCalledTimes(1));
  expect(createProduct.mock.calls[0]?.[1].variants[0]).toEqual(expect.objectContaining({ stockOnHand: 2147483647, weightGrams: 100000 }));
  expect(createProduct.mock.calls[0]?.[2]).toBe('synthetic-product');
  change(screen.getByLabelText(/^Stock/), '0');
  fireEvent.submit(screen.getByLabelText(/^Stock/).closest('form')!);
  await waitFor(() => expect(createProduct).toHaveBeenCalledTimes(2));
  expect(createProduct.mock.calls[1]?.[1].variants[0]?.stockOnHand).toBe(0);
});
it('does not coerce blank stock or threshold to zero, and preserves a rejected versioned stock edit', async () => {
  managedProducts = [{ id: 'synthetic-product', name: 'Producto sintético', status: 'draft', category: 'apparel',
    variants: [{ id: 'variant-1', name: 'Variante sintética', sku: 'SKU-1', version: 7,
      stockOnHand: 12, reorderThreshold: 3, stockReserved: 2, stockSold: 1, active: true, stockMode: 'finite' }], images: [] } as MerchProduct];
  updateStock.mockRejectedValue(new Error('Synthetic version conflict'));
  await mount();
  const stock = screen.getByLabelText(/^Existencia/), threshold = screen.getByLabelText(/^Alerta de stock/);
  const submit = screen.getByRole('button', { name: 'Actualizar stock' });
  for (const [field, raw] of [[stock, ''], [threshold, ''], [stock, '2.5'], [threshold, '1e3']] as const) {
    change(field, raw); expect(field.getAttribute('aria-invalid')).toBe('true');
    expect((submit as HTMLButtonElement).disabled).toBe(true);
    fireEvent.submit(stock.closest('form')!);
  }
  expect(updateStock).not.toHaveBeenCalled();
  expect(screen.queryByText(/El stock cambió en otro lugar/)).toBeNull();
  change(stock, '9'); change(threshold, '0');
  fireEvent.click(submit);
  await waitFor(() => expect(updateStock).toHaveBeenCalledTimes(1));
  expect(updateStock).toHaveBeenCalledWith('synthetic-store', 'variant-1', { stockOnHand: 9, reorderThreshold: 0, active: true, version: 7 });
  await screen.findByText(/El stock cambió en otro lugar/);
  expect((stock as HTMLInputElement).value).toBe('9');
  expect((threshold as HTMLInputElement).value).toBe('0');
});

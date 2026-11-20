import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import type { ReactNode } from 'react';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';

const createRevision = jest.fn<(...args: unknown[]) => Promise<unknown>>().mockResolvedValue({ id: 'revision' });
const service = {
  scId: 'service', scCode: 'synthetic-studio', scName: 'Servicio sintético',
  scNameEs: 'Servicio sintético', scNameEn: 'Synthetic service',
  scCategoryId: 'service-categories', scPricingModelId: 'service-pricing-models',
  scRateCents: 12500, scCurrency: 'USD', scCurrencyId: 'currencies',
  scRequiresEngineer: false, scDefaultResources: [], scSortOrder: 10, scActive: true,
};
jest.unstable_mockModule('../api/services', () => ({ Services: { list: async () => [service] } }));
jest.unstable_mockModule('../api/rooms', () => ({ Rooms: { list: async () => [] } }));
jest.unstable_mockModule('../api/catalogs', () => ({ Catalogs: {
  listItems: async (code: string) => ({ items: code === 'services'
    ? [{ id: 'service', version: 4 }]
    : [{ id: code, code: code === 'currencies' ? 'USD' : 'synthetic', name: code }] }),
  listRevisions: async () => [], createRevision,
} }));
jest.unstable_mockModule('react-i18next', () => ({ useTranslation: () => ({ t: (key: string) => key }) }));
jest.unstable_mockModule('../components/LazyPaginatedList', () => ({
  default: ({ items, renderItems }: { items: unknown[]; renderItems: (rows: unknown[]) => ReactNode }) => renderItems(items),
}));
const { default: ServiceTypesPage } = await import('./ServiceTypesPage');
let client: QueryClient;
beforeEach(() => {
  createRevision.mockClear();
  service.scRateCents = 12500;
  client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
});
afterEach(() => { cleanup(); client.clear(); });

it('edits a currency price without a cents calculation and blocks lossy input before saving', async () => {
  render(<QueryClientProvider client={client}><ServiceTypesPage /></QueryClientProvider>);
  fireEvent.click(await screen.findByRole('button', { name: 'Crear revisión' }));
  const price = await screen.findByRole('textbox', { name: 'Precio (USD)' });
  expect(price).toHaveValue('125.00');
  fireEvent.change(screen.getByRole('textbox', { name: 'Motivo del cambio' }), { target: { value: 'Revisión sintética' } });
  fireEvent.change(price, { target: { value: '25.501' } });
  const save = screen.getByRole('button', { name: 'Guardar borrador' });
  expect(price).toHaveAttribute('aria-invalid', 'true');
  expect(save).toBeDisabled();
  fireEvent.click(save);
  expect(createRevision).not.toHaveBeenCalled();
  fireEvent.change(price, { target: { value: '25,50' } });
  expect(save).toBeEnabled();
  fireEvent.click(save);
  await waitFor(() => expect(createRevision).toHaveBeenCalledWith('services', expect.objectContaining({
    entityId: 'service', baseVersion: 4,
    serviceOffering: expect.objectContaining({ rateCents: 2550, currencyId: 'currencies' }),
  })));
});


it('keeps the last cent of a large exact price visible in the service list', async () => {
  service.scRateCents = Number.MAX_SAFE_INTEGER;
  render(<QueryClientProvider client={client}><ServiceTypesPage /></QueryClientProvider>);
  const whole = new Intl.NumberFormat().format(90071992547409n);
  const decimal = new Intl.NumberFormat().formatToParts(1.1).find((part) => part.type === 'decimal')!.value;
  await screen.findByText(`USD ${whole}${decimal}91`);
});

it('refuses to edit an existing price that the browser cannot represent exactly', async () => {
  service.scRateCents = Number.MAX_SAFE_INTEGER + 1;
  render(<QueryClientProvider client={client}><ServiceTypesPage /></QueryClientProvider>);
  await screen.findByText('Precio no disponible');
  fireEvent.click(screen.getByRole('button', { name: 'Crear revisión' }));
  await screen.findByText('No podemos editar este precio con precisión. Recarga el catálogo o solicita su revisión.');
  expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
  expect(createRevision).not.toHaveBeenCalled();
});

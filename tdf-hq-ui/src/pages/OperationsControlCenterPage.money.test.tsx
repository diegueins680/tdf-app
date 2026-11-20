import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { OperationsFilters } from '../api/operations';
import i18n from '../i18n';

const item = {
  id: '11111111-1111-4111-8111-111111111111',
  organizationId: '00000000-0000-4000-8000-000000000001',
  branchId: '00000000-0000-4000-8000-000000000002',
  sourceSystem: 'tdf-hq', sourceChannel: 'web', entityType: 'course_registration', entityId: '7',
  uncorrelated: false, correlationKey: 'course_registration:7', titleEs: 'Inscripción requiere atención',
  titleEn: 'Registration needs attention', descriptionEs: 'Revisar inscripción', descriptionEn: 'Review registration',
  status: 'new', priority: 'high', recommendedPriority: 'high', severity: 'warning', seen: false,
  firstSeenBy: null, firstSeenAt: null, assigneePartyId: null, responsibleTeam: null, customerPartyId: null,
  serviceKey: null, amountMinor: null, currency: null, paymentState: null,
  createdAt: '2026-08-09T12:00:00Z', updatedAt: '2026-08-09T12:00:00Z', dueAt: '2026-08-10T12:00:00Z',
  snoozedUntil: null, waitingReason: null, waitingExternalDependency: false, resumeAt: null,
  resolvedAt: null, archivedAt: null, slaState: 'on_track', version: 1, metadata: {},
} as const;


let locale = 'es';
let views: { id: string; name: string; filters: unknown }[] = [];
const list = jest.fn<(filters: OperationsFilters) => Promise<{ items: (typeof item)[]; nextCursor: null; hasMore: boolean }>>();
const saveView = jest.fn<(input: { filters: OperationsFilters }) => Promise<unknown>>();
jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({ useLocalePreferences: () => ({ locale, currency: 'USD', timezone: 'America/Guayaquil' }) }));
jest.unstable_mockModule('../components/party-selector/PartySelector', () => ({ PartySelector: () => null }));
jest.unstable_mockModule('../api/operations', () => ({ Operations: {
  list, saveView, savedViews: async () => views,
  metrics: async () => ({ currency: 'USD' }),
  events: async () => ({ events: [], lastEventId: null, retryAfterMs: 15000 }),
} }));
const { default: OperationsPage } = await import('./OperationsControlCenterPage');
let client: QueryClient;
async function mount() {
  client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  render(<QueryClientProvider client={client}><MemoryRouter><OperationsPage /></MemoryRouter></QueryClientProvider>);
  await waitFor(() => expect(list).toHaveBeenCalled());
}
async function filters() {
  fireEvent.click(screen.getByRole('button', { name: /^(Filtros|Filters)/ }));
  return { min: await screen.findByRole('textbox', { name: /^(Monto mínimo|Minimum amount)$/ }), max: screen.getByRole('textbox', { name: /^(Monto máximo|Maximum amount)$/ }) };
}
const change = (input: HTMLElement, value: string) => fireEvent.change(input, { target: { value } });
const apply = () => fireEvent.click(screen.getByRole('button', { name: /^(Aplicar filtros|Apply filters)$/ }));
const settle = () => act(async () => { await new Promise(resolve => setTimeout(resolve, 30)); });
beforeEach(async () => {
  locale = 'es'; views = [];
  await i18n.changeLanguage('es');
  list.mockReset().mockResolvedValue({ items: [item], nextCursor: null, hasMore: false });
  saveView.mockReset().mockResolvedValue({});
});
afterEach(() => { cleanup(); client.clear(); });

it('preserves decimal-comma input and distinguishes an explicit zero from a cleared lower bound', async () => {
  await mount();
  const { min, max } = await filters();
  change(min, '25,50');
  await waitFor(() => expect(list).toHaveBeenLastCalledWith(expect.objectContaining({ minAmountMinor: 2550 })));
  expect((min as HTMLInputElement).value).toBe('25,50');
  change(min, '0');
  await waitFor(() => expect(list).toHaveBeenLastCalledWith(expect.objectContaining({ minAmountMinor: 0 })));
  expect((min as HTMLInputElement).value).toBe('0');
  change(max, '10.05');
  await waitFor(() => expect(list).toHaveBeenLastCalledWith(expect.objectContaining({ minAmountMinor: 0, maxAmountMinor: 1005 })));
  change(min, '');
  await waitFor(() => expect(list).toHaveBeenLastCalledWith(expect.objectContaining({ minAmountMinor: undefined, maxAmountMinor: 1005 })));
});

it.each(['es', 'en'])('keeps invalid range text local in %s, retaining the last applied query', async language => {
  locale = language; await i18n.changeLanguage(language);
  await mount(); const { min } = await filters();
  change(min, '4.99');
  await waitFor(() => expect(list).toHaveBeenLastCalledWith(expect.objectContaining({ minAmountMinor: 499 })));
  for (const raw of ['4,', '1e2', '4.999', '90071992547409.92']) {
    list.mockClear(); change(min, raw); await settle();
    expect(min.getAttribute('aria-invalid')).toBe('true');
    expect((min as HTMLInputElement).value).toBe(raw);
    expect(screen.getByRole<HTMLButtonElement>('button', { name: /^(Aplicar filtros|Apply filters)$/ }).disabled).toBe(true);
    expect(list).not.toHaveBeenCalled(); expect(saveView).not.toHaveBeenCalled();
  }
  change(min, '5,01');
  await waitFor(() => expect(list).toHaveBeenLastCalledWith(expect.objectContaining({ minAmountMinor: 501 })));
});

it('does not query inverted bounds and applies them together after correction', async () => {
  await mount(); const { min, max } = await filters();
  change(max, '40');
  await waitFor(() => expect(list).toHaveBeenLastCalledWith(expect.objectContaining({ maxAmountMinor: 4000 })));
  list.mockClear(); change(min, '50'); await settle();
  expect(min.getAttribute('aria-invalid')).toBe('true'); expect(max.getAttribute('aria-invalid')).toBe('true');
  expect(list).not.toHaveBeenCalled();
  change(max, '100');
  await waitFor(() => expect(list).toHaveBeenLastCalledWith(expect.objectContaining({ minAmountMinor: 5000, maxAmountMinor: 10000 })));
});

it('preserves signed filters and the safe-number precision boundary', async () => {
  await mount(); const { min, max } = await filters();
  change(min, '-1,01'); change(max, '90071992547409.91');
  await waitFor(() => expect(list).toHaveBeenLastCalledWith(expect.objectContaining({ minAmountMinor: -101, maxAmountMinor: Number.MAX_SAFE_INTEGER })));
});

it('hydrates zero from a saved view and clears both displayed and applied bounds', async () => {
  views = [{ id: 'zero', name: 'Vista cero', filters: { minAmountMinor: 0, maxAmountMinor: 1005 } }];
  await mount();
  fireEvent.mouseDown(screen.getByRole('combobox', { name: 'Vistas guardadas' }));
  fireEvent.click(await screen.findByRole('option', { name: 'Vista cero' }));
  await waitFor(() => expect(list).toHaveBeenLastCalledWith(expect.objectContaining({ minAmountMinor: 0, maxAmountMinor: 1005 })));
  const { min, max } = await filters();
  expect((min as HTMLInputElement).value).toBe('0.00'); expect((max as HTMLInputElement).value).toBe('10.05');
  fireEvent.click(screen.getByRole('button', { name: 'Limpiar', exact: true }));
  await waitFor(() => expect(list).toHaveBeenLastCalledWith({ limit: 60 }));
  expect((min as HTMLInputElement).value).toBe(''); expect((max as HTMLInputElement).value).toBe('');
});

it.each([
  { minAmountMinor: Number.MAX_SAFE_INTEGER + 1 },
  { minAmountMinor: 1.5 }, { minAmountMinor: '0' }, { minAmountMinor: 200, maxAmountMinor: 100 },
])('rejects invalid saved amounts %j without silently dropping the range', async filters => {
  views = [{ id: 'invalid', name: 'Vista inválida', filters }];
  await mount(); list.mockClear();
  fireEvent.mouseDown(screen.getByRole('combobox', { name: 'Vistas guardadas' }));
  fireEvent.click(await screen.findByRole('option', { name: 'Vista inválida' }));
  await screen.findByText(/La vista guardada contiene importes inválidos/);
  await settle(); expect(list).not.toHaveBeenCalled(); expect(saveView).not.toHaveBeenCalled();
});

it('saves the applied exact bounds in a view', async () => {
  await mount(); const { min } = await filters(); change(min, '25,50');
  await waitFor(() => expect(list).toHaveBeenLastCalledWith(expect.objectContaining({ minAmountMinor: 2550 })));
  apply();
  fireEvent.click(await screen.findByRole('button', { name: 'Guardar vista', exact: true }));
  const dialog = await screen.findByRole('dialog');
  const name = dialog.querySelector('input'); if (!name) throw new Error('Missing view name');
  change(name, 'Importe exacto');
  fireEvent.click(screen.getByRole('button', { name: 'Guardar', exact: true }));
  await waitFor(() => expect(saveView).toHaveBeenCalledWith(expect.objectContaining({ filters: expect.objectContaining({ minAmountMinor: 2550 }) })));
});

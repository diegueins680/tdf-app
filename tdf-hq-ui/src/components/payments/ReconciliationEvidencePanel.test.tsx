/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { CommerceReconciliationEntry, CommerceReconciliationReport, CommerceReconciliationFilters } from '../../api/commerceOperations';

const getMock = jest.fn<(filters: CommerceReconciliationFilters) => Promise<CommerceReconciliationReport>>();
jest.unstable_mockModule('../../api/commerceOperations', () => ({ CommerceOperations: { listReconciliationExceptions: getMock } }));
let language = 'es';
jest.unstable_mockModule('react-i18next', () => ({ useTranslation: () => ({ i18n: { resolvedLanguage: language } }) }));
const { default: ReconciliationEvidencePanel, formatReconciliationMinor } = await import('./ReconciliationEvidencePanel');

const checkoutId = '00000000-0000-4000-8000-000000000abc';
const entry = (overrides: Partial<CommerceReconciliationEntry> = {}): CommerceReconciliationEntry => ({
  creId: '00000000-0000-4000-8000-000000000100', creProvider: 'payphone', creStatus: 'open',
  creReason: 'closed_checkout_approval', creCheckoutId: checkoutId,
  crePaymentAttemptId: '00000000-0000-4000-8000-000000000102',
  creExpectedMinor: '12515', creActualMinor: '12515', creCurrency: 'USD',
  creDetectedAt: '2026-09-15T12:00:00Z', creResolvedAt: null, ...overrides,
});
const report = (overrides: Partial<CommerceReconciliationReport> = {}): CommerceReconciliationReport => ({
  crrGeneratedAt: '2026-09-15T12:20:00Z', crrEnvironment: 'sandbox', crrStatus: 'open', crrCheckoutId: null,
  crrSchemaReady: true, crrEntries: [entry()], crrLimit: 25, crrOffset: 0, crrHasMore: false, ...overrides,
});
let client: QueryClient;
const mount = () => {
  client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  return render(<QueryClientProvider client={client}><ReconciliationEvidencePanel /></QueryClientProvider>);
};
const echo = (filters: CommerceReconciliationFilters) => report({
  crrEnvironment: filters.environment ?? 'sandbox', crrStatus: filters.status ?? null,
  crrCheckoutId: filters.checkoutId ?? null, crrOffset: filters.offset ?? 0, crrHasMore: true,
});

beforeEach(() => { language = 'es'; getMock.mockReset().mockResolvedValue(report()); });
afterEach(() => { cleanup(); client?.clear(); });

it.each([
  ['0', 'USD', 'USD 0.00'], ['-1', 'USD', 'USD -0.01'], ['12515', 'USD', 'USD 125.15'],
  ['9223372036854775807', 'USD', 'USD 92233720368547758.07'],
  ['-9223372036854775808', 'USD', 'USD -92233720368547758.08'],
  ['12515', 'JPY', '12515 unidades menores · JPY'], ['100', null, '100 unidades menores · —'],
  ['100', 'synthetic_private_currency', '100 unidades menores · —'],
  [null, 'USD', '—'], [12515, 'USD', '—'], ['1.5', 'USD', '—'], ['1e2', 'USD', '—'],
  ['01', 'USD', '—'], ['9223372036854775808', 'USD', '—'], ['-9223372036854775809', 'USD', '—'],
])('formats exact minor units without floating point: %s / %s', (value, currency, expected) => {
  expect(formatReconciliationMinor(value, currency)).toBe(expected);
});

it('defaults to sandbox/open and exposes only read-only report navigation', async () => {
  mount();
  await screen.findByRole('article');
  expect(getMock).toHaveBeenCalledWith({ environment: 'sandbox', status: 'open', checkoutId: undefined, offset: 0, limit: 25 });
  expect(screen.getByText('Monto esperado: USD 125.15')).toBeTruthy();
  expect(screen.getByText(`Checkout vinculado: ${checkoutId}`)).toBeTruthy();
  expect(screen.getByText(/Resuelto o ignorado no libera un pago retenido/)).toBeTruthy();
  expect(screen.getAllByRole('button')).toHaveLength(4);
  expect(screen.getByRole('button', { name: 'Página anterior de evidencia' }).hasAttribute('disabled')).toBe(true);
  expect(screen.getByRole('button', { name: 'Página siguiente de evidencia' }).hasAttribute('disabled')).toBe(true);
});

it('distinguishes unavailable schema from a verified empty result', async () => {
  getMock.mockResolvedValue(report({ crrSchemaReady: false, crrEntries: [] }));
  mount();
  await screen.findByText(/esquema de conciliación no está disponible/);
  expect(screen.queryByText(/No hay excepciones/)).toBeNull();
  getMock.mockResolvedValue(report({ crrEntries: [] }));
  fireEvent.click(screen.getByRole('button', { name: 'Actualizar evidencia de conciliación' }));
  await screen.findByText(/No hay excepciones/);
});

it('hides cached rows while refreshing and after failure without rendering server diagnostics', async () => {
  mount();
  await screen.findByRole('article');
  let rejectRequest: (error: Error) => void = () => undefined;
  getMock.mockImplementation(() => new Promise((_resolve, reject) => { rejectRequest = reject; }));
  fireEvent.click(screen.getByRole('button', { name: 'Actualizar evidencia de conciliación' }));
  await screen.findByRole('status');
  expect(screen.queryByRole('article')).toBeNull();
  rejectRequest(new Error('synthetic_private_database_diagnostic'));
  await screen.findByText(/Evidencia no disponible/);
  expect(screen.queryByRole('article')).toBeNull();
  expect(document.body.textContent).not.toContain('synthetic_private_database_diagnostic');
});

it.each([
  { crrEnvironment: 'production' }, { crrStatus: 'resolved' }, { crrCheckoutId: checkoutId },
  { crrOffset: 25 }, { crrLimit: 100 },
] satisfies Partial<CommerceReconciliationReport>[])('rejects response filter mismatches: %j', async (overrides) => {
  getMock.mockResolvedValue(report(overrides));
  mount();
  await screen.findByText(/Evidencia no disponible/);
  expect(screen.queryByRole('article')).toBeNull();
});

it('resets pagination on environment, status, and validated checkout filters', async () => {
  getMock.mockImplementation(async (filters) => echo(filters));
  mount();
  await screen.findByRole('article');
  fireEvent.click(screen.getByRole('button', { name: 'Página siguiente de evidencia' }));
  await waitFor(() => expect(getMock).toHaveBeenLastCalledWith(expect.objectContaining({ offset: 25 })));
  fireEvent.change(screen.getByLabelText('Estado de revisión'), { target: { value: '' } });
  await waitFor(() => expect(getMock).toHaveBeenLastCalledWith(expect.objectContaining({ status: undefined, offset: 0 })));
  fireEvent.change(screen.getByLabelText('Entorno de conciliación'), { target: { value: 'production' } });
  await waitFor(() => expect(getMock).toHaveBeenLastCalledWith(expect.objectContaining({ environment: 'production', offset: 0 })));
  fireEvent.change(screen.getByLabelText('Filtrar por UUID de checkout'), { target: { value: ` ${checkoutId.toUpperCase()} ` } });
  fireEvent.click(screen.getByRole('button', { name: 'Aplicar filtro de checkout' }));
  await waitFor(() => expect(getMock).toHaveBeenLastCalledWith(expect.objectContaining({ checkoutId, offset: 0 })));
  fireEvent.change(screen.getByLabelText('Filtrar por UUID de checkout'), { target: { value: '' } });
  fireEvent.click(screen.getByRole('button', { name: 'Aplicar filtro de checkout' }));
  await waitFor(() => expect(getMock).toHaveBeenLastCalledWith(expect.objectContaining({ checkoutId: undefined })));
});

it('does not query malformed checkout input or display previous evidence after invalid submission', async () => {
  mount();
  await screen.findByRole('article');
  fireEvent.change(screen.getByLabelText('Filtrar por UUID de checkout'), { target: { value: 'invalid&environment=production' } });
  fireEvent.click(screen.getByRole('button', { name: 'Aplicar filtro de checkout' }));
  await screen.findByText(/Ingresa un UUID de checkout válido/);
  expect(getMock).toHaveBeenCalledTimes(1);
  expect(screen.queryByRole('article')).toBeNull();
});

it('keeps resolved reviews neutral, absent links unverified, and unknown amounts distinct from zero', async () => {
  getMock.mockResolvedValue(report({ crrEntries: [entry({ creStatus: 'resolved', creExpectedMinor: '0', creActualMinor: null,
    creCheckoutId: null, crePaymentAttemptId: null, creResolvedAt: '2026-09-15T13:00:00Z' })] }));
  mount();
  const article = await screen.findByRole('article');
  expect(article.querySelector('.MuiChip-colorSuccess')).toBeNull();
  expect(screen.getByText('Monto esperado: USD 0.00')).toBeTruthy();
  expect(screen.getByText('Monto observado: —')).toBeTruthy();
  expect(screen.getByText(/Sin vínculo único verificado/)).toBeTruthy();
  expect(screen.queryByText(/^Checkout vinculado:/)).toBeNull();
});

it('supports English labels and preserves non-USD scale explicitly', async () => {
  language = 'en';
  getMock.mockResolvedValue(report({ crrEntries: [entry({ creExpectedMinor: '123', creCurrency: 'JPY' })] }));
  mount();
  await screen.findByText('Expected amount: 123 minor units · JPY');
  expect(screen.getByText(/Resolved or ignored does not release a held payment/)).toBeTruthy();
});

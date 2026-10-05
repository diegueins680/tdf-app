/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { CommerceProviderQueries, CommerceProviderQuery, CommerceProviderQueryFilters } from '../../api/commerceOperations';

const getMock = jest.fn<(filters: CommerceProviderQueryFilters) => Promise<CommerceProviderQueries>>();
jest.unstable_mockModule('../../api/commerceOperations', () => ({ CommerceOperations: { listProviderQueries: getMock } }));
jest.unstable_mockModule('react-i18next', () => ({ useTranslation: () => ({ i18n: { resolvedLanguage: 'es' } }) }));
const { default: ProviderQueryRecoveryPanel } = await import('./ProviderQueryRecoveryPanel');

const job = (overrides: Partial<CommerceProviderQuery> = {}): CommerceProviderQuery => ({
  cpqOperationId: '00000000-0000-4000-8000-000000000100',
  cpqCheckoutId: '00000000-0000-4000-8000-000000000101',
  cpqPaymentAttemptId: '00000000-0000-4000-8000-000000000102',
  cpqProvider: 'payphone', cpqStatus: 'dead_letter', cpqAttemptCount: 24,
  cpqOperationStatus: 'processing', cpqOutcomeCertainty: 'ambiguous',
  cpqCreatedAt: '2026-09-15T12:00:00Z', cpqLastAttemptAt: '2026-09-15T12:10:00Z',
  cpqNextAttemptAt: '2026-09-15T13:10:00Z', cpqLeaseExpiresAt: null,
  cpqCompletedAt: '2026-09-15T12:10:00Z', cpqLastOutcome: 'retry_exhausted', ...overrides,
});
const report = (overrides: Partial<CommerceProviderQueries> = {}): CommerceProviderQueries => ({
  cpqsGeneratedAt: '2026-09-15T12:20:00Z', cpqsEnvironment: 'sandbox', cpqsSchemaReady: true,
  cpqsRecoveryFlagEnabled: false, cpqsJobs: [job()], cpqsBudgets: [], cpqsLimit: 25,
  cpqsOffset: 0, cpqsHasMore: false, ...overrides,
});
let client: QueryClient;
const mount = () => {
  client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  return render(<QueryClientProvider client={client}><ProviderQueryRecoveryPanel /></QueryClientProvider>);
};

beforeEach(() => getMock.mockReset().mockResolvedValue(report()));
afterEach(() => { cleanup(); client?.clear(); });

it('uses sandbox by default and offers only report navigation, never a payment replay', async () => {
  mount();
  await screen.findByText(/Operación original: 00000000/);
  expect(getMock).toHaveBeenCalledWith({ environment: 'sandbox', status: 'dead_letter', offset: 0, limit: 25 });
  expect(screen.getByText(/recuperación programada está desactivada/)).toBeTruthy();
  expect(screen.getByText(/Certeza del resultado: ambiguous/)).toBeTruthy();
  expect(screen.getAllByRole('button')).toHaveLength(3);
  expect(screen.getByRole('button', { name: 'Página anterior de consultas' }).hasAttribute('disabled')).toBe(true);
  expect(screen.getByRole('button', { name: 'Página siguiente de consultas' }).hasAttribute('disabled')).toBe(true);
});

it('distinguishes missing schema from an empty queue and does not expose fake empty success', async () => {
  getMock.mockResolvedValue(report({ cpqsSchemaReady: false, cpqsJobs: [] }));
  mount();
  await screen.findByText(/esquema de recuperación no está instalado/);
  expect(screen.queryByText(/No hay consultas registradas/)).toBeNull();
});

it('displays a verified empty response and warns that the database switch is not worker readiness', async () => {
  getMock.mockResolvedValue(report({ cpqsJobs: [], cpqsRecoveryFlagEnabled: true }));
  mount();
  await screen.findByText(/No hay consultas registradas/);
  expect(screen.getByText(/No confirma un procesador activo/)).toBeTruthy();
});

it('hides cached rows after a failed refresh and never displays server diagnostics', async () => {
  mount();
  await screen.findByText(/Operación original: 00000000/);
  getMock.mockRejectedValue(new Error('synthetic_private_database_password'));
  fireEvent.click(screen.getByRole('button', { name: 'Actualizar informe de consultas' }));
  await screen.findByText(/Informe de consultas no disponible/);
  expect(screen.queryByText(/Operación original: 00000000/)).toBeNull();
  expect(document.body.textContent).not.toContain('synthetic_private_database_password');
});

it('resets pagination on filter/environment changes and hides wrong-environment responses', async () => {
  getMock.mockImplementation(async (filters) => report({ cpqsHasMore: true, cpqsOffset: filters.offset ?? 0 }));
  mount();
  await screen.findByText(/Operación original: 00000000/);
  fireEvent.click(screen.getByRole('button', { name: 'Página siguiente de consultas' }));
  await waitFor(() => expect(getMock).toHaveBeenLastCalledWith({ environment: 'sandbox', status: 'dead_letter', offset: 25, limit: 25 }));
  fireEvent.change(screen.getByLabelText('Estado de consulta'), { target: { value: 'retry' } });
  await waitFor(() => expect(getMock).toHaveBeenLastCalledWith({ environment: 'sandbox', status: 'retry', offset: 0, limit: 25 }));
  fireEvent.change(screen.getByLabelText('Entorno de consultas'), { target: { value: 'production' } });
  await waitFor(() => expect(getMock).toHaveBeenLastCalledWith({ environment: 'production', status: 'retry', offset: 0, limit: 25 }));
  await screen.findByText(/Informe de consultas no disponible/);
  expect(screen.queryByText(/Operación original: 00000000/)).toBeNull();
});

it('keeps completed status checks distinct from a confirmed no-charge operation', async () => {
  getMock.mockImplementation(async (filters) => report({ cpqsJobs: filters.status === 'completed'
    ? [job({ cpqStatus: 'completed', cpqOperationStatus: 'confirmed_no_charge', cpqOutcomeCertainty: 'confirmed_no_charge', cpqLastOutcome: 'query_applied' })] : [] }));
  mount();
  fireEvent.change(screen.getByLabelText('Estado de consulta'), { target: { value: 'completed' } });
  await screen.findByText(/Certeza del resultado: confirmed_no_charge/);
  expect(screen.getByText(/Una consulta finalizada no significa un pago exitoso/)).toBeTruthy();
  expect(screen.getByRole('article').querySelector('.MuiChip-colorSuccess')).toBeNull();
  expect(screen.queryByText(/No antes de:/)).toBeNull();
});

it('hides existing rows while another environment is loading', async () => {
  mount();
  await screen.findByText(/Operación original: 00000000/);
  getMock.mockImplementation(() => new Promise(() => undefined));
  fireEvent.change(screen.getByLabelText('Entorno de consultas'), { target: { value: 'production' } });
  await screen.findByRole('status');
  expect(screen.queryByText(/Operación original: 00000000/)).toBeNull();
});

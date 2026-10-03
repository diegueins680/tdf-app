import { jest, test, expect, beforeEach } from '@jest/globals';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';

const get = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const post = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const put = jest.fn<(...args: unknown[]) => Promise<unknown>>();
jest.unstable_mockModule('../api/client', () => ({ get, post, put }));
const { default: RecordsIngestionPage } = await import('./RecordsIngestionPage');

beforeEach(() => {
  jest.clearAllMocks();
  get.mockResolvedValue({ enabled: false, intervalSeconds: 7200, nextScheduledAt: null, sources: [], runs: [{
    id: 'run-1', key: 'records-youtube:full:7:original-key', dryRun: false, status: 'partial', startedAt: '2026-09-26T00:00:00Z',
    report: { sourceAccountId: '7', full: true, phase: 'known', checkpoint: null, pages: 2, counts: { created: 3 } },
  }] });
  post.mockResolvedValue({ status: 'completed' });
  put.mockResolvedValue({});
});
function mount() {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  return render(<QueryClientProvider client={client}><RecordsIngestionPage /></QueryClientProvider>);
}
test('resumes the original reconciliation identity even between pagination phases', async () => {
  mount();
  fireEvent.click(await screen.findByRole('button', { name: 'Continuar ejecución' }));
  await waitFor(() => expect(post).toHaveBeenCalledWith('/admin/records-ingestion/runs', {
    sourceAccountId: 7, executionKey: 'original-key', reconciliation: true, dryRun: false,
  }));
});
test('shows a stopped schedule and preserves the saved interval until edited', async () => {
  mount();
  expect(await screen.findByText(/Programación detenida/)).toBeTruthy();
  const interval = screen.getByRole('spinbutton', { name: 'Frecuencia en segundos' });
  expect((interval as HTMLInputElement).value).toBe('7200');
  fireEvent.change(interval, { target: { value: '300.5' } });
  expect(screen.getByRole<HTMLButtonElement>('button', { name: 'Guardar frecuencia' }).disabled).toBe(true);
});

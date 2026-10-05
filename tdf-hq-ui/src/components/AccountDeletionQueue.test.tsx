import { jest } from '@jest/globals';
import { render, screen, fireEvent, cleanup } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { I18nextProvider } from 'react-i18next';
import { createInstance } from 'i18next';
import es from '../i18n/locales/accountDeletion.es';
import type { AccountDeletionActionDTO, LegacyFeedbackDTO } from '../api/types';

let formEnabled = true;
jest.unstable_mockModule('../config/accountDeletionRollout', () => ({ isAccountDeletionFormEnabled: () => formEnabled }));
beforeEach(() => { formEnabled = true; });
const resolve = jest.fn<(id: string, outcome: 'completed' | 'rejected', note: string) => Promise<AccountDeletionActionDTO>>();
const list = jest.fn<(filters: unknown) => Promise<LegacyFeedbackDTO[]>>();
jest.unstable_mockModule('../api/internalFeedback', () => ({ InternalFeedback: { listLegacy: list, resolveDeletion: resolve } }));
const { default: Queue } = await import('./AccountDeletionQueue');
const record = (id: number) => ({ lfdId: `request-${id}`, lfdTitle: `Deletion ${id}`, lfdDescription: 'account_deletion_request\n', lfdCreatedBy: id, lfdCreatedAt: '2026-10-05T12:00:00Z' }) as LegacyFeedbackDTO;
async function show() {
  const i18n = createInstance(); await i18n.init({ lng: 'es', resources: { es: { translation: { accountDeletion: es } } } });
  render(<I18nextProvider i18n={i18n}><QueryClientProvider client={new QueryClient({ defaultOptions: { queries: { retry: false } } })}><Queue /></QueryClientProvider></I18nextProvider>);
}
afterEach(() => { cleanup(); list.mockReset(); resolve.mockReset(); });
it('keeps the eleventh request visible and exposes older pages with authoritative owner IDs', async () => {
  list.mockResolvedValueOnce(Array.from({ length: 20 }, (_, index) => record(index + 1))).mockResolvedValueOnce([record(21)]).mockResolvedValue(Array.from({ length: 20 }, (_, index) => record(index + 1)));
  await show();
  expect((await screen.findByText('Deletion 11')).textContent).toBe('Deletion 11');
  expect(screen.getByText(/Solicitud request-11 · Cuenta autenticada/).textContent).toContain('11');
  expect(list).toHaveBeenCalledWith({ accountDeletionOnly: true, offset: 0 });
  fireEvent.click(screen.getByRole('button', { name: 'Siguiente' }));
  expect((await screen.findByText('Deletion 21')).textContent).toBe('Deletion 21');
  expect(list).toHaveBeenLastCalledWith({ accountDeletionOnly: true, offset: 20 });
  expect(screen.getByRole('button', { name: 'Siguiente' }).hasAttribute('disabled')).toBe(true);
  fireEvent.click(screen.getByRole('button', { name: 'Anterior' }));
  expect((await screen.findByText('Deletion 11')).textContent).toBe('Deletion 11');
});
it('exposes loading errors and permits retry rather than claiming an empty queue', async () => {
  list.mockRejectedValueOnce(new Error('unauthorized')).mockResolvedValueOnce([]);
  await show();
  expect((await screen.findByRole('alert')).textContent).toContain('No se pudieron cargar');
  fireEvent.click(screen.getByRole('button', { name: 'Actualizar' }));
  expect((await screen.findByText(es.queueEmpty)).textContent).toBe(es.queueEmpty);
});

it('requires a note, persists the outcome and displays the authoritative operator receipt after refresh', async () => {
  const receipt: AccountDeletionActionDTO = { adaOutcome: 'completed', adaNote: 'Verified synthetic fulfilment', adaActor: 40, adaCreatedAt: '2026-10-05T15:00:00Z' };
  list.mockResolvedValueOnce([record(1)]).mockResolvedValue([{ ...record(1), lfdDeletionHistory: [receipt] }]);
  resolve.mockResolvedValue(receipt);
  await show();
  const button = await screen.findByRole('button', { name: es.markCompleted });
  expect(button.hasAttribute('disabled')).toBe(true);
  fireEvent.change(screen.getByLabelText(es.resolutionNote), { target: { value: receipt.adaNote } });
  fireEvent.click(button);
  expect((await screen.findByText(es.completed)).textContent).toBe(es.completed);
  expect(resolve).toHaveBeenCalledWith('request-1', 'completed', receipt.adaNote);
  expect(screen.getByText(/Operador 40/).textContent).toContain('40');
  expect(screen.queryByRole('button', { name: es.markCompleted })).toBeNull();
});
it('does not claim resolution when a concurrent update rejects the action', async () => {
  list.mockResolvedValue([record(1)]); resolve.mockRejectedValue(new Error('409'));
  await show(); await screen.findByText('Deletion 1');
  fireEvent.change(screen.getByLabelText(es.resolutionNote), { target: { value: 'Synthetic rejection reason' } });
  fireEvent.click(screen.getByRole('button', { name: es.markRejected }));
  expect((await screen.findByText(es.resolutionError)).textContent).toBe(es.resolutionError);
  expect(screen.queryByText(es.rejected)).toBeNull();
});

it('retains the authoritative terminal receipt when the subsequent queue refresh fails', async () => {
  const receipt: AccountDeletionActionDTO = { adaOutcome: 'completed', adaNote: 'Verified synthetic fulfilment', adaActor: 40, adaCreatedAt: '2026-10-05T15:00:00Z' };
  list.mockResolvedValueOnce([record(1)]).mockRejectedValue(new Error('refresh unavailable'));
  resolve.mockResolvedValue(receipt);
  await show(); await screen.findByText('Deletion 1');
  fireEvent.change(screen.getByLabelText(es.resolutionNote), { target: { value: receipt.adaNote } });
  fireEvent.click(screen.getByRole('button', { name: es.markCompleted }));
  await screen.findByText(es.queueError);
  expect(screen.getByText(es.completed)).toBeTruthy();
  expect(screen.getByText(receipt.adaNote)).toBeTruthy();
  expect(screen.queryByRole('button', { name: es.markCompleted })).toBeNull();
  expect(screen.queryByRole('button', { name: es.markRejected })).toBeNull();
  expect(resolve).toHaveBeenCalledTimes(1);
});

it('does not query or expose the new admin workflow before rollout', async () => {
  formEnabled = false; await show();
  expect(screen.queryByRole('heading', { name: es.queueTitle })).toBeNull();
  expect(list).not.toHaveBeenCalled(); expect(resolve).not.toHaveBeenCalled();
});

import { jest } from '@jest/globals';
import { render, screen, fireEvent, cleanup } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { I18nextProvider } from 'react-i18next';
import { createInstance } from 'i18next';
import es from '../i18n/locales/accountDeletion.es';
import type { LegacyFeedbackDTO } from '../api/types';

const list = jest.fn<(filters: unknown) => Promise<LegacyFeedbackDTO[]>>();
jest.unstable_mockModule('../api/internalFeedback', () => ({ InternalFeedback: { listLegacy: list } }));
const { default: Queue } = await import('./AccountDeletionQueue');
const record = (id: number) => ({ lfdId: `request-${id}`, lfdTitle: `Deletion ${id}`, lfdDescription: 'account_deletion_request\n', lfdCreatedBy: id, lfdCreatedAt: '2026-10-05T12:00:00Z' }) as LegacyFeedbackDTO;
async function show() {
  const i18n = createInstance(); await i18n.init({ lng: 'es', resources: { es: { translation: { accountDeletion: es } } } });
  render(<I18nextProvider i18n={i18n}><QueryClientProvider client={new QueryClient({ defaultOptions: { queries: { retry: false } } })}><Queue /></QueryClientProvider></I18nextProvider>);
}
afterEach(() => { cleanup(); list.mockReset(); });
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

import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { SessionUser } from '../session/SessionContext';
import type { SessionResponseDTO } from '../api/session';
import i18n from '../i18n';
import { expectNoSeriousAccessibilityViolations } from '../test/accessibility';

let session: SessionUser | null;
let categoriesAvailable = true;
const logout = jest.fn();
const snapshot = jest.fn<() => Promise<SessionResponseDTO | null>>();
const request = jest.fn<(...args: unknown[]) => Promise<void>>();
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session, loading: false, logout }) }));
jest.unstable_mockModule('../api/session', () => ({ loadSessionSnapshot: snapshot }));
jest.unstable_mockModule('../api/accountDeletion', () => ({ requestAccountDeletion: request }));
jest.unstable_mockModule('../api/catalogs', () => ({ Catalogs: { listPublicBatch: async () => ({ catalogs: [
  { catalog: { code: 'feedback-categories' }, items: categoriesAvailable ? [{ id: 'category', code: 'idea', active: true, workflowState: 'published' }] : [] },
  { catalog: { code: 'feedback-severities' }, items: [{ id: 'severity', code: 'p4', active: true, workflowState: 'published' }] },
] }) } }));
const { default: Page } = await import('./AccountDeletionPage');
function mount() { return render(<QueryClientProvider client={new QueryClient({ defaultOptions: { queries: { retry: false } } })}><MemoryRouter><Page /></MemoryRouter></QueryClientProvider>); }
beforeEach(async () => {
  await i18n.changeLanguage('es'); categoriesAvailable = true;
  session = { username: 'owner@example.com', displayName: 'Owner', roles: [], partyId: 42 };
  snapshot.mockReset(); snapshot.mockResolvedValue(session as SessionResponseDTO);
  request.mockReset(); request.mockResolvedValue(); logout.mockClear();
});
afterEach(cleanup);
it('requires sign-in and preserves the deletion destination, without sending anything', () => {
  session = null; mount();
  expect(screen.getByRole('link', { name: 'Iniciar sesión para continuar' }).getAttribute('href')).toBe('/login?redirect=%2Fcuenta%2Feliminar');
  expect(screen.queryByRole('checkbox')).toBeNull();
  expect(snapshot).not.toHaveBeenCalled(); expect(request).not.toHaveBeenCalled();
});
it('requires explicit confirmation and records receipt rather than completed deletion', async () => {
  mount();
  const button = await screen.findByRole<HTMLButtonElement>('button', { name: 'Solicitar eliminación de esta cuenta' });
  expect(button.disabled).toBe(true);
  fireEvent.click(screen.getByRole('checkbox'));
  await waitFor(() => expect(button.disabled).toBe(false));
  fireEvent.click(button);
  expect((await screen.findByRole('status')).textContent).toContain('Solicitud de eliminación recibida');
  expect(request).toHaveBeenCalledTimes(1);
  expect(request).toHaveBeenCalledWith({ partyId: 42, categoryId: 'category', severityId: 'severity', locale: 'es' });
  expect(screen.queryByRole('checkbox')).toBeNull();
});
it('keeps failures recoverable without showing backend diagnostics or claiming success', async () => {
  request.mockRejectedValue(new Error('private diagnostic'));
  mount(); const button = await screen.findByRole<HTMLButtonElement>('button', { name: 'Solicitar eliminación de esta cuenta' });
  fireEvent.click(screen.getByRole('checkbox')); await waitFor(() => expect(button.disabled).toBe(false)); fireEvent.click(button);
  expect((await screen.findByRole('alert')).textContent).toContain('No pudimos confirmar el envío');
  expect(screen.queryByText('private diagnostic')).toBeNull();
  expect(screen.queryByRole('status')).toBeNull();
});
it('offers reauthentication when the live account differs, instead of using a cached identity', async () => {
  snapshot.mockResolvedValue({ ...session, partyId: 43 } as SessionResponseDTO); mount();
  const link = await screen.findByRole('link', { name: 'Iniciar sesión para continuar' });
  expect(screen.queryByRole('checkbox')).toBeNull(); fireEvent.click(link);
  expect(logout).toHaveBeenCalledTimes(1); expect(request).not.toHaveBeenCalled();
});
it('fails closed when a published non-bug category is unavailable', async () => {
  categoriesAvailable = false; mount();
  expect((await screen.findByRole('alert')).textContent).toContain('No se envió ninguna solicitud');
  fireEvent.click(screen.getByRole('checkbox'));
  expect(screen.getByRole<HTMLButtonElement>('button', { name: 'Solicitar eliminación de esta cuenta' }).disabled).toBe(true);
  expect(request).not.toHaveBeenCalled();
});
it('translates the authenticated form and provides accessible semantics', async () => {
  await i18n.changeLanguage('en'); const view = mount();
  expect(await screen.findByRole('button', { name: 'Request deletion of this account' })).toBeTruthy();
  expect(screen.getByRole('heading', { level: 1 }).textContent).toContain('Delete your TDF account');
  expect(document.querySelector('meta[name="robots"]')?.getAttribute('content')).toBe('noindex,nofollow');
  await expectNoSeriousAccessibilityViolations(view.container);
});
it('sends only once while a request is pending, even for repeated form submissions', async () => {
  let finish!: () => void;
  request.mockReturnValue(new Promise<void>(resolve => { finish = resolve; }));
  mount(); const button = await screen.findByRole<HTMLButtonElement>('button', { name: 'Solicitar eliminación de esta cuenta' });
  fireEvent.click(screen.getByRole('checkbox')); await waitFor(() => expect(button.disabled).toBe(false));
  fireEvent.submit(button.closest('form')!); fireEvent.submit(button.closest('form')!);
  await waitFor(() => expect(request).toHaveBeenCalledTimes(1));
  await act(async () => { finish(); });
  expect((await screen.findByRole('status')).textContent).toContain('Solicitud de eliminación recibida');
});

import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
const submit = jest.fn<(...args: unknown[]) => Promise<void>>();
const create = jest.fn();
const update = jest.fn();
const createUser = jest.fn();
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: null }) }));
jest.unstable_mockModule('../api/liveSessions', () => ({ submitLiveSessionIntake: submit }));
jest.unstable_mockModule('../api/parties', () => ({ Parties: { create, update, getOne: jest.fn() } }));
jest.unstable_mockModule('../api/admin', () => ({ Admin: { createUser } }));
jest.unstable_mockModule('../components/party-selector/PartySelector', () => ({ PartySelector: () => <p>Private contact selector</p> }));
jest.unstable_mockModule('../components/EnrollmentSuccessDialog', () => ({ default: ({ open }: { open: boolean }) => open ? <p>Persisted receipt</p> : null }));
jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({ useLocalePreferences: () => ({ locale: 'es' }) }));
jest.unstable_mockModule('../api/catalogs', () => ({ Catalogs: { listPublicBatch: async () => ({ catalogs: [] }) } }));
const { LiveSessionIntakeForm } = await import('./LiveSessionIntakePage');
const show = (code = 'synthetic-code') => {
 const client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
 const element = (accessCode?: string) => <QueryClientProvider client={client}><LiveSessionIntakeForm variant="public" accessCode={accessCode} /></QueryClientProvider>;
 const view = render(element(code));
 return { changeCode: (value?: string) => view.rerender(element(value)) };
};
const fill = () => {
 fireEvent.change(screen.getByLabelText(/Nombre de la banda/), { target: { value: 'Banda sintética' } });
 fireEvent.change(screen.getByLabelText(/Nombre completo/), { target: { value: 'Músico sintético' } });
 fireEvent.click(screen.getByRole('checkbox', { name: /Acepto los términos/ }));
};
const send = () => fireEvent.click(screen.getByRole('button', { name: 'Enviar Live Session' }));
beforeEach(() => { jest.clearAllMocks(); localStorage.clear(); });
afterEach(cleanup);
it('submits only with the validated code, without ambient CRM mutations, and awaits persistence', async () => {
 let finish!: () => void;
 submit.mockImplementation(() => new Promise<void>(resolve => { finish = resolve; }));
 show(); fill();
 expect(screen.queryByText('Private contact selector')).not.toBeInTheDocument();
 send(); await waitFor(() => expect(submit).toHaveBeenCalledTimes(1));
 expect(screen.getByRole('button', { name: 'Cancelar' })).toBeDisabled();
 fireEvent.click(screen.getByRole('button', { name: 'Cancelar' }));
 expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
 expect(submit).toHaveBeenCalledWith(expect.objectContaining({ bandName: 'Banda sintética', musicians: [expect.objectContaining({ name: 'Músico sintético', isExisting: false })] }), 'synthetic-code', expect.any(String));
 expect(create).not.toHaveBeenCalled(); expect(update).not.toHaveBeenCalled(); expect(createUser).not.toHaveBeenCalled();
 expect(screen.queryByText('Persisted receipt')).not.toBeInTheDocument();
 await act(async () => { finish(); });
 await screen.findByText('Persisted receipt');
});
it('ignores a successful old submission after switching away and back to the same code', async () => {
 let finish!: () => void;
 submit.mockImplementation(() => new Promise<void>(resolve => { finish = resolve; }));
 const page = show(); fill(); send(); await waitFor(() => expect(submit).toHaveBeenCalledTimes(1));
 page.changeCode(undefined); page.changeCode('synthetic-code');
 await act(async () => { finish(); });
 await waitFor(() => expect(screen.getByRole('button', { name: 'Enviar Live Session' })).toBeEnabled());
 expect(screen.queryByText('Persisted receipt')).not.toBeInTheDocument();
 expect(screen.getByRole('checkbox', { name: /Acepto los términos/ })).toBeChecked();
});
it('keeps input and offers retry after a persistence failure', async () => {
 submit.mockRejectedValueOnce(new Error('Synthetic persistence failure')).mockResolvedValueOnce(undefined);
 show(); fill(); send(); await screen.findByText('Synthetic persistence failure');
 expect(screen.queryByText('Persisted receipt')).not.toBeInTheDocument();
 expect(screen.getByLabelText(/Nombre de la banda/)).toHaveValue('Banda sintética');
 send(); await screen.findByText('Persisted receipt'); expect(submit).toHaveBeenCalledTimes(2);
 expect(submit.mock.calls[0]?.[2]).toBe(submit.mock.calls[1]?.[2]);
});

it('does not disclose legacy or another account’s draft before verifying the owner', async () => {
 localStorage.setItem('live-session-draft', JSON.stringify({ bandName: 'Private legacy band' }));
 localStorage.setItem('live-session-draft:public:99', JSON.stringify({ bandName: 'Other account band' }));
 show();
 expect(screen.getByLabelText(/Nombre de la banda/)).toHaveValue('');
 expect(localStorage.getItem('live-session-draft')).toContain('Private legacy band');
});

it('sends new internal musicians atomically without pre-creating contacts or accounts', async () => {
 const client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
 submit.mockResolvedValue(undefined);
 render(<QueryClientProvider client={client}><LiveSessionIntakeForm variant="internal" draftOwner={17} /></QueryClientProvider>);
 fill(); send(); await screen.findByText('Persisted receipt');
 expect(create).not.toHaveBeenCalled(); expect(update).not.toHaveBeenCalled(); expect(createUser).not.toHaveBeenCalled();
 expect(submit.mock.calls[0]?.[2]).toEqual(expect.any(String));
});

it.each([false, true])('offers existing-contact reuse only with CRM access (%s)', (canReuseContacts) => {
 const client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
 render(<QueryClientProvider client={client}><LiveSessionIntakeForm variant="internal" draftOwner={17} canReuseContacts={canReuseContacts} /></QueryClientProvider>);
 expect(Boolean(screen.queryByText('Private contact selector'))).toBe(canReuseContacts);
 expect(screen.getByText('Se creará un contacto para esta sesión')).toBeInTheDocument();
 expect(screen.queryByText('Se creará usuario y contacto automáticamente')).not.toBeInTheDocument();
});

it('explicitly abandons the owned draft and starts a new operation after warning about uncertain sends', async () => {
 const back = jest.spyOn(window.history, 'back').mockImplementation(() => {});
 const client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
 submit.mockRejectedValueOnce(new Error('Response lost')).mockResolvedValueOnce(undefined);
 try {
  render(<QueryClientProvider client={client}><LiveSessionIntakeForm variant="internal" draftOwner={17} /></QueryClientProvider>);
  fill(); send(); await screen.findByText('Response lost');
  const oldKey = submit.mock.calls[0]?.[2];
  fireEvent.click(screen.getByRole('button', { name: 'Cancelar' }));
  expect(screen.getByText(/una solicitud anterior podría haberse guardado/)).toBeInTheDocument();
  fireEvent.click(screen.getByRole('button', { name: 'Seguir editando' }));
  await waitFor(() => expect(screen.queryByRole('dialog')).not.toBeInTheDocument());
  expect(screen.getByLabelText(/Nombre de la banda/)).toHaveValue('Banda sintética');
  fireEvent.click(screen.getByRole('button', { name: 'Cancelar' }));
  fireEvent.click(screen.getByRole('button', { name: 'Descartar borrador' }));
  await waitFor(() => expect(screen.queryByRole('dialog')).not.toBeInTheDocument());
  expect(back).toHaveBeenCalledTimes(1);
  expect(screen.getByLabelText(/Nombre de la banda/)).toHaveValue('');
  const draft = JSON.parse(localStorage.getItem('live-session-draft:internal:17') ?? '{}') as { bandName?: string; submissionKey?: string };
  expect(draft.bandName).toBe(''); expect(draft.submissionKey).not.toBe(oldKey);
  fill(); send(); await screen.findByText('Persisted receipt');
  expect(submit.mock.calls[1]?.[2]).not.toBe(oldKey);
 } finally { back.mockRestore(); }
});

import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';

const preference = jest.fn(async () => ({ inApp: false, supportsExtendedMoments: true }));
const save = jest.fn<(enabled: boolean) => Promise<{ inApp: boolean }>>();
const summary = jest.fn(async () => ({ enabled: false, eventFinished: false, candidates: 5, consented: 0,
  invitations: 0, reminders: 0, opened: 0, published: 0, uniqueAuthors: 0, moments: 0 }));
const setCampaign = jest.fn(async (eventId: string, enabled: boolean) => ({ ...(await summary()), enabled }));
const open = jest.fn(async () => ({ recorded: true }));
jest.unstable_mockModule('../../api/eventMemoryInvitations', () => ({ EventMemoryInvitations: {
  preferences: preference, setPreference: save, summary, setCampaign, open,
} }));
jest.unstable_mockModule('../../api/client', () => ({ ApiError: class extends Error { status = 503; } }));
let activeParty = 9;
jest.unstable_mockModule('../../session/SessionContext', () => ({ useSession: () => ({ session: { partyId: activeParty } }) }));
const { default: Controls } = await import('./EventMemoryInvitations');
function mount(organizer = false, entry = '/social/eventos/141') {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  const view = render(<QueryClientProvider client={client}><MemoryRouter initialEntries={[entry]}><Controls eventId="141" organizer={organizer} /></MemoryRouter></QueryClientProvider>);
  return () => { view.unmount(); client.clear(); };
}
beforeEach(() => { jest.clearAllMocks(); activeParty = 9; save.mockReset().mockResolvedValue({ inApp: true }); });
it('requires an explicit preference and preserves the server setting after a failed save', async () => {
  save.mockRejectedValueOnce(new Error('Network failed'));
  const close = mount();
  const box = await screen.findByRole('checkbox', { name: /Recibir invitaciones/ });
  expect((box as HTMLInputElement).checked).toBe(false);
  expect(save).not.toHaveBeenCalled();
  fireEvent.click(box);
  await screen.findByText(/No se pudo guardar/);
  expect(save).toHaveBeenCalledWith(true);
  expect((box as HTMLInputElement).checked).toBe(false);
  expect(summary).not.toHaveBeenCalled();
  close();
});
it('shows organizer aggregates and explains the completion hold without sending from the browser', async () => {
  const close = mount(true);
  await screen.findByText(/Envío en espera/);
  expect(screen.getByText(/Cuentas relacionadas: 5/)).toBeTruthy();
  expect(setCampaign).not.toHaveBeenCalled();
  expect(save).not.toHaveBeenCalled();
  close();
});
it('attributes only a syntactically valid invitation while preserving the event context', async () => {
  const key = '91914100-0000-4000-8000-000000000001';
  const close = mount(false, `/social/eventos/141?compose=1&invitation=${key}#momentos`);
  await waitFor(() => expect(open).toHaveBeenCalledWith('141', key));
  expect(open).toHaveBeenCalledTimes(1);
  close();
});

it('does not grant invitation consent after credentials change before the next session render', async () => {
  const close = mount(false);
  const checkbox = await screen.findByRole('checkbox', { name: 'Recibir invitaciones a compartir recuerdos dentro de TDF' });
  const { advanceAuthSessionEpoch } = await import('../../session/authEvents');
  advanceAuthSessionEpoch();
  fireEvent.click(checkbox);
  await waitFor(() => expect(save.mock.calls.length > 0 || Boolean(screen.queryByText(/No se pudo guardar o consultar/))).toBe(true));
  expect(save).not.toHaveBeenCalled();
  close();
});

it('does not replace the new account preference with a late response from the previous account', async () => {
  let finish!: (value: { inApp: boolean }) => void;
  const pending = new Promise<{ inApp: boolean }>((resolve) => { finish = resolve; });
  save.mockReturnValueOnce(pending);
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  const tree = () => <QueryClientProvider client={client}><MemoryRouter><Controls eventId="141" organizer={false} /></MemoryRouter></QueryClientProvider>;
  const view = render(tree());
  fireEvent.click(await screen.findByRole('checkbox', { name: /Recibir invitaciones/ }));
  await waitFor(() => expect(save).toHaveBeenCalledWith(true));
  const { advanceAuthSessionEpoch } = await import('../../session/authEvents');
  advanceAuthSessionEpoch(); activeParty = 10; view.rerender(tree());
  await screen.findByRole('checkbox', { name: /Recibir invitaciones/ });
  await act(async () => { finish({ inApp: true }); await pending; });
  await waitFor(() => expect(screen.getByRole('checkbox', { name: /Recibir invitaciones/ })).toHaveProperty('disabled', false));
  expect(screen.getByRole('checkbox', { name: /Recibir invitaciones/ })).toHaveProperty('checked', false);
  expect(save).toHaveBeenCalledTimes(1);
  view.unmount(); client.clear();
});

import { jest } from '@jest/globals';
import { act, fireEvent, render, screen, waitFor } from '@testing-library/react';
import '@testing-library/jest-dom';
const preview = jest.fn<() => Promise<unknown>>(); const accept = jest.fn<() => Promise<unknown>>();
const request = jest.fn<() => Promise<unknown>>(); const confirm = jest.fn<() => Promise<unknown>>();
let session: { partyId: number; displayName: string } | null = { partyId: 2, displayName: 'Cuenta sintética' };
jest.unstable_mockModule('../api/eventCrm', () => ({ previewTeamInvitation: preview, acceptTeamInvitation: accept, requestEmailProof: request, confirmEmailProof: confirm }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session }) }));
jest.unstable_mockModule('./LoginPage', () => ({ default: () => <p>Registro e inicio de sesión existentes</p> }));
const { default: Page } = await import('./EventTeamInvitationPage');
beforeEach(() => { session = { partyId: 2, displayName: 'Cuenta sintética' }; preview.mockReset(); accept.mockReset(); request.mockReset(); confirm.mockReset(); window.history.replaceState({}, '', '/event-team/accept#' + 'a'.repeat(43)); });
it('requires authentication before reviewing or consuming the invitation', () => {
  session = null; render(<Page />); expect(screen.getByText('Registro e inicio de sesión existentes')).toBeInTheDocument();
  expect(preview).not.toHaveBeenCalled(); expect(accept).not.toHaveBeenCalled();
});
it('shows event role and permissions before offering acceptance, then consumes once', async () => {
  preview.mockResolvedValue({ eventId: 10, eventTitle: 'Ensayo', venue: 'Domo', startTime: '2026-12-05T16:00:00Z', role: 'coordinator', expiresAt: '2026-11-14T16:00:00Z', permissions: ['crm.all', 'logistics'] });
  accept.mockResolvedValue({ eventId: 10, membershipId: 'synthetic', role: 'coordinator' });
  render(<Page />); expect(screen.queryByText('Aceptar este rol y sus permisos')).not.toBeInTheDocument();
  fireEvent.click(screen.getByText('Revisar evento, rol y permisos'));
  await screen.findByText(/Consultar y gestionar el embudo completo de este evento/); expect(accept).not.toHaveBeenCalled();
  fireEvent.click(screen.getByText('Aceptar este rol y sus permisos'));
  await screen.findByText('Invitación aceptada. El enlace ya no puede volver a usarse.');
  expect(accept).toHaveBeenCalledTimes(1); expect(window.location.hash).toBe('');
  expect(screen.queryByText('Aceptar este rol y sus permisos')).not.toBeInTheDocument();
});
it('shows a neutral failure for a wrong, expired or revoked link without consumption', async () => {
  preview.mockRejectedValue(new Error('recipient_verification_required')); render(<Page />);
  fireEvent.click(screen.getByText('Revisar evento, rol y permisos'));
  await screen.findByText(/Comprueba la cuenta/); expect(accept).not.toHaveBeenCalled();
});
it('does not retain a preview when the authenticated account changes', async () => {
  preview.mockResolvedValue({ eventId: 10, eventTitle: 'Ensayo reservado', venue: 'Domo', startTime: '2026-12-05T16:00:00Z', role: 'sales', expiresAt: '2026-11-14T16:00:00Z', permissions: ['crm.assigned'] });
  const ui = render(<Page />); fireEvent.click(screen.getByText('Revisar evento, rol y permisos')); await screen.findByText('Ensayo reservado');
  session = { partyId: 3, displayName: 'Otra cuenta' }; ui.rerender(<Page />);
  await waitFor(() => expect(screen.queryByText('Ensayo reservado')).not.toBeInTheDocument());
});
it('discards a previous account preview that arrives after the account changes', async () => {
  let finish!: (value: unknown) => void;
  preview.mockImplementation(() => new Promise(resolve => { finish = resolve; }));
  const ui = render(<Page />);
  fireEvent.click(screen.getByText('Revisar evento, rol y permisos'));
  session = { partyId: 3, displayName: 'Otra cuenta' }; ui.rerender(<Page />);
  await act(async () => finish({ eventId: 10, eventTitle: 'Evento de la cuenta anterior', venue: 'Domo', startTime: '2026-12-05T16:00:00Z', role: 'sales', expiresAt: '2026-11-14T16:00:00Z', permissions: ['crm.assigned'] }));
  expect(screen.queryByText('Evento de la cuenta anterior')).not.toBeInTheDocument();
  expect(screen.queryByText('Aceptar este rol y sus permisos')).not.toBeInTheDocument();
  expect(screen.getByText('Revisar evento, rol y permisos')).toBeEnabled();
  expect(accept).not.toHaveBeenCalled();
});
it('does not apply a previous account acceptance response to the new session or its URL', async () => {
  preview.mockResolvedValue({ eventId: 10, eventTitle: 'Ensayo', venue: 'Domo', startTime: '2026-12-05T16:00:00Z', role: 'sales', expiresAt: '2026-11-14T16:00:00Z', permissions: ['crm.assigned'] });
  let finish!: (value: unknown) => void;
  accept.mockImplementation(() => new Promise(resolve => { finish = resolve; }));
  const ui = render(<Page />);
  fireEvent.click(screen.getByText('Revisar evento, rol y permisos'));
  await screen.findByText('Ensayo');
  fireEvent.click(screen.getByText('Aceptar este rol y sus permisos'));
  session = { partyId: 3, displayName: 'Otra cuenta' }; ui.rerender(<Page />);
  await act(async () => finish({ eventId: 10, membershipId: 'synthetic', role: 'sales' }));
  expect(screen.queryByText('Invitación aceptada. El enlace ya no puede volver a usarse.')).not.toBeInTheDocument();
  expect(screen.queryByText('Abrir CRM del evento')).not.toBeInTheDocument();
  expect(window.location.hash).toBe('#' + 'a'.repeat(43));
});

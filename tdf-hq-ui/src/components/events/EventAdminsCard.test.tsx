/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { ApiError } from '../../api/client';
import type { EventAdminInvitationIssuedDTO, EventAdminRosterDTO } from '../../api/socialEvents';

const token = 'c0ffee12'.repeat(8);
const listMock = jest.fn<(eventId: string) => Promise<EventAdminRosterDTO>>();
const createMock = jest.fn<(eventId: string, name: string) => Promise<EventAdminInvitationIssuedDTO>>();
const revokeMock = jest.fn<(eventId: string, invitationId: string) => Promise<void>>();
const removeMock = jest.fn<(eventId: string, partyId: string) => Promise<void>>();

jest.unstable_mockModule('../../api/socialEvents', () => ({
  SocialEventsAPI: {
    listEventAdmins: (eventId: string) => listMock(eventId),
    createEventAdminInvitation: (eventId: string, name: string) => createMock(eventId, name),
    revokeEventAdminInvitation: (eventId: string, id: string) => revokeMock(eventId, id),
    removeEventAdmin: (eventId: string, id: string) => removeMock(eventId, id),
  },
}));

const { default: EventAdminsCard } = await import('./EventAdminsCard');

const pending = { eaiId: 'inv-1', eaiInviteeName: 'Ana Pérez', eaiStatus: 'pending' as const, eaiCreatedAt: '2026-11-11T10:00:00Z', eaiExpiresAt: '2026-11-25T10:00:00Z' };
const ownerRoster: EventAdminRosterDTO = {
  earAccess: 'owner',
  earAdmins: [{ eamPartyId: '31', eamDisplayName: 'Luis Mora', eamGrantedAt: '2026-11-01T10:00:00Z' }],
  earInvitations: [
    pending,
    { eaiId: 'inv-0', eaiInviteeName: 'Luis Mora', eaiStatus: 'accepted', eaiCreatedAt: '2026-10-30T10:00:00Z', eaiExpiresAt: '2026-11-13T10:00:00Z', eaiAcceptedAt: '2026-11-01T10:00:00Z' },
  ],
};

const renderCard = (access: 'owner' | 'admin' = 'owner') => {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  return render(
    <QueryClientProvider client={client}>
      <EventAdminsCard eventId="121" access={access} accountScope="account:7" formatDate={(value) => value.slice(0, 10)} />
    </QueryClientProvider>,
  );
};

describe('EventAdminsCard', () => {
  const writeText = jest.fn<(text: string) => Promise<void>>();
  const confirmSpy = jest.spyOn(window, 'confirm');

  beforeEach(() => {
    listMock.mockReset().mockResolvedValue(ownerRoster);
    createMock.mockReset().mockResolvedValue({ eaisInvitation: { ...pending, eaiId: 'inv-2', eaiInviteeName: 'Carla Ruiz' }, eaisToken: token });
    revokeMock.mockReset().mockResolvedValue(undefined);
    removeMock.mockReset().mockResolvedValue(undefined);
    writeText.mockReset().mockResolvedValue(undefined);
    confirmSpy.mockClear();
    Object.defineProperty(navigator, 'clipboard', { configurable: true, value: { writeText } });
  });

  it('lists administrators and invitations with their status for the organizer', async () => {
    renderCard();
    const admins = await screen.findByRole('list', { name: 'Administradores' });
    expect(within(admins).getByText('Luis Mora')).toBeTruthy();
    expect(within(admins).getByText('Administra desde 2026-11-01')).toBeTruthy();
    const invitations = screen.getByRole('list', { name: 'Invitaciones' });
    expect(within(invitations).getByText('Pendiente')).toBeTruthy();
    expect(within(invitations).getByText('Aceptada')).toBeTruthy();
    // Only a pending invitation can be revoked.
    expect(within(invitations).getAllByRole('button')).toHaveLength(1);
    expect(listMock).toHaveBeenCalledWith('121');
  });

  it('generates a link once, shows how to handle it and copies it', async () => {
    renderCard();
    const name = await screen.findByRole<HTMLInputElement>('textbox', { name: 'Nombre de la persona invitada' });
    const generate = screen.getByRole<HTMLButtonElement>('button', { name: 'Generar enlace de invitación' });
    expect(generate.disabled).toBe(true);
    fireEvent.change(name, { target: { value: '  Carla Ruiz ' } });
    fireEvent.click(generate);
    const field = await screen.findByRole<HTMLInputElement>('textbox', { name: 'Enlace de invitación' });
    expect(createMock).toHaveBeenCalledWith('121', 'Carla Ruiz');
    expect(field.value).toBe(`${window.location.origin}/eventos/121#admin=${token}`);
    expect(new URL(field.value).search).toBe('');
    expect(field.readOnly).toBe(true);
    expect(screen.getByText(/se muestra una sola vez.*14 días.*una sola cuenta.*canal privado/)).toBeTruthy();
    expect(name.value).toBe('');
    fireEvent.click(screen.getByRole('button', { name: 'Copiar enlace' }));
    expect(await screen.findByText('Enlace copiado. Envíalo por un canal privado.')).toBeTruthy();
    expect(writeText).toHaveBeenCalledWith(field.value);
    await waitFor(() => expect(listMock.mock.calls.length).toBeGreaterThan(1));
  });

  it('explains the pending-invitation limit', async () => {
    createMock.mockRejectedValue(new ApiError('Too many pending administrator invitations; revoke one first', 409));
    renderCard();
    fireEvent.change(await screen.findByRole('textbox', { name: 'Nombre de la persona invitada' }), { target: { value: 'Carla' } });
    fireEvent.click(screen.getByRole('button', { name: 'Generar enlace de invitación' }));
    expect(await screen.findByText('Hay demasiadas invitaciones pendientes. Revoca una antes de generar otra.')).toBeTruthy();
    expect(screen.queryByRole('textbox', { name: 'Enlace de invitación' })).toBeNull();
  });

  it('removes an administrator only after the in-page confirmation', async () => {
    renderCard();
    fireEvent.click(await screen.findByRole('button', { name: 'Quitar a Luis Mora' }));
    const dialog = await screen.findByRole('dialog', { name: 'Quitar administrador' });
    expect(removeMock).not.toHaveBeenCalled();
    expect(within(dialog).getByText(/Luis Mora dejará de administrar este evento/)).toBeTruthy();
    fireEvent.click(within(dialog).getByRole('button', { name: 'Quitar administrador' }));
    await waitFor(() => expect(removeMock).toHaveBeenCalledWith('121', '31'));
    await waitFor(() => expect(screen.queryByRole('dialog')).toBeNull());
    expect(confirmSpy).not.toHaveBeenCalled();
  });

  it('cancelling the confirmation changes nothing', async () => {
    renderCard();
    fireEvent.click(await screen.findByRole('button', { name: 'Revocar la invitación de Ana Pérez' }));
    const dialog = await screen.findByRole('dialog', { name: 'Revocar invitación' });
    fireEvent.click(within(dialog).getByRole('button', { name: 'Cancelar' }));
    await waitFor(() => expect(screen.queryByRole('dialog')).toBeNull());
    expect(revokeMock).not.toHaveBeenCalled();
  });

  it('revokes a pending invitation and explains one that was already accepted', async () => {
    revokeMock.mockRejectedValueOnce(new ApiError('This invitation was already accepted; remove the administrator instead', 409));
    renderCard();
    fireEvent.click(await screen.findByRole('button', { name: 'Revocar la invitación de Ana Pérez' }));
    const dialog = await screen.findByRole('dialog', { name: 'Revocar invitación' });
    fireEvent.click(within(dialog).getByRole('button', { name: 'Revocar invitación' }));
    expect(await within(dialog).findByText(/La invitación ya fue aceptada/)).toBeTruthy();
    fireEvent.click(within(dialog).getByRole('button', { name: 'Revocar invitación' }));
    await waitFor(() => expect(screen.queryByRole('dialog')).toBeNull());
    expect(revokeMock).toHaveBeenCalledTimes(2);
    expect(revokeMock).toHaveBeenLastCalledWith('121', 'inv-1');
  });

  it('shows a delegated administrator the roster without invitation or removal controls', async () => {
    listMock.mockResolvedValue({ ...ownerRoster, earAccess: 'admin', earInvitations: [] });
    renderCard('admin');
    expect(await screen.findByText('Luis Mora')).toBeTruthy();
    expect(screen.getByText(/Administras este evento por invitación/)).toBeTruthy();
    expect(screen.queryByRole('button', { name: 'Generar enlace de invitación' })).toBeNull();
    expect(screen.queryByRole('button', { name: 'Quitar a Luis Mora' })).toBeNull();
    expect(screen.queryByRole('list', { name: 'Invitaciones' })).toBeNull();
  });
});

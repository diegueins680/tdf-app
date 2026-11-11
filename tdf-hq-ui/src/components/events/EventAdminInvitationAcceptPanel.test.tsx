/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { StrictMode } from 'react';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter, Route, Routes, useLocation } from 'react-router-dom';
import { ApiError } from '../../api/client';
import type { EventAdminAccessDTO, EventAdminInvitationPreviewDTO } from '../../api/socialEvents';
import { buildEventAdminInvitationLink } from '../../utils/eventAdminInvitationLink';

const token = 'A1b2c3d4'.repeat(8);
const previewMock = jest.fn<(token: string) => Promise<EventAdminInvitationPreviewDTO>>();
const acceptMock = jest.fn<(token: string) => Promise<EventAdminAccessDTO>>();
let activeSession: { partyId: number; username: string } | null = null;
let sessionLoading = false;

jest.unstable_mockModule('../../api/socialEvents', () => ({
  SocialEventsAPI: {
    previewEventAdminInvitation: (value: string) => previewMock(value),
    acceptEventAdminInvitation: (value: string) => acceptMock(value),
  },
}));

jest.unstable_mockModule('../../session/SessionContext', () => ({
  useSession: () => ({ session: activeSession, loading: sessionLoading }),
  getStoredSessionToken: () => null,
}));

jest.unstable_mockModule('../../contexts/LocalePreferencesContext', () => ({
  useLocalePreferences: () => ({ locale: 'es-EC', timezone: 'America/Guayaquil' }),
}));

const { default: EventAdminInvitationAcceptPanel, eventAdminInvitationStorageKey } = await import('./EventAdminInvitationAcceptPanel');

const previewFor = (eventId: string): EventAdminInvitationPreviewDTO => ({
  eaipEventId: eventId,
  eaipEventTitle: 'Noche sintética',
  eaipEventStart: '2026-12-05T01:00:00Z',
  eaipInviteeName: 'Ana',
  eaipExpiresAt: '2026-11-25T00:00:00Z',
});

function LocationProbe() {
  const location = useLocation();
  return <output data-testid="location">{`${location.pathname}${location.search}${location.hash}`}</output>;
}

const tree = (entry: string) => (
  <StrictMode>
    <MemoryRouter initialEntries={[entry]}>
      <Routes>
        <Route path="/eventos/:eventId" element={<><EventAdminInvitationAcceptPanel /><LocationProbe /></>} />
      </Routes>
    </MemoryRouter>
  </StrictMode>
);
const renderPanel = (entry: string) => render(tree(entry));
const currentLocation = () => screen.getByTestId('location').textContent;
const heading = () => screen.queryByRole('heading', { name: 'Invitación para administrar este evento' });

describe('EventAdminInvitationAcceptPanel', () => {
  beforeEach(() => {
    activeSession = null;
    sessionLoading = false;
    window.sessionStorage.clear();
    previewMock.mockReset().mockResolvedValue(previewFor('141'));
    acceptMock.mockReset().mockResolvedValue({ eaaEventId: '141', eaaAccess: 'admin' });
  });

  it('builds the invitation link on the public event route with the token only in the fragment', () => {
    const link = new URL(buildEventAdminInvitationLink('https://tdf.test', '141', token));
    expect(link.pathname).toBe('/eventos/141');
    expect(link.search).toBe('');
    expect(link.hash).toBe(`#admin=${token}`);
  });

  it.each([
    '/eventos/141',
    '/eventos/141#agenda',
    '/eventos/141#admin=not-a-token',
    `/eventos/141?admin=${token}`,
    `/eventos/141#${token}`,
  ])('renders nothing and calls nothing for %s', (entry) => {
    activeSession = { partyId: 9, username: 'ana' };
    renderPanel(entry);
    expect(heading()).toBeNull();
    expect(previewMock).not.toHaveBeenCalled();
    expect(window.sessionStorage.length).toBe(0);
  });

  it('ignores a pending token stored for another event', () => {
    activeSession = { partyId: 9, username: 'ana' };
    window.sessionStorage.setItem(eventAdminInvitationStorageKey('999'), token);
    renderPanel('/eventos/141');
    expect(heading()).toBeNull();
    expect(previewMock).not.toHaveBeenCalled();
  });

  it('asks a signed-out visitor to sign in or create an account and keeps the token out of those links', async () => {
    renderPanel(`/eventos/141#admin=${token}`);
    expect(await screen.findByText(/Te invitaron a administrar este evento en TDF Records/)).toBeTruthy();
    expect(screen.getByRole('link', { name: 'Crear una cuenta' }).getAttribute('href')).toBe('/login?signup=1&redirect=%2Feventos%2F141');
    expect(screen.getByRole('link', { name: 'Ya tengo cuenta: iniciar sesión' }).getAttribute('href')).toBe('/login?redirect=%2Feventos%2F141');
    expect(window.sessionStorage.getItem(eventAdminInvitationStorageKey('141'))).toBe(token);
    expect(previewMock).not.toHaveBeenCalled();
    expect(acceptMock).not.toHaveBeenCalled();
  });

  it('waits for the session bootstrap before deciding', () => {
    sessionLoading = true;
    renderPanel(`/eventos/141#admin=${token}`);
    expect(screen.getByText('Comprobando tu sesión…')).toBeTruthy();
    expect(screen.queryByRole('link', { name: 'Crear una cuenta' })).toBeNull();
    expect(previewMock).not.toHaveBeenCalled();
  });

  it('previews and accepts exactly once, links to the event workspace and clears the fragment', async () => {
    activeSession = { partyId: 9, username: 'ana' };
    renderPanel(`/eventos/141?utm_source=whatsapp#admin=${token}`);
    expect(await screen.findByText('Listo: ya eres administrador de este evento.')).toBeTruthy();
    expect(screen.getByText('Noche sintética')).toBeTruthy();
    expect(screen.getByRole('link', { name: 'Administrar el evento' }).getAttribute('href')).toBe('/social/eventos/141');
    // StrictMode re-runs effects; the invitation must still be redeemed once.
    expect(previewMock).toHaveBeenCalledTimes(1);
    expect(acceptMock).toHaveBeenCalledTimes(1);
    expect(acceptMock).toHaveBeenCalledWith(token);
    expect(window.sessionStorage.getItem(eventAdminInvitationStorageKey('141'))).toBeNull();
    // Router navigation is a transition and may land after the status text.
    await waitFor(() => expect(currentLocation()).toBe('/eventos/141?utm_source=whatsapp'));
  });

  it('accepts automatically when sign-in returns to the bare event path with the token in tab storage', async () => {
    window.sessionStorage.setItem(eventAdminInvitationStorageKey('141'), token);
    activeSession = { partyId: 9, username: 'ana' };
    renderPanel('/eventos/141');
    expect(await screen.findByText('Listo: ya eres administrador de este evento.')).toBeTruthy();
    expect(acceptMock).toHaveBeenCalledTimes(1);
    expect(acceptMock).toHaveBeenCalledWith(token);
    expect(window.sessionStorage.getItem(eventAdminInvitationStorageKey('141'))).toBeNull();
  });

  it('accepts without another click once the visitor signs in on the same page', async () => {
    const view = renderPanel(`/eventos/141#admin=${token}`);
    expect(await screen.findByRole('link', { name: 'Crear una cuenta' })).toBeTruthy();
    activeSession = { partyId: 9, username: 'ana' };
    view.rerender(tree(`/eventos/141#admin=${token}`));
    expect(await screen.findByText('Listo: ya eres administrador de este evento.')).toBeTruthy();
    expect(acceptMock).toHaveBeenCalledTimes(1);
  });

  it('shows the generic unusable message for a 404, forgets the token and clears the fragment', async () => {
    activeSession = { partyId: 9, username: 'ana' };
    previewMock.mockRejectedValue(new ApiError('This invitation link is invalid, expired, revoked or already used', 404));
    renderPanel(`/eventos/141#admin=${token}`);
    expect(await screen.findByText(/El enlace es inválido, venció, fue revocado o ya fue usado/)).toBeTruthy();
    expect(acceptMock).not.toHaveBeenCalled();
    expect(window.sessionStorage.getItem(eventAdminInvitationStorageKey('141'))).toBeNull();
    // Router navigation is a transition and may land after the status text.
    await waitFor(() => expect(currentLocation()).toBe('/eventos/141'));
  });

  it('tells the organizer that the link is meant for the invited person', async () => {
    activeSession = { partyId: 7, username: 'organizer' };
    acceptMock.mockRejectedValue(new ApiError('The organizer already manages this event', 409));
    renderPanel(`/eventos/141#admin=${token}`);
    expect(await screen.findByText(/Tú organizas este evento/)).toBeTruthy();
    expect(screen.getByRole('link', { name: 'Administrar el evento' }).getAttribute('href')).toBe('/social/eventos/141');
    expect(window.sessionStorage.getItem(eventAdminInvitationStorageKey('141'))).toBeNull();
    // Router navigation is a transition and may land after the status text.
    await waitFor(() => expect(currentLocation()).toBe('/eventos/141'));
  });

  it('moves to the invited event instead of accepting on a different event page', async () => {
    activeSession = { partyId: 9, username: 'ana' };
    previewMock.mockResolvedValue(previewFor('202'));
    acceptMock.mockResolvedValue({ eaaEventId: '202', eaaAccess: 'admin' });
    renderPanel(`/eventos/141#admin=${token}`);
    // Nothing is accepted until the page shown is the invited event's page.
    await waitFor(() => expect(currentLocation()).toMatch(/^\/eventos\/202/));
    expect(await screen.findByText('Listo: ya eres administrador de este evento.')).toBeTruthy();
    expect(previewMock).toHaveBeenCalledTimes(2);
    expect(acceptMock).toHaveBeenCalledTimes(1);
    expect(screen.getByRole('link', { name: 'Administrar el evento' }).getAttribute('href')).toBe('/social/eventos/202');
    expect(window.sessionStorage.getItem(eventAdminInvitationStorageKey('141'))).toBeNull();
    expect(window.sessionStorage.getItem(eventAdminInvitationStorageKey('202'))).toBeNull();
    // Router navigation is a transition and may land after the status text.
    await waitFor(() => expect(currentLocation()).toBe('/eventos/202'));
  });

  it('keeps the token and offers a retry after a transient failure', async () => {
    activeSession = { partyId: 9, username: 'ana' };
    acceptMock.mockRejectedValueOnce(new Error('Failed to fetch'));
    renderPanel(`/eventos/141#admin=${token}`);
    const retry = await screen.findByRole('button', { name: 'Reintentar' });
    expect(window.sessionStorage.getItem(eventAdminInvitationStorageKey('141'))).toBe(token);
    expect(currentLocation()).toBe(`/eventos/141#admin=${token}`);
    fireEvent.click(retry);
    await waitFor(() => expect(screen.getByText('Listo: ya eres administrador de este evento.')).toBeTruthy());
    expect(acceptMock).toHaveBeenCalledTimes(2);
  });
});

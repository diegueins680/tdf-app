import { jest } from '@jest/globals';
jest.unstable_mockModule('../features/interactions/InteractionPanel', () => ({ InteractionPanel: () => null }));
jest.unstable_mockModule('../components/events/EventMemoryInvitations', () => ({ default: () => null }));
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import type { EventAdminAccessDTO, EventAdminRosterDTO, SocialEventDTO, SocialEventMomentDTO, SocialTicketTierDTO } from '../api/socialEvents';

const preferenceMock = jest.fn(async () => ({ inApp: false, supportsExtendedMoments: true }));
const uploadMomentMediaMock = jest.fn<(eventId: string, file: File) => Promise<{ eiuPublicUrl: string }>>();
const createMomentMock = jest.fn<(eventId: string, body: unknown) => Promise<SocialEventMomentDTO>>();
let activeSession: { partyId: number; modules?: string[] } | null = null;
jest.unstable_mockModule('../api/eventMemoryInvitations', () => ({ EventMemoryInvitations: { preferences: preferenceMock } }));
const getEventMock = jest.fn<(eventId: string) => Promise<SocialEventDTO>>();
const listMomentsMock = jest.fn<(eventId: string) => Promise<SocialEventMomentDTO[]>>();
const listTicketTiersMock = jest.fn<(eventId: string) => Promise<SocialTicketTierDTO[]>>();
const getEventAdminAccessMock = jest.fn<(eventId: string) => Promise<EventAdminAccessDTO>>();
const listEventAdminsMock = jest.fn<(eventId: string) => Promise<EventAdminRosterDTO>>();
const getStorefrontMock = jest.fn<(eventId: number) => Promise<{ checkoutAvailable: boolean }>>();

jest.unstable_mockModule('../api/socialEvents', () => ({
  SocialEventsAPI: {
    getEvent: (eventId: string) => getEventMock(eventId),
    listMoments: (eventId: string) => listMomentsMock(eventId),
    listTicketTiers: (eventId: string) => listTicketTiersMock(eventId),
    createMoment: createMomentMock,
    uploadMomentImage: jest.fn(),
    uploadMomentMedia: uploadMomentMediaMock,
    createTicketTier: jest.fn(),
    getEventAdminAccess: (eventId: string) => getEventAdminAccessMock(eventId),
    listEventAdmins: (eventId: string) => listEventAdminsMock(eventId),
    listArtists: jest.fn(async () => []),
  },
}));

jest.unstable_mockModule('../api/catalogs', () => ({
  Catalogs: { getItem: jest.fn() },
}));

jest.unstable_mockModule('../api/eventTickets', () => ({
  EventTickets: {
    getStorefront: (eventId: number) => getStorefrontMock(eventId),
  },
}));

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: activeSession }),
  getStoredSessionToken: () => null,
}));

jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({
  useLocalePreferences: () => ({
    currency: 'USD',
    locale: 'es-EC',
    timezone: 'America/Guayaquil',
  }),
}));

jest.unstable_mockModule('../components/reviews/ExperienceReviews', () => ({
  default: () => null,
}));

const { default: SocialEventDetailPage } = await import('./SocialEventDetailPage');
const { ApiError } = await import('../api/client');

const eventFixture: SocialEventDTO = {
  eventId: '121',
  eventOrganizerPartyId: '7',
  eventTitle: 'Listening Party — Labii & Llama Este Pez',
  eventDescription: 'Una noche de música en vivo.',
  eventStart: '2026-09-10T20:00:00-05:00',
  eventPublicListable: true,
  eventTicketPurchaseEnabled: true,
  eventCurrency: 'USD',
  eventArtists: [],
};

const tierFixture: SocialTicketTierDTO = {
  ticketTierId: 'tier-general',
  ticketTierEventId: '121',
  ticketTierCode: 'GENERAL',
  ticketTierName: 'General',
  ticketTierPriceCents: 500,
  ticketTierCurrency: 'USD',
  ticketTierQuantityTotal: 100,
  ticketTierQuantitySold: 0,
  ticketTierActive: true,
};

const renderPage = (entry = '/social/eventos/121') => {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  const view = render(
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={[entry]}>
        <Routes>
          <Route path="/social/eventos/:eventId" element={<SocialEventDetailPage />} />
        </Routes>
      </MemoryRouter>
    </QueryClientProvider>,
  );
  return {
    ...view,
    queryClient,
    unmount: () => {
      view.unmount();
      queryClient.clear();
    },
  };
};

describe('SocialEventDetailPage ticket sharing', () => {
  beforeEach(() => {
    activeSession = null;
    preferenceMock.mockClear();
    createMomentMock.mockReset();
    uploadMomentMediaMock.mockReset();
    getEventMock.mockReset().mockResolvedValue(eventFixture);
    listMomentsMock.mockReset().mockResolvedValue([]);
    listTicketTiersMock.mockReset().mockResolvedValue([tierFixture]);
    getStorefrontMock.mockReset().mockResolvedValue({ checkoutAvailable: true });
    getEventAdminAccessMock.mockReset().mockResolvedValue({ eaaEventId: '121', eaaAccess: 'none' });
    listEventAdminsMock.mockReset().mockResolvedValue({ earAccess: 'admin', earAdmins: [], earInvitations: [] });
    Object.defineProperty(navigator, 'share', { configurable: true, value: undefined });
    Object.defineProperty(navigator, 'clipboard', { configurable: true, value: undefined });
  });

  it.each([403, 404])('keeps a planning event visible when memories return HTTP %i', async (status) => {
    activeSession = { partyId: 9, modules: ['Scheduling'] };
    getEventMock.mockResolvedValue({
      ...eventFixture,
      eventWorkflowStateCode: 'planning',
      eventPublicListable: false,
    });
    listMomentsMock.mockRejectedValue(new ApiError('Event not found', status));
    const view = renderPage();

    await screen.findByText('Los recuerdos de este evento no están disponibles para tu cuenta.');
    expect(screen.getByRole('heading', { name: eventFixture.eventTitle })).toBeTruthy();
    expect(screen.getByRole('link', { name: 'Logística' }).getAttribute('href')).toBe('/social/eventos/121/logistica');
    expect(screen.queryByText('Event not found')).toBeNull();
    expect(screen.queryByText('No se encontró el evento.')).toBeNull();
    expect(screen.queryByRole('button', { name: 'Publicar' })).toBeNull();
    expect(listMomentsMock).toHaveBeenCalledTimes(1);
    view.unmount();
  });

  it('offers planning to the event organizer without any module', async () => {
    activeSession = { partyId: 7 };
    getEventMock.mockResolvedValue({ ...eventFixture, eventWorkflowStateCode: 'planning', eventPublicListable: false });
    const view = renderPage();

    await screen.findByRole('heading', { name: eventFixture.eventTitle });
    expect(screen.getByRole('link', { name: 'Logística' }).getAttribute('href')).toBe('/social/eventos/121/logistica');
    view.unmount();
  });

  it('keeps memories available when the API allows access to a planning event', async () => {
    activeSession = { partyId: 9 };
    getEventMock.mockResolvedValue({
      ...eventFixture,
      eventWorkflowStateCode: 'planning',
      eventPublicListable: false,
    });
    const view = renderPage();

    expect(await screen.findByRole('button', { name: 'Publicar' })).toBeTruthy();
    expect(screen.getByText('Todavía no hay publicaciones')).toBeTruthy();
    // A signed-in visitor who neither manages the event nor has scheduling access is not offered planning.
    expect(screen.queryByRole('link', { name: 'Logística' })).toBeNull();
    expect(listMomentsMock).toHaveBeenCalledWith('121');
    view.unmount();
  });

  it('lets the viewer retry a temporary memories failure without reloading the event', async () => {
    listMomentsMock.mockRejectedValueOnce(new ApiError('Service unavailable', 503));
    const view = renderPage();

    await screen.findByText('No se pudieron cargar los recuerdos del evento.');
    fireEvent.click(screen.getByRole('button', { name: 'Reintentar' }));
    await screen.findByText('Todavía no hay publicaciones');
    expect(getEventMock).toHaveBeenCalledTimes(1);
    expect(listMomentsMock).toHaveBeenCalledTimes(2);
    view.unmount();
  });

  it('shows the event-level error when the event request itself returns 404', async () => {
    getEventMock.mockRejectedValue(new ApiError('Event not found', 404));
    const view = renderPage();

    await screen.findByText('No se encontró el evento.');
    expect(listMomentsMock).not.toHaveBeenCalled();
    expect(screen.queryByText('Recuerdos de la comunidad')).toBeNull();
    view.unmount();
  });

  it('publishes a text memory with its server-supported type and keeps failed text in the editor', async () => {
    activeSession = { partyId: 9 };
    createMomentMock.mockRejectedValueOnce(new Error('Connection failed'));
    const view = renderPage('/social/eventos/121?compose=1');
    const type = await screen.findByRole('combobox', { name: 'Tipo de recuerdo' });
    fireEvent.mouseDown(type);
    fireEvent.click(await screen.findByRole('option', { name: 'Solo texto' }));
    const caption = screen.getByRole('textbox', { name: 'Describe tu publicación' });
    fireEvent.change(caption, { target: { value: 'Un recuerdo sin archivo' } });
    fireEvent.click(screen.getByRole('button', { name: 'Publicar' }));
    await screen.findByText('Connection failed');
    expect(createMomentMock).toHaveBeenCalledWith('121', { emCreateCaption: 'Un recuerdo sin archivo', emCreateMediaUrl: '', emCreateMediaType: 'text' });
    expect((caption as HTMLInputElement).value).toBe('Un recuerdo sin archivo');
    expect(caption.getAttribute('maxlength')).toBe('280');
    view.unmount();
  });

  it('retains the exact event and composer intent through registration or sign-in', async () => {
    const view = renderPage('/social/eventos/121?compose=1#momentos');
    const link = await screen.findByRole('link', { name: 'Iniciar sesión o crear una cuenta' });
    expect(new URL(link.getAttribute('href')!, 'https://example.test').searchParams.get('redirect')).toBe('/social/eventos/121?compose=1#momentos');
    view.unmount();
  });

  it('keeps the chosen input focused when the current account snapshot refreshes after returning to compose', async () => {
    activeSession = { partyId: 9 };
    const entry = '/social/eventos/121?compose=1#momentos';
    const view = renderPage(entry);
    const caption = await screen.findByRole('textbox', { name: 'Describe tu publicación' });
    await waitFor(() => expect((caption as HTMLTextAreaElement).disabled).toBe(false));
    const media = screen.getByRole('textbox', { name: 'Enlace de imagen o video' });
    act(() => { media.focus(); });
    expect(document.activeElement).toBe(media);
    activeSession = { partyId: 9 };
    view.rerender(<QueryClientProvider client={view.queryClient}><MemoryRouter initialEntries={[entry]}>
      <Routes><Route path="/social/eventos/:eventId" element={<SocialEventDetailPage />} /></Routes>
    </MemoryRouter></QueryClientProvider>);
    expect(document.activeElement).toBe(media);
    view.unmount();
  });

  it('keeps a user-selected input and draft when the same event refreshes after returning to compose', async () => {
    activeSession = { partyId: 9 };
    const view = renderPage('/social/eventos/121?compose=1#momentos');
    const caption = await screen.findByRole('textbox', { name: 'Describe tu publicación' });
    await waitFor(() => expect((caption as HTMLTextAreaElement).disabled).toBe(false));
    fireEvent.change(caption, { target: { value: 'Borrador que sigo preparando' } });
    const media = screen.getByRole('textbox', { name: 'Enlace de imagen o video' });
    act(() => { media.focus(); });
    getEventMock.mockResolvedValue({ ...eventFixture, eventDescription: 'Información actualizada del mismo evento' });
    await act(async () => { await view.queryClient.refetchQueries({ type: 'active' }); });
    expect((caption as HTMLTextAreaElement).value).toBe('Borrador que sigo preparando');
    expect(document.activeElement).toBe(media);
    view.unmount();
  });

  it('focuses the requested composer when its owned draft first becomes ready', async () => {
    activeSession = { partyId: 9 };
    const view = renderPage('/social/eventos/121?compose=1#momentos');
    const caption = await screen.findByRole('textbox', { name: 'Describe tu publicación' });
    await waitFor(() => expect((caption as HTMLTextAreaElement).disabled).toBe(false));
    await waitFor(() => expect(document.activeElement).toBe(caption));
    view.unmount();
  });

  it('links artists by the existing canonical Party identity without treating a social artist ID as a Party', async () => {
    getEventMock.mockResolvedValue({ ...eventFixture, eventArtists: [
      { artistId: '999', artistPartyId: '55', artistName: 'Synthetic Artist' },
      { artistId: '1000', artistName: 'Unlinked Artist' },
    ] });
    const view = renderPage();
    expect((await screen.findByRole('link', { name: 'Synthetic Artist' })).getAttribute('href')).toBe('/a/55');
    expect(screen.queryByRole('link', { name: 'Unlinked Artist' })).toBeNull();
    view.unmount();
  });

  it('does not dispatch a draft when credentials change before the session render', async () => {
    activeSession = { partyId: 9 };
    createMomentMock.mockResolvedValue({ emId: '999', emAuthorName: 'Synthetic B', emCaption: 'Private draft for A', emMediaType: 'text', emMediaUrl: '' });
    const view = renderPage();
    const kind = await screen.findByRole('combobox', { name: 'Tipo de recuerdo' });
    fireEvent.mouseDown(kind);
    fireEvent.click(await screen.findByRole('option', { name: 'Solo texto' }));
    fireEvent.change(screen.getByRole('textbox', { name: 'Describe tu publicación' }), { target: { value: 'Private draft for A' } });
    const { advanceAuthSessionEpoch } = await import('../session/authEvents');
    advanceAuthSessionEpoch();
    activeSession = { partyId: 10 };
    // Credentials change synchronously; the old button can still exist until
    // React commits the session update. No publication may cross that boundary.
    fireEvent.click(screen.getByRole('button', { name: 'Publicar', exact: true }));
    await waitFor(() => expect(Boolean(screen.queryByText('Tu sesión cambió. Vuelve al evento antes de publicar.')) || createMomentMock.mock.calls.length > 0).toBe(true));
    expect(createMomentMock).not.toHaveBeenCalled();
    await screen.findByText('Tu sesión cambió. Vuelve al evento antes de publicar.');
    expect(uploadMomentMediaMock).not.toHaveBeenCalled();
    view.unmount();
  });

  it('does not upload the previous account file when credentials change before render', async () => {
    activeSession = { partyId: 9 };
    const view = renderPage();
    await screen.findByRole('combobox', { name: 'Tipo de recuerdo' });
    fireEvent.change(view.container.querySelector('input[type="file"]')!, { target: { files: [new File(['synthetic'], 'private-A.png', { type: 'image/png' })] } });
    const { advanceAuthSessionEpoch } = await import('../session/authEvents');
    advanceAuthSessionEpoch(); activeSession = { partyId: 10 };
    fireEvent.click(screen.getByRole('button', { name: 'Publicar', exact: true }));
    await waitFor(() => expect(Boolean(screen.queryByText('Tu sesión cambió. Vuelve al evento antes de publicar.')) || uploadMomentMediaMock.mock.calls.length > 0).toBe(true));
    expect(uploadMomentMediaMock).not.toHaveBeenCalled();
    expect(createMomentMock).not.toHaveBeenCalled();
    view.unmount();
  });

  it('retains the selected file after a malformed upload reply and permits a confirmed retry', async () => {
    activeSession = { partyId: 9 };
    uploadMomentMediaMock.mockResolvedValueOnce({} as { eiuPublicUrl: string }).mockResolvedValueOnce({ eiuPublicUrl: 'https://assets.persona.test/confirmed.png' });
    createMomentMock.mockResolvedValue({ emId: '993', emAuthorName: 'Synthetic', emMediaUrl: 'https://assets.persona.test/confirmed.png', emMediaType: 'image' });
    const view = renderPage();
    await screen.findByRole('combobox', { name: 'Tipo de recuerdo' });
    fireEvent.change(view.container.querySelector('input[type="file"]')!, { target: { files: [new File(['synthetic'], 'retry.png', { type: 'image/png' })] } });
    fireEvent.click(screen.getByRole('button', { name: 'Publicar', exact: true }));
    await screen.findByText('No se pudo confirmar la carga. Conservamos el archivo para reintentar.');
    expect(createMomentMock).not.toHaveBeenCalled();
    expect(screen.getByText('retry.png')).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Publicar', exact: true }));
    await waitFor(() => expect(createMomentMock).toHaveBeenCalledTimes(1));
    expect(uploadMomentMediaMock).toHaveBeenCalledTimes(2);
    view.unmount();
  });

  it('does not publish an uploaded draft after the authentication epoch changes', async () => {
    activeSession = { partyId: 9 };
    let finishUpload!: (value: { eiuPublicUrl: string }) => void;
    uploadMomentMediaMock.mockImplementation(() => new Promise((resolve) => { finishUpload = resolve; }));
    const view = renderPage();
    await screen.findByRole('combobox', { name: 'Tipo de recuerdo' });
    fireEvent.change(view.container.querySelector('input[type="file"]')!, { target: { files: [new File(['synthetic'], 'memory.png', { type: 'image/png' })] } });
    fireEvent.click(screen.getByRole('button', { name: 'Publicar', exact: true }));
    await waitFor(() => expect(uploadMomentMediaMock).toHaveBeenCalledTimes(1));
    const { advanceAuthSessionEpoch } = await import('../session/authEvents');
    advanceAuthSessionEpoch();
    finishUpload({ eiuPublicUrl: 'https://assets.persona.test/previous-account.png' });
    await screen.findByText('Tu sesión cambió. Vuelve al evento antes de publicar.');
    expect(createMomentMock).not.toHaveBeenCalled();
    view.unmount();
  });

  it('removes the previous account memories while the new account feed is pending', async () => {
    activeSession = { partyId: 9 };
    listMomentsMock.mockResolvedValueOnce([
      { emId: '987', emAuthorName: 'Ana', emCaption: 'Recuerdo privado de la cuenta anterior', emMediaUrl: '', emMediaType: 'text' },
    ]);
    const view = renderPage();
    await screen.findByText('Recuerdo privado de la cuenta anterior');
    let finishFeed!: (moments: SocialEventMomentDTO[]) => void;
    listMomentsMock.mockImplementation(() => new Promise((resolve) => { finishFeed = resolve; }));
    activeSession = { partyId: 10 };
    view.rerender(
      <QueryClientProvider client={view.queryClient}>
        <MemoryRouter initialEntries={['/social/eventos/121']}>
          <Routes><Route path="/social/eventos/:eventId" element={<SocialEventDetailPage />} /></Routes>
        </MemoryRouter>
      </QueryClientProvider>,
    );
    await waitFor(() => expect(listMomentsMock).toHaveBeenCalledTimes(2));
    expect(screen.queryByText('Recuerdo privado de la cuenta anterior')).toBeNull();
    finishFeed([]);
    await screen.findByText('Todavía no hay publicaciones');
    expect(screen.queryByText('Recuerdo privado de la cuenta anterior')).toBeNull();
    view.unmount();
  });

  it('preserves the new account draft when an earlier publication finishes late', async () => {
    activeSession = { partyId: 9 };
    let finish!: (value: SocialEventMomentDTO) => void;
    const pending = new Promise<SocialEventMomentDTO>((resolve) => { finish = resolve; });
    createMomentMock.mockReturnValueOnce(pending);
    const view = renderPage();
    fireEvent.mouseDown(await screen.findByRole('combobox', { name: 'Tipo de recuerdo' }));
    fireEvent.click(await screen.findByRole('option', { name: 'Solo texto' }));
    fireEvent.change(screen.getByRole('textbox', { name: 'Describe tu publicación' }), { target: { value: 'Synthetic account A draft' } });
    fireEvent.click(screen.getByRole('button', { name: 'Publicar', exact: true }));
    await waitFor(() => expect(createMomentMock).toHaveBeenCalledTimes(1));
    const { advanceAuthSessionEpoch } = await import('../session/authEvents');
    advanceAuthSessionEpoch(); activeSession = { partyId: 10 };
    view.rerender(<QueryClientProvider client={view.queryClient}><MemoryRouter initialEntries={['/social/eventos/121']}>
      <Routes><Route path="/social/eventos/:eventId" element={<SocialEventDetailPage />} /></Routes>
    </MemoryRouter></QueryClientProvider>);
    const caption = await screen.findByRole('textbox', { name: 'Describe tu publicación' });
    await waitFor(() => expect(caption).toHaveProperty('disabled', false));
    expect(caption).toHaveProperty('value', '');
    fireEvent.change(caption, { target: { value: 'Synthetic account B draft' } });
    await act(async () => { finish({ emId: '991', emAuthorName: 'Synthetic A', emMediaType: 'text', emMediaUrl: '', emCaption: 'Synthetic account A draft' }); await pending; });
    await waitFor(() => expect(view.queryClient.isMutating()).toBe(0));
    expect(screen.getByRole('textbox', { name: 'Describe tu publicación' })).toHaveProperty('value', 'Synthetic account B draft');
    expect(createMomentMock).toHaveBeenCalledTimes(1);
    view.unmount();
  });

  it('loads, displays and focuses the exact deep-linked moment after the list arrives', async () => {
    const scroll = jest.fn();
    Object.defineProperty(HTMLElement.prototype, 'scrollIntoView', { configurable: true, value: scroll });
    let resolveEvent!: (event: SocialEventDTO) => void;
    getEventMock.mockImplementation(() => new Promise((resolve) => { resolveEvent = resolve; }));
    listMomentsMock.mockResolvedValue([
      { emId: '986', emAuthorName: 'Other', emMediaUrl: 'https://example.test/other.jpg', emMediaType: 'image' },
      { emId: '987', emAuthorName: 'Ana', emCaption: 'Exact moment', emMediaUrl: 'https://example.test/moment.jpg', emMediaType: 'image' },
    ]);
    const view = renderPage('/social/eventos/121?moment=987');
    await waitFor(() => expect(getEventMock).toHaveBeenCalledWith('121'));
    expect(listMomentsMock).not.toHaveBeenCalled();
    resolveEvent(eventFixture);
    const region = await screen.findByRole('region', { name: 'Publicación de Ana' });
    await waitFor(() => expect(document.activeElement).toBe(region));
    expect(screen.getByAltText('Exact moment').getAttribute('src')).toBe('https://example.test/moment.jpg');
    expect(scroll).toHaveBeenCalledTimes(1);
    expect(listMomentsMock).toHaveBeenCalledWith('121');
    view.unmount();
  });

  it('explains unavailable moment links without focusing unrelated media', async () => {
    const view = renderPage('/social/eventos/121?moment=987');
    expect(await screen.findByText('Esta publicación ya no está disponible o no tienes acceso.')).toBeTruthy();
    view.unmount();
  });

  it('gives a delegated administrator the management tools but not the organizer-only invitation controls', async () => {
    activeSession = { partyId: 9 };
    getEventAdminAccessMock.mockResolvedValue({ eaaEventId: '121', eaaAccess: 'admin' });
    listEventAdminsMock.mockResolvedValue({
      earAccess: 'admin',
      earAdmins: [{ eamPartyId: '9', eamDisplayName: 'Ana Pérez', eamGrantedAt: '2026-11-01T15:00:00Z' }],
      earInvitations: [],
    });
    const view = renderPage();
    expect(await screen.findByRole('heading', { name: 'Administradores del evento' })).toBeTruthy();
    expect(await screen.findByText('Ana Pérez')).toBeTruthy();
    expect(screen.getByRole('heading', { name: 'Preparar evento' })).toBeTruthy();
    expect(screen.getByRole('heading', { name: 'Artistas del evento' })).toBeTruthy();
    expect(screen.getByText('Crear tipo de ticket')).toBeTruthy();
    expect(screen.getByRole('heading', { name: 'Entradas de cortesía' })).toBeTruthy();
    expect(screen.getByRole('button', { name: 'Emitir cortesía' })).toBeTruthy();
    expect(screen.queryByRole('button', { name: 'Generar enlace de invitación' })).toBeNull();
    expect(getEventAdminAccessMock).toHaveBeenCalledWith('121');
    view.unmount();
  });

  it('lets the organizer invite administrators and keeps every management tool away from other accounts', async () => {
    activeSession = { partyId: 7 };
    getEventAdminAccessMock.mockResolvedValue({ eaaEventId: '121', eaaAccess: 'owner' });
    listEventAdminsMock.mockResolvedValue({ earAccess: 'owner', earAdmins: [], earInvitations: [] });
    const organizerView = renderPage();
    expect(await screen.findByRole('button', { name: 'Generar enlace de invitación' })).toBeTruthy();
    expect(screen.getByRole('heading', { name: 'Entradas de cortesía' })).toBeTruthy();
    organizerView.unmount();

    activeSession = { partyId: 9 };
    getEventAdminAccessMock.mockResolvedValue({ eaaEventId: '121', eaaAccess: 'none' });
    const attendeeView = renderPage();
    expect(await screen.findByText('General')).toBeTruthy();
    await waitFor(() => expect(getEventAdminAccessMock).toHaveBeenCalledTimes(2));
    expect(screen.queryByRole('heading', { name: 'Administradores del evento' })).toBeNull();
    expect(screen.queryByRole('heading', { name: 'Entradas de cortesía' })).toBeNull();
    expect(screen.queryByRole('heading', { name: 'Preparar evento' })).toBeNull();
    expect(screen.queryByText('Crear tipo de ticket')).toBeNull();
    attendeeView.unmount();

    activeSession = null;
    const guestView = renderPage();
    expect(await screen.findByText('General')).toBeTruthy();
    expect(getEventAdminAccessMock).toHaveBeenCalledTimes(2);
    guestView.unmount();
  });

  it('shows the share action only after a ticket tier exists', async () => {
    listTicketTiersMock.mockResolvedValue([]);
    const view = renderPage();

    expect(await screen.findByText('Aún no hay tickets para este evento.')).toBeTruthy();
    expect(screen.queryByRole('button', { name: 'Compartir entradas' })).toBeNull();
    expect(getStorefrontMock).not.toHaveBeenCalled();

    view.unmount();
  });

  it('hides the share action when the public storefront cannot accept checkout', async () => {
    getStorefrontMock.mockResolvedValue({ checkoutAvailable: false });
    const view = renderPage();

    expect(await screen.findByText('General')).toBeTruthy();
    await waitFor(() => expect(getStorefrontMock).toHaveBeenCalledWith(121));
    expect(screen.queryByRole('button', { name: 'Compartir entradas' })).toBeNull();

    view.unmount();
  });

  it('does not load or share a storefront for a non-public event', async () => {
    getEventMock.mockResolvedValue({ ...eventFixture, eventPublicListable: false });
    const view = renderPage();

    expect(await screen.findByText('General')).toBeTruthy();
    expect(getStorefrontMock).not.toHaveBeenCalled();
    expect(screen.queryByRole('button', { name: 'Compartir entradas' })).toBeNull();

    view.unmount();
  });

  it('hides a cached share action when the event stops allowing public ticket purchases', async () => {
    const view = renderPage();

    expect(await screen.findByRole('button', { name: 'Compartir entradas' })).toBeTruthy();
    getEventMock.mockResolvedValue({ ...eventFixture, eventPublicListable: false });
    await view.queryClient.invalidateQueries({ queryKey: ['social-event', '121'] });

    await waitFor(() => {
      expect(screen.queryByRole('button', { name: 'Compartir entradas' })).toBeNull();
    });
    expect(getStorefrontMock).toHaveBeenCalledTimes(1);

    view.unmount();
  });

  it('hides a cached share action when the event loses its last ticket tier', async () => {
    const view = renderPage();

    expect(await screen.findByRole('button', { name: 'Compartir entradas' })).toBeTruthy();
    listTicketTiersMock.mockResolvedValue([]);
    await view.queryClient.invalidateQueries({
      queryKey: ['social-event-ticket-tiers', '121'],
    });

    await waitFor(() => {
      expect(screen.queryByRole('button', { name: 'Compartir entradas' })).toBeNull();
    });
    expect(getStorefrontMock).toHaveBeenCalledTimes(1);

    view.unmount();
  });

  it('shares the canonical public purchase URL through the device share sheet', async () => {
    const shareMock = jest.fn<(data?: ShareData) => Promise<void>>().mockResolvedValue(undefined);
    Object.defineProperty(navigator, 'share', { configurable: true, value: shareMock });
    const view = renderPage();

    fireEvent.click(await screen.findByRole('button', { name: 'Compartir entradas' }));

    await waitFor(() => {
      expect(shareMock).toHaveBeenCalledWith({
        title: `Entradas para ${eventFixture.eventTitle}`,
        text: `Compra tus entradas para ${eventFixture.eventTitle}.`,
        url: 'http://localhost/eventos/121/entradas',
      });
    });
    expect(await screen.findByText('Enlace de compra compartido.')).toBeTruthy();

    view.unmount();
  });

  it('copies the canonical public purchase URL when native sharing is unavailable', async () => {
    const writeTextMock = jest.fn<(text: string) => Promise<void>>().mockResolvedValue(undefined);
    Object.defineProperty(navigator, 'clipboard', {
      configurable: true,
      value: { writeText: writeTextMock },
    });
    const view = renderPage();

    fireEvent.click(await screen.findByRole('button', { name: 'Compartir entradas' }));

    await waitFor(() => {
      expect(writeTextMock).toHaveBeenCalledWith('http://localhost/eventos/121/entradas');
    });
    expect(await screen.findByText('Enlace de compra copiado al portapapeles.')).toBeTruthy();

    view.unmount();
  });
});

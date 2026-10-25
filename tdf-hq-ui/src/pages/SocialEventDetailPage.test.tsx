import { jest } from '@jest/globals';
jest.unstable_mockModule('../features/interactions/InteractionPanel', () => ({ InteractionPanel: () => null }));
jest.unstable_mockModule('../components/events/EventMemoryInvitations', () => ({ default: () => null }));
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import type { SocialEventDTO, SocialEventMomentDTO, SocialTicketTierDTO } from '../api/socialEvents';

const preferenceMock = jest.fn(async () => ({ inApp: false, supportsExtendedMoments: true }));
const createMomentMock = jest.fn<(eventId: string, body: unknown) => Promise<SocialEventMomentDTO>>();
let activeSession: { partyId: number } | null = null;
jest.unstable_mockModule('../api/eventMemoryInvitations', () => ({ EventMemoryInvitations: { preferences: preferenceMock } }));
const getEventMock = jest.fn<(eventId: string) => Promise<SocialEventDTO>>();
const listMomentsMock = jest.fn<(eventId: string) => Promise<SocialEventMomentDTO[]>>();
const listTicketTiersMock = jest.fn<(eventId: string) => Promise<SocialTicketTierDTO[]>>();
const getStorefrontMock = jest.fn<(eventId: number) => Promise<{ checkoutAvailable: boolean }>>();

jest.unstable_mockModule('../api/socialEvents', () => ({
  SocialEventsAPI: {
    getEvent: (eventId: string) => getEventMock(eventId),
    listMoments: (eventId: string) => listMomentsMock(eventId),
    listTicketTiers: (eventId: string) => listTicketTiersMock(eventId),
    createMoment: createMomentMock,
    uploadMomentImage: jest.fn(),
    createTicketTier: jest.fn(),
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
    getEventMock.mockReset().mockResolvedValue(eventFixture);
    listMomentsMock.mockReset().mockResolvedValue([]);
    listTicketTiersMock.mockReset().mockResolvedValue([tierFixture]);
    getStorefrontMock.mockReset().mockResolvedValue({ checkoutAvailable: true });
    Object.defineProperty(navigator, 'share', { configurable: true, value: undefined });
    Object.defineProperty(navigator, 'clipboard', { configurable: true, value: undefined });
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

  it('links artists by the existing canonical Party identity without treating a social artist ID as a Party', async () => {
    getEventMock.mockResolvedValue({ ...eventFixture, eventArtists: [
      { artistId: '999', artistPartyId: '55', artistName: 'Synthetic Artist' },
      { artistId: '1000', artistName: 'Unlinked Artist' },
    ] });
    const view = renderPage();
    expect((await screen.findByRole('link', { name: 'Synthetic Artist' })).getAttribute('href')).toBe('/artista/55');
    expect(screen.queryByRole('link', { name: 'Unlinked Artist' })).toBeNull();
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
    await waitFor(() => expect(listMomentsMock).toHaveBeenCalledWith('121'));
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

import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import type { SocialEventDTO, SocialTicketTierDTO } from '../api/socialEvents';

const getEventMock = jest.fn<(eventId: string) => Promise<SocialEventDTO>>();
const listMomentsMock = jest.fn<(eventId: string) => Promise<never[]>>();
const listTicketTiersMock = jest.fn<(eventId: string) => Promise<SocialTicketTierDTO[]>>();
const getStorefrontMock = jest.fn<(eventId: number) => Promise<{ checkoutAvailable: boolean }>>();

jest.unstable_mockModule('../api/socialEvents', () => ({
  SocialEventsAPI: {
    getEvent: (eventId: string) => getEventMock(eventId),
    listMoments: (eventId: string) => listMomentsMock(eventId),
    listTicketTiers: (eventId: string) => listTicketTiersMock(eventId),
    createMoment: jest.fn(),
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
  useSession: () => ({ session: null }),
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

const renderPage = () => {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  const view = render(
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={['/social/eventos/121']}>
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
    getEventMock.mockReset().mockResolvedValue(eventFixture);
    listMomentsMock.mockReset().mockResolvedValue([]);
    listTicketTiersMock.mockReset().mockResolvedValue([tierFixture]);
    getStorefrontMock.mockReset().mockResolvedValue({ checkoutAvailable: true });
    Object.defineProperty(navigator, 'share', { configurable: true, value: undefined });
    Object.defineProperty(navigator, 'clipboard', { configurable: true, value: undefined });
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

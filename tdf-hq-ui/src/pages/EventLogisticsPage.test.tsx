import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { render, screen } from '@testing-library/react';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import type { EventLogisticsPlanDTO, SocialEventDTO } from '../api/socialEvents';

const getEventMock = jest.fn<(eventId: string) => Promise<SocialEventDTO>>();
const getLogisticsPlanMock = jest.fn<(eventId: string) => Promise<EventLogisticsPlanDTO>>();

jest.unstable_mockModule('../api/socialEvents', () => ({
  SocialEventsAPI: {
    getEvent: (eventId: string) => getEventMock(eventId),
    getLogisticsPlan: (eventId: string) => getLogisticsPlanMock(eventId),
    listVenues: jest.fn(async () => []),
  },
}));

jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({
  useLocalePreferences: () => ({ currency: 'USD', locale: 'es-EC', timezone: 'America/Guayaquil' }),
}));

jest.unstable_mockModule('../components/party-selector/PartySelector', () => ({
  UserSelector: () => null,
}));

const { default: EventLogisticsPage } = await import('./EventLogisticsPage');

const eventFixture: SocialEventDTO = {
  eventId: '42',
  eventOrganizerPartyId: '7',
  eventTitle: 'Noche de prueba',
  eventStart: '2026-12-01T20:00:00-05:00',
  eventArtists: [],
};

const planFixture: EventLogisticsPlanDTO = {
  elgEventId: '42',
  elgAccessRole: 'owner',
  elgSettings: { elsTimezone: 'America/Guayaquil', elsDefaultTravelMode: 'drive' },
  elgMembers: [],
  elgPlaces: [],
  elgActivities: [],
  elgIssues: [],
};

const renderPage = () => {
  const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  return render(
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={['/social/eventos/42/logistica']}>
        <Routes>
          <Route path="/social/eventos/:eventId/logistica" element={<EventLogisticsPage />} />
        </Routes>
      </MemoryRouter>
    </QueryClientProvider>,
  );
};

describe('EventLogisticsPage access', () => {
  beforeEach(() => {
    getEventMock.mockReset();
    getLogisticsPlanMock.mockReset();
    getEventMock.mockResolvedValue(eventFixture);
  });

  it('gives the owner role returned for a delegated administrator the team and settings controls', async () => {
    getLogisticsPlanMock.mockResolvedValue(planFixture);
    const view = renderPage();

    expect(await screen.findByText('Configuración y equipo')).toBeTruthy();
    expect(getLogisticsPlanMock).toHaveBeenCalledWith('42');
    view.unmount();
  });

  it('shows the refusal of the API and no plan controls to an account outside the event', async () => {
    getLogisticsPlanMock.mockRejectedValue(new Error("You are not on this event's logistics team"));
    const view = renderPage();

    expect(await screen.findByText("You are not on this event's logistics team")).toBeTruthy();
    expect(screen.queryByText('Configuración y equipo')).toBeNull();
    expect(screen.queryByRole('button', { name: /Guardar/ })).toBeNull();
    view.unmount();
  });
});

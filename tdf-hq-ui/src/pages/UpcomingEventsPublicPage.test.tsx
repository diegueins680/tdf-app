import { jest } from '@jest/globals';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { PublicUpcomingEventDTO } from '../api/socialEvents';

const listPublicUpcomingEventsMock = jest.fn<
  (opts?: { city?: string; startAfter?: string; limit?: number; signal?: AbortSignal }) => Promise<PublicUpcomingEventDTO[]>
>();

jest.unstable_mockModule('../api/socialEvents', () => ({
  SocialEventsAPI: {
    listPublicUpcomingEvents: listPublicUpcomingEventsMock,
  },
}));

const { default: UpcomingEventsPublicPage } = await import('./UpcomingEventsPublicPage');

const renderPage = () => {
  const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  return render(
    <MemoryRouter>
      <QueryClientProvider client={queryClient}>
        <UpcomingEventsPublicPage />
      </QueryClientProvider>
    </MemoryRouter>,
  );
};

describe('UpcomingEventsPublicPage', () => {
  beforeEach(() => {
    listPublicUpcomingEventsMock.mockReset().mockResolvedValue([]);
  });

  it('includes the trimmed city in the query sent to the API', async () => {
    renderPage();

    await waitFor(() => expect(listPublicUpcomingEventsMock).toHaveBeenCalledTimes(1));
    fireEvent.change(screen.getByRole('textbox', { name: 'Filtrar próximos eventos por ciudad' }), {
      target: { value: '  Quito  ' },
    });

    await waitFor(() => expect(listPublicUpcomingEventsMock).toHaveBeenLastCalledWith(
      expect.objectContaining({ city: 'Quito', limit: 50 }),
    ));
  });

  it('renders each event with its poster or the event fallback', async () => {
    listPublicUpcomingEventsMock.mockResolvedValue([
      {
        publicUpcomingEventId: '94',
        publicUpcomingEventTitle: 'Erick Brian en Quito – Tour 2026',
        publicUpcomingEventDescription: 'Una noche de música en Quito.',
        publicUpcomingEventStart: '2026-08-27T23:00:00Z',
        publicUpcomingEventCity: 'Quito',
        publicUpcomingEventImageUrl: 'https://images.example.test/erick-brian.jpeg',
        publicUpcomingEventWorkflowStateCode: 'published',
      },
      {
        publicUpcomingEventId: '84',
        publicUpcomingEventTitle: 'Evento sin afiche',
        publicUpcomingEventStart: '2026-08-28T23:00:00Z',
        publicUpcomingEventCity: 'Quito',
        publicUpcomingEventImageUrl: null,
        publicUpcomingEventWorkflowStateCode: 'published',
      },
    ]);

    renderPage();

    const poster = await screen.findByRole('img', { name: 'Afiche de Erick Brian en Quito – Tour 2026' });
    expect(poster.getAttribute('src')).toBe('https://images.example.test/erick-brian.jpeg');
    const fallback = screen.getByRole('img', { name: 'Imagen de referencia para Evento sin afiche' });
    expect(fallback.getAttribute('src')).toBe('http://localhost/event-fallback.svg');
  });
});

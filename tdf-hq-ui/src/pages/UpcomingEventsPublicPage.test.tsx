import { jest } from '@jest/globals';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { PublicUpcomingEventDTO } from '../api/socialEvents';

const listPublicUpcomingEventsMock = jest.fn<
  (opts?: { city?: string; startAfter?: string; limit?: number }) => Promise<PublicUpcomingEventDTO[]>
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
});

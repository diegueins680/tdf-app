import { jest } from '@jest/globals';
import { act, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { PublicUpcomingEventDTO } from '../api/socialEvents';

const listPublicUpcomingEventsMock = jest.fn<
  (opts?: { city?: string; startAfter?: string; limit?: number; signal?: AbortSignal }) => Promise<PublicUpcomingEventDTO[]>
>();
const funnelCapture = jest.fn();
const analytics = { ready: true, capture: funnelCapture };
jest.unstable_mockModule('../analytics/useAnalytics', () => ({ useAnalytics: () => analytics }));

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
    sessionStorage.clear();
    funnelCapture.mockClear();
    listPublicUpcomingEventsMock.mockReset().mockResolvedValue([]);
  });

  it('counts a public impression only after half the card is visible and disconnects on unmount', async () => {
    const originalObserver = globalThis.IntersectionObserver;
    let notify: IntersectionObserverCallback | undefined;
    const observe = jest.fn();
    const disconnect = jest.fn();
    const observer = { observe, disconnect, unobserve: jest.fn(), takeRecords: () => [],
      root: null, rootMargin: '0px', thresholds: [0.5] } as IntersectionObserver;
    globalThis.IntersectionObserver = jest.fn((callback: IntersectionObserverCallback) => {
      notify = callback;
      return observer;
    }) as unknown as typeof IntersectionObserver;
    try {
      listPublicUpcomingEventsMock.mockResolvedValue([{ publicUpcomingEventId: '41',
        publicUpcomingEventTitle: 'Evento', publicUpcomingEventStart: '2030-08-20T22:00:00Z',
        publicUpcomingEventWorkflowStateCode: 'published' }]);
      const view = renderPage();
      await waitFor(() => expect(observe).toHaveBeenCalled());
      expect(funnelCapture).not.toHaveBeenCalled();
      notify?.([{ isIntersecting: true, intersectionRatio: 0.1 } as IntersectionObserverEntry], observer);
      expect(funnelCapture).not.toHaveBeenCalled();
      notify?.([{ isIntersecting: true, intersectionRatio: 0.5 } as IntersectionObserverEntry], observer);
      notify?.([{ isIntersecting: true, intersectionRatio: 1 } as IntersectionObserverEntry], observer);
      expect(funnelCapture).toHaveBeenCalledTimes(1);
      expect(funnelCapture).toHaveBeenCalledWith('ticketing_event_impression', expect.objectContaining({ event_id: 41 }));
      view.unmount();
      expect(disconnect).toHaveBeenCalled();
    } finally { globalThis.IntersectionObserver = originalObserver; }
  });

  it('includes the trimmed city in the query sent to the API', async () => {
    jest.useFakeTimers();
    const view = renderPage();
    try {
      await act(async () => { await jest.advanceTimersByTimeAsync(0); });
      expect(listPublicUpcomingEventsMock).toHaveBeenCalledTimes(1);
      fireEvent.change(screen.getByRole('textbox', { name: 'Filtrar próximos eventos por ciudad' }), {
        target: { value: '  Quito  ' },
      });
      await act(async () => { await jest.advanceTimersByTimeAsync(349); });
      expect(listPublicUpcomingEventsMock).toHaveBeenCalledTimes(1);
      await act(async () => { await jest.advanceTimersByTimeAsync(1); });
      expect(listPublicUpcomingEventsMock).toHaveBeenLastCalledWith(
        expect.objectContaining({ city: 'Quito', limit: 50 }),
      );
    } finally {
      view.unmount();
      jest.useRealTimers();
    }
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

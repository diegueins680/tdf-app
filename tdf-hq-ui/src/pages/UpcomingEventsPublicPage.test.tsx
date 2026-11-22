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

  it('retains a server-resolved city alias even when the display city uses another language', async () => {
    listPublicUpcomingEventsMock.mockImplementation(async options => options?.city === 'New York' ? [{
      publicUpcomingEventId: '88', publicUpcomingEventTitle: 'Sesión en Nueva York',
      publicUpcomingEventStart: '2099-01-01T20:00:00Z', publicUpcomingEventCity: 'Nueva York',
      publicUpcomingEventWorkflowStateCode: 'announced',
    }] : []);
    const view = renderPage();
    fireEvent.change(screen.getByRole('textbox', { name: 'Filtrar próximos eventos por ciudad' }), { target: { value: 'New York' } });
    await screen.findByText('Sesión en Nueva York');
    view.unmount();
  });

  it('shows venue with city and what the ticket tiers support, and nothing when they are silent', async () => {
    const base = { publicUpcomingEventStart: '2030-08-20T22:00:00Z', publicUpcomingEventWorkflowStateCode: 'published' };
    listPublicUpcomingEventsMock.mockResolvedValue([
      { ...base, publicUpcomingEventId: '1', publicUpcomingEventTitle: 'Con entradas', publicUpcomingEventVenueName: 'Sala Uno',
        publicUpcomingEventCity: 'Quito', publicUpcomingEventTicketFromPriceCents: 1500, publicUpcomingEventTicketCurrency: 'usd',
        publicUpcomingEventTicketsSoldOut: false },
      { ...base, publicUpcomingEventId: '2', publicUpcomingEventTitle: 'Sin cupos', publicUpcomingEventCity: 'Cuenca',
        publicUpcomingEventTicketFromPriceCents: null, publicUpcomingEventTicketsSoldOut: true },
      // A reference price alone is not a ticket price, and an old API sends no ticket fields at all.
      { ...base, publicUpcomingEventId: '3', publicUpcomingEventTitle: 'Solo referencia', publicUpcomingEventVenueName: 'Loja',
        publicUpcomingEventCity: 'loja', publicUpcomingEventPriceCents: 0, publicUpcomingEventCurrency: 'USD' },
      { ...base, publicUpcomingEventId: '4', publicUpcomingEventTitle: 'Moneda rara', publicUpcomingEventTicketFromPriceCents: 1500,
        publicUpcomingEventTicketCurrency: 'no-es-moneda', publicUpcomingEventTicketsSoldOut: false },
    ]);
    renderPage();

    const withTickets = (await screen.findByRole('link', { name: /Con entradas/ }));
    expect(withTickets.textContent).toContain('Sala Uno · Quito');
    expect(withTickets.textContent).toMatch(/Entradas desde \$\s?15,00/);

    const soldOut = screen.getByRole('link', { name: /Sin cupos/ });
    expect(soldOut.textContent).toContain('Cuenca');
    expect(soldOut.textContent).toContain('Agotado');
    expect(soldOut.textContent).not.toContain('Entradas desde');

    const referenceOnly = screen.getByRole('link', { name: /Solo referencia/ });
    expect(referenceOnly.textContent).toContain('Loja');
    expect(referenceOnly.textContent).not.toContain(' · ');
    for (const silent of [referenceOnly, screen.getByRole('link', { name: /Moneda rara/ })]) {
      expect(silent.textContent).not.toMatch(/Entradas desde|Agotado|Gratis/);
    }
  });
});

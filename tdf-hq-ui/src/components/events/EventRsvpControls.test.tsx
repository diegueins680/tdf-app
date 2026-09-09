import { jest } from '@jest/globals';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { StrictMode } from 'react';

const navigateMock = jest.fn();
const captureMock = jest.fn();
const getMyRsvpMock = jest.fn<() => Promise<unknown>>();
const getRsvpSummaryMock = jest.fn<() => Promise<unknown>>();
const upsertMyRsvpMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const deleteMyRsvpMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
let sessionValue: Record<string, unknown> | null = null;

jest.unstable_mockModule('react-router-dom', () => ({
  useNavigate: () => navigateMock,
}));

jest.unstable_mockModule('../../session/SessionContext', () => ({
  useSession: () => ({ session: sessionValue }),
}));

jest.unstable_mockModule('../../analytics/useAnalytics', () => ({
  useAnalytics: () => ({ capture: captureMock }),
}));

jest.unstable_mockModule('../../api/socialEvents', () => ({
  SocialEventsAPI: {
    getMyRsvp: getMyRsvpMock,
    getRsvpSummary: getRsvpSummaryMock,
    upsertMyRsvp: upsertMyRsvpMock,
    deleteMyRsvp: deleteMyRsvpMock,
  },
}));

const { default: EventRsvpControls } = await import('./EventRsvpControls');

function renderControls() {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  return render(
    <StrictMode>
      <QueryClientProvider client={client}>
        <EventRsvpControls
          eventId="42"
          title="Festival TDF"
          start="2030-03-02T20:00:00Z"
          timezone="UTC"
          venue="Teatro Sucre"
          locale="es-EC"
          eligible
          publicShareEligible
          initialSummary={{ rsvpAcceptedCount: 2, rsvpMaybeCount: 3 }}
          origin="public_event_detail"
        />
      </QueryClientProvider>
    </StrictMode>,
  );
}

describe('EventRsvpControls', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    window.sessionStorage.clear();
    sessionValue = null;
    getMyRsvpMock.mockResolvedValue(null);
    getRsvpSummaryMock.mockResolvedValue({ rsvpAcceptedCount: 2, rsvpMaybeCount: 3 });
    upsertMyRsvpMock.mockResolvedValue({
      rsvpEventId: '42',
      rsvpStatus: 'accepted',
      rsvpShowOnProfile: true,
    });
    deleteMyRsvpMock.mockResolvedValue(undefined);
  });

  it('preserves an anonymous RSVP intent and opens signup with the public return route', () => {
    renderControls();

    fireEvent.click(screen.getByRole('button', { name: 'Voy' }));

    expect(navigateMock).toHaveBeenCalledWith('/login?signup=1&intent=events&redirect=%2Feventos%2F42');
    const stored = window.sessionStorage.getItem('tdf:event-rsvp-intent:v1') ?? '';
    expect(stored).toContain('"eventId":"42"');
    expect(stored).toContain('"status":"accepted"');
    expect(stored).not.toMatch(/partyId|email|token/);
  });

  it('sends a self-scoped upsert and confirms the authoritative selected state', async () => {
    sessionValue = { partyId: '7', preferences: { showEventRsvpsOnProfile: true } };
    renderControls();

    const interestedButton = await screen.findByRole('button', { name: 'Me interesa' });
    await waitFor(() => expect((interestedButton as HTMLButtonElement).disabled).toBe(false));
    fireEvent.click(interestedButton);

    await waitFor(() => expect(upsertMyRsvpMock).toHaveBeenCalledWith('42', {
      rsvpStatus: 'maybe',
      rsvpShowOnProfile: true,
    }));
    expect(upsertMyRsvpMock.mock.calls[0]?.[1]).not.toHaveProperty('rsvpPartyId');
    await waitFor(() => expect(screen.getByRole('region', { name: 'Tu RSVP' }).getAttribute('aria-busy')).toBe('false'));
  });

  it('copies only the canonical public event URL', async () => {
    const writeText = jest.fn<() => Promise<void>>().mockResolvedValue(undefined);
    Object.defineProperty(navigator, 'clipboard', { configurable: true, value: { writeText } });
    renderControls();

    fireEvent.click(screen.getByRole('button', { name: 'Copiar enlace' }));

    await waitFor(() => expect(writeText).toHaveBeenCalledWith('http://localhost/eventos/42?utm_source=tdf_web&utm_medium=copy&utm_campaign=event_rsvp'));
  });
});

import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter, useNavigate, type NavigateFunction } from 'react-router-dom';

import type { PublicUpcomingEventDTO, SocialRsvpDTO, SocialRsvpWriteDTO } from '../../api/socialEvents';
import type { ArtistProfileDTO } from '../../api/types';

(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

const listMock = jest.fn<(opts?: { startAfter?: string }) => Promise<PublicUpcomingEventDTO[]>>();
const profileMock = jest.fn<(segment: string) => Promise<ArtistProfileDTO>>();
const artistEventsMock = jest.fn<(partyId: number, startAfter: string, signal?: AbortSignal) => Promise<PublicUpcomingEventDTO[]>>();
const getRsvpMock = jest.fn<(eventId: string) => Promise<SocialRsvpDTO | null>>();
const upsertMock = jest.fn<(eventId: string, input: SocialRsvpWriteDTO) => Promise<SocialRsvpDTO>>();
let mockSession: { partyId: number; preferences?: { showEventRsvpsOnProfile?: boolean } } | null = { partyId: 7 };
let mockSessionLoading = false;

jest.unstable_mockModule('../../api/socialEvents', () => ({
  SocialEventsAPI: {
    listPublicUpcomingEvents: (opts?: { startAfter?: string }) => listMock(opts),
    getMyRsvp: (eventId: string) => getRsvpMock(eventId),
    upsertMyRsvp: (eventId: string, input: SocialRsvpWriteDTO) => upsertMock(eventId, input),
  },
}));
jest.unstable_mockModule('../../api/client', () => ({ API_BASE_URL: 'https://api.example.test' }));
jest.unstable_mockModule('../../api/fans', () => ({ Fans: { getPublicArtist: profileMock } }));
jest.unstable_mockModule('../../api/artistUpcomingEvents', () => ({ listArtistUpcomingEvents: artistEventsMock }));
jest.unstable_mockModule('../../session/SessionContext', () => ({
  useSession: () => ({ session: mockSession, loading: mockSessionLoading }),
}));

const { default: i18n } = await import('../../i18n');
const { default: UpcomingEventsCarousel } = await import('./UpcomingEventsCarousel');
const { eventRsvpQueryKeys } = await import('./eventRsvpQueryKeys');

const flush = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

async function waitFor(check: () => boolean) {
  for (let i = 0; i < 50 && !check(); i += 1) {
    await act(async () => { await flush(); });
  }
}

// Relative to the run date: the carousel drops events that have already started.
const inTwoWeeks = new Date(Date.now() + 14 * 86_400_000).toISOString();
const event = (id: string, title: string, overrides: Partial<PublicUpcomingEventDTO> = {}): PublicUpcomingEventDTO => ({
  publicUpcomingEventId: id,
  publicUpcomingEventTitle: title,
  publicUpcomingEventStart: inTwoWeeks,
  publicUpcomingEventVenueName: 'Andes Brewing',
  publicUpcomingEventWorkflowStateCode: 'published',
  ...overrides,
});

async function render(path = '/buscar') {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  let root: Root | null = createRoot(container);
  let navigate!: NavigateFunction;
  function Content() {
    navigate = useNavigate();
    return <UpcomingEventsCarousel />;
  }
  await act(async () => {
    root?.render(
      <MemoryRouter initialEntries={[path]}>
        <QueryClientProvider client={queryClient}>
          <Content />
        </QueryClientProvider>
      </MemoryRouter>,
    );
    for (let i = 0; i < 4; i += 1) await flush();
  });
  return {
    container,
    queryClient,
    navigate: (path: string) => act(async () => { void navigate(path); await flush(); }),
    cleanup: async () => {
      await act(async () => { root?.unmount(); await flush(); });
      root = null;
      queryClient.clear();
      container.remove();
    },
  };
}

const button = (container: HTMLElement, label: string) =>
  Array.from(container.querySelectorAll<HTMLButtonElement>('button')).find((b) => b.textContent === label);

describe('UpcomingEventsCarousel', () => {
  beforeEach(async () => {
    await i18n.changeLanguage('es');
    listMock.mockReset();
    profileMock.mockReset().mockResolvedValue({ apArtistId: 115 } as ArtistProfileDTO);
    artistEventsMock.mockReset().mockResolvedValue([]);
    getRsvpMock.mockReset();
    upsertMock.mockReset();
    mockSession = { partyId: 7 };
    mockSessionLoading = false;
    window.sessionStorage.clear();
  });

  it.each(['/artista/115', '/a/verde-70'])('puts every artist date first on %s, even beyond eight, without duplicate cards', async (path) => {
    const dates = Array.from({ length: 12 }, (_, i) => event(String(200 + i), `Artist concert ${i + 1}`));
    artistEventsMock.mockResolvedValue(dates);
    listMock.mockResolvedValue([event('1', 'Earlier general concert'), dates[0]!]);
    getRsvpMock.mockResolvedValue(null);
    const before = Date.now();
    const view = await render(path);
    try {
      await waitFor(() => view.container.querySelectorAll('[role="listitem"]').length === 13);
      const cards = Array.from(view.container.querySelectorAll('[role="listitem"]'));
      expect(cards.map((card) => card.querySelector('a')?.getAttribute('href')))
        .toEqual([...dates.map((date) => `/eventos/${date.publicUpcomingEventId}`), '/eventos/1']);
      expect(artistEventsMock).toHaveBeenCalledWith(115, expect.any(String), expect.any(AbortSignal));
      expect(Date.parse(artistEventsMock.mock.calls[0]![1])).toBeGreaterThanOrEqual(before);
      expect(profileMock).toHaveBeenCalledWith(path.split('/').pop());
    } finally { await view.cleanup(); }
  });

  it('shows artist dates even when the general agenda fails and drops artist dates that already started', async () => {
    listMock.mockRejectedValue(new Error('offline'));
    artistEventsMock.mockResolvedValue([
      event('past', 'Past artist concert', { publicUpcomingEventStart: new Date(Date.now() - 1000).toISOString() }),
      event('artist', 'Future artist concert'),
    ]);
    getRsvpMock.mockResolvedValue(null);
    const view = await render('/artista/115');
    try {
      await waitFor(() => (view.container.textContent ?? '').includes('Future artist concert'));
      expect(view.container.textContent).not.toContain('Past artist concert');
    } finally { await view.cleanup(); }
  });

  it.each(['empty', 'error'])('keeps the general agenda when artist dates are %s', async (mode) => {
    listMock.mockResolvedValue([event('general', 'General concert')]);
    if (mode === 'error') artistEventsMock.mockRejectedValue(new Error('offline'));
    getRsvpMock.mockResolvedValue(null);
    const view = await render('/artista/115');
    try {
      await waitFor(() => artistEventsMock.mock.calls.length > 0);
      expect(view.container.textContent).toContain('General concert');
    } finally { await view.cleanup(); }
  });

  it('does not fetch artist events outside an artist profile', async () => {
    listMock.mockResolvedValue([event('general', 'General concert')]);
    getRsvpMock.mockResolvedValue(null);
    const view = await render('/buscar');
    try {
      await waitFor(() => (view.container.textContent ?? '').includes('General concert'));
      expect(profileMock).not.toHaveBeenCalled();
      expect(artistEventsMock).not.toHaveBeenCalled();
    } finally { await view.cleanup(); }
  });

  it('never carries dates from the previous artist into a different profile while its dates load', async () => {
    listMock.mockResolvedValue([event('general', 'General concert')]);
    profileMock.mockImplementation(async (segment) => ({ apArtistId: segment === '115' ? 115 : 337 } as ArtistProfileDTO));
    let finishSecond!: (events: PublicUpcomingEventDTO[]) => void;
    artistEventsMock.mockImplementation(async (id) => id === 115 ? [event('first', 'First artist concert')]
      : new Promise((resolve) => { finishSecond = resolve; }));
    getRsvpMock.mockResolvedValue(null);
    const view = await render('/artista/115');
    try {
      await waitFor(() => (view.container.textContent ?? '').includes('First artist concert'));
      await view.navigate('/a/another-artist');
      await waitFor(() => artistEventsMock.mock.calls.some(([id]) => id === 337));
      expect(view.container.textContent).not.toContain('First artist concert');
      expect(view.container.textContent).toContain('General concert');
      await act(async () => { finishSecond([event('second', 'Second artist concert')]); await flush(); });
      await waitFor(() => (view.container.textContent ?? '').includes('Second artist concert'));
      expect(view.container.querySelector('[role="listitem"]')?.textContent).toContain('Second artist concert');
      await view.navigate('/buscar');
      expect(view.container.textContent).not.toContain('Second artist concert');
      expect(view.container.textContent).toContain('General concert');
    } finally { await view.cleanup(); }
  });

  it('starts at the first artist card when dates arrive after the general agenda and when the profile changes', async () => {
    listMock.mockResolvedValue([event('general', 'General concert')]);
    let finishDates!: (events: PublicUpcomingEventDTO[]) => void;
    artistEventsMock.mockReturnValue(new Promise((resolve) => { finishDates = resolve; }));
    getRsvpMock.mockResolvedValue(null);
    const view = await render('/artista/115');
    try {
      await waitFor(() => artistEventsMock.mock.calls.length > 0);
      const list = view.container.querySelector<HTMLElement>('[role="list"]')!;
      list.scrollLeft = 300;
      await act(async () => { finishDates([event('artist', 'Artist concert')]); await flush(); });
      await waitFor(() => (view.container.textContent ?? '').includes('Artist concert'));
      expect(list.scrollLeft).toBe(0);
      list.scrollLeft = 200;
      await view.navigate('/buscar');
      expect(list.scrollLeft).toBe(0);
    } finally { await view.cleanup(); }
  });

  it('shows upcoming events with the signed-in user\'s RSVP status', async () => {
    listMock.mockResolvedValue([event('141', 'PATCH CULTURE vol.1'), event('143', 'ELECTROETNIA')]);
    getRsvpMock.mockImplementation(async (id) => (id === '141'
      ? { rsvpEventId: '141', rsvpStatus: 'accepted', rsvpShowOnProfile: false }
      : null));
    const view = await render();
    try {
      await waitFor(() => (view.container.textContent ?? '').includes('Vas'));
      expect(view.container.textContent).toContain('Próximos eventos');
      expect(view.container.textContent).toContain('PATCH CULTURE vol.1');
      expect(view.container.textContent).toContain('Vas');
      expect(view.container.querySelector('a[href="/eventos/141"]')).not.toBeNull();
      expect(button(view.container, 'Asistiré')).toBeDefined();
    } finally {
      await view.cleanup();
    }
  });

  it('RSVPs with one tap using the profile-visibility preference', async () => {
    mockSession = { partyId: 7, preferences: { showEventRsvpsOnProfile: false } };
    listMock.mockResolvedValue([event('143', 'ELECTROETNIA')]);
    getRsvpMock.mockResolvedValue(null);
    upsertMock.mockResolvedValue({ rsvpEventId: '143', rsvpStatus: 'accepted', rsvpShowOnProfile: false });
    const view = await render();
    try {
      await waitFor(() => button(view.container, 'Asistiré')?.disabled === false);
      await act(async () => {
        button(view.container, 'Asistiré')?.click();
        for (let i = 0; i < 3; i += 1) await flush();
      });
      expect(upsertMock).toHaveBeenCalledWith('143', { rsvpStatus: 'accepted', rsvpShowOnProfile: false });
      await waitFor(() => (view.container.textContent ?? '').includes('Vas'));
      expect(view.container.textContent).toContain('Vas');
    } finally {
      await view.cleanup();
    }
  });

  it('keeps one-tap attendance disabled until the viewer\'s RSVP is known, so a "maybe" is never overwritten', async () => {
    listMock.mockResolvedValue([event('143', 'ELECTROETNIA')]);
    let deliver!: (value: SocialRsvpDTO | null) => void;
    getRsvpMock.mockReturnValue(new Promise((resolve) => { deliver = resolve; }));
    const view = await render();
    try {
      await waitFor(() => Boolean(button(view.container, 'Asistiré')));
      expect(button(view.container, 'Asistiré')?.disabled).toBe(true);
      await act(async () => {
        button(view.container, 'Asistiré')?.click();
        await flush();
      });
      expect(upsertMock).not.toHaveBeenCalled();

      await act(async () => {
        deliver({ rsvpEventId: '143', rsvpStatus: 'maybe', rsvpShowOnProfile: true });
        await flush();
      });
      await waitFor(() => (view.container.textContent ?? '').includes('Quizás'));
      expect(button(view.container, 'Asistiré')).toBeUndefined();
      expect(upsertMock).not.toHaveBeenCalled();
    } finally {
      await view.cleanup();
    }
  });

  it('does not offer one-tap attendance when the RSVP read fails', async () => {
    listMock.mockResolvedValue([event('143', 'ELECTROETNIA')]);
    getRsvpMock.mockRejectedValue(new Error('offline'));
    const view = await render();
    try {
      await waitFor(() => view.container.textContent?.includes('ELECTROETNIA') === true && getRsvpMock.mock.calls.length > 0);
      for (let i = 0; i < 4; i += 1) await act(async () => { await flush(); });
      // No permanently disabled button: the card still links to the event, where RSVP is explained.
      expect(button(view.container, 'Asistiré')).toBeFalsy();
      expect(view.container.querySelector('a[href="/eventos/143"]')).not.toBeNull();
    } finally {
      await view.cleanup();
    }
  });

  it('writes the RSVP to the cache entries the event detail controls read', async () => {
    listMock.mockResolvedValue([event('143', 'ELECTROETNIA')]);
    getRsvpMock.mockResolvedValue(null);
    upsertMock.mockResolvedValue({ rsvpEventId: '143', rsvpStatus: 'accepted', rsvpShowOnProfile: true });
    const view = await render();
    try {
      view.queryClient.setQueryData(eventRsvpQueryKeys.summary('143'), { stale: true });
      view.queryClient.setQueryData(eventRsvpQueryKeys.feed('7'), { stale: true });
      await waitFor(() => button(view.container, 'Asistiré')?.disabled === false);
      await act(async () => {
        button(view.container, 'Asistiré')?.click();
        for (let i = 0; i < 3; i += 1) await flush();
      });
      await waitFor(() => (view.container.textContent ?? '').includes('Vas'));
      expect(view.queryClient.getQueryData(eventRsvpQueryKeys.mine('143', 7)))
        .toEqual({ rsvpEventId: '143', rsvpStatus: 'accepted', rsvpShowOnProfile: true });
      expect(view.queryClient.getQueryState(eventRsvpQueryKeys.summary('143'))?.isInvalidated).toBe(true);
      expect(view.queryClient.getQueryState(eventRsvpQueryKeys.feed('7'))?.isInvalidated).toBe(true);
    } finally {
      await view.cleanup();
    }
  });

  it('says when a one-tap RSVP was not saved and lets the viewer retry', async () => {
    listMock.mockResolvedValue([event('143', 'ELECTROETNIA')]);
    getRsvpMock.mockResolvedValue(null);
    upsertMock.mockRejectedValueOnce(new Error('offline'));
    upsertMock.mockResolvedValueOnce({ rsvpEventId: '143', rsvpStatus: 'accepted', rsvpShowOnProfile: true });
    const view = await render();
    try {
      await waitFor(() => button(view.container, 'Asistiré')?.disabled === false);
      await act(async () => {
        button(view.container, 'Asistiré')?.click();
        for (let i = 0; i < 3; i += 1) await flush();
      });
      await waitFor(() => Boolean(view.container.querySelector('[role="alert"]')));
      expect(view.container.querySelector('[role="alert"]')?.textContent).toContain('No se guardó tu asistencia');
      expect(view.container.textContent).not.toContain('Vas');

      await act(async () => {
        button(view.container, 'Reintentar')?.click();
        for (let i = 0; i < 3; i += 1) await flush();
      });
      await waitFor(() => (view.container.textContent ?? '').includes('Vas'));
      expect(upsertMock).toHaveBeenCalledTimes(2);
      expect(view.container.querySelector('[role="alert"]')).toBeNull();
    } finally {
      await view.cleanup();
    }
  });

  it('drops the failure notice once the RSVP is saved elsewhere', async () => {
    listMock.mockResolvedValue([event('143', 'ELECTROETNIA')]);
    getRsvpMock.mockResolvedValue(null);
    upsertMock.mockRejectedValueOnce(new Error('offline'));
    const view = await render();
    try {
      await waitFor(() => button(view.container, 'Asistiré')?.disabled === false);
      await act(async () => {
        button(view.container, 'Asistiré')?.click();
        for (let i = 0; i < 3; i += 1) await flush();
      });
      await waitFor(() => Boolean(view.container.querySelector('[role="alert"]')));

      // The event page's controls write the same cache entry.
      await act(async () => {
        view.queryClient.setQueryData(eventRsvpQueryKeys.mine('143', 7), { rsvpEventId: '143', rsvpStatus: 'accepted', rsvpShowOnProfile: true });
        await flush();
      });
      await waitFor(() => (view.container.textContent ?? '').includes('Vas'));
      expect(view.container.querySelector('[role="alert"]')).toBeNull();
    } finally {
      await view.cleanup();
    }
  });

  it('shows an explicit decline instead of offering to overwrite it with one tap', async () => {
    listMock.mockResolvedValue([event('143', 'ELECTROETNIA')]);
    getRsvpMock.mockResolvedValue({ rsvpEventId: '143', rsvpStatus: 'declined', rsvpShowOnProfile: false });
    const view = await render();
    try {
      await waitFor(() => (view.container.textContent ?? '').includes('No vas'));
      expect(view.container.textContent).toContain('No vas');
      expect(button(view.container, 'Asistiré')).toBeUndefined();
    } finally {
      await view.cleanup();
    }
  });

  it('asks for events from the current time and leaves out any that already started', async () => {
    const before = Date.now();
    listMock.mockResolvedValue([
      event('140', 'YA EMPEZÓ', { publicUpcomingEventStart: new Date(before - 60_000).toISOString() }),
      event('143', 'ELECTROETNIA'),
    ]);
    getRsvpMock.mockResolvedValue(null);
    const view = await render();
    try {
      await waitFor(() => (view.container.textContent ?? '').includes('ELECTROETNIA'));
      expect(view.container.textContent).not.toContain('YA EMPEZÓ');
      const requested = Date.parse(listMock.mock.calls[0]?.[0]?.startAfter ?? '');
      expect(requested).toBeGreaterThanOrEqual(before);
      expect(requested).toBeLessThanOrEqual(Date.now());
    } finally {
      await view.cleanup();
    }
  });

  it('shows the start in the event\'s own timezone', async () => {
    // 01:30 UTC is the previous evening in Guayaquil (UTC-5) and the next morning in Tokyo.
    const start = new Date(Date.now() + 14 * 86_400_000);
    start.setUTCHours(1, 30, 0, 0);
    listMock.mockResolvedValue([
      event('141', 'QUITO', { publicUpcomingEventStart: start.toISOString(), publicUpcomingEventTimezone: 'America/Guayaquil' }),
      event('142', 'TOKIO', { publicUpcomingEventStart: start.toISOString(), publicUpcomingEventTimezone: 'Asia/Tokyo' }),
    ]);
    getRsvpMock.mockResolvedValue(null);
    const view = await render();
    try {
      await waitFor(() => (view.container.textContent ?? '').includes('TOKIO'));
      const content = view.container.textContent ?? '';
      expect(content).toContain('20:30');
      expect(content).toContain('10:30');
    } finally {
      await view.cleanup();
    }
  });

  it('waits for the server to confirm a stored session before showing or loading anything', async () => {
    mockSessionLoading = true;
    listMock.mockResolvedValue([event('143', 'ELECTROETNIA')]);
    getRsvpMock.mockResolvedValue(null);
    const view = await render();
    try {
      for (let i = 0; i < 4; i += 1) await act(async () => { await flush(); });
      expect(view.container.textContent).toBe('');
      expect(listMock).not.toHaveBeenCalled();
      expect(getRsvpMock).not.toHaveBeenCalled();
    } finally {
      await view.cleanup();
    }
  });

  it('keeps each event\'s RSVP result separate when two are tapped in a row', async () => {
    listMock.mockResolvedValue([event('141', 'PATCH CULTURE vol.1'), event('143', 'ELECTROETNIA')]);
    getRsvpMock.mockResolvedValue(null);
    let failFirst!: (reason: Error) => void;
    upsertMock.mockImplementation((eventId) => (eventId === '141'
      ? new Promise((_resolve, reject) => { failFirst = reject; })
      : Promise.resolve({ rsvpEventId: '143', rsvpStatus: 'accepted', rsvpShowOnProfile: true })));
    const view = await render();
    const cards = () => Array.from(view.container.querySelectorAll<HTMLElement>('[role="listitem"]'));
    const attendIn = (card: HTMLElement) => Array.from(card.querySelectorAll('button')).find((b) => /Asistiré|Reintentar/.test(b.textContent ?? ''));
    try {
      await waitFor(() => cards().length === 2 && cards().every((card) => attendIn(card)?.disabled === false));
      await act(async () => { attendIn(cards()[0]!)?.click(); for (let i = 0; i < 3; i += 1) await flush(); });
      await act(async () => { attendIn(cards()[1]!)?.click(); for (let i = 0; i < 3; i += 1) await flush(); });
      await waitFor(() => (cards()[1]?.textContent ?? '').includes('Vas'));
      // The first write is still pending: its button stays disabled even though another finished.
      expect(attendIn(cards()[0]!)?.disabled).toBe(true);

      await act(async () => { failFirst(new Error('offline')); for (let i = 0; i < 3; i += 1) await flush(); });
      await waitFor(() => Boolean(cards()[0]?.querySelector('[role="alert"]')));
      expect(cards()[0]?.textContent).toContain('No se guardó tu asistencia');
      expect(cards()[1]?.querySelector('[role="alert"]')).toBeNull();
      expect(cards()[1]?.textContent).toContain('Vas');
    } finally {
      await view.cleanup();
    }
  });

  it('drops an event once it starts, even without a new fetch', async () => {
    jest.useFakeTimers({ advanceTimers: true });
    try {
      const soon = new Date(Date.now() + 90_000).toISOString();
      listMock.mockResolvedValue([event('140', 'EMPIEZA PRONTO', { publicUpcomingEventStart: soon }), event('143', 'ELECTROETNIA')]);
      getRsvpMock.mockResolvedValue(null);
      const view = await render();
      try {
        await waitFor(() => (view.container.textContent ?? '').includes('EMPIEZA PRONTO'));
        await act(async () => { jest.advanceTimersByTime(3 * 60_000); await flush(); });
        await waitFor(() => !(view.container.textContent ?? '').includes('EMPIEZA PRONTO'));
        expect(view.container.textContent).not.toContain('EMPIEZA PRONTO');
        expect(view.container.textContent).toContain('ELECTROETNIA');
        expect(listMock).toHaveBeenCalledTimes(1);
      } finally {
        await view.cleanup();
      }
    } finally {
      jest.useRealTimers();
    }
  });

  it('follows the active language for labels and dates', async () => {
    await i18n.changeLanguage('en');
    listMock.mockResolvedValue([event('141', 'PATCH CULTURE vol.1'), event('143', 'ELECTROETNIA')]);
    getRsvpMock.mockImplementation(async (id) => (id === '141'
      ? { rsvpEventId: '141', rsvpStatus: 'accepted', rsvpShowOnProfile: false }
      : null));
    const view = await render();
    try {
      await waitFor(() => (view.container.textContent ?? '').includes('Going'));
      const content = view.container.textContent ?? '';
      expect(content).toContain('Upcoming events');
      expect(content).toContain('See all');
      expect(content).toContain(new Intl.DateTimeFormat('en', { month: 'short' }).format(new Date(inTwoWeeks)));
      expect(content).not.toMatch(/Próximos|Asistiré|Vas/);
      expect(button(view.container, "I'll go")).toBeDefined();
      expect(view.container.querySelector('button[aria-label="Hide upcoming events"]')).not.toBeNull();
    } finally {
      await view.cleanup();
    }
  });

  it('renders nothing instead of failing when the events response is not a list', async () => {
    listMock.mockResolvedValue({ error: 'unexpected' } as unknown as PublicUpcomingEventDTO[]);
    const view = await render();
    try {
      for (let i = 0; i < 4; i += 1) await act(async () => { await flush(); });
      expect(view.container.textContent).toBe('');
      expect(getRsvpMock).not.toHaveBeenCalled();
    } finally {
      await view.cleanup();
    }
  });

  it('stays hidden for visitors, on the events page, and after dismissal', async () => {
    listMock.mockResolvedValue([event('141', 'PATCH CULTURE vol.1')]);
    getRsvpMock.mockResolvedValue(null);

    mockSession = null;
    let view = await render();
    expect(view.container.textContent).toBe('');
    await view.cleanup();

    mockSession = { partyId: 7 };
    view = await render('/social/eventos');
    expect(view.container.textContent).toBe('');
    await view.cleanup();

    view = await render();
    await waitFor(() => Boolean(view.container.querySelector('button[aria-label="Ocultar próximos eventos"]')));
    expect(view.container.textContent).toContain('PATCH CULTURE vol.1');
    await act(async () => {
      view.container.querySelector<HTMLButtonElement>('button[aria-label="Ocultar próximos eventos"]')?.click();
      await flush();
    });
    expect(view.container.textContent).toBe('');
    await view.cleanup();

    view = await render();
    expect(view.container.textContent).toBe('');
    await view.cleanup();
  });
});

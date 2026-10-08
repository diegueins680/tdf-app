import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';

import type { PublicUpcomingEventDTO, SocialRsvpDTO, SocialRsvpWriteDTO } from '../../api/socialEvents';

(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

const listMock = jest.fn<() => Promise<PublicUpcomingEventDTO[]>>();
const getRsvpMock = jest.fn<(eventId: string) => Promise<SocialRsvpDTO | null>>();
const upsertMock = jest.fn<(eventId: string, input: SocialRsvpWriteDTO) => Promise<SocialRsvpDTO>>();
let mockSession: { partyId: number; preferences?: { showEventRsvpsOnProfile?: boolean } } | null = { partyId: 7 };

jest.unstable_mockModule('../../api/socialEvents', () => ({
  SocialEventsAPI: {
    listPublicUpcomingEvents: () => listMock(),
    getMyRsvp: (eventId: string) => getRsvpMock(eventId),
    upsertMyRsvp: (eventId: string, input: SocialRsvpWriteDTO) => upsertMock(eventId, input),
  },
}));
jest.unstable_mockModule('../../api/client', () => ({ API_BASE_URL: 'https://api.example.test' }));
jest.unstable_mockModule('../../session/SessionContext', () => ({
  useSession: () => ({ session: mockSession }),
}));

const { default: UpcomingEventsCarousel } = await import('./UpcomingEventsCarousel');

const flush = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

async function waitFor(check: () => boolean) {
  for (let i = 0; i < 50 && !check(); i += 1) {
    await act(async () => { await flush(); });
  }
}

const event = (id: string, title: string): PublicUpcomingEventDTO => ({
  publicUpcomingEventId: id,
  publicUpcomingEventTitle: title,
  publicUpcomingEventStart: '2026-10-24T20:00:00-05:00',
  publicUpcomingEventVenueName: 'Andes Brewing',
  publicUpcomingEventWorkflowStateCode: 'published',
});

async function render(path = '/buscar') {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  let root: Root | null = createRoot(container);
  await act(async () => {
    root?.render(
      <MemoryRouter initialEntries={[path]}>
        <QueryClientProvider client={queryClient}>
          <UpcomingEventsCarousel />
        </QueryClientProvider>
      </MemoryRouter>,
    );
    for (let i = 0; i < 4; i += 1) await flush();
  });
  return {
    container,
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
  beforeEach(() => {
    listMock.mockReset();
    getRsvpMock.mockReset();
    upsertMock.mockReset();
    mockSession = { partyId: 7 };
    window.sessionStorage.clear();
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
      await waitFor(() => Boolean(button(view.container, 'Asistiré')));
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

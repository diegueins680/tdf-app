import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { MemoryRouter, Route, Routes, useLocation } from 'react-router-dom';

import type { ArtistProfileDTO, FanFollowDTO } from '../api/types';
import { expectNoSeriousAccessibilityViolations } from '../test/accessibility';

const getPublicArtistMock = jest.fn<(artistRef: number | string) => Promise<ArtistProfileDTO>>();
const getReleasesMock = jest.fn(async () => []);
const listFollowsMock = jest.fn<() => Promise<FanFollowDTO[]>>();
const followMock = jest.fn(async () => ({
  ffArtistId: 17,
  ffArtistName: 'Las Sintéticas',
  ffStartedAt: '2026-09-07T12:00:00Z',
}));
const unfollowMock = jest.fn(async () => undefined);
const captureFirstValueMock = jest.fn(async () => true);
const analyticsClientMock = { capture: jest.fn() };
let sessionMock: {
  username: string;
  displayName: string;
  roles: string[];
  modules: string[];
  partyId: number;
} | null = null;

jest.unstable_mockModule('../api/fans', () => ({
  Fans: {
    getPublicArtist: getPublicArtistMock,
    getReleases: getReleasesMock,
    listFollows: listFollowsMock,
    follow: followMock,
    unfollow: unfollowMock,
  },
}));

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: sessionMock }),
}));

jest.unstable_mockModule('../analytics/posthog', () => ({
  getAnalyticsClient: () => analyticsClientMock,
}));

jest.unstable_mockModule('../analytics/onboardingProgress', () => ({
  captureFirstValueOnce: captureFirstValueMock,
}));

jest.unstable_mockModule('../hooks/useMetaTags', () => ({ useMetaTags: jest.fn() }));
jest.unstable_mockModule('../components/ArtistFansList', () => ({ default: () => null }));
jest.unstable_mockModule('../components/LazyPaginatedList', () => ({ default: () => null }));
jest.unstable_mockModule('react-i18next', () => ({
  useTranslation: () => ({
    t: (key: string, options?: { artist?: string }) => ({
      'artistFollow.authCta': 'Crear cuenta o ingresar para seguir',
      'artistFollow.resumeAction': 'Seguir ahora',
      'artistFollow.resumeMessage': `Ya ingresaste. Continúa donde estabas y sigue a ${options?.artist ?? ''}.`,
      'artistFollow.resumeError': `No pudimos seguir a ${options?.artist ?? ''}. Revisa tu conexión e inténtalo de nuevo.`,
    })[key] ?? key,
  }),
}));

const { default: ArtistPublicPage } = await import('./ArtistPublicPage');

const artist: ArtistProfileDTO = {
  apArtistId: 17,
  apDisplayName: 'Las Sintéticas',
  apSlug: 'las-sinteticas',
  apBio: 'Banda ficticia para pruebas de continuidad.',
  apCity: 'Quito',
  apGenreIds: [],
  apFollowerCount: 3,
  apHasUserAccount: true,
};

function LocationProbe() {
  const location = useLocation();
  return <output aria-label="Ubicación actual">{`${location.pathname}${location.search}`}</output>;
}

function renderPage(initialEntry: string) {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { retry: false },
      mutations: { retry: false },
    },
  });
  const view = render(
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={[initialEntry]}>
        <Routes>
          <Route
            path="/a/:slugOrId"
            element={<><LocationProbe /><ArtistPublicPage /></>}
          />
        </Routes>
      </MemoryRouter>
    </QueryClientProvider>,
  );
  return { ...view, queryClient };
}

describe('ArtistPublicPage follow continuity', () => {
  beforeEach(() => {
    sessionMock = null;
    getPublicArtistMock.mockReset().mockResolvedValue(artist);
    getReleasesMock.mockReset().mockResolvedValue([]);
    listFollowsMock.mockReset().mockResolvedValue([]);
    followMock.mockReset().mockResolvedValue({
      ffArtistId: 17,
      ffArtistName: 'Las Sintéticas',
      ffStartedAt: '2026-09-07T12:00:00Z',
    });
    unfollowMock.mockClear();
    captureFirstValueMock.mockClear();
  });

  afterEach(() => {
    cleanup();
  });

  it('names both the artist and release loading states', async () => {
    let resolveArtist: ((value: ArtistProfileDTO) => void) | undefined;
    getPublicArtistMock.mockImplementation(() => new Promise((resolve) => {
      resolveArtist = resolve;
    }));
    getReleasesMock.mockImplementation(() => new Promise(() => undefined));
    const view = renderPage('/a/las-sinteticas');

    expect(await screen.findByRole('progressbar', { name: 'Cargando perfil del artista' })).toBeTruthy();
    await act(async () => {
      resolveArtist?.(artist);
    });
    expect(await screen.findByRole('progressbar', { name: 'Cargando lanzamientos del artista' })).toBeTruthy();
    view.queryClient.clear();
  });

  it('gives a guest an artist-bound signup return path', async () => {
    const view = renderPage('/a/las-sinteticas');

    const follow = await screen.findByRole('link', { name: 'Crear cuenta o ingresar para seguir' });
    const authUrl = new URL(follow.getAttribute('href') ?? '', 'https://tdf.example.test');
    expect(authUrl.pathname).toBe('/login');
    expect(authUrl.searchParams.get('signup')).toBe('1');
    expect(authUrl.searchParams.get('intent')).toBe('follow_artists');
    expect(authUrl.searchParams.get('redirect')).toBe('/a/las-sinteticas?resume=follow&artistId=17');
    expect(listFollowsMock).not.toHaveBeenCalled();
    await waitFor(() => expect(screen.queryByRole('progressbar')).toBeNull());
    await expectNoSeriousAccessibilityViolations(view.container);
    view.queryClient.clear();
  });

  it('follows only after explicit confirmation and clears the resume query on success', async () => {
    sessionMock = {
      username: 'fan-42',
      displayName: 'Fan 42',
      roles: ['customer'],
      modules: [],
      partyId: 42,
    };
    const view = renderPage('/a/las-sinteticas?resume=follow&artistId=17');

    expect(await screen.findByText('Ya ingresaste. Continúa donde estabas y sigue a Las Sintéticas.')).toBeTruthy();
    expect(followMock).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Seguir ahora' }));

    await waitFor(() => expect(followMock).toHaveBeenCalledWith(17));
    await waitFor(() => expect(captureFirstValueMock).toHaveBeenCalledWith(
      analyticsClientMock,
      42,
      'artist_followed',
    ));
    await waitFor(() => {
      expect(screen.getByRole('status', { name: 'Ubicación actual' }).textContent).toBe('/a/las-sinteticas');
    });
    view.queryClient.clear();
  });

  it('fails closed when the resume target does not match the rendered artist', async () => {
    sessionMock = {
      username: 'fan-42',
      displayName: 'Fan 42',
      roles: ['customer'],
      modules: [],
      partyId: 42,
    };
    const view = renderPage('/a/las-sinteticas?resume=follow&artistId=99');

    expect(await screen.findByRole('button', { name: 'Seguir a Las Sintéticas' })).toBeTruthy();
    expect(screen.queryByRole('button', { name: 'Seguir ahora' })).toBeNull();
    expect(followMock).not.toHaveBeenCalled();
    expect(screen.getByRole('status', { name: 'Ubicación actual' }).textContent).toBe(
      '/a/las-sinteticas?resume=follow&artistId=99',
    );
    view.queryClient.clear();
  });

  it('removes an exact resume query without another mutation when the artist is already followed', async () => {
    sessionMock = {
      username: 'fan-42',
      displayName: 'Fan 42',
      roles: ['customer'],
      modules: [],
      partyId: 42,
    };
    listFollowsMock.mockResolvedValue([{
      ffArtistId: 17,
      ffArtistName: 'Las Sintéticas',
      ffStartedAt: '2026-09-06T12:00:00Z',
    }]);
    const view = renderPage('/a/las-sinteticas?resume=follow&artistId=17');

    await waitFor(() => {
      expect(screen.getByRole('status', { name: 'Ubicación actual' }).textContent).toBe('/a/las-sinteticas');
    });
    expect(await screen.findByRole('button', { name: 'Dejar de seguir a Las Sintéticas' })).toBeTruthy();
    expect(followMock).not.toHaveBeenCalled();
    expect(captureFirstValueMock).not.toHaveBeenCalled();
    view.queryClient.clear();
  });

  it('keeps the exact resume state and shows recovery copy when following fails', async () => {
    sessionMock = {
      username: 'fan-42',
      displayName: 'Fan 42',
      roles: ['customer'],
      modules: [],
      partyId: 42,
    };
    followMock.mockRejectedValue(new Error('offline'));
    const view = renderPage('/a/las-sinteticas?resume=follow&artistId=17');

    fireEvent.click(await screen.findByRole('button', { name: 'Seguir ahora' }));

    expect(await screen.findByText(
      'No pudimos seguir a Las Sintéticas. Revisa tu conexión e inténtalo de nuevo.',
    )).toBeTruthy();
    expect(captureFirstValueMock).not.toHaveBeenCalled();
    expect(screen.getByRole('status', { name: 'Ubicación actual' }).textContent).toBe(
      '/a/las-sinteticas?resume=follow&artistId=17',
    );
    view.queryClient.clear();
  });
});

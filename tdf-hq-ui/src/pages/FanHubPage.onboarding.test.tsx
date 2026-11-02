import '../i18n';
import type { OnboardingProgressDTO } from '../api/session';
import { jest } from '@jest/globals';
import { act } from 'react';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';

import { expectNoSeriousAccessibilityViolations } from '../test/accessibility';
import type { OnboardingProgressDTO, OnboardingCompletionResultDTO } from '../api/session';
import type { ArtistProfileDTO, ArtistProfileUpsert } from '../api/types';
import type { DirectorySearchResponse } from '../api/directory';
import type { SessionUser } from '../session/SessionContext';
import es from '../i18n/locales/es';

const loadOnboardingProgressMock = jest.fn<(apiToken?: string) => Promise<OnboardingProgressDTO>>();
const completeOnboardingProgressMock = jest.fn<() => Promise<unknown>>();
const getMyArtistProfileMock = jest.fn<() => Promise<ArtistProfileDTO | null>>(async () => null);
const updateMyArtistProfileMock = jest.fn<(payload: ArtistProfileUpsert) => Promise<ArtistProfileDTO>>();
const directorySearchMock = jest.fn<() => Promise<DirectorySearchResponse>>(async () => ({ items: [], sponsoredItems: [], facets: { total: 0, entityTypes: {}, cities: [] } }));
const listArtistsMock = jest.fn(async () => []);
const listFollowsMock = jest.fn(async () => []);
const listMyClubsMock = jest.fn(async () => []);
const releaseFeedRefetchMock = jest.fn(async () => undefined);
const profileRefetchMock = jest.fn(async () => undefined);
let sessionLoading = false;
let sessionMock: SessionUser | null = {
  username: 'fan-42',
  displayName: 'Fan 42',
  roles: ['customer'],
  modules: [] as string[],
  partyId: 42,
};

jest.unstable_mockModule('../api/directory', () => ({ Directory: { taxonomies: async () => ({ cities: [], genres: [] }), search: directorySearchMock } }));

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: sessionMock, loading: sessionLoading }),
  getActiveSession: () => sessionMock,
  getStoredSessionToken: () => null,
}));

jest.unstable_mockModule('react-i18next', () => ({
  useTranslation: () => ({ t: (key: string) => es.fanHubOnboarding[key.replace('fanHubOnboarding.', '') as keyof typeof es.fanHubOnboarding] ?? key }),
}));

jest.unstable_mockModule('../api/session', () => ({
  loadOnboardingProgress: loadOnboardingProgressMock,
  completeOnboardingProgress: completeOnboardingProgressMock,
}));

jest.unstable_mockModule('../api/fans', () => ({
  Fans: {
    listArtists: listArtistsMock,
    listFollows: listFollowsMock,
    listMyClubs: listMyClubsMock,
    getMyArtistProfile: getMyArtistProfileMock,
    updateMyArtistProfile: updateMyArtistProfileMock,
    requestMyFanRole: jest.fn(),
    follow: jest.fn(),
    unfollow: jest.fn(),
  },
}));

jest.unstable_mockModule('../api/records', () => ({
  Records: {
    getFeed: jest.fn(async () => ({
      collections: [],
      releases: [],
      recordings: [],
      sessions: [],
    })),
  },
}));

jest.unstable_mockModule('../api/catalogs', () => ({
  Catalogs: {
    listPublicItems: jest.fn(async () => ({ items: [] })),
  },
}));

jest.unstable_mockModule('../api/admin', () => ({
  Admin: { updateArtistRelease: jest.fn() },
}));

jest.unstable_mockModule('../api/drive', () => ({ uploadToDrive: jest.fn() }));
jest.unstable_mockModule('../api/musicReleases', () => ({
  musicReleases: { listPublic: jest.fn(async () => []) },
}));
jest.unstable_mockModule('../hooks/useCmsContent', () => ({
  useCmsContent: () => ({ data: undefined }),
}));
jest.unstable_mockModule('../features/fans/useFanProfile', () => ({
  useFanProfile: () => ({
    profileDraft: {
      fpuDisplayName: '',
      fpuBio: '',
      fpuCity: '',
      fpuFavoriteGenreIds: [],
      fpuAvatarUrl: '',
    },
    profileQuery: {
      data: undefined,
      isError: false,
      isLoading: false,
      refetch: profileRefetchMock,
    },
    saveProfile: jest.fn(),
    setProfileDraft: jest.fn(),
    updateProfileMutation: { isPending: false },
  }),
}));
jest.unstable_mockModule('../features/releases/useReleaseFeed', () => ({
  useReleaseFeed: () => ({
    hasReleaseTargets: false,
    releaseFeed: [],
    releaseFeedQuery: {
      isError: false,
      isFetching: false,
      isLoading: false,
      refetch: releaseFeedRefetchMock,
    },
  }),
}));

jest.unstable_mockModule('../analytics/posthog', () => ({
  getAnalyticsClient: () => ({ capture: jest.fn() }),
}));
jest.unstable_mockModule('../analytics/onboardingProgress', () => ({
  captureFirstValueOnce: jest.fn(async () => false),
}));

jest.unstable_mockModule('../components/GoogleDriveUploadWidget', () => ({ default: () => null }));
jest.unstable_mockModule('../components/LazyPaginatedList', () => ({ default: () => null }));
jest.unstable_mockModule('../components/StreamingPlayer', () => ({ default: () => null }));
jest.unstable_mockModule('../features/releases/ReleaseFeed', () => ({ ReleaseFeed: () => null }));
jest.unstable_mockModule('../features/fans/FanProfileEditor', () => ({ FanProfileEditor: () => null }));
jest.unstable_mockModule('../features/fans/FollowedArtists', () => ({ FollowedArtists: () => null }));
jest.unstable_mockModule('../features/fanclubs/FanClubPreview', () => ({ FanClubPreview: () => null }));

const { default: FanHubPage } = await import('./FanHubPage');

const eligibleProgress: OnboardingProgressDTO = {
  eligible: true, signupCompletedAt: '2026-09-15T00:00:00Z', onboardingIntent: 'follow_artists',
  completedAt: null, firstValue: null, firstValueCompletedAt: null, updatedAt: '2026-09-15T00:00:00Z',
};
const completedProgress: OnboardingProgressDTO = { ...eligibleProgress, eligible: false, completedAt: '2026-09-15T00:05:00Z' };
const completedReceipt: OnboardingCompletionResultDTO = { newlyCompleted: true, progress: completedProgress };

function renderPage(initialEntry = '/fans') {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { retry: false },
      mutations: { retry: false },
    },
  });
  const node = () => (
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={[initialEntry]}>
        <FanHubPage />
      </MemoryRouter>
    </QueryClientProvider>
  );
  const view = render(node());
  return {
    ...view,
    queryClient,
    rerenderPage: () => view.rerender(node()),
  };
}

function closeOnboardingAlert() {
  fireEvent.click(screen.getByRole('button', { name: 'Cerrar primeros pasos' }));
}

describe('FanHubPage authoritative onboarding continuity', () => {
  beforeEach(() => {
    sessionLoading = false;
    localStorage.clear();
    sessionMock = {
      username: 'fan-42',
      displayName: 'Fan 42',
      roles: ['customer'],
      modules: [],
      partyId: 42,
    };
    loadOnboardingProgressMock.mockReset().mockResolvedValue(eligibleProgress);
    completeOnboardingProgressMock.mockReset().mockResolvedValue(completedReceipt);
    listArtistsMock.mockClear();
    listFollowsMock.mockClear();
    listMyClubsMock.mockClear();
  });

  afterEach(() => {
    cleanup();
  });

  it('fails closed while eligibility is loading and when the account is already complete', async () => {
    loadOnboardingProgressMock.mockImplementation(() => new Promise(() => undefined));
    const loadingView = renderPage();
    expect(screen.queryByText('Primeros pasos')).toBeNull();
    loadingView.queryClient.clear();
    loadingView.unmount();

    loadOnboardingProgressMock.mockReset().mockResolvedValue(completedProgress);
    const completedView = renderPage();
    await waitFor(() => expect(loadOnboardingProgressMock).toHaveBeenCalled());
    expect(screen.queryByText('Primeros pasos')).toBeNull();
    completedView.queryClient.clear();
  });

  it('renders eligible guidance without performing a completion side effect', async () => {
    const view = renderPage();

    expect(await screen.findByText('Primeros pasos')).toBeTruthy();
    expect(completeOnboardingProgressMock).not.toHaveBeenCalled();
    await expectNoSeriousAccessibilityViolations(view.container);
    view.queryClient.clear();
  });

  it('restores the guidance and offers retry when completion persistence fails', async () => {
    completeOnboardingProgressMock
      .mockRejectedValueOnce(new Error('offline'))
      .mockResolvedValueOnce(completedReceipt);
    const view = renderPage();

    expect(await screen.findByText('Primeros pasos')).toBeTruthy();
    closeOnboardingAlert();
    expect(await screen.findByText(
      'No pudimos guardar que terminaste estos primeros pasos. Puedes reintentarlo sin perder tu progreso.',
    )).toBeTruthy();
    expect(screen.getByText('Primeros pasos')).toBeTruthy();

    const failureAlert = screen.getByText(
      'No pudimos guardar que terminaste estos primeros pasos. Puedes reintentarlo sin perder tu progreso.',
    ).closest('[role="alert"]');
    const retry = failureAlert?.querySelector<HTMLButtonElement>('button');
    if (!retry) throw new Error('Expected completion retry button');
    fireEvent.click(retry);

    await waitFor(() => expect(completeOnboardingProgressMock).toHaveBeenCalledTimes(2));
    await waitFor(() => {
      expect(screen.queryByText('No pudimos guardar que terminaste estos primeros pasos.', { exact: false })).toBeNull();
      expect(screen.queryByText('Primeros pasos')).toBeNull();
    });
    view.queryClient.clear();
  });

  it('shows a fail-closed retry state when eligibility cannot be loaded', async () => {
    loadOnboardingProgressMock
      .mockRejectedValueOnce(new Error('offline'))
      .mockResolvedValueOnce(eligibleProgress);
    const view = renderPage();

    const message = await screen.findByText(
      'No pudimos cargar tus primeros pasos. No mostraremos información de otra cuenta; revisa tu conexión e inténtalo de nuevo.',
    );
    expect(screen.queryByText('Primeros pasos')).toBeNull();
    const retry = message.closest('[role="alert"]')?.querySelector<HTMLButtonElement>('button');
    if (!retry) throw new Error('Expected eligibility retry button');
    fireEvent.click(retry);

    expect(await screen.findByText('Primeros pasos')).toBeTruthy();
    await waitFor(() => expect(screen.queryByText('No pudimos cargar tus primeros pasos.', { exact: false })).toBeNull());
    view.queryClient.clear();
  });

  it('reloads the returning account without replaying a previous session failure', async () => {
    let rejectCompletion: ((reason?: unknown) => void) | undefined;
    completeOnboardingProgressMock.mockImplementation(() => new Promise((_resolve, reject) => {
      rejectCompletion = reject;
    }));
    loadOnboardingProgressMock
      .mockResolvedValueOnce(eligibleProgress)
      .mockResolvedValue(completedProgress);
    const view = renderPage();

    expect(await screen.findByText('Primeros pasos')).toBeTruthy();
    closeOnboardingAlert();
    sessionMock = {
      username: 'fan-84',
      displayName: 'Fan 84',
      roles: ['customer'],
      modules: [],
      partyId: 84,
    };
    view.rerenderPage();
    await waitFor(() => expect(loadOnboardingProgressMock).toHaveBeenCalledTimes(2));

    await act(async () => {
      rejectCompletion?.(new Error('late offline response'));
    });
    expect(screen.queryByText('Primeros pasos')).toBeNull();
    expect(screen.queryByText('No pudimos guardar que terminaste estos primeros pasos.', { exact: false })).toBeNull();

    loadOnboardingProgressMock.mockResolvedValue(eligibleProgress);
    sessionMock = { ...sessionMock, username: 'fan-42', displayName: 'Fan 42', partyId: 42 };
    view.rerenderPage();
    expect(await screen.findByText('Primeros pasos')).toBeTruthy();
    // A new authenticated lifetime must follow its fresh server read, not an
    // error callback from credentials that have already been replaced.
    expect(screen.queryByText('No pudimos guardar que terminaste estos primeros pasos.', { exact: false })).toBeNull();
    completeOnboardingProgressMock.mockResolvedValue({ newlyCompleted: true, progress: completedProgress });
    closeOnboardingAlert();
    await waitFor(() => expect(screen.queryByText('Primeros pasos')).toBeNull());
    view.queryClient.clear();
  });

  it('keeps the current failure actionable when old requests fail out of order', async () => {
    const failures = new Map<number, (reason: Error) => void>();
    completeOnboardingProgressMock.mockImplementation(() => new Promise((_resolve, reject) => {
      failures.set(sessionMock.partyId, reject);
    }));
    const view = renderPage();
    expect(await screen.findByText('Primeros pasos')).toBeTruthy();
    closeOnboardingAlert();
    await waitFor(() => expect(failures.has(42)).toBe(true));

    sessionMock = { ...sessionMock, username: 'fan-84', displayName: 'Fan 84', partyId: 84 };
    view.rerenderPage();
    expect(await screen.findByText('Primeros pasos')).toBeTruthy();
    closeOnboardingAlert();
    await waitFor(() => expect(failures.has(84)).toBe(true));
    await act(async () => {
      failures.get(84)?.(new Error('current account offline'));
    });
    expect(await screen.findByText('No pudimos guardar que terminaste estos primeros pasos.', { exact: false })).toBeTruthy();
    await act(async () => {
      failures.get(42)?.(new Error('previous account offline'));
    });
    expect(screen.getByText('Primeros pasos')).toBeTruthy();
    expect(await screen.findByText('No pudimos guardar que terminaste estos primeros pasos.', { exact: false })).toBeTruthy();

    sessionMock = { ...sessionMock, username: 'fan-42', displayName: 'Fan 42', partyId: 42 };
    view.rerenderPage();
    expect(await screen.findByText('Primeros pasos')).toBeTruthy();
    // A new authenticated lifetime must follow its fresh server read, not an
    // error callback from credentials that have already been replaced.
    expect(screen.queryByText('No pudimos guardar que terminaste estos primeros pasos.', { exact: false })).toBeNull();
    completeOnboardingProgressMock.mockResolvedValue({ newlyCompleted: true, progress: completedProgress });
    closeOnboardingAlert();
    await waitFor(() => expect(screen.queryByText('Primeros pasos')).toBeNull());
    view.queryClient.clear();
  });

  it('does not read or show account guidance until session hydration completes', async () => {
    sessionLoading = true;
    const view = renderPage();
    expect(screen.queryByText('Primeros pasos')).toBeNull();
    expect(loadOnboardingProgressMock).not.toHaveBeenCalled();
    sessionLoading = false;
    view.rerenderPage();
    await screen.findByText('Primeros pasos');
    expect(completeOnboardingProgressMock).not.toHaveBeenCalled();
  });

  it('ignores legacy dismissal and blocked storage without persisting onboarding locally', async () => {
    localStorage.setItem('fanhub-onboarding-dismissed', '1');
    const storageRead = jest.spyOn(Storage.prototype, 'getItem').mockImplementation(() => { throw new Error('storage blocked'); });
    const storageWrite = jest.spyOn(Storage.prototype, 'setItem');
    try {
      renderPage();
      await screen.findByText('Primeros pasos');
      closeOnboardingAlert();
      await waitFor(() => expect(screen.queryByText('Primeros pasos')).toBeNull());
      expect(storageRead).not.toHaveBeenCalledWith('fanhub-onboarding-dismissed');
      expect(storageWrite.mock.calls.every(([key]) => key === 'fan-hub:genre-filter')).toBe(true);
    } finally {
      storageRead.mockRestore();
      storageWrite.mockRestore();
    }
  });

  it.each([null, {}, { ...eligibleProgress, eligible: 'true' }, { ...eligibleProgress, completedAt: 'yesterday' },
    { ...eligibleProgress, signupCompletedAt: null }])(
    'rejects malformed eligibility payload %#', async (payload) => {
      loadOnboardingProgressMock.mockResolvedValueOnce(payload as OnboardingProgressDTO);
      renderPage();
      await screen.findByText(es.fanHubOnboarding.loadError);
      expect(screen.queryByText('Primeros pasos')).toBeNull();
      expect(completeOnboardingProgressMock).not.toHaveBeenCalled();
    },
  );

  it.each([{}, { newlyCompleted: true }, { newlyCompleted: false, progress: eligibleProgress },
    { newlyCompleted: true, progress: { ...eligibleProgress, eligible: false } }])(
    'does not mistake a malformed or nonterminal receipt for success %#', async (receipt) => {
      completeOnboardingProgressMock.mockResolvedValueOnce(receipt);
      renderPage();
      await screen.findByText('Primeros pasos');
      closeOnboardingAlert();
      await screen.findByText(es.fanHubOnboarding.saveError);
      expect(screen.getByText('Primeros pasos')).toBeTruthy();
      expect(completeOnboardingProgressMock).toHaveBeenCalledTimes(1);
    },
  );

  it('coalesces double close, sends no first value, and accepts an already-terminal receipt', async () => {
    let resolveExit: ((value: unknown) => void) | undefined;
    completeOnboardingProgressMock.mockImplementationOnce(() => new Promise((resolve) => { resolveExit = resolve; }));
    sessionMock = { ...sessionMock!, apiToken: 'synthetic-exit-token' };
    renderPage();
    await screen.findByText('Primeros pasos');
    const close = screen.getByRole('button', { name: 'Cerrar primeros pasos' });
    await act(async () => { fireEvent.click(close); fireEvent.click(close); });
    expect(completeOnboardingProgressMock).toHaveBeenCalledTimes(1);
    expect(completeOnboardingProgressMock).toHaveBeenCalledWith(undefined, 'synthetic-exit-token');
    expect(loadOnboardingProgressMock).toHaveBeenCalledWith('synthetic-exit-token');
    expect((await screen.findByText(es.fanHubOnboarding.saving)).textContent).toBe(es.fanHubOnboarding.saving);
    expect(screen.getByText('Primeros pasos')).toBeTruthy();
    await act(async () => { resolveExit?.({ newlyCompleted: false, progress: completedProgress }); });
    await waitFor(() => expect(screen.queryByText('Primeros pasos')).toBeNull());
  });

  it.each(['guest', 'manager'])('keeps %s tips separate from account completion', async (mode) => {
    sessionMock = mode === 'guest' ? null : { username: 'manager', displayName: 'Manager', partyId: 42, roles: ['Admin'], modules: ['Admin'] };
    renderPage(mode === 'manager' ? '/inicio' : '/fans');
    await screen.findByText(mode === 'manager' ? 'Lo más útil ahora' : 'Primeros pasos');
    closeOnboardingAlert();
    expect(screen.queryByText(mode === 'manager' ? 'Lo más útil ahora' : 'Primeros pasos')).toBeNull();
    expect(loadOnboardingProgressMock).not.toHaveBeenCalled();
    expect(completeOnboardingProgressMock).not.toHaveBeenCalled();
  });

  it.each(['same-party rotation', 'unmount'])('ignores late eligibility after %s', async (change) => {
    let resolveRead: ((value: OnboardingProgressDTO) => void) | undefined;
    loadOnboardingProgressMock.mockImplementationOnce(() => new Promise((resolve) => { resolveRead = resolve; }));
    const view = renderPage();
    await waitFor(() => expect(loadOnboardingProgressMock).toHaveBeenCalledTimes(1));
    if (change === 'unmount') view.unmount();
    else {
      sessionMock = { ...sessionMock!, apiToken: 'replacement-token' };
      loadOnboardingProgressMock.mockResolvedValue(completedProgress);
      view.rerenderPage();
      await waitFor(() => expect(loadOnboardingProgressMock).toHaveBeenCalledTimes(2));
    }
    await act(async () => { resolveRead?.(eligibleProgress); });
    expect(screen.queryByText('Primeros pasos')).toBeNull();
    expect(completeOnboardingProgressMock).not.toHaveBeenCalled();
  });

  it('does not let an old completion hide guidance after a session leaves and returns', async () => {
    let resolveExit: ((value: unknown) => void) | undefined;
    completeOnboardingProgressMock.mockImplementationOnce(() => new Promise((resolve) => { resolveExit = resolve; }));
    const original = sessionMock;
    const view = renderPage();
    await screen.findByText('Primeros pasos');
    closeOnboardingAlert();
    await waitFor(() => expect(completeOnboardingProgressMock).toHaveBeenCalledTimes(1));
    sessionMock = null;
    view.rerenderPage();
    sessionMock = original;
    view.rerenderPage();
    await waitFor(() => expect(loadOnboardingProgressMock).toHaveBeenCalledTimes(2));
    await act(async () => { resolveExit?.(completedReceipt); });
    expect(await screen.findByText('Primeros pasos')).toBeTruthy();
    expect(screen.queryByText(es.fanHubOnboarding.saveError)).toBeNull();
    closeOnboardingAlert();
    await waitFor(() => expect(completeOnboardingProgressMock).toHaveBeenCalledTimes(2));
  });

  it('does not reopen acknowledged guidance when a pre-completion refresh resolves late', async () => {
    let resolveExit: ((value: unknown) => void) | undefined;
    let resolveRead: ((value: OnboardingProgressDTO) => void) | undefined;
    completeOnboardingProgressMock.mockImplementationOnce(() => new Promise((resolve) => { resolveExit = resolve; }));
    const view = renderPage();
    await screen.findByText('Primeros pasos');
    closeOnboardingAlert();
    await screen.findByText(es.fanHubOnboarding.saving);
    loadOnboardingProgressMock.mockImplementationOnce(() => new Promise((resolve) => { resolveRead = resolve; }));
    let refresh: Promise<void> | undefined;
    await act(async () => { refresh = view.queryClient.invalidateQueries({ queryKey: ['onboarding-progress'] }); });
    await waitFor(() => expect(loadOnboardingProgressMock).toHaveBeenCalledTimes(2));
    await act(async () => { resolveExit?.(completedReceipt); });
    await waitFor(() => expect(screen.queryByText(es.fanHubOnboarding.saving)).toBeNull());
    await act(async () => { resolveRead?.(eligibleProgress); await refresh; });
    expect(screen.queryByText('Primeros pasos')).toBeNull();
    expect(completeOnboardingProgressMock).toHaveBeenCalledTimes(1);
  });
});


describe('artist photo continuity in Community', () => {
  afterEach(() => { cleanup(); });

  it('refreshes an open search and cached artist previews after any artist saves a photo', async () => {
    sessionMock = { username: 'artist-42', displayName: 'New artist', roles: ['Artist'], modules: [], partyId: 42, apiToken: 'synthetic-test-token' };
    loadOnboardingProgressMock.mockResolvedValue(completedProgress);
    let profile = { apArtistId: 42, apDisplayName: 'New artist', apSlug: 'new-artist', apHeroImageUrl: 'https://cdn.example.test/old-photo.jpg', apGenreIds: [] } as unknown as ArtistProfileDTO;
    getMyArtistProfileMock.mockImplementation(async () => profile);
    updateMyArtistProfileMock.mockImplementation(async (payload) => {
      profile = { ...profile, apHeroImageUrl: payload.apuHeroImageUrl };
      return profile;
    });
    directorySearchMock.mockImplementation(async () => ({
      items: [{ type: 'profile', id: 'artist-42', slug: '42', canonicalPath: '/artista/42', title: 'New artist', imageUrl: profile.apHeroImageUrl, location: {}, taxonomy: { genreIds: [], professionIds: [], serviceIds: [], instrumentIds: [] }, score: 1, scoreBreakdown: {}, sponsored: false }],
      sponsoredItems: [], facets: { total: 1, entityTypes: { profile: 1 }, cities: [] },
    }));
    const imageLoader = jest.spyOn(window, 'Image').mockImplementation(() => {
      const image = document.createElement('img');
      Object.defineProperty(image, 'src', { set: () => { queueMicrotask(() => image.dispatchEvent(new Event('load'))); } });
      return image;
    });
    const view = renderPage('/mi-artista?q=New');
    view.queryClient.setQueryData(['public-artist', 'new-artist'], profile);
    view.queryClient.setQueryData(['public-artist', '42'], profile);
    try {
      const card = await screen.findByRole('article');
      const image = await waitFor(() => {
        const result = card.querySelector('img');
        expect(result).not.toBeNull();
        return result!;
      });
      expect(image.getAttribute('src')).toBe('https://cdn.example.test/old-photo.jpg');
      const input = await screen.findByLabelText('URL de portada (opcional)');
      await waitFor(() => expect(input).toHaveProperty('value', 'https://cdn.example.test/old-photo.jpg'));
      fireEvent.change(input, { target: { value: 'https://cdn.example.test/new-photo.jpg' } });
      fireEvent.click(screen.getByRole('button', { name: 'Actualizar perfil de artista' }));
      await waitFor(() => expect(updateMyArtistProfileMock.mock.calls.at(-1)?.[0]).toEqual(expect.objectContaining({ apuArtistId: 42, apuHeroImageUrl: 'https://cdn.example.test/new-photo.jpg' })));
      await waitFor(() => expect(image.getAttribute('src')).toBe('https://cdn.example.test/new-photo.jpg'));
      expect(view.queryClient.getQueryState(['public-artist', 'new-artist'])?.isInvalidated).toBe(true);
      expect(view.queryClient.getQueryState(['public-artist', '42'])?.isInvalidated).toBe(true);
    } finally { view.queryClient.clear(); imageLoader.mockRestore(); }
  });
});


describe('artist public link never uses an unsaved slug', () => {
  const initialArtist = (): ArtistProfileDTO => ({
    apArtistId: 42,
    apDisplayName: 'Artist 42',
    apSlug: 'published-artist',
    apGenreIds: [],
  } as unknown as ArtistProfileDTO);

  beforeEach(() => {
    sessionLoading = false;
    sessionMock = {
      username: 'artist-42', displayName: 'Artist 42', roles: ['Artist'],
      modules: [], partyId: 42, apiToken: 'synthetic-artist-token',
    };
    loadOnboardingProgressMock.mockReset().mockResolvedValue(completedProgress);
    completeOnboardingProgressMock.mockReset().mockResolvedValue(completedReceipt);
    updateMyArtistProfileMock.mockReset();
    getMyArtistProfileMock.mockReset().mockResolvedValue(initialArtist());
  });

  afterEach(() => { cleanup(); });

  it('keeps public view and copy pointed at the persisted slug while an artist edits a draft', async () => {
    const view = renderPage('/mi-artista');
    try {
      const publicLink = await screen.findByRole('link', { name: 'Ver público' });
      expect(publicLink.getAttribute('href')).toBe('/a/published-artist');
      const slug = await screen.findByRole('textbox', { name: 'Slug público' });
      await waitFor(() => expect((slug as HTMLInputElement).value).toBe('published-artist'));
      fireEvent.change(slug, { target: { value: 'not-saved-yet' } });
      expect(publicLink.getAttribute('href')).toBe('/a/published-artist');
      expect(screen.getByRole('button', { name: 'Copiar enlace' })).toBeTruthy();
      expect(updateMyArtistProfileMock).not.toHaveBeenCalled();
    } finally { view.queryClient.clear(); }
  });

  it('changes the shareable link only after a matching authoritative save receipt', async () => {
    let persisted = initialArtist();
    getMyArtistProfileMock.mockImplementation(async () => persisted);
    updateMyArtistProfileMock.mockImplementation(async () => {
      persisted = { ...persisted, apSlug: 'server-confirmed-slug' };
      return persisted;
    });
    const view = renderPage('/mi-artista');
    try {
      await screen.findByRole('link', { name: 'Ver público' });
      const slug = await screen.findByRole('textbox', { name: 'Slug público' });
      fireEvent.change(slug, { target: { value: 'requested-slug' } });
      expect(screen.getByRole('link', { name: 'Ver público' }).getAttribute('href')).toBe('/a/published-artist');
      fireEvent.click(screen.getByRole('button', { name: 'Actualizar perfil de artista' }));
      await waitFor(() => expect(updateMyArtistProfileMock).toHaveBeenCalledTimes(1));
      await waitFor(() => expect(screen.getByRole('link', { name: 'Ver público' }).getAttribute('href')).toBe('/a/server-confirmed-slug'));
      expect(view.queryClient.getQueryData<ArtistProfileDTO>(['artist-profile', 42])?.apSlug).toBe('server-confirmed-slug');
    } finally { view.queryClient.clear(); }
  });

  it('rejects a mismatched account save receipt and never shares its slug', async () => {
    updateMyArtistProfileMock.mockResolvedValue({
      ...initialArtist(), apArtistId: 99, apSlug: 'other-artist',
    });
    const view = renderPage('/mi-artista');
    try {
      await screen.findByRole('link', { name: 'Ver público' });
      fireEvent.click(screen.getByRole('button', { name: 'Actualizar perfil de artista' }));
      await screen.findByText(/No pudimos confirmar el perfil guardado/);
      expect(screen.getByRole('link', { name: 'Ver público' }).getAttribute('href')).toBe('/a/published-artist');
      expect(view.queryClient.getQueryData<ArtistProfileDTO>(['artist-profile', 42])?.apSlug).toBe('published-artist');
    } finally { view.queryClient.clear(); }
  });
});

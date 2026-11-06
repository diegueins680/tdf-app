import { jest } from '@jest/globals';
import { act, type ReactNode } from 'react';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';

import { expectNoSeriousAccessibilityViolations } from '../test/accessibility';
import type { OnboardingProgressDTO, OnboardingCompletionResultDTO } from '../api/session';
import type { SessionUser } from '../session/SessionContext';
import type { ArtistProfileDTO, FanFollowDTO } from '../api/types';
import es from '../i18n/locales/es';

const loadOnboardingProgressMock = jest.fn<(apiToken?: string) => Promise<OnboardingProgressDTO>>();
const completeOnboardingProgressMock = jest.fn<() => Promise<unknown>>();
const listArtistsMock = jest.fn<() => Promise<ArtistProfileDTO[]>>(async () => []);
const listFollowsMock = jest.fn<() => Promise<FanFollowDTO[]>>(async () => []);
const followMock = jest.fn<(artistId: number) => Promise<FanFollowDTO>>();
const unfollowMock = jest.fn<(artistId: number) => Promise<void>>();
const captureFirstValueMock = jest.fn(async () => false);
const listMyClubsMock = jest.fn(async () => []);
const getMyArtistProfileMock = jest.fn<() => Promise<ArtistProfileDTO | null>>(async () => null);
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

jest.unstable_mockModule('../api/directory', () => ({ Directory: { taxonomies: async () => ({ cities: [], genres: [] }), search: async () => ({ items: [], sponsoredItems: [], facets: { total: 0, entityTypes: {}, cities: [] } }) } }));

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: sessionMock, loading: sessionLoading }),
  getActiveSession: () => sessionMock,
  // The canonical release API reaches SessionContext through api/client.
  getStoredSessionToken: () => null,
}));

jest.unstable_mockModule('react-i18next', () => ({
  useTranslation: () => ({ t: (key: string) => es.fanHubOnboarding[key.replace('fanHubOnboarding.', '') as keyof typeof es.fanHubOnboarding] ?? (key === 'artistFollow.changeError' ? es.artistFollow.changeError : key) }),
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
    updateMyArtistProfile: jest.fn(),
    requestMyFanRole: jest.fn(),
    follow: followMock,
    unfollow: unfollowMock,
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
  captureFirstValueOnce: captureFirstValueMock,
}));

jest.unstable_mockModule('../components/GoogleDriveUploadWidget', () => ({ default: () => null }));
jest.unstable_mockModule('../components/LazyPaginatedList', () => ({ default: ({ items, renderItems }: { items: unknown[]; renderItems: (items: unknown[]) => ReactNode }) => renderItems(items) }));
jest.unstable_mockModule('../components/StreamingPlayer', () => ({ default: () => null }));
jest.unstable_mockModule('../features/releases/ReleaseFeed', () => ({ ReleaseFeed: () => null }));
jest.unstable_mockModule('../features/fans/FanProfileEditor', () => ({ FanProfileEditor: () => null }));
jest.unstable_mockModule('../features/fans/FollowedArtists', () => ({ FollowedArtists: () => null }));
jest.unstable_mockModule('../features/fans/ProfileSectionCard', () => ({ ProfileSectionCard: () => null }));
jest.unstable_mockModule('../features/fanclubs/FanClubPreview', () => ({ FanClubPreview: () => null }));

const { default: FanHubPage } = await import('./FanHubPage');

const eligibleProgress: OnboardingProgressDTO = {
  eligible: true, signupCompletedAt: '2026-09-15T00:00:00Z', onboardingIntent: 'follow_artists',
  completedAt: null, firstValue: null, firstValueCompletedAt: null, updatedAt: '2026-09-15T00:00:00Z',
};
const completedProgress: OnboardingProgressDTO = { ...eligibleProgress, eligible: false, completedAt: '2026-09-15T00:05:00Z' };
const completedReceipt: OnboardingCompletionResultDTO = { newlyCompleted: true, progress: completedProgress };

function renderPage(initialEntry = '/fans', knownFollows?: FanFollowDTO[]) {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { retry: false },
      mutations: { retry: false },
    },
  });
  if (knownFollows !== undefined) queryClient.setQueryData(['fan-follows', sessionMock?.partyId ?? null], knownFollows);
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
    listArtistsMock.mockReset().mockResolvedValue([]);
    listFollowsMock.mockReset().mockResolvedValue([]);
    followMock.mockReset();unfollowMock.mockReset();captureFirstValueMock.mockClear();
    listMyClubsMock.mockClear();
    getMyArtistProfileMock.mockReset().mockResolvedValue(null);
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

  it('ignores a late completion failure from the previous Party', async () => {
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

  it.each([false, true])('reconciles the community card after an uncertain committed change (wasFollowing=%s)', async wasFollowing => {
    const artist: ArtistProfileDTO = { apArtistId: 17, apDisplayName: 'Artista ficticio', apSlug: 'artista-ficticio', apGenreIds: [], apFollowerCount: 0, apHasUserAccount: true };
    const follow = { ffArtistId: 17, ffArtistName: artist.apDisplayName, ffStartedAt: '2026-10-26T06:00:00Z' };
    let serverFollowing = wasFollowing;
    listArtistsMock.mockReset().mockResolvedValue([artist]);
    listFollowsMock.mockReset().mockImplementation(async () => serverFollowing ? [follow] : []);
    captureFirstValueMock.mockClear();followMock.mockReset();unfollowMock.mockReset();
    const loseResponse = async () => { serverFollowing = !wasFollowing; throw new TypeError('Synthetic response lost after commit'); };
    if (wasFollowing) unfollowMock.mockImplementationOnce(loseResponse);else followMock.mockImplementationOnce(loseResponse);
    const view = renderPage();
    const buttons = await screen.findAllByRole('button', { name: wasFollowing ? 'Siguiendo' : 'Seguir', exact: true });
    const button = buttons[buttons.length - 1]!;
    await waitFor(() => expect((button as HTMLButtonElement).disabled).toBe(false));
    await act(async () => { fireEvent.click(button);fireEvent.click(button); });
    await waitFor(() => expect(listFollowsMock).toHaveBeenCalledTimes(2));
    const after = await screen.findAllByRole('button', { name: wasFollowing ? 'Seguir' : 'Siguiendo', exact: true });
    await waitFor(() => expect(after.some(node => !(node as HTMLButtonElement).disabled)).toBe(true));
    expect(followMock).toHaveBeenCalledTimes(wasFollowing ? 0 : 1);expect(unfollowMock).toHaveBeenCalledTimes(wasFollowing ? 1 : 0);
    expect(captureFirstValueMock).toHaveBeenCalledTimes(wasFollowing ? 0 : 1);
    view.unmount();view.queryClient.clear();
  });

  it('does not submit a community follow before its current state is known', async () => {
    const artist: ArtistProfileDTO = { apArtistId: 17, apDisplayName: 'Artista ficticio', apGenreIds: [], apFollowerCount: 0, apHasUserAccount: true };
    listArtistsMock.mockReset().mockResolvedValue([artist]);followMock.mockReset();unfollowMock.mockReset();
    let failRead!: (reason: Error) => void;
    listFollowsMock.mockReset().mockImplementation(() => new Promise((_resolve, reject) => { failRead = reject; }));
    const view = renderPage();const buttons = await screen.findAllByRole('button', { name: 'Seguir', exact: true });
    expect(buttons.every(node => (node as HTMLButtonElement).disabled)).toBe(true);
    fireEvent.click(buttons[buttons.length - 1]!);expect(followMock).not.toHaveBeenCalled();
    await act(async () => { failRead(new TypeError('Synthetic state unavailable')); });
    expect(screen.getAllByRole('button', { name: 'Seguir', exact: true }).every(node => (node as HTMLButtonElement).disabled)).toBe(true);
    expect(followMock).not.toHaveBeenCalled();expect(unfollowMock).not.toHaveBeenCalled();view.unmount();view.queryClient.clear();
  });

  it('keeps a failed-before-commit change retryable without fabricating a relationship', async () => {
    const artist: ArtistProfileDTO = { apArtistId: 17, apDisplayName: 'Artista ficticio', apGenreIds: [], apFollowerCount: 0, apHasUserAccount: true };
    listArtistsMock.mockResolvedValue([artist]);followMock.mockRejectedValue(new TypeError('Synthetic request never reached server'));
    const view = renderPage();const buttons = await screen.findAllByRole('button', { name: 'Seguir', exact: true });
    await waitFor(() => expect((buttons[buttons.length - 1] as HTMLButtonElement).disabled).toBe(false));
    fireEvent.click(buttons[buttons.length - 1]!);
    await screen.findByText(es.artistFollow.changeError);
    await waitFor(() => expect(listFollowsMock).toHaveBeenCalledTimes(2));
    expect(screen.queryByRole('button', { name: 'Siguiendo', exact: true })).toBeNull();
    expect(followMock).toHaveBeenCalledTimes(1);expect(captureFirstValueMock).not.toHaveBeenCalled();
    view.unmount();view.queryClient.clear();
  });

  it('explicitly retries an unavailable confirmation read without repeating a committed follow', async () => {
    const artist: ArtistProfileDTO = { apArtistId: 17, apDisplayName: 'Artista ficticio', apGenreIds: [], apFollowerCount: 0, apHasUserAccount: true };
    const follow = { ffArtistId: 17, ffArtistName: artist.apDisplayName, ffStartedAt: '2026-10-26T06:00:00Z' };
    listArtistsMock.mockResolvedValue([artist]);
    listFollowsMock.mockResolvedValueOnce([]).mockRejectedValueOnce(new TypeError('Synthetic confirmation read unavailable')).mockResolvedValue([follow]);
    followMock.mockRejectedValue(new TypeError('Synthetic response lost after commit'));
    const view = renderPage();const buttons = await screen.findAllByRole('button', { name: 'Seguir', exact: true });
    await waitFor(() => expect((buttons[buttons.length - 1] as HTMLButtonElement).disabled).toBe(false));
    fireEvent.click(buttons[buttons.length - 1]!);
    await screen.findByText(es.artistFollow.changeError);
    const retry = await screen.findByRole('button', { name: 'Reintentar', exact: true });
    await act(async () => { fireEvent.click(retry);fireEvent.click(retry); });
    await screen.findByRole('button', { name: 'Siguiendo', exact: true });
    await waitFor(() => expect(screen.queryByText(es.artistFollow.changeError)).toBeNull());
    expect(followMock).toHaveBeenCalledTimes(1);expect(listFollowsMock).toHaveBeenCalledTimes(3);
    expect(captureFirstValueMock).toHaveBeenCalledTimes(1);view.unmount();view.queryClient.clear();
  });

  it('does not confirm the old community follow after an account switch during recovery', async () => {
    const artist: ArtistProfileDTO = { apArtistId: 17, apDisplayName: 'Artista ficticio', apGenreIds: [], apFollowerCount: 0, apHasUserAccount: true };
    const follow = { ffArtistId: 17, ffArtistName: artist.apDisplayName, ffStartedAt: '2026-10-26T06:00:00Z' };
    let completeRead!: (rows: FanFollowDTO[]) => void;
    listArtistsMock.mockResolvedValue([artist]);listFollowsMock.mockResolvedValueOnce([])
      .mockImplementationOnce(() => new Promise(resolve => { completeRead = resolve; })).mockResolvedValue([]);
    followMock.mockRejectedValue(new TypeError('Synthetic response lost after commit'));
    const view = renderPage();const buttons = await screen.findAllByRole('button', { name: 'Seguir', exact: true });
    await waitFor(() => expect((buttons[buttons.length - 1] as HTMLButtonElement).disabled).toBe(false));
    fireEvent.click(buttons[buttons.length - 1]!);
    await waitFor(() => expect(listFollowsMock).toHaveBeenCalledTimes(2));
    sessionMock = { ...sessionMock!, username: 'other-43', partyId: 43 };view.rerenderPage();
    await act(async () => { completeRead([follow]); });
    await waitFor(() => expect(listFollowsMock).toHaveBeenCalledTimes(3));
    expect(view.queryClient.getQueryData(['fan-follows', 42])).toEqual([]);
    expect(view.queryClient.getQueryData(['fan-follows', 43])).toEqual([]);
    expect(captureFirstValueMock).not.toHaveBeenCalled();
    expect(screen.queryByText(es.artistFollow.changeError)).toBeNull();view.unmount();view.queryClient.clear();
  });

  it.each(['Artist', 'Admin'])('keeps fan membership access gated for %s-only accounts with cached data', async role => {
    sessionMock = { ...sessionMock!, roles: [role] };
    const artist: ArtistProfileDTO = { apArtistId: 17, apDisplayName: 'Artista ficticio', apGenreIds: [], apFollowerCount: 0, apHasUserAccount: true };
    listArtistsMock.mockResolvedValue([artist]);
    listFollowsMock.mockRejectedValue(new Error('Synthetic403: requires Fan or Customer'));
    const view = renderPage('/fans', []);
    const buttons = await screen.findAllByRole('button', { name: 'Seguir', exact: true });
    expect(listFollowsMock).not.toHaveBeenCalled();
    expect(buttons.every(node => (node as HTMLButtonElement).disabled)).toBe(true);
    buttons.forEach(node => fireEvent.click(node));
    expect(followMock).not.toHaveBeenCalled();expect(unfollowMock).not.toHaveBeenCalled();
    expect(screen.queryByText('Tuvimos un problema cargando tu información. Revisa tu conexión o intenta de nuevo.')).toBeNull();
    view.unmount();view.queryClient.clear();
  });

  it('retains fan membership access for an artist with the Customer role', async () => {
    sessionMock = { ...sessionMock!, roles: ['Artist', 'Customer'] };
    const artist: ArtistProfileDTO = { apArtistId: 17, apDisplayName: 'Artista ficticio', apGenreIds: [], apFollowerCount: 0, apHasUserAccount: true };
    listArtistsMock.mockResolvedValue([artist]);
    const view = renderPage();const buttons = await screen.findAllByRole('button', { name: 'Seguir', exact: true });
    await waitFor(() => expect((buttons[buttons.length - 1] as HTMLButtonElement).disabled).toBe(false));
    expect(listFollowsMock).toHaveBeenCalledTimes(1);
    expect(followMock).not.toHaveBeenCalled();view.unmount();view.queryClient.clear();
  });

  it.each(['Artist', 'Admin'])('keeps artist-profile retry separate from denied fan membership access for %s-only accounts', async role => {
    sessionMock = { ...sessionMock!, roles: [role] };
    const artist: ArtistProfileDTO = { apArtistId: 17, apDisplayName: 'Artista ficticio', apGenreIds: [], apFollowerCount: 0, apHasUserAccount: true };
    listArtistsMock.mockResolvedValue([artist]);
    getMyArtistProfileMock.mockRejectedValueOnce(new Error('Synthetic artist-profile503')).mockResolvedValue(null);
    listFollowsMock.mockRejectedValue(new Error('Synthetic403: requires Fan or Customer'));
    profileRefetchMock.mockClear();
    const view = renderPage('/fans', []);
    fireEvent.click(await screen.findByRole('button', { name: 'Reintentar', exact: true }));
    await waitFor(() => expect(listArtistsMock).toHaveBeenCalledTimes(2));
    await screen.findAllByRole('button', { name: 'Seguir', exact: true });
    expect(getMyArtistProfileMock).toHaveBeenCalledTimes(2);
    expect(listFollowsMock).not.toHaveBeenCalled();expect(profileRefetchMock).not.toHaveBeenCalled();
    expect(screen.queryByText('Tuvimos un problema cargando tu información. Revisa tu conexión o intenta de nuevo.')).toBeNull();
    view.unmount();view.queryClient.clear();
  });

});

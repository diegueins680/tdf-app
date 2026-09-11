import { jest } from '@jest/globals';
import { act } from 'react';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';

import { expectNoSeriousAccessibilityViolations } from '../test/accessibility';

interface OnboardingProgressFixture {
  eligible: boolean;
  completed: boolean;
}

const loadOnboardingProgressMock = jest.fn<() => Promise<OnboardingProgressFixture>>();
const completeOnboardingProgressMock = jest.fn<() => Promise<unknown>>();
const listArtistsMock = jest.fn(async () => []);
const listFollowsMock = jest.fn(async () => []);
const listMyClubsMock = jest.fn(async () => []);
const releaseFeedRefetchMock = jest.fn(async () => undefined);
const profileRefetchMock = jest.fn(async () => undefined);
let sessionMock = {
  username: 'fan-42',
  displayName: 'Fan 42',
  roles: ['customer'],
  modules: [] as string[],
  partyId: 42,
};

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: sessionMock }),
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
    getMyArtistProfile: jest.fn(),
    updateMyArtistProfile: jest.fn(),
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
jest.unstable_mockModule('../features/fans/ProfileSectionCard', () => ({ ProfileSectionCard: () => null }));
jest.unstable_mockModule('../features/fanclubs/FanClubPreview', () => ({ FanClubPreview: () => null }));

const { default: FanHubPage } = await import('./FanHubPage');

const eligibleProgress: OnboardingProgressFixture = { eligible: true, completed: false };
const completedProgress: OnboardingProgressFixture = { eligible: false, completed: true };

function renderPage() {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { retry: false },
      mutations: { retry: false },
    },
  });
  const node = () => (
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={['/fans']}>
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
  const title = screen.getByText('Primeros pasos');
  const alert = title.closest('[role="alert"]');
  const close = alert?.querySelector<HTMLButtonElement>('button');
  if (!close) throw new Error('Expected onboarding close button');
  fireEvent.click(close);
}

describe('FanHubPage authoritative onboarding continuity', () => {
  beforeEach(() => {
    sessionMock = {
      username: 'fan-42',
      displayName: 'Fan 42',
      roles: ['customer'],
      modules: [],
      partyId: 42,
    };
    loadOnboardingProgressMock.mockReset().mockResolvedValue(eligibleProgress);
    completeOnboardingProgressMock.mockReset().mockResolvedValue({ newlyCompleted: true });
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
      .mockResolvedValueOnce({ newlyCompleted: true });
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
});

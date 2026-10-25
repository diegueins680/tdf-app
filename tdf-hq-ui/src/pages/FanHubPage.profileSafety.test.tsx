import '../i18n';
import { jest } from '@jest/globals';
import { act, type ReactNode } from 'react';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { ArtistProfileDTO, ArtistProfileUpsert, FanProfileDTO, FanProfileUpdate } from '../api/types';
import type { SessionUser } from '../session/SessionContext';

const getArtistProfile = jest.fn<() => Promise<ArtistProfileDTO>>();
const updateArtistProfile = jest.fn<(payload: ArtistProfileUpsert) => Promise<ArtistProfileDTO>>();
const getFanProfile = jest.fn<() => Promise<FanProfileDTO>>();
const updateFanProfile = jest.fn<(payload: FanProfileUpdate) => Promise<FanProfileDTO>>();
let session: SessionUser | null;
let onUploadComplete: ((files: { publicUrl: string; name: string }[]) => void) | undefined;

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session, loading: false }),
  getActiveSession: () => session,
  getStoredSessionToken: () => null,
}));
jest.unstable_mockModule('../api/fans', () => ({
  Fans: {
    getMyArtistProfile: getArtistProfile,
    updateMyArtistProfile: updateArtistProfile,
    getProfile: getFanProfile,
    updateProfile: updateFanProfile,
    listArtists: jest.fn(async () => []),
    listFollows: jest.fn(async () => []),
    listMyClubs: jest.fn(async () => []),
    requestMyFanRole: jest.fn(),
    follow: jest.fn(),
    unfollow: jest.fn(),
  },
}));
jest.unstable_mockModule('../api/catalogs', () => ({
  Catalogs: {
    listPublicItems: jest.fn(async () => ({ items: [{
      id: '10000000-0000-4000-8000-000000000004',
      name: 'Indie', active: true, workflowState: 'published', sortOrder: 0,
    }] })),
  },
}));
jest.unstable_mockModule('../api/records', () => ({
  Records: { getFeed: jest.fn(async () => ({ collections: [], releases: [], recordings: [], sessions: [] })) },
}));
jest.unstable_mockModule('../api/admin', () => ({ Admin: { updateArtistRelease: jest.fn() } }));
jest.unstable_mockModule('../api/drive', () => ({ uploadToDrive: jest.fn() }));
jest.unstable_mockModule('../api/musicReleases', () => ({
  musicReleases: { listPublic: jest.fn(async () => []) },
}));
jest.unstable_mockModule('../hooks/useCmsContent', () => ({ useCmsContent: () => ({ data: undefined }) }));
jest.unstable_mockModule('../analytics/posthog', () => ({ getAnalyticsClient: () => ({ capture: jest.fn() }) }));
jest.unstable_mockModule('../analytics/onboardingProgress', () => ({ captureFirstValueOnce: jest.fn(async () => false) }));
jest.unstable_mockModule('../features/fans/useFanHubOnboarding', () => ({
  useFanHubOnboarding: () => ({ visible: false, loading: false, saving: false, saveError: false, loadError: false }),
}));
jest.unstable_mockModule('../features/releases/useReleaseFeed', () => ({
  useReleaseFeed: () => ({ hasReleaseTargets: false, releaseFeed: [], releaseFeedQuery: {
    isError: false, isFetching: false, isLoading: false, refetch: jest.fn(),
  } }),
}));
jest.unstable_mockModule('../features/discovery/CommunityDiscovery', () => ({ default: () => null }));
jest.unstable_mockModule('../components/GoogleDriveUploadWidget', () => ({
  default: ({ onComplete }: { onComplete: typeof onUploadComplete }) => {
    onUploadComplete = onComplete;
    return null;
  },
}));
jest.unstable_mockModule('../components/LazyPaginatedList', () => ({ default: () => null }));
jest.unstable_mockModule('../components/StreamingPlayer', () => ({ default: () => null }));
jest.unstable_mockModule('../features/releases/ReleaseFeed', () => ({ ReleaseFeed: () => null }));
jest.unstable_mockModule('../features/fans/FollowedArtists', () => ({ FollowedArtists: () => null }));
jest.unstable_mockModule('../features/fanclubs/FanClubPreview', () => ({ FanClubPreview: () => null }));

const { default: FanHubPage } = await import('./FanHubPage');

function deferred<T>() {
  let resolve!: (value: T) => void;
  let reject!: (reason: unknown) => void;
  const promise = new Promise<T>((complete, fail) => { resolve = complete; reject = fail; });
  return { promise, resolve, reject };
}

function artist(partyId = 42): ArtistProfileDTO {
  return {
    apArtistId: partyId,
    apDisplayName: `Artist ${partyId}`,
    apSlug: `artist-${partyId}`,
    apBio: `Biography ${partyId}`,
    apCity: 'Quito',
    apHeroImageUrl: `https://images.example.test/${partyId}.jpg`,
    apSpotifyUrl: `https://open.spotify.com/artist/artist${partyId}`,
    apYoutubeUrl: `https://www.youtube.com/@artist${partyId}`,
    apWebsiteUrl: `https://artist${partyId}.example.test`,
    apGenreIds: ['10000000-0000-4000-8000-000000000004'],
    apHighlights: `Highlights ${partyId}`,
    apFollowerCount: 3,
    apHasUserAccount: true,
  };
}

function fan(partyId = 42): FanProfileDTO {
  return { fpArtistId: partyId, fpDisplayName: `Fan ${partyId}`, fpBio: `Fan biography ${partyId}`,
    fpCity: 'Quito', fpAvatarUrl: `https://images.example.test/fan${partyId}.jpg`,
    fpFavoriteGenreIds: ['10000000-0000-4000-8000-000000000004'] };
}

function setAccount(partyId: number, role = 'Artist') {
  session = { username: `account-${partyId}`, displayName: `Account ${partyId}`, partyId, roles: [role], modules: [] };
}

const clients: QueryClient[] = [];
function renderPage() {
  const client = new QueryClient({ defaultOptions: {
    queries: { retry: false, gcTime: 0, staleTime: Infinity }, mutations: { retry: false },
  } });
  clients.push(client);
  const wrap = (children: ReactNode) => (
    <QueryClientProvider client={client}><MemoryRouter initialEntries={['/fans']}>
      {children}
    </MemoryRouter></QueryClientProvider>
  );
  const view = render(wrap(<FanHubPage focusArtist />));
  return { ...view, client, rerenderPage: () => view.rerender(wrap(<FanHubPage focusArtist />)) };
}

const artistSave = () => screen.getByRole('button', { name: 'Actualizar perfil de artista' });
const artistName = () => screen.getByLabelText<HTMLInputElement>('Nombre artístico');

beforeEach(() => {
  localStorage.clear();
  onUploadComplete = undefined;
  setAccount(42);
  getArtistProfile.mockReset().mockResolvedValue(artist());
  updateArtistProfile.mockReset().mockResolvedValue(artist());
  getFanProfile.mockReset().mockResolvedValue(fan());
  updateFanProfile.mockReset().mockResolvedValue(fan());
});

afterEach(() => {
  cleanup();
  clients.splice(0).forEach((client) => client.clear());
});

describe('artist profile writes require the current account baseline', () => {
  it('cannot replace a persisted profile with a blank draft while its initial read is pending', async () => {
    const pending = deferred<ArtistProfileDTO>();
    getArtistProfile.mockReturnValue(pending.promise);
    const view = renderPage();
    await waitFor(() => expect(getArtistProfile).toHaveBeenCalledTimes(1));
    await waitFor(() => expect(view.client.getQueryData(['catalog', 'public', 'genres', 'es'])).toBeDefined());

    expect(artistSave().hasAttribute('disabled')).toBe(true);
    fireEvent.click(artistSave());
    expect(updateArtistProfile).not.toHaveBeenCalled();
  });

  it('blocks a failed read, then retries and saves one edit without erasing the recovered fields', async () => {
    getArtistProfile.mockRejectedValueOnce(Object.assign(new Error('profile temporarily unavailable'), { status: 503 }));
    renderPage();
    await screen.findByText('No pudimos cargar tu perfil de artista.');
    expect(artistSave().hasAttribute('disabled')).toBe(true);
    fireEvent.click(artistSave());
    expect(updateArtistProfile).not.toHaveBeenCalled();

    const retry = screen.getAllByRole('button', { name: /reintentar|volver a cargar/i });
    fireEvent.click(retry[0]!);
    await waitFor(() => expect(artistName().value).toBe('Artist 42'));
    fireEvent.change(screen.getByLabelText('Ciudad'), { target: { value: 'Cuenca' } });
    fireEvent.click(artistSave());
    await waitFor(() => expect(updateArtistProfile).toHaveBeenCalledTimes(1));
    expect(updateArtistProfile.mock.calls[0]![0]).toMatchObject({
      apuArtistId: 42, apuDisplayName: 'Artist 42', apuCity: 'Cuenca',
      apuBio: 'Biography 42', apuHeroImageUrl: artist().apHeroImageUrl,
      apuSpotifyUrl: artist().apSpotifyUrl, apuYoutubeUrl: artist().apYoutubeUrl,
      apuWebsiteUrl: artist().apWebsiteUrl, apuHighlights: 'Highlights 42',
      apuGenreIds: artist().apGenreIds,
    });
  });

  it('clears account A fields immediately and forbids saving them to account B before B loads', async () => {
    const pendingB = deferred<ArtistProfileDTO>();
    getArtistProfile.mockResolvedValueOnce(artist()).mockReturnValue(pendingB.promise);
    const view = renderPage();
    await waitFor(() => expect(artistName().value).toBe('Artist 42'));
    fireEvent.change(screen.getByLabelText('Ciudad'), { target: { value: 'A unsaved city' } });

    setAccount(84);
    view.rerenderPage();
    expect(artistName().value).toBe('');
    expect(screen.getByLabelText<HTMLInputElement>('URL de portada (opcional)').value).toBe('');
    expect(artistSave().hasAttribute('disabled')).toBe(true);
    fireEvent.click(artistSave());
    expect(updateArtistProfile).not.toHaveBeenCalled();

    await act(async () => { pendingB.resolve(artist(84)); });
    await waitFor(() => expect(artistName().value).toBe('Artist 84'));
    fireEvent.change(screen.getByLabelText('Ciudad'), { target: { value: 'B new city' } });
    fireEvent.click(artistSave());
    await waitFor(() => expect(updateArtistProfile).toHaveBeenCalledTimes(1));
    expect(updateArtistProfile.mock.calls[0]![0]).toMatchObject({
      apuArtistId: 84, apuDisplayName: 'Artist 84', apuCity: 'B new city', apuBio: 'Biography 84',
    });
  });

  it('keeps a delayed A read from hydrating B after the account changes', async () => {
    const pendingA = deferred<ArtistProfileDTO>();
    const pendingB = deferred<ArtistProfileDTO>();
    getArtistProfile.mockReturnValueOnce(pendingA.promise).mockReturnValueOnce(pendingB.promise);
    const view = renderPage();
    await waitFor(() => expect(getArtistProfile).toHaveBeenCalledTimes(1));
    setAccount(84);
    view.rerenderPage();
    await waitFor(() => expect(getArtistProfile).toHaveBeenCalledTimes(2));
    await act(async () => { pendingA.resolve(artist()); });
    expect(artistName().value).toBe('');
    expect(artistSave().hasAttribute('disabled')).toBe(true);
    await act(async () => { pendingB.resolve(artist(84)); });
    await waitFor(() => expect(artistName().value).toBe('Artist 84'));
  });

  it('does not show an old account save receipt or overwrite the new account draft', async () => {
    const saveA = deferred<ArtistProfileDTO>();
    updateArtistProfile.mockReturnValue(saveA.promise);
    getArtistProfile.mockResolvedValueOnce(artist()).mockResolvedValue(artist(84));
    const view = renderPage();
    await waitFor(() => expect(artistName().value).toBe('Artist 42'));
    fireEvent.click(artistSave());
    await waitFor(() => expect(updateArtistProfile).toHaveBeenCalledTimes(1));

    setAccount(84);
    view.rerenderPage();
    await waitFor(() => expect(artistName().value).toBe('Artist 84'));
    fireEvent.change(screen.getByLabelText('Ciudad'), { target: { value: 'B unsaved city' } });
    await act(async () => { saveA.resolve(artist()); });
    expect(artistName().value).toBe('Artist 84');
    expect(screen.getByLabelText<HTMLInputElement>('Ciudad').value).toBe('B unsaved city');
    expect(screen.queryByText('Perfil de artista actualizado.')).toBeNull();
  });

  it('preserves unsaved edits when a background read refreshes the same artist profile', async () => {
    getArtistProfile.mockResolvedValueOnce(artist()).mockResolvedValue({ ...artist(), apCity: 'Server city' });
    const view = renderPage();
    await waitFor(() => expect(artistName().value).toBe('Artist 42'));
    fireEvent.change(screen.getByLabelText('Ciudad'), { target: { value: 'My unsaved city' } });
    await act(async () => { await view.client.refetchQueries({ type: 'active' }); });
    await act(async () => { await new Promise((resolve) => setTimeout(resolve, 0)); });
    expect(getArtistProfile).toHaveBeenCalledTimes(2);
    expect(screen.getByLabelText<HTMLInputElement>('Ciudad').value).toBe('My unsaved city');
    fireEvent.click(artistSave());
    await waitFor(() => expect(updateArtistProfile).toHaveBeenCalledTimes(1));
    expect(updateArtistProfile.mock.calls[0]![0].apuCity).toBe('My unsaved city');
  });

  it('rejects an upload completion from an earlier A editor after switching A to B and back to A', async () => {
    getArtistProfile.mockImplementation(async () => artist(session?.partyId ?? 0));
    const view = renderPage();
    await waitFor(() => expect(artistName().value).toBe('Artist 42'));
    const earlierUploadCompletion = onUploadComplete;
    expect(earlierUploadCompletion).toBeDefined();

    setAccount(84);
    view.rerenderPage();
    await waitFor(() => expect(artistName().value).toBe('Artist 84'));
    setAccount(42);
    view.rerenderPage();
    await waitFor(() => expect(artistName().value).toBe('Artist 42'));
    await act(async () => {
      earlierUploadCompletion!([{ publicUrl: 'https://images.example.test/stale-upload.jpg', name: 'old upload' }]);
    });
    expect(screen.getByLabelText<HTMLInputElement>('URL de portada (opcional)').value).toBe(artist().apHeroImageUrl);
    fireEvent.click(artistSave());
    await waitFor(() => expect(updateArtistProfile).toHaveBeenCalledTimes(1));
    expect(updateArtistProfile.mock.calls[0]![0].apuHeroImageUrl).toBe(artist().apHeroImageUrl);
  });

  it('keeps generating a new profile slug until the artist explicitly customizes it', async () => {
    getArtistProfile.mockResolvedValue({ ...artist(), apSlug: null });
    renderPage();
    await waitFor(() => expect(artistName().value).toBe('Artist 42'));
    fireEvent.change(artistName(), { target: { value: 'New' } });
    expect(screen.getByLabelText<HTMLInputElement>('Slug público').value).toBe('new');
    fireEvent.change(artistName(), { target: { value: 'New Artist Name' } });
    expect(screen.getByLabelText<HTMLInputElement>('Slug público').value).toBe('new-artist-name');
    fireEvent.change(screen.getByLabelText('Slug público'), { target: { value: 'custom-stage' } });
    fireEvent.change(artistName(), { target: { value: 'Changed Display Name' } });
    expect(screen.getByLabelText<HTMLInputElement>('Slug público').value).toBe('custom-stage');
  });

  it('does not disclose a late account A save error in account B', async () => {
    const saveA = deferred<ArtistProfileDTO>();
    updateArtistProfile.mockReturnValue(saveA.promise);
    getArtistProfile.mockResolvedValueOnce(artist()).mockResolvedValue(artist(84));
    const view = renderPage();
    await waitFor(() => expect(artistName().value).toBe('Artist 42'));
    fireEvent.click(artistSave());
    await waitFor(() => expect(updateArtistProfile).toHaveBeenCalledTimes(1));
    setAccount(84);
    view.rerenderPage();
    await waitFor(() => expect(artistName().value).toBe('Artist 84'));
    await act(async () => { saveA.reject(new Error('Account A private save failure')); });
    expect(artistName().value).toBe('Artist 84');
    expect(screen.queryByText('Account A private save failure')).toBeNull();
  });
});

describe('fan profile writes require the current account baseline', () => {
  it('cannot save blank fan fields while the persisted profile read is pending', async () => {
    setAccount(42, 'Fan');
    getFanProfile.mockReturnValue(deferred<FanProfileDTO>().promise);
    const view = renderPage();
    await waitFor(() => expect(getFanProfile).toHaveBeenCalledTimes(1));
    await waitFor(() => expect(view.client.getQueryData(['catalog', 'public', 'genres', 'es'])).toBeDefined());
    const save = screen.getByRole('button', { name: 'Guardar perfil' });
    expect(save.hasAttribute('disabled')).toBe(true);
    fireEvent.click(save);
    expect(updateFanProfile).not.toHaveBeenCalled();
  });

  it('does not carry account A fan fields into B while B has no successful baseline', async () => {
    setAccount(42, 'Fan');
    const pendingB = deferred<FanProfileDTO>();
    getFanProfile.mockResolvedValueOnce(fan()).mockReturnValue(pendingB.promise);
    const view = renderPage();
    await waitFor(() => expect(screen.getByLabelText<HTMLInputElement>('Nombre público').value).toBe('Fan 42'));
    setAccount(84, 'Fan');
    view.rerenderPage();
    expect(screen.getByLabelText<HTMLInputElement>('Nombre público').value).toBe('');
    const save = screen.getByRole('button', { name: 'Guardar perfil' });
    expect(save.hasAttribute('disabled')).toBe(true);
    fireEvent.click(save);
    expect(updateFanProfile).not.toHaveBeenCalled();
  });

  it('recovers a failed fan read before enabling a save and preserves all untouched persisted fields', async () => {
    setAccount(42, 'Fan');
    getFanProfile.mockRejectedValueOnce(Object.assign(new Error('fan profile temporarily unavailable'), { status: 503 }));
    renderPage();
    await waitFor(() => expect(screen.getAllByRole('button', { name: /reintentar|volver a cargar/i }).length).toBeGreaterThan(0));
    const save = screen.getByRole('button', { name: 'Guardar perfil' });
    expect(save.hasAttribute('disabled')).toBe(true);
    fireEvent.click(save);
    expect(updateFanProfile).not.toHaveBeenCalled();

    fireEvent.click(screen.getAllByRole('button', { name: /reintentar|volver a cargar/i })[0]!);
    await waitFor(() => expect(screen.getByLabelText<HTMLInputElement>('Nombre público').value).toBe('Fan 42'));
    fireEvent.change(screen.getByLabelText('Ciudad'), { target: { value: 'Cuenca' } });
    fireEvent.click(screen.getByRole('button', { name: 'Guardar perfil' }));
    await waitFor(() => expect(updateFanProfile).toHaveBeenCalledTimes(1));
    expect(updateFanProfile.mock.calls[0]![0]).toMatchObject({
      fpuDisplayName: 'Fan 42', fpuBio: 'Fan biography 42', fpuCity: 'Cuenca',
      fpuAvatarUrl: fan().fpAvatarUrl, fpuFavoriteGenreIds: fan().fpFavoriteGenreIds,
    });
  });

  it('preserves B fan edits when an A save succeeds late and then submits only B fields', async () => {
    setAccount(42, 'Fan');
    const saveA = deferred<FanProfileDTO>();
    updateFanProfile.mockReturnValueOnce(saveA.promise).mockResolvedValue(fan(84));
    getFanProfile.mockResolvedValueOnce(fan()).mockResolvedValue(fan(84));
    const view = renderPage();
    await waitFor(() => expect(screen.getByLabelText<HTMLInputElement>('Nombre público').value).toBe('Fan 42'));
    fireEvent.click(screen.getByRole('button', { name: 'Guardar perfil' }));
    await waitFor(() => expect(updateFanProfile).toHaveBeenCalledTimes(1));
    setAccount(84, 'Fan');
    view.rerenderPage();
    await waitFor(() => expect(screen.getByLabelText<HTMLInputElement>('Nombre público').value).toBe('Fan 84'));
    fireEvent.change(screen.getByLabelText('Ciudad'), { target: { value: 'B unsaved fan city' } });
    await act(async () => { saveA.resolve(fan()); });
    await waitFor(() => expect(screen.getByRole('button', { name: 'Guardar perfil' }).hasAttribute('disabled')).toBe(false));
    expect(screen.getByLabelText<HTMLInputElement>('Nombre público').value).toBe('Fan 84');
    expect(screen.getByLabelText<HTMLInputElement>('Ciudad').value).toBe('B unsaved fan city');
    fireEvent.click(screen.getByRole('button', { name: 'Guardar perfil' }));
    await waitFor(() => expect(updateFanProfile).toHaveBeenCalledTimes(2));
    expect(updateFanProfile.mock.calls[1]![0]).toMatchObject({
      fpuDisplayName: 'Fan 84', fpuBio: 'Fan biography 84', fpuCity: 'B unsaved fan city',
      fpuAvatarUrl: fan(84).fpAvatarUrl,
    });
  });
});

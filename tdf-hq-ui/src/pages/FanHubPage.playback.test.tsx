import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import type { ReleaseFeedItem } from '../features/releases/ReleasePlayerActions';

const session = { username: 'synthetic-fan', partyId: 42, roles: ['fan'], modules: [] };
let release: ReleaseFeedItem;
let artistFallback: string | null = null;
const events: CustomEvent[] = [];
const takeovers: Event[] = [];
const takeoverListener = (event: Event) => takeovers.push(event);
const listener = (event: Event) => events.push(event as CustomEvent);
const fixtureRelease = (): ReleaseFeedItem => ({ arArtistId: 7, arReleaseId: 42, arTitle: 'Luna Baja', artistName: 'Sofia Marquez',
  arReleaseDate: '2026-01-01', arDescription: null, arCoverImageUrl: null, arSpotifyUrl: 'https://open.spotify.com/track/synthetic', arYoutubeUrl: null });

jest.unstable_mockModule('react-i18next', () => ({ useTranslation: () => ({ t: (key: string) => key }) }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session, loading: false }), getActiveSession: () => session, getStoredSessionToken: () => null }));
jest.unstable_mockModule('../api/fans', () => ({ Fans: {
  listArtists: async () => [], listFollows: async () => [{ ffArtistId: 7, ffArtistName: 'Sofia Marquez', ffSpotifyUrl: artistFallback }],
  listMyClubs: async () => [], getMyArtistProfile: async () => null,
} }));
jest.unstable_mockModule('../api/records', () => ({ Records: { getFeed: async () => ({ collections: [], releases: [], recordings: [], sessions: [] }) } }));
jest.unstable_mockModule('../api/musicReleases', () => ({ musicReleases: { listPublic: async () => [] } }));
jest.unstable_mockModule('../api/catalogs', () => ({ Catalogs: { listPublicItems: async () => ({ items: [] }) } }));
jest.unstable_mockModule('../api/admin', () => ({ Admin: { updateArtistRelease: jest.fn() } }));
jest.unstable_mockModule('../api/drive', () => ({ uploadToDrive: jest.fn() }));
jest.unstable_mockModule('../hooks/useCmsContent', () => ({ useCmsContent: () => ({ data: undefined }) }));
jest.unstable_mockModule('../features/discovery/CommunityDiscovery', () => ({ default: () => null }));
jest.unstable_mockModule('../features/fans/useFanHubOnboarding', () => ({ useFanHubOnboarding: () => ({ visible: false, saving: false, saveError: false, loadError: false, loading: false, dismiss: jest.fn(), retryLoad: jest.fn() }) }));
jest.unstable_mockModule('../features/fans/useFanProfile', () => ({ useFanProfile: () => ({
  profileDraft: { fpuDisplayName: '', fpuBio: '', fpuCity: '', fpuFavoriteGenreIds: [], fpuAvatarUrl: '' },
  profileQuery: { data: undefined, isError: false, isLoading: false }, saveProfile: jest.fn(), setProfileDraft: jest.fn(), updateProfileMutation: { isPending: false },
}) }));
jest.unstable_mockModule('../features/releases/useReleaseFeed', () => ({ useReleaseFeed: () => ({ hasReleaseTargets: true, releaseFeed: [release],
  releaseFeedQuery: { isError: false, isFetching: false, isLoading: false, refetch: jest.fn() } }) }));
jest.unstable_mockModule('../analytics/posthog', () => ({ getAnalyticsClient: () => ({ capture: jest.fn() }) }));
jest.unstable_mockModule('../analytics/onboardingProgress', () => ({ captureFirstValueOnce: jest.fn() }));
jest.unstable_mockModule('../components/GoogleDriveUploadWidget', () => ({ default: () => null }));
jest.unstable_mockModule('../components/LazyPaginatedList', () => ({ default: () => null }));
// Existing embedded-provider rendering has its own media utility/component
// coverage; retain the real ReleaseFeed Escuchar button and FanHub handler.
jest.unstable_mockModule('../components/StreamingPlayer', () => ({ default: () => null }));
jest.unstable_mockModule('../features/interactions/InteractionPanel', () => ({ InteractionPanel: () => null }));
jest.unstable_mockModule('../features/fans/FanProfileEditor', () => ({ FanProfileEditor: () => null }));
jest.unstable_mockModule('../features/fans/FollowedArtists', () => ({ FollowedArtists: () => null }));
jest.unstable_mockModule('../features/fans/ProfileSectionCard', () => ({ ProfileSectionCard: () => null }));
jest.unstable_mockModule('../features/fanclubs/FanClubPreview', () => ({ FanClubPreview: () => null }));
jest.unstable_mockModule('../player/analytics', () => ({ installPlayerAnalyticsBridge: () => () => undefined }));
const { default: FanHubPage } = await import('./FanHubPage');
const { PlayerProvider, usePlayer } = await import('../player/PlayerProvider');
let player: ReturnType<typeof usePlayer>;
function PlaybackProbe() { player = usePlayer(); return null; }
const clients: QueryClient[] = [];
function mount(withPlayer = false) {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  clients.push(client);
  const page = <Routes><Route path="/fans" element={<FanHubPage />} /></Routes>;
  return render(<QueryClientProvider client={client}><MemoryRouter initialEntries={['/fans']}>
    {withPlayer ? <PlayerProvider><PlaybackProbe />{page}</PlayerProvider> : page}
  </MemoryRouter></QueryClientProvider>);
}
beforeEach(() => { release = fixtureRelease(); artistFallback = null; events.length = 0; takeovers.length = 0; localStorage.clear(); window.addEventListener('tdf-player-load-track', listener); window.addEventListener('tdf-radio-takeover', takeoverListener); });
afterEach(() => { cleanup(); jest.restoreAllMocks(); clients.splice(0).forEach((client) => client.clear()); window.removeEventListener('tdf-player-load-track', listener); window.removeEventListener('tdf-radio-takeover', takeoverListener); });

it.each([
  ['https://open.spotify.com/track/synthetic', 'Spotify'],
  ['https://youtube.com/watch?v=dQw4w9WgXcQ', 'YouTube'],
  ['https://drive.google.com/file/d/synthetic/view', 'Google Drive'],
])('routes the actual feed action for %s to a labeled provider link without a native selection', async (url, provider) => {
  release.arSpotifyUrl = url;
  const open = jest.spyOn(window, 'open').mockImplementation(() => null);
  mount();
  fireEvent.click(await screen.findByRole('button', { name: 'Escuchar', exact: true }));
  const link = await screen.findByRole('link', { name: `Abrir en ${provider} (nueva pestaña)` });
  expect(link.getAttribute('href')).toBe(url);
  expect(link.getAttribute('rel')).toBe('noopener noreferrer');
  expect(link.getAttribute('target')).toBe('_blank');
  expect(screen.getByText('La reproducción y sus controles estarán en la plataforma externa.')).toBeTruthy();
  expect(takeovers).toHaveLength(0);
  link.addEventListener('click', (event) => event.preventDefault(), { once: true });
  fireEvent.click(link);
  expect(open).not.toHaveBeenCalled();
  expect(takeovers).toHaveLength(1);
  expect(events).toHaveLength(0);
});

it.each([
  'https://media.example.invalid/object?signature=synthetic%2Fsignature&expires=1',
  'https://drive.google.com/uc?export=download&id=synthetic',
])('selects uploaded/direct media %s with stable identity and artist attribution', async (url) => {
  release.arSpotifyUrl = url;
  mount();
  fireEvent.click(await screen.findByRole('button', { name: 'Escuchar', exact: true }));
  expect(screen.queryByRole('dialog')).toBeNull();
  expect(events).toHaveLength(1);
  expect(events[0]?.detail.track).toMatchObject({ id: 'legacy-release-42', title: 'Luna Baja', artist: 'Sofia Marquez',
    sources: [{ url, quality: 'high', mediaType: 'audio/mpeg' }] });
});

it('identifies an artist-profile fallback as navigation to the artist instead of a selected release recording', async () => {
  release.arSpotifyUrl = null;
  artistFallback = 'https://open.spotify.com/artist/synthetic';
  mount();
  fireEvent.click(await screen.findByRole('button', { name: 'Escuchar', exact: true }));
  await screen.findByRole('dialog', { name: 'Perfil de Sofia Marquez en Spotify' });
  expect(screen.getByText('Este enlace abre el perfil del artista; no identifica una pista de este lanzamiento.')).toBeTruthy();
  expect(events).toHaveLength(0);
});


it.each(['javascript:alert(1)', 'https://open.spotify.com@evil.invalid/track/id'])('exposes no executable playback action for unsafe saved URL %s', async (url) => {
  release.arSpotifyUrl = url;
  release.arYoutubeUrl = url;
  mount();
  await screen.findByText('Novedades de tus artistas');
  expect(screen.queryByRole('button', { name: 'Escuchar', exact: true })).toBeNull();
  expect(screen.queryByRole('link', { name: 'Ver en YouTube' })).toBeNull();
  expect(screen.queryByRole('dialog')).toBeNull();
  expect(events).toHaveLength(0);
});


it('preserves the current selection and position, pausing only when the provider link is activated', async () => {
  // This is a JSDOM command/state integration test, not decoder evidence.
  jest.spyOn(HTMLMediaElement.prototype, 'load').mockImplementation(() => undefined);
  jest.spyOn(HTMLMediaElement.prototype, 'pause').mockImplementation(() => undefined);
  jest.spyOn(HTMLMediaElement.prototype, 'currentSrc', 'get').mockImplementation(function (this: HTMLMediaElement) { return this.src; });
  jest.spyOn(HTMLMediaElement.prototype, 'paused', 'get').mockReturnValue(false);
  jest.spyOn(HTMLMediaElement.prototype, 'play').mockImplementation(function (this: HTMLMediaElement) {
    this.dispatchEvent(new Event('playing')); return Promise.resolve();
  });
  mount(true);
  await act(async () => { player.playTrack({ id: 'already-selected', title: 'Existing authorized track', artist: 'Synthetic artist',
    sources: [{ url: 'https://media.example.invalid/existing', quality: 'high', mediaType: 'audio/mpeg' }] }); });
  act(() => player.seek(8));
  fireEvent.click(await screen.findByRole('button', { name: 'Escuchar', exact: true }));
  await screen.findByRole('dialog', { name: 'Escuchar en Spotify' });
  expect(player.status).toBe('playing');
  fireEvent.click(screen.getByRole('button', { name: 'Cerrar', exact: true }));
  expect(player.status).toBe('playing');
  expect(takeovers).toHaveLength(0);
  fireEvent.click(await screen.findByRole('button', { name: 'Escuchar', exact: true }));
  const link = await screen.findByRole('link', { name: 'Abrir en Spotify (nueva pestaña)' });
  link.addEventListener('click', (event) => event.preventDefault(), { once: true });
  fireEvent.click(link);
  expect(player.status).toBe('paused');
  expect(player.currentTrack?.id).toBe('already-selected');
  expect(player.currentTime).toBe(8);
  expect(takeovers).toHaveLength(1);
  expect(events).toHaveLength(0);
});

import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, useLocation } from 'react-router-dom';
import i18n from 'i18next';
import { initReactI18next } from 'react-i18next';
import es from '../i18n/locales/directorySearch.es';
import en from '../i18n/locales/directorySearch.en';
import type { MusicPublicTrackSearchHit } from '../api/musicReleases';

const directorySearch = jest.fn<() => Promise<{ items: []; sponsoredItems: []; facets: { total: number; entityTypes: object; cities: [] } }>>();
const tracks = Array.from({ length: 13 }, (_, n): MusicPublicTrackSearchHit => ({ trackId: `track-${n}`, recordingId: `recording-${n}`,
  releaseId: 'release', releaseVersionId: 'version', releaseSlug: 'synthetic-release', releaseTitle: 'Lanzamiento', title: `Pista ${n}`,
  displayArtist: 'Artista', durationMs: 120000, sources: [{ assetId: `asset-${n}`, role: 'stream_audio', mediaType: 'audio/mp4', technicalMetadata: {} }] }));
const searchTracks = jest.fn(async (_query: string, page?: { offset?: number }) => page?.offset ? [tracks[12]!] : tracks);
jest.unstable_mockModule('../api/musicReleases', () => ({ musicReleases: { searchTracks, listPublic: jest.fn(async () => []) } }));
jest.unstable_mockModule('../api/directory', () => ({ Directory: { search: directorySearch, suggestions: jest.fn(async () => []),
  taxonomies: jest.fn(async () => ({ locale: 'es', professions: [], classifiedCategories: [], compensationTypes: [], serviceOfferings: [], currencies: [], instruments: [], genres: [], cities: [] })),
  favorites: jest.fn(async () => []), addFavorite: jest.fn(), removeFavorite: jest.fn() } }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: null }), getActiveSession: () => null, getStoredSessionToken: () => null }));
jest.unstable_mockModule('../hooks/useMetaTags', () => ({ useMetaTags: jest.fn() }));
jest.unstable_mockModule('../analytics/posthog', () => ({ getAnalyticsClient: () => null }));
jest.unstable_mockModule('../analytics/onboardingProgress', () => ({ captureFirstValueOnce: jest.fn() }));
jest.unstable_mockModule('../components/directory/OpenStreetMapResults', () => ({ default: () => <div>Map fixture</div> }));
jest.unstable_mockModule('../api/client', () => ({ API_BASE_URL: 'https://api.tdfrecords.net' }));
const { default: DirectorySearchPage } = await import('./DirectorySearchPage');
await i18n.use(initReactI18next).init({ lng: 'es', fallbackLng: 'es', interpolation: { escapeValue: false },
  resources: { es: { translation: { directorySearch: es } }, en: { translation: { directorySearch: en } } } });
const clients: QueryClient[] = [];
function Location() { const location = useLocation(); return <output data-testid="location">{location.pathname}{location.search}</output>; }
function mount(path: string) {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } }); clients.push(client);
  return render(<QueryClientProvider client={client}><MemoryRouter initialEntries={[path]}><Location /><DirectorySearchPage /></MemoryRouter></QueryClientProvider>);
}
beforeEach(async () => {
  await i18n.changeLanguage('es'); directorySearch.mockReset().mockResolvedValue({ items: [], sponsoredItems: [], facets: { total: 0, entityTypes: {}, cities: [] } });
  searchTracks.mockReset().mockImplementation(async (_query, page) => page?.offset ? [tracks[12]!] : tracks);
});
afterEach(() => { cleanup(); clients.splice(0).forEach((c) => c.clear()); });

it('keeps music playable when directory fails and music pagination does not rewrite directory URL filters', async () => {
  directorySearch.mockRejectedValue(new Error('directory down'));
  mount('/buscar?q=Pista&remote=true');
  const play = await screen.findByRole('button', { name: 'Reproducir Pista 1' });
  expect(await screen.findByText(es.searchError)).toBeTruthy();
  expect(screen.getByRole('region', { name: 'Resultados musicales' })).toBeTruthy();
  await act(async () => { play.focus(); }); expect(document.activeElement).toBe(play); expect(play.tagName).toBe('BUTTON');
  expect(getComputedStyle(play).minHeight).toBe('44px');
  fireEvent.click(screen.getByRole('button', { name: 'Página musical siguiente' }));
  await screen.findByRole('button', { name: 'Reproducir Pista 12' });
  expect(screen.getByTestId('location').textContent).toBe('/buscar?q=Pista&remote=true');
});

it('scopes the directory empty message without hiding authorized music', async () => {
  mount('/buscar?q=Pista');
  await screen.findByRole('button', { name: 'Reproducir Pista 1' });
  expect(await screen.findByText(es.music.directoryEmpty)).toBeTruthy();
  expect(screen.queryByText(es.emptyTitle)).toBeNull();
});

it('removes old music actions immediately while the actual search field is still debouncing', async () => {
  mount('/buscar?q=Pista&remote=true');
  await screen.findByRole('button', { name: 'Reproducir Pista 1' });
  fireEvent.change(screen.getByRole('combobox', { name: es.queryLabel }), { target: { value: 'Nueva' } });
  expect(screen.queryByRole('button', { name: 'Reproducir Pista 1' })).toBeNull();
  expect(screen.queryByRole('link', { name: 'Ver lanzamiento Lanzamiento' })).toBeNull();
  expect(searchTracks.mock.calls.some(([query]) => query === 'Nueva')).toBe(false);
  await waitFor(() => expect(searchTracks).toHaveBeenLastCalledWith('Nueva', expect.objectContaining({ offset: 0, limit: 13, signal: expect.any(AbortSignal) })));
  expect(screen.getByTestId('location').textContent).toBe('/buscar?q=Nueva&remote=true');
});

it('keeps the directory empty state independent from a failed music request and retries only music', async () => {
  searchTracks.mockRejectedValueOnce(new Error('music unavailable'));
  mount('/buscar?q=Pista');
  await screen.findByText(es.music.error);
  expect(screen.getByText(es.music.directoryEmpty)).toBeTruthy();
  const directoryCalls = directorySearch.mock.calls.length;
  fireEvent.click(screen.getByRole('button', { name: es.music.retry }));
  await screen.findByRole('button', { name: 'Reproducir Pista 1' });
  expect(directorySearch.mock.calls).toHaveLength(directoryCalls);
});

it.each(['/inicio?q=Pista', '/inicio/?q=Pista', '/Inicio?q=Pista'])('never mounts or queries the new region on %s', async (path) => {
  mount(path);
  await screen.findByRole('heading', { name: 'Bienvenido a TDF Records' });
  expect(screen.queryByRole('region', { name: 'Resultados musicales' })).toBeNull();
  expect(searchTracks).not.toHaveBeenCalled();
});

it('shows the music prompt without making a blank-query request', async () => {
  mount('/buscar');
  expect(await screen.findByText(es.music.prompt)).toBeTruthy();
  expect(searchTracks).not.toHaveBeenCalled();
});

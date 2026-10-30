import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import i18n from 'i18next';
import { initReactI18next } from 'react-i18next';
import es from '../../i18n/locales/directorySearch.es';
import en from '../../i18n/locales/directorySearch.en';
import type { MusicPublicTrackSearchHit } from '../../api/musicReleases';
import { PLAYER_LOAD_TRACK_EVENT } from '../../player/types';

interface Request { limit?: number; offset?: number; signal?: AbortSignal }
const searchTracks = jest.fn<(query: string, page?: Request) => Promise<MusicPublicTrackSearchHit[]>>();
const getPublic = jest.fn();
const getAssetAccess = jest.fn();
jest.unstable_mockModule('../../api/musicReleases', () => ({ musicReleases: { searchTracks, getPublic, getAssetAccess } }));
const { default: MusicSearchResults } = await import('./MusicSearchResults');
await i18n.use(initReactI18next).init({ lng: 'es', fallbackLng: 'es', interpolation: { escapeValue: false },
  resources: { es: { translation: { directorySearch: es } }, en: { translation: { directorySearch: en } } } });

const hit = (index: number): MusicPublicTrackSearchHit => ({ trackId: `track-${index}`, recordingId: `recording-${index}`,
  releaseId: `release-${index}`, releaseVersionId: `version-${index}`, releaseSlug: `release-${index}`, releaseTitle: `Álbum ${index}`,
  title: `Pista ${index}`, displayArtist: 'Artista', durationMs: 90000,
  sources: [{ assetId: `asset-${index}`, role: 'stream_audio', mediaType: 'audio/mp4', technicalMetadata: { bitrate_kbps: 160 } }] });
const clients: QueryClient[] = [];
function mount(query = 'Pista', changingQuery = false) {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } }); clients.push(client);
  const tree = (q: string, changing: boolean) => <QueryClientProvider client={client}><MemoryRouter>
    <MusicSearchResults query={q} changingQuery={changing} /></MemoryRouter></QueryClientProvider>;
  const view = render(tree(query, changingQuery));
  return { ...view, update: (q: string, changing = false) => view.rerender(tree(q, changing)) };
}
beforeEach(async () => { await i18n.changeLanguage('es'); searchTracks.mockReset().mockResolvedValue([hit(1), hit(2)]); getPublic.mockClear(); getAssetAccess.mockClear(); });
afterEach(() => { cleanup(); clients.splice(0).forEach((c) => c.clear()); });

it('does not query blank text or autoplay and selects exact B synchronously with deferred page sources', async () => {
  const commands: CustomEvent[] = [];
  const listener = (event: Event) => commands.push(event as CustomEvent);
  window.addEventListener(PLAYER_LOAD_TRACK_EVENT, listener);
  try {
    const view = mount('   ');
    expect(searchTracks).not.toHaveBeenCalled();
    view.update('Pista');
    const play = await screen.findByRole('button', { name: 'Reproducir Pista 2' });
    expect(commands).toHaveLength(0);
    expect(getPublic).not.toHaveBeenCalled(); expect(getAssetAccess).not.toHaveBeenCalled();
    fireEvent.click(play);
    expect(commands).toHaveLength(1);
    expect(commands[0]?.detail).toMatchObject({ autoplay: true, track: { id: 'track-2', recordingId: 'recording-2', releaseId: 'release-2',
      releaseVersionId: 'version-2', sources: [{ url: '', assetId: 'asset-2' }] }, queue: [{ id: 'track-1' }, { id: 'track-2' }] });
    expect(screen.getByRole('link', { name: 'Ver lanzamiento Álbum 2' }).getAttribute('href')).toBe('/musica/release-2');
    expect(searchTracks).toHaveBeenCalledWith('Pista', expect.objectContaining({ limit: 13, offset: 0, signal: expect.any(AbortSignal) }));
  } finally { window.removeEventListener(PLAYER_LOAD_TRACK_EVENT, listener); }
});

it('hides old actions immediately during a draft change, aborts superseded queries and rejects late A', async () => {
  let finishA: (hits: MusicPublicTrackSearchHit[]) => void = () => undefined;
  let signalA: AbortSignal | undefined;
  searchTracks.mockImplementation((q, page) => q === 'A' ? (signalA = page?.signal, new Promise((resolve) => { finishA = resolve; })) : Promise.resolve([hit(2)]));
  const view = mount('A');
  await waitFor(() => expect(searchTracks).toHaveBeenCalled());
  view.update('B');
  await screen.findByRole('button', { name: 'Reproducir Pista 2' });
  expect(signalA?.aborted).toBe(true);
  await act(async () => { finishA([hit(1)]); });
  expect(screen.queryByRole('button', { name: 'Reproducir Pista 1' })).toBeNull();
  view.update('B', true);
  expect(screen.queryByRole('button', { name: 'Reproducir Pista 2' })).toBeNull();
  expect(screen.queryByRole('link', { name: 'Ver lanzamiento Álbum 2' })).toBeNull();
});

it('fetches thirteen, displays twelve, paginates independently and resets to page one for each query change', async () => {
  searchTracks.mockImplementation((_q, page) => Promise.resolve(Array.from({ length: page?.offset ? 1 : 13 }, (_, n) => hit((page?.offset ?? 0) + n + 1))));
  const view = mount('A');
  await screen.findByRole('button', { name: 'Reproducir Pista 12' });
  expect(screen.queryByRole('button', { name: 'Reproducir Pista 13' })).toBeNull();
  fireEvent.click(screen.getByRole('button', { name: 'Página musical siguiente' }));
  await screen.findByRole('button', { name: 'Reproducir Pista 13' });
  expect(searchTracks).toHaveBeenLastCalledWith('A', expect.objectContaining({ offset: 12 }));
  expect(screen.getByRole('button', { name: 'Página musical siguiente' }).hasAttribute('disabled')).toBe(true);
  view.update('B'); await screen.findByRole('button', { name: 'Reproducir Pista 1' });
  view.update('A'); await screen.findByRole('button', { name: 'Reproducir Pista 1' });
  expect(searchTracks).toHaveBeenLastCalledWith('A', expect.objectContaining({ offset: 0 }));
});

it('keeps unavailable errors distinct from empty results and supports explicit retry in English', async () => {
  await i18n.changeLanguage('en'); searchTracks.mockRejectedValueOnce(new Error('disabled')).mockResolvedValue([]);
  mount('Song');
  expect((await screen.findByRole('alert')).textContent).toContain(en.music.error);
  expect(screen.queryByText(en.music.empty)).toBeNull();
  fireEvent.click(screen.getByRole('button', { name: 'Retry music search' }));
  await screen.findByText(en.music.empty);
  expect(screen.getByRole('region', { name: 'Music results' })).toBeTruthy();
});

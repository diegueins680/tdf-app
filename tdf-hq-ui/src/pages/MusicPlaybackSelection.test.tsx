import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import type { LoadPlayerTrackDetail } from '../player/types';

const assetAccess = jest.fn<(assetId: string) => Promise<{ url: string; mediaType: string; expiresAt: string }>>();
const sources = (id: string) => [
  { assetId: `${id}-full`, role: 'stream_audio' as const, mediaType: 'audio/mp4', technicalMetadata: { bitrate_kbps: 256 } },
  { assetId: `${id}-preview`, role: 'preview_audio' as const, mediaType: 'audio/mp4', technicalMetadata: { bitrate_kbps: 96 } },
];
const tracks = ['first', 'second'].map((id, index) => ({
  trackId: id, recordingId: `${id}-recording`, discNumber: 1, trackNumber: index + 1,
  title: `Pista ${id}`, displayArtist: 'Artista', durationMs: 60000,
  explicitContent: 'not_explicit', sources: sources(id),
}));
const release = { id: 'release', versionId: 'version', slug: 'album', kind: 'album',
  title: 'Álbum', displayArtist: 'Artista', coverAssets: [], tracks, availability: [] };
const favorites = tracks.map((track) => ({ ...track, available: true, releaseId: 'release', releaseVersionId: 'version' }));

jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: { partyId: 101 } }) }));
jest.unstable_mockModule('../api/musicReleases', () => ({ musicReleases: {
  getPublic: async () => release,
  listFavorites: async () => favorites,
  listPlaylists: async () => [],
  playbackHistory: async () => [],
  getAssetAccess: assetAccess,
} }));

const { default: MusicReleasePublicPage } = await import('./MusicReleasePublicPage');
const { default: MusicLibraryPage } = await import('./MusicLibraryPage');
const events: LoadPlayerTrackDetail[] = [];
const listener = (event: Event) => events.push((event as CustomEvent<LoadPlayerTrackDetail>).detail);
const clients: QueryClient[] = [];
beforeEach(() => {
  events.length = 0;
  assetAccess.mockReset().mockImplementation(() => new Promise(() => undefined));
  window.addEventListener('tdf-player-load-track', listener);
});
afterEach(() => {
  cleanup();
  clients.splice(0).forEach((client) => client.clear());
  window.removeEventListener('tdf-player-load-track', listener);
});
function mount(path: string) {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  clients.push(client);
  return render(<QueryClientProvider client={client}><MemoryRouter initialEntries={[path]}>
    <Routes><Route path="/musica/album" element={<MusicReleasePublicPage />} />
      <Route path="/musica/biblioteca" element={<MusicLibraryPage />} /></Routes>
  </MemoryRouter></QueryClientProvider>);
}

it('selects the requested release track immediately and defers queue authorization to the engine', async () => {
  mount('/musica/album');
  const buttons = await screen.findAllByRole('button', { name: 'Reproducir', exact: true });
  fireEvent.click(buttons[1]!);
  expect(events).toHaveLength(1);
  expect(events[0]?.track.id).toBe('second');
  expect(events[0]?.queue?.map((track) => track.id)).toEqual(['first', 'second']);
  expect(events[0]?.track.sources).toEqual([
    { assetId: 'second-full', url: '', quality: 'high', mediaType: 'audio/mp4', preview: false },
    { assetId: 'second-preview', url: '', quality: 'low', mediaType: 'audio/mp4', preview: true },
  ]);
  expect(events[0]?.track.previewEndMs).toBe(30000);
  expect(assetAccess).not.toHaveBeenCalled();
});

it('cannot publish a late old-page selection after another surface selects a track', async () => {
  const pending: (() => void)[] = [];
  assetAccess.mockImplementation((assetId) => new Promise((resolve) => pending.push(() => resolve({
    url: `https://media.invalid/${assetId}`, mediaType: 'audio/mp4', expiresAt: '2099-01-01T00:00:00Z',
  }))));
  const view = mount('/musica/album');
  fireEvent.click(await screen.findByRole('button', { name: 'Reproducir release' }));
  const latest = { id: 'external', title: 'External', artist: 'Artista', sources: [{ url: 'https://media.invalid/external', mediaType: 'audio/mp4', quality: 'high' as const }] };
  window.dispatchEvent(new CustomEvent('tdf-player-load-track', { detail: { track: latest } }));
  view.unmount();
  await act(async () => { pending.forEach((resolve) => resolve()); });
  expect(events.map((event) => event.track.id)).toEqual(['first', 'external']);
});

it('selects library favorites without authorizing every queued rendition first', async () => {
  mount('/musica/biblioteca');
  const button = await screen.findByRole('button', { name: 'Reproducir favoritos' });
  // The enabled state reflects the loaded library snapshot.
  await act(async () => undefined);
  fireEvent.click(button);
  expect(events).toHaveLength(1);
  expect(events[0]?.track.id).toBe('first');
  expect(events[0]?.queue?.map((track) => track.id)).toEqual(['first', 'second']);
  expect(events[0]?.queue?.every((track) => track.sources.every((source) => source.url === '' && Boolean(source.assetId)))).toBe(true);
  expect(assetAccess).not.toHaveBeenCalled();
});

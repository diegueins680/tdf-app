import { jest } from '@jest/globals';
import { act, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, Route, Routes, useLocation } from 'react-router-dom';
import { readSafeRedirectPath } from '../utils/loginRouting';

let authenticated = false;
let sessionPartyId = 101;
const listFavorites = jest.fn<() => Promise<{ recordingId: string }[]>>();
const unfavorite = jest.fn<() => Promise<void>>();
beforeEach(() => { authenticated = false; sessionPartyId = 101; listFavorites.mockReset().mockResolvedValue([]); unfavorite.mockReset().mockResolvedValue(undefined); });
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: authenticated ? { partyId: sessionPartyId } : null }) }));
jest.unstable_mockModule('../api/musicReleases', () => ({ musicReleases: {
  listFavorites, unfavorite, listPlaylists: async () => [],
  getPublic: async () => ({ id: 'release', versionId: 'version', slug: 'single-propio', kind: 'single',
    title: 'Single propio', displayArtist: 'Artista', coverAssets: [],
    tracks: [{ trackId: 'track', recordingId: 'recording', trackNumber: 1, title: 'Pista propia',
      displayArtist: 'Artista', durationMs: 62000, explicitContent: 'not_explicit', sources: [] }],
    availability: [{ ruleId: 'free', trackId: null, downloadPolicy: 'free' }] }),
} }));

const { default: MusicReleasePublicPage } = await import('./MusicReleasePublicPage');
function LoginTarget() {
  const location = useLocation();
  return <div data-testid="login-target">{readSafeRedirectPath(location.search) ?? 'missing redirect'}</div>;
}

it.each(['Guardar Pista propia en favoritos', 'Añadir Pista propia a una playlist', 'Descargar', 'Reportar una infracción'])(
  '%s preserves the release using the canonical safe login redirect', async (action) => {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
    const view = render(<QueryClientProvider client={client}><MemoryRouter initialEntries={['/musica/single-propio']}>
      <Routes><Route path="/musica/:slug" element={<MusicReleasePublicPage />} /><Route path="/login" element={<LoginTarget />} /></Routes>
    </MemoryRouter></QueryClientProvider>);
    try {
      fireEvent.click(await screen.findByRole('button', { name: action, exact: true }));
      expect((await screen.findByTestId('login-target')).textContent).toBe('/musica/single-propio');
    } finally { view.unmount(); client.clear(); }
  },
);

it('waits for the favorite snapshot and mutation before allowing another toggle', async () => {
  authenticated = true;
  let resolveSnapshot: (rows: { recordingId: string }[]) => void = () => undefined;
  listFavorites.mockImplementationOnce(() => new Promise((resolve) => { resolveSnapshot = resolve; }));
  let resolveRemoval: () => void = () => undefined;
  unfavorite.mockImplementationOnce(() => new Promise((resolve) => { resolveRemoval = resolve; }));
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  const view = render(<QueryClientProvider client={client}><MemoryRouter initialEntries={['/musica/single-propio']}>
    <Routes><Route path="/musica/:slug" element={<MusicReleasePublicPage />} /></Routes>
  </MemoryRouter></QueryClientProvider>);
  try {
    const pending = await screen.findByRole('button', { name: 'Guardar Pista propia en favoritos' });
    expect(pending.hasAttribute('disabled')).toBe(true);
    await act(async () => { resolveSnapshot([{ recordingId: 'recording' }]); });
    const remove = await screen.findByRole('button', { name: 'Quitar Pista propia de favoritos' });
    await waitFor(() => expect(remove.hasAttribute('disabled')).toBe(false));
    fireEvent.click(remove);
    await waitFor(() => expect(remove.hasAttribute('disabled')).toBe(true));
    expect(unfavorite).toHaveBeenCalledTimes(1);
    await act(async () => { resolveRemoval(); });
    const saved = await screen.findByRole('button', { name: 'Guardar Pista propia en favoritos' });
    await waitFor(() => expect(saved.hasAttribute('disabled')).toBe(false));
    expect(listFavorites).toHaveBeenCalledTimes(2);
  } finally { view.unmount(); client.clear(); }
});

it('does not reuse another account favorite snapshot when the session changes', async () => {
  authenticated = true;
  listFavorites.mockResolvedValueOnce([{ recordingId: 'recording' }]);
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  const element = () => <QueryClientProvider client={client}><MemoryRouter initialEntries={['/musica/single-propio']}>
    <Routes><Route path="/musica/:slug" element={<MusicReleasePublicPage />} /></Routes>
  </MemoryRouter></QueryClientProvider>;
  const view = render(element());
  try {
    await screen.findByRole('button', { name: 'Quitar Pista propia de favoritos' });
    sessionPartyId = 202;
    view.rerender(element());
    await screen.findByRole('button', { name: 'Guardar Pista propia en favoritos' });
    await waitFor(() => expect(listFavorites).toHaveBeenCalledTimes(2));
  } finally { view.unmount(); client.clear(); }
});

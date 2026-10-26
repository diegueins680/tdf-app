import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, Route, Routes, useLocation, useNavigate } from 'react-router-dom';
import type { MusicPublicReleaseSummary } from '../api/musicReleases';

interface PageRequest { limit: number; offset: number; signal: AbortSignal }
const listPublic = jest.fn<(query?: string, artistPartyId?: number, page?: PageRequest) => Promise<MusicPublicReleaseSummary[]>>();
jest.unstable_mockModule('../api/musicReleases', () => ({ musicReleases: { listPublic, getAssetAccess: jest.fn() } }));
const { default: MusicCatalogPage } = await import('./MusicCatalogPage');
const releases = Array.from({ length: 49 }, (_, index): MusicPublicReleaseSummary => ({
  id: `release-${index + 1}`, artistPartyId: 1, slug: `release-${index + 1}`, kind: 'single',
  versionId: `version-${index + 1}`, versionNumber: 1, title: `Lanzamiento ${index + 1}`,
  displayArtist: 'Artista', releaseAtUtc: null, labelName: null, publishedAt: '2026-01-01T00:00:00Z', coverAssetId: null,
}));
const clients: QueryClient[] = [];
function NavigationProbe() {
  const location = useLocation();
  const navigate = useNavigate();
  return <><output data-testid="location">{location.pathname}{location.search}</output><button onClick={() => navigate(-1)}>Volver en historial</button></>;
}
function mount(path = '/musica') {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  clients.push(client);
  return render(<QueryClientProvider client={client}><MemoryRouter initialEntries={[path]}>
    <NavigationProbe /><Routes><Route path="/musica" element={<MusicCatalogPage />} />
      <Route path="/musica/:slug" element={<div>Detalle del lanzamiento</div>} /></Routes>
  </MemoryRouter></QueryClientProvider>);
}
beforeEach(() => {
  listPublic.mockReset().mockImplementation(async (_query, _artist, page) => releases.slice(page?.offset ?? 0, (page?.offset ?? 0) + (page?.limit ?? 24)));
});
afterEach(() => { cleanup(); clients.splice(0).forEach((client) => client.clear()); });

it('makes release 25 reachable and preserves the query through forward and browser-back navigation', async () => {
  mount('/musica?q=Álbum');
  await screen.findByRole('heading', { name: 'Lanzamiento 1', exact: true });
  expect(screen.queryByRole('heading', { name: 'Lanzamiento 25', exact: true })).toBeNull();
  fireEvent.click(screen.getByRole('button', { name: 'Página siguiente' }));
  await screen.findByRole('heading', { name: 'Lanzamiento 25', exact: true });
  expect(screen.queryByRole('heading', { name: 'Lanzamiento 1', exact: true })).toBeNull();
  expect(listPublic).toHaveBeenLastCalledWith('Álbum', undefined, expect.objectContaining({ limit: 25, offset: 24 }));
  expect(screen.getByTestId('location').textContent).toBe('/musica?q=%C3%81lbum&page=2');
  fireEvent.click(screen.getByRole('link', { name: /Lanzamiento 25 Artista SINGLE/ }));
  await screen.findByText('Detalle del lanzamiento');
  fireEvent.click(screen.getByRole('button', { name: 'Volver en historial' }));
  await screen.findByRole('heading', { name: 'Lanzamiento 25', exact: true });
  fireEvent.click(screen.getByRole('button', { name: 'Volver en historial' }));
  await screen.findByRole('heading', { name: 'Lanzamiento 1', exact: true });
  expect(screen.getByRole('textbox', { name: 'Buscar por lanzamiento o artista' }).getAttribute('value')).toBe('Álbum');
});

it('resets pagination on a changed query and rejects a late response from the previous query', async () => {
  let resolveOld: (rows: MusicPublicReleaseSummary[]) => void = () => undefined;
  let oldSignal: AbortSignal | undefined;
  listPublic.mockImplementation((query, _artist, page) => {
    if (query === 'anterior') {
      oldSignal = page?.signal;
      return new Promise((resolve) => { resolveOld = resolve; });
    }
    return Promise.resolve([{ ...releases[0]!, id: 'new', title: 'Nuevo resultado' }]);
  });
  mount('/musica?q=anterior&page=2');
  await waitFor(() => expect(listPublic).toHaveBeenCalled());
  fireEvent.change(screen.getByRole('textbox', { name: 'Buscar por lanzamiento o artista' }), { target: { value: 'nuevo' } });
  await screen.findByRole('heading', { name: 'Nuevo resultado', exact: true });
  expect(screen.getByTestId('location').textContent).toBe('/musica?q=nuevo');
  expect(listPublic).toHaveBeenLastCalledWith('nuevo', undefined, expect.objectContaining({ limit: 25, offset: 0 }));
  expect(oldSignal?.aborted).toBe(true);
  await act(async () => { resolveOld([{ ...releases[24]!, title: 'Resultado obsoleto' }]); });
  expect(screen.queryByRole('heading', { name: 'Resultado obsoleto' })).toBeNull();
  expect(screen.getByRole('heading', { name: 'Nuevo resultado' })).toBeTruthy();
});

it('disables next on a terminal page, retains previous on an empty page, and retries a failed request', async () => {
  listPublic.mockRejectedValueOnce(new Error('synthetic unavailable'));
  mount('/musica?page=4');
  fireEvent.click(await screen.findByRole('button', { name: 'Reintentar catálogo' }));
  await screen.findByText('No hay lanzamientos en esta página. Vuelve a la anterior.');
  expect(screen.getByRole('button', { name: 'Página siguiente' }).hasAttribute('disabled')).toBe(true);
  expect(screen.getByRole('button', { name: 'Página anterior' }).hasAttribute('disabled')).toBe(false);
  fireEvent.click(screen.getByRole('button', { name: 'Página anterior' }));
  await screen.findByRole('heading', { name: 'Lanzamiento 49', exact: true });
  expect(screen.getByRole('button', { name: 'Página siguiente' }).hasAttribute('disabled')).toBe(true);
});

it.each(['-1', '0', 'NaN', '1.5', '9007199254740992'])('treats invalid page %s as the first page and deduplicates repeated IDs', async (page) => {
  listPublic.mockResolvedValue([releases[0]!, releases[0]!]);
  mount(`/musica?page=${page}`);
  await screen.findByRole('heading', { name: 'Lanzamiento 1', exact: true });
  expect(screen.getAllByRole('heading', { name: 'Lanzamiento 1', exact: true })).toHaveLength(1);
  expect(listPublic).toHaveBeenCalledWith('', undefined, expect.objectContaining({ offset: 0 }));
  expect(screen.getByRole('button', { name: 'Página anterior' }).hasAttribute('disabled')).toBe(true);
});

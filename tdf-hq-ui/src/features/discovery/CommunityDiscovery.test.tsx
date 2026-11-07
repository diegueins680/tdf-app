import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { DirectorySearchQuery, DirectorySearchResponse } from '../../api/directory';

const response = (title: string): DirectorySearchResponse => ({ items: [{ id: 'artist-42', type: 'profile', slug: '42', canonicalPath: '/artista/42', title, location: {}, taxonomy: { genreIds: [], professionIds: [], serviceIds: [], instrumentIds: [] }, score: 1, scoreBreakdown: {}, sponsored: false }], sponsoredItems: [], facets: { total: 1, entityTypes: { profile: 1 }, cities: [] } });
const search = jest.fn<(query: DirectorySearchQuery, signal?: AbortSignal) => Promise<DirectorySearchResponse>>();
jest.unstable_mockModule('../../api/directory', () => ({ Directory: { search, taxonomies: async () => ({ genres: [], cities: [] }) } }));
const { default: CommunityDiscovery } = await import('./CommunityDiscovery');
const { directoryResultPath } = await import('./useDirectorySearch');

afterEach(() => { cleanup(); jest.useRealTimers(); });
beforeEach(() => { search.mockReset(); search.mockResolvedValue(response('Initial')); });
function mount(path = '/fans') {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  render(<QueryClientProvider client={client}><MemoryRouter initialEntries={[path]}><CommunityDiscovery /></MemoryRouter></QueryClientProvider>);
  return client;
}

test('debounces input, cancels obsolete requests and ignores an out-of-order response', async () => {
  let finishOld: ((value: DirectorySearchResponse) => void) | undefined;
  let oldSignal: AbortSignal | undefined;
  search.mockImplementation((query, signal) => {
    if (query.q === 'old') { oldSignal = signal; return new Promise((resolve) => { finishOld = resolve; }); }
    return Promise.resolve(response(query.q === 'new' ? 'Newest artist' : 'Initial'));
  });
  mount();
  await screen.findByText('Initial');
  jest.useFakeTimers();
  const input = screen.getByLabelText('Buscar artistas y perfiles');
  fireEvent.change(input, { target: { value: 'o' } });
  fireEvent.change(input, { target: { value: 'old' } });
  await act(async () => { jest.advanceTimersByTime(299); });
  expect(search).toHaveBeenCalledTimes(1);
  await act(async () => { jest.advanceTimersByTime(1); });
  expect(search.mock.calls.at(-1)?.[0].q).toBe('old');
  fireEvent.change(input, { target: { value: 'new' } });
  await act(async () => { jest.advanceTimersByTime(300); });
  expect(oldSignal?.aborted).toBe(true);
  await act(async () => { jest.advanceTimersByTime(1); });
  expect(screen.getByText('Newest artist')).toBeTruthy();
  await act(async () => { finishOld?.(response('Obsolete artist')); });
  expect(screen.queryByText('Obsolete artist')).toBeNull();
});

test('restores URL filters and exposes canonical artist links and clear control', async () => {
  mount('/fans?q=Luna&profileKind=band');
  await waitFor(() => expect(search).toHaveBeenCalledWith(expect.objectContaining({ q: 'Luna', profileKind: 'band' }), expect.any(AbortSignal)));
  expect(screen.getByLabelText('Buscar artistas y perfiles')).toHaveProperty('value', 'Luna');
  expect((await screen.findByRole('link', { name: /Initial/ })).getAttribute('href')).toBe('/artista/42');
  fireEvent.click(screen.getByRole('button', { name: 'Limpiar búsqueda' }));
  await waitFor(() => expect(search.mock.calls.at(-1)?.[0].q).toBeUndefined());
});

test('rejects external or malformed canonical destinations', () => {
  const item = response('Artist').items[0]!;
  expect(directoryResultPath({ ...item, canonicalPath: '//evil.example', slug: 'a/b' })).toBe('/directorio/a%2Fb');
});

test('recovers from a failed search without losing the query', async () => {
  search.mockRejectedValueOnce(new Error('Temporary fixture failure'));
  mount('/fans?q=Luna');
  await screen.findByText('No pudimos cargar los perfiles. Intenta de nuevo.');
  fireEvent.click(screen.getByRole('button', { name: 'Reintentar' }));
  await screen.findByText('Initial');
  expect(screen.getByLabelText('Buscar artistas y perfiles')).toHaveProperty('value', 'Luna');
});

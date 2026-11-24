import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { DirectorySearchQuery, DirectorySearchResponse } from '../../api/directory';

const response = (title: string): DirectorySearchResponse => ({ items: [{ id: 'artist-42', type: 'profile', slug: '42', canonicalPath: '/artista/42', title, location: {}, taxonomy: { genreIds: [], professionIds: [], serviceIds: [], instrumentIds: [] }, score: 1, scoreBreakdown: {}, sponsored: false }], sponsoredItems: [], facets: { total: 1, entityTypes: { profile: 1 }, cities: [] } });
const pageOf = (title: string, nextCursor?: string): DirectorySearchResponse => ({ ...response(title), facets: { total: 30, entityTypes: { profile: 30 }, cities: [] }, nextCursor });
const search = jest.fn<(query: DirectorySearchQuery, signal?: AbortSignal) => Promise<DirectorySearchResponse>>();
jest.unstable_mockModule('../../api/directory', () => ({ Directory: { search, taxonomies: async () => ({ genres: [], cities: [] }) } }));
const filterOptions = jest.fn(async () => ({
  countries: [{ id: 'aaaaaaaa-0000-4000-8000-000000000001', code: 'EC', nameEs: 'Ecuador', nameEn: 'Ecuador' }],
  regions: [{ id: 'bbbbbbbb-0000-4000-8000-000000000001', countryId: 'aaaaaaaa-0000-4000-8000-000000000001', code: 'EC-P', nameEs: 'Pichincha', nameEn: 'Pichincha' }],
  cities: [{ id: 'cccccccc-0000-4000-8000-000000000001', countryId: 'aaaaaaaa-0000-4000-8000-000000000001', regionId: 'bbbbbbbb-0000-4000-8000-000000000001', code: 'quito', nameEs: 'Quito', nameEn: 'Quito' }],
  genres: [
    { id: 'dddddddd-0000-4000-8000-000000000001', code: 'latin', nameEs: 'Latina', nameEn: 'Latin', parentId: null },
    { id: 'dddddddd-0000-4000-8000-000000000002', code: 'cumbia', nameEs: 'Cumbia', nameEn: 'Cumbia', parentId: 'dddddddd-0000-4000-8000-000000000001' },
  ],
}));
jest.unstable_mockModule('../../api/taxonomy', () => ({ Taxonomy: { filterOptions } }));
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

test('pages through profiles one server page at a time and returns to visited pages without refetching', async () => {
  search.mockImplementation((query) => Promise.resolve(query.cursor === 'c1' ? pageOf('Third page artist') : query.cursor ? pageOf('Second page artist', 'c1') : pageOf('First page artist', 'c0')));
  mount();
  await screen.findByText('First page artist');
  expect(search.mock.calls[0]?.[0].limit).toBe(12);
  expect(screen.getByText('1–12 de 30 perfiles')).toBeTruthy();
  expect(screen.getByRole('button', { name: 'Ir a la página anterior' })).toHaveProperty('disabled', true);
  fireEvent.click(screen.getByRole('button', { name: 'Ir a la página siguiente' }));
  await screen.findByText('Second page artist');
  expect(screen.queryByText('First page artist')).toBeNull();
  expect(search.mock.calls.at(-1)?.[0].cursor).toBe('c0');
  expect(screen.getByText('13–24 de 30 perfiles')).toBeTruthy();
  const calls = search.mock.calls.length;
  fireEvent.click(screen.getByRole('button', { name: 'Ir a la página anterior' }));
  await screen.findByText('First page artist');
  expect(search).toHaveBeenCalledTimes(calls);
  fireEvent.click(screen.getByRole('button', { name: 'Ir a la página siguiente' }));
  await screen.findByText('Second page artist');
  fireEvent.click(screen.getByRole('button', { name: 'Ir a la página siguiente' }));
  await screen.findByText('Third page artist');
  expect(screen.getByRole('button', { name: 'Ir a la página siguiente' })).toHaveProperty('disabled', true);
});

test('restores the selected result page and page size from the URL', async () => {
  search.mockImplementation((query) => Promise.resolve(query.cursor === 'c1' ? pageOf('Third page artist') : query.cursor ? pageOf('Second page artist', 'c1') : pageOf('First page artist', 'c0')));
  mount('/fans?page=2');
  await screen.findByText('Second page artist');
  expect(screen.queryByText('First page artist')).toBeNull();
  expect(screen.getByText('13–24 de 30 perfiles')).toBeTruthy();
  expect(search.mock.calls.map(([query]) => query.cursor)).toEqual([undefined, 'c0']);
  cleanup();
  search.mockClear();
  mount('/fans?perPage=24');
  await screen.findByText('First page artist');
  expect(search.mock.calls[0]?.[0].limit).toBe(24);
});

test('stops restoring a page after its request fails and asks again only on retry', async () => {
  let failSecond = true;
  search.mockImplementation((query) => {
    if (!query.cursor) return Promise.resolve(pageOf('First page artist', 'c0'));
    return failSecond ? Promise.reject(new Error('unavailable')) : Promise.resolve(pageOf('Second page artist', 'c1'));
  });
  mount('/fans?page=2');
  expect(await screen.findByText('No pudimos cargar los perfiles. Intenta de nuevo.')).toBeTruthy();
  const cursorCalls = () => search.mock.calls.filter(([query]) => query.cursor).length;
  await act(async () => { await new Promise((resolve) => setTimeout(resolve, 50)); });
  expect(cursorCalls()).toBe(1);
  expect(screen.getByText('No pudimos cargar los perfiles. Intenta de nuevo.')).toBeTruthy();
  failSecond = false;
  fireEvent.click(screen.getByRole('button', { name: 'Reintentar' }));
  await screen.findByText('Second page artist');
  expect(cursorCalls()).toBe(2);
});

test('discards a pending page change when the search changes before it finishes', async () => {
  let finishSecond: ((value: DirectorySearchResponse) => void) | undefined;
  search.mockImplementation((query) => {
    if (query.q === 'Luna') return Promise.resolve(pageOf('Luna first page', 'l0'));
    if (query.cursor) return new Promise((resolve) => { finishSecond = resolve; });
    return Promise.resolve(pageOf('First page artist', 'c0'));
  });
  mount();
  await screen.findByText('First page artist');
  fireEvent.click(screen.getByRole('button', { name: 'Ir a la página siguiente' }));
  await waitFor(() => expect(finishSecond).toBeDefined());
  fireEvent.change(screen.getByLabelText('Buscar artistas y perfiles'), { target: { value: 'Luna' } });
  fireEvent.submit(screen.getByRole('search', { name: 'Buscar en Comunidad' }));
  await screen.findByText('Luna first page');
  await act(async () => { finishSecond?.(pageOf('Second page artist', 'c1')); });
  expect(screen.getByText('Luna first page')).toBeTruthy();
  expect(screen.getByText('1–12 de 30 perfiles')).toBeTruthy();
  expect(screen.queryByText('Second page artist')).toBeNull();
});

test('shows profiles with a picture before those without on the current page', async () => {
  const item = response('x').items[0]!;
  const row = (id: string, title: string, imageUrl?: string | null) => ({ ...item, id, title, canonicalPath: `/artista/${id}`, imageUrl });
  search.mockResolvedValue({ ...response('x'), items: [
    row('1', 'No picture A'), row('2', 'Pictured B', 'https://cdn.example.test/b.jpg'), row('3', 'Blank C', '  '),
    row('4', 'Fallback D', '/artist-fallback.svg'), row('5', 'Pictured E', 'https://cdn.example.test/e.jpg'),
  ] });
  mount();
  await screen.findByText('Pictured B');
  expect(screen.getAllByRole('heading', { level: 3 }).map((heading) => heading.textContent))
    .toEqual(['Pictured B', 'Pictured E', 'No picture A', 'Blank C', 'Fallback D']);
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

test('TAX-FILTER-001: shared filters from the URL reach the search and legacy genre/city links still work', async () => {
  mount('/fans?f.genre=dddddddd-0000-4000-8000-000000000002&f.city=cccccccc-0000-4000-8000-000000000001');
  await waitFor(() => expect(search).toHaveBeenCalledWith(expect.objectContaining({
    genreIds: ['dddddddd-0000-4000-8000-000000000002'],
    cityIds: ['cccccccc-0000-4000-8000-000000000001'],
  }), expect.any(AbortSignal)));
  cleanup();
  search.mockClear();
  mount('/fans?genreId=dddddddd-0000-4000-8000-000000000001');
  await waitFor(() => expect(search).toHaveBeenCalledWith(expect.objectContaining({
    genreIds: ['dddddddd-0000-4000-8000-000000000001'],
  }), expect.any(AbortSignal)));
  await waitFor(() => expect(screen.getAllByTestId('community-taxonomy-chip').map((chip) => chip.textContent)).toEqual(['Latina']));
});

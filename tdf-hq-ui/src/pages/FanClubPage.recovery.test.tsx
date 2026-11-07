import { jest } from '@jest/globals';
import { act } from 'react';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider, onlineManager } from '@tanstack/react-query';
import { MemoryRouter, Route, Routes } from 'react-router-dom';

const club = { fcName: 'Club Sintético', fcOfficers: [], fcFollowerCount: 3 };
const getClub = jest.fn<() => Promise<typeof club | undefined>>();
const getMyClub = jest.fn<() => Promise<typeof club | undefined>>();
const listClubFeed = jest.fn(async () => []);
const listClubPosts = jest.fn(async () => []);
const listClubEvents = jest.fn(async () => []);
let authenticated = true;
jest.unstable_mockModule('../api/fans', () => ({ Fans: { getClub, getMyClub, listClubFeed, listClubPosts, listClubEvents } }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: authenticated ? { partyId: 8, roles: [], modules: [] } : null }) }));
jest.unstable_mockModule('../components/GoogleDriveUploadWidget', () => ({ default: () => null }));
jest.unstable_mockModule('../features/interactions/InteractionPanel', () => ({ InteractionPanel: () => null }));
jest.unstable_mockModule('@mui/icons-material', () => Object.fromEntries(['PushPinOutlined', 'VisibilityOff', 'HowToVote', 'CalendarMonth', 'Forum', 'Groups', 'Add', 'PhotoLibrary', 'Report', 'Person', 'LockOutlined', 'MailOutline', 'Whatshot', 'TrendingUp', 'FiberNew'].map(name => [name, () => null])));
const { default: FanClubPage } = await import('./FanClubPage');
const clients: QueryClient[] = [];
function mount(route = '/fans/clubs/9', cachedClub = false) {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  clients.push(client);
  if (cachedClub) client.setQueryData(['fan-club', 9, 'auth'], club);
  render(<QueryClientProvider client={client}><MemoryRouter initialEntries={[route]}><Routes><Route path="/fans/clubs/:artistId" element={<FanClubPage />} /></Routes></MemoryRouter></QueryClientProvider>);
  return client;
}
function fail(status: number) { return Object.assign(new Error('private diagnostic not for display'), { status }); }
beforeEach(() => {
  jest.clearAllMocks(); authenticated = true;
  getClub.mockReset().mockResolvedValue(club); getMyClub.mockReset().mockResolvedValue(club);
});
afterEach(() => { onlineManager.setOnline(true); cleanup(); clients.splice(0).forEach(client => client.clear()); });

it.each([503, 408])('recovers a %i without falsely claiming that the club is absent', async status => {
  getMyClub.mockRejectedValueOnce(fail(status)).mockResolvedValueOnce(club);
  mount();
  await screen.findByText('No pudimos cargar el club', { exact: true });
  expect(screen.queryByText(/no tiene un club de fans activo/)).toBeNull();
  expect(screen.queryByText(/private diagnostic/)).toBeNull();
  expect(listClubFeed).not.toHaveBeenCalled();
  expect(screen.getByRole('link', { name: 'Ver perfil del artista' }).getAttribute('href')).toBe('/artista/9');
  fireEvent.click(screen.getByRole('button', { name: 'Reintentar', exact: true }));
  await screen.findByRole('heading', { name: 'Club Sintético', level: 3 });
  await waitFor(() => expect(listClubFeed).toHaveBeenCalledTimes(1));
  expect(getMyClub).toHaveBeenCalledTimes(2);
});

it('offers retry for a network failure without displaying its raw message', async () => {
  getMyClub.mockRejectedValue(new TypeError('private diagnostic not for display'));
  mount();
  await screen.findByRole('button', { name: 'Reintentar', exact: true });
  expect(screen.queryByText(/private diagnostic/)).toBeNull();
  expect(screen.queryByText('Club no disponible', { exact: true })).toBeNull();
});

it.each([401, 403])('preserves the club deep link when login is offered after %i', async status => {
  getMyClub.mockRejectedValue(fail(status));
  mount('/fans/clubs/9?post=17#publication');
  const login = await screen.findByRole('link', { name: 'Ingresar de nuevo' });
  const destination = new URL(login.getAttribute('href')!, 'https://tdf.local');
  expect(destination.pathname).toBe('/login');
  expect(destination.searchParams.get('redirect')).toBe('/fans/clubs/9?post=17#publication');
  expect(screen.queryByText('Club no disponible', { exact: true })).toBeNull();
  expect(listClubPosts).not.toHaveBeenCalled();
});

it.each([true, false])('offers the existing public artist page for a confirmed404 (authenticated=%s)', async signedIn => {
  authenticated = signedIn;
  getMyClub.mockRejectedValue(fail(404));getClub.mockRejectedValue(fail(404));
  mount();
  await screen.findByText('Club no disponible', { exact: true });
  expect(screen.getByRole('link', { name: 'Ver perfil del artista' }).getAttribute('href')).toBe('/artista/9');
  expect(screen.getByRole('link', { name: 'Volver a la comunidad' }).getAttribute('href')).toBe('/fans');
  expect(listClubFeed).not.toHaveBeenCalled();
  expect((signedIn ? getMyClub : getClub)).toHaveBeenCalledTimes(1);
  expect((signedIn ? getClub : getMyClub)).not.toHaveBeenCalled();
});

it('waits for the club before requesting its authenticated feed', async () => {
  let resolveClub!: (value: typeof club) => void;
  getMyClub.mockReturnValue(new Promise(resolve => { resolveClub = resolve; }));
  mount();
  expect(listClubFeed).not.toHaveBeenCalled();
  await act(async () => { resolveClub(club); });
  await screen.findByRole('heading', { name: 'Club Sintético', level: 3 });
  await waitFor(() => expect(listClubFeed).toHaveBeenCalledTimes(1));
});

it('keeps a readable cached club when a background refresh fails', async () => {
  const client = mount();await screen.findByRole('heading', { name: 'Club Sintético', level: 3 });
  getMyClub.mockRejectedValue(fail(503));
  await act(async () => { await client.invalidateQueries({ queryKey: ['fan-club', 9] }); });
  expect(screen.getByRole('heading', { name: 'Club Sintético', level: 3 })).not.toBeNull();
  expect(screen.queryByText('Club no disponible', { exact: true })).toBeNull();
});

it.each(['0', '-1', '9invalid', '9007199254740992', '1e2', '0x9', '1.0', '+9', '9%20'])('does not query a different club for invalid identifier%s', async identifier => {
  mount(`/fans/clubs/${identifier}`);
  await screen.findByRole('link', { name: 'Volver a la comunidad' });
  expect(screen.queryByRole('link', { name: 'Ver perfil del artista' })).toBeNull();
  expect(getMyClub).not.toHaveBeenCalled();expect(getClub).not.toHaveBeenCalled();expect(listClubFeed).not.toHaveBeenCalled();
});

// Neither a malformed empty200 nor a paused offline request proves absence.
it('keeps an empty successful response retryable', async () => {
  getMyClub.mockResolvedValueOnce(undefined);mount();
  await screen.findByRole('button', { name: 'Reintentar', exact: true });
  expect(screen.queryByText('Club no disponible', { exact: true })).toBeNull();
  expect(listClubFeed).not.toHaveBeenCalled();
});
it('does not claim absence while the initial request is paused offline', async () => {
  onlineManager.setOnline(false);mount();
  await screen.findByRole('button', { name: 'Reintentar', exact: true });
  expect(screen.queryByText('Club no disponible', { exact: true })).toBeNull();
  expect(getMyClub).not.toHaveBeenCalled();
  await act(async () => { onlineManager.setOnline(true); });
  await screen.findByRole('heading', { name: 'Club Sintético', level: 3 });
});

it('rejects an invalid alias even when the numeric club is cached', async () => {
  mount('/fans/clubs/0x9', true);
  await screen.findByText('Club no disponible', { exact: true });
  expect(screen.queryByRole('heading', { name: 'Club Sintético', level: 3 })).toBeNull();
  expect(screen.queryByRole('link', { name: 'Ver perfil del artista' })).toBeNull();
  expect(getMyClub).not.toHaveBeenCalled();
});

import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act, Suspense } from 'react';
import { BrowserRouter, Link } from 'react-router-dom';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';

let authenticated = true;
let loading = false;
jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: authenticated ? { username: 'listener' } : null, loading }),
}));
jest.unstable_mockModule('../api/radio', () => ({ RadioAPI: {
  search: jest.fn(async () => [{ rsId: 'fixture', rsName: 'Navigation radio', rsStreamUrl: 'https://radio.test/live' }]),
  clearPresence: jest.fn(async () => undefined),
  setPresence: jest.fn(async () => undefined),
  nowPlaying: jest.fn(async () => ({})),
  listAutoStopOptions: jest.fn(async () => ({ options: [] })),
} }));
jest.unstable_mockModule('../api/catalogs', () => ({ Catalogs: {
  listItems: jest.fn(async () => ({ items: [], total: 0 })),
} }));
jest.unstable_mockModule('../utils/tidalAgent', () => ({ generateTidalCode: jest.fn() }));
const { default: RoutedRadioWidget } = await import('./RoutedRadioWidget');

const destinations = ['/inicio', '/marketplace', '/musica', '/configuracion/inscripciones-curso', '/records', '/fans', '/solicitudes-acceso/revision'];
let play: jest.SpiedFunction<HTMLMediaElement['play']>;
let pause: jest.SpiedFunction<HTMLMediaElement['pause']>;
let load: jest.SpiedFunction<HTMLMediaElement['load']>;

beforeEach(() => {
  authenticated = true;
  loading = false;
  window.localStorage.clear();
  window.history.replaceState(null, '', '/solicitudes-acceso/revision');
  play = jest.spyOn(HTMLMediaElement.prototype, 'play').mockImplementation(async () => undefined);
  pause = jest.spyOn(HTMLMediaElement.prototype, 'pause').mockImplementation(() => undefined);
  load = jest.spyOn(HTMLMediaElement.prototype, 'load').mockImplementation(() => undefined);
});
afterEach(() => jest.restoreAllMocks());

function setup() {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  const tree = () => <QueryClientProvider client={client}><BrowserRouter>
    <nav>{destinations.map((path) => <Link key={path} to={path}>{path}</Link>)}<Link to="/inicio#radio">Open radio</Link></nav>
    <Suspense fallback={null}><RoutedRadioWidget /></Suspense>
  </BrowserRouter></QueryClientProvider>;
  const result = render(tree());
  return { ...result, refresh: () => result.rerender(tree()), client };
}

it('keeps the same audio and controls playing across menu destinations and Back/Forward', async () => {
  window.localStorage.setItem('radio-volume', '0.35');
  const view = setup();
  try {
    await waitFor(() => expect(screen.getByTestId('radio-docked-status').textContent).toContain('Navigation radio'), { timeout: 10000 });
    const audio = view.container.querySelector('audio')!;
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Reproducir radio' })); });
    expect(screen.getByRole('button', { name: 'Pausar radio' })).toBeTruthy();
    audio.currentTime = 42;
    const pauses = pause.mock.calls.length;
    const loads = load.mock.calls.length;
    const plays = play.mock.calls.length;
    const source = audio.src;
    for (const path of destinations) {
      fireEvent.click(screen.getByRole('link', { name: path, exact: true }));
      expect(window.location.pathname).toBe(path);
      expect(screen.getByTestId('radio-docked-bar')).toBeTruthy();
      expect(screen.getByRole('button', { name: 'Pausar radio' })).toBeTruthy();
      expect(view.container.querySelector('audio')).toBe(audio);
      expect(audio.src).toBe(source);
      expect(audio.currentTime).toBe(42);
      expect(audio.volume).toBe(0.35);
    }
    await act(async () => { window.history.back(); });
    await waitFor(() => expect(window.location.pathname).toBe('/fans'));
    await act(async () => { window.history.forward(); });
    await waitFor(() => expect(window.location.pathname).toBe('/solicitudes-acceso/revision'));
    expect(view.container.querySelector('audio')).toBe(audio);
    expect(pause).toHaveBeenCalledTimes(pauses);
    expect(load).toHaveBeenCalledTimes(loads);
    expect(play).toHaveBeenCalledTimes(plays);
    // Pausing is an explicit user action; navigation must not undo it either.
    fireEvent.click(screen.getByRole('button', { name: 'Pausar radio' }));
    fireEvent.click(screen.getByRole('link', { name: '/inicio', exact: true }));
    expect(screen.getByRole('button', { name: 'Reproducir radio' })).toBeTruthy();
    expect(view.container.querySelector('audio')).toBe(audio);
  } finally { view.unmount(); view.client.clear(); }
});

it('defers the radio on quiet landing routes, then retains it after explicitly opening it', async () => {
  window.history.replaceState(null, '', '/inicio');
  const view = setup();
  try {
    expect(view.container.querySelector('audio')).toBeNull();
    fireEvent.click(screen.getByRole('link', { name: 'Open radio' }));
    await screen.findByTestId('radio-docked-bar');
    const audio = view.container.querySelector('audio');
    fireEvent.click(screen.getByRole('link', { name: '/inicio', exact: true }));
    expect(view.container.querySelector('audio')).toBe(audio);
    expect(screen.getByTestId('radio-docked-bar')).toBeTruthy();
  } finally { view.unmount(); view.client.clear(); }
});

it('waits for authentication and removes the player on logout without retaining activation for the next session', async () => {
  loading = true;
  const view = setup();
  try {
    expect(view.container.querySelector('audio')).toBeNull();
    loading = false;
    view.refresh();
    await screen.findByTestId('radio-docked-bar');
    authenticated = false;
    view.refresh();
    expect(view.container.querySelector('audio')).toBeNull();
    fireEvent.click(screen.getByRole('link', { name: '/inicio', exact: true }));
    authenticated = true;
    view.refresh();
    expect(view.container.querySelector('audio')).toBeNull();
  } finally { view.unmount(); view.client.clear(); }
});

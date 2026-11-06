import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter, useNavigate } from 'react-router-dom';
import type { SessionUser } from '../session/SessionContext';
import type { PlayerTrack } from './types';

const userA: SessionUser = { partyId: 101, username: 'synthetic-A', displayName: 'Synthetic A', roles: ['customer'] };
const userB: SessionUser = { partyId: 202, username: 'synthetic-B', displayName: 'Synthetic B', roles: ['customer'] };
let snapshot: () => Promise<SessionUser | null>;
const unexpectedNetwork = jest.fn<(...args: unknown[]) => Promise<never>>(() => Promise.reject(new Error('Unexpected real network')));
jest.unstable_mockModule('../api/session', () => ({
  loadSessionSnapshot: () => snapshot(), logoutSessionRequest: async () => undefined,
  reconcileOnboardingProgress: async () => ({ newlyCompleted: false, progress: { eligible: false, completedAt: null, firstValue: null } }),
  completeOnboardingProgress: async () => ({ newlyCompleted: false, progress: { eligible: false, completedAt: null, firstValue: null } }),
}));
jest.unstable_mockModule('../api/catalogs', () => ({ Catalogs: { listPublicBatch: async () => ({ catalogs: [] }) } }));
jest.unstable_mockModule('../analytics/posthog', () => ({ getAnalyticsClient: () => ({ ready: false, capture() {}, identify() {}, reset() {} }) }));
jest.unstable_mockModule('./analytics', () => ({ installPlayerAnalyticsBridge: () => () => undefined }));
jest.unstable_mockModule('./assetAccess', () => ({ authorizePlayerAsset: jest.fn() }));
jest.unstable_mockModule('../utils/logger', () => ({ logger: { warn: jest.fn(), error: jest.fn() } }));
jest.unstable_mockModule('react-i18next', () => ({ useTranslation: () => ({ t: (key: string) => key, i18n: { language: 'es', resolvedLanguage: 'es' } }) }));
// Actual App, GlobalPlayer and session boundaries; only route contents, APIs,
// layout adapters and native media are deterministic test doubles.
jest.unstable_mockModule('../routes/AppRoutes', () => ({ default: Controls }));
const { useSession, SESSION_STORAGE_KEY } = await import('../session/SessionContext');
const { SessionProviders } = await import('../session/SessionProviders');
const { PlayerProvider, usePlayer } = await import('./PlayerProvider');
const { default: App } = await import('../App');
const KEY = 'tdf-global-player/v1';
const track = (id = 'A'): PlayerTrack => ({ id: `public-${id}`, title: `SYNTHETIC_LISTENER_${id}`, artist: 'Public fixture artist',
  sources: [{ url: `https://media.invalid/${id}.m4a`, quality: 'high', mediaType: 'audio/mp4' }] });
let session: ReturnType<typeof useSession>;
let player: ReturnType<typeof usePlayer>;
function Probe() { player = usePlayer(); return <output>{player.currentTrack?.title ?? 'empty-player'}</output>; }
function Controls() {
  session = useSession(); player = usePlayer(); const navigate = useNavigate();
  return <main><output data-testid="actor">{session.session?.partyId ?? 'anonymous'}:{String(session.loading)}</output>
    <button onClick={() => session.login(userA)}>login A</button><button onClick={() => session.login(userB)}>login B</button>
    <button onClick={() => session.logout()}>logout</button><button onClick={() => player.playTrack(track())}>select A</button>
    <button onClick={() => player.playTrack(track('B'))}>select B</button>
    <button onClick={() => navigate('/musica/releases/fixture')}>navigate</button></main>;
}
const mountApp = () => render(<MemoryRouter initialEntries={['/musica']}><SessionProviders><App /></SessionProviders></MemoryRouter>);
const actor = async (id: number | 'anonymous') => { await waitFor(() => expect(screen.getByTestId('actor').textContent).toBe(`${id}:false`)); };
const readSaved = () => JSON.parse(localStorage.getItem(KEY) ?? '{}') as { ownerPartyId?: number | null; queue?: { tracks: PlayerTrack[] }; positions?: Record<string, number> };
const seed = (owner: unknown = 101) => {
  localStorage.setItem(KEY, JSON.stringify({ queue: { tracks: [track()], currentIndex: 0, shuffle: true, repeat: 'queue' },
    volume: 0.37, muted: true, quality: 'low', positions: { 'public-A': 12, invalid: -4, text: 'oops' },
    ...(owner === 'legacy' ? {} : { ownerPartyId: owner }) })); return localStorage.getItem(KEY);
};
const choose = async (id: 'A' | 'B', seconds: number) => {
  fireEvent.click(screen.getByRole('button', { name: `select ${id}` }));
  await waitFor(() => expect(player.status).toBe('playing'));
  act(() => { player.seek(seconds); document.querySelector('audio')!.dispatchEvent(new Event('timeupdate')); });
};
const native = new WeakMap<HTMLMediaElement, { paused: boolean; readyState: number }>();
const nativeState = (audio: HTMLMediaElement) => { const state = native.get(audio) ?? { paused: true, readyState: HTMLMediaElement.HAVE_NOTHING }; native.set(audio, state); return state; };
const originalFetch = globalThis.fetch;
beforeEach(() => {
  localStorage.clear(); sessionStorage.clear(); snapshot = async () => null; unexpectedNetwork.mockClear(); globalThis.fetch = unexpectedNetwork;
  Object.defineProperty(window, 'matchMedia', { configurable: true, value: (media: string) => ({ matches: false, media, addListener() {}, removeListener() {}, addEventListener() {}, removeEventListener() {} }) });
  Object.defineProperty(globalThis, 'ResizeObserver', { configurable: true, value: class { observe() {} unobserve() {} disconnect() {} } });
  jest.spyOn(HTMLMediaElement.prototype, 'currentSrc', 'get').mockImplementation(function (this: HTMLMediaElement) { return this.src; });
  jest.spyOn(HTMLMediaElement.prototype, 'paused', 'get').mockImplementation(function (this: HTMLMediaElement) { return nativeState(this).paused; });
  jest.spyOn(HTMLMediaElement.prototype, 'readyState', 'get').mockImplementation(function (this: HTMLMediaElement) { return nativeState(this).readyState; });
  jest.spyOn(HTMLMediaElement.prototype, 'load').mockImplementation(function (this: HTMLMediaElement) {
    nativeState(this).readyState = HTMLMediaElement.HAVE_NOTHING;
    this.currentTime = 0;
    const source = this.getAttribute('src');
    queueMicrotask(() => {
      if (!source || this.getAttribute('src') !== source) return;
      nativeState(this).readyState = HTMLMediaElement.HAVE_METADATA;
      this.dispatchEvent(new Event('loadedmetadata'));
      this.dispatchEvent(new Event('timeupdate'));
    });
  });
  jest.spyOn(HTMLMediaElement.prototype, 'pause').mockImplementation(function (this: HTMLMediaElement) { nativeState(this).paused = true; this.dispatchEvent(new Event('pause')); });
  jest.spyOn(HTMLMediaElement.prototype, 'play').mockImplementation(function (this: HTMLMediaElement) { nativeState(this).paused = false; this.dispatchEvent(new Event('playing')); return Promise.resolve(); });
});
afterEach(() => { cleanup(); jest.useRealTimers(); jest.restoreAllMocks(); globalThis.fetch = originalFetch; expect(unexpectedNetwork).not.toHaveBeenCalled(); });

it('actual App clears prior titles, queue and positions on logout and direct account changes', async () => {
  mountApp(); await actor('anonymous');
  fireEvent.click(screen.getByRole('button', { name: 'login A' })); await actor(101);
  act(() => { player.setVolume(0.37); player.setMuted(true); player.setQuality('low'); });
  await choose('A', 12); await waitFor(() => expect(readSaved().positions?.['public-A']).toBe(12));
  const audio = document.querySelector('audio')!; const calls = jest.mocked(audio.play).mock.calls.length;
  fireEvent.click(screen.getByRole('button', { name: 'logout' })); await actor('anonymous');
  expect(player.currentTrack).toBeNull(); expect(player.queue.tracks).toEqual([]); expect(player.currentTime).toBe(0);
  expect(screen.queryByText('SYNTHETIC_LISTENER_A')).toBeNull(); expect(audio.paused).toBe(true);
  expect(jest.mocked(audio.play).mock.calls.length).toBe(calls);
  expect(player.volume).toBe(0.37); expect(player.muted).toBe(true); expect(player.quality).toBe('low');
  fireEvent.click(screen.getByRole('button', { name: 'login B' })); await actor(202);
  expect(player.currentTrack).toBeNull(); await choose('B', 23);
  await waitFor(() => expect(readSaved().positions?.['public-B']).toBe(23));
  fireEvent.click(screen.getByRole('button', { name: 'login A' })); await actor(101);
  expect(player.currentTrack).toBeNull();
  await waitFor(() => expect(readSaved().ownerPartyId).toBe(101));
  expect(readSaved().queue?.tracks).toEqual([]); expect(readSaved().positions).toEqual({});
});

it.each(['success', 'failure'] as const)('holds saved state during pending bootstrap then restores same listener on %s', async (outcome) => {
  const saved = seed(); localStorage.setItem(SESSION_STORAGE_KEY, JSON.stringify(userA));
  let resolve: (value: SessionUser | null) => void = () => undefined; let reject: (error: Error) => void = () => undefined;
  snapshot = () => new Promise((yes, no) => { resolve = yes; reject = no; });
  mountApp(); expect(player.currentTrack).toBeNull(); expect(screen.queryByText('SYNTHETIC_LISTENER_A')).toBeNull();
  await act(async () => { await new Promise((done) => setTimeout(done, 300)); });
  expect(localStorage.getItem(KEY)).toBe(saved);
  await act(async () => { if (outcome === 'success') resolve(userA); else reject(new Error('synthetic session offline')); });
  await actor(101); await waitFor(() => expect(player.currentTime).toBe(12));
  expect(player.currentTrack?.id).toBe('public-A'); expect(player.status).toBe('paused');
  expect(player.queue.shuffle).toBe(true); expect(player.queue.repeat).toBe('queue');
  expect(jest.mocked(HTMLMediaElement.prototype.play)).not.toHaveBeenCalled();
  await waitFor(() => expect(readSaved().positions).toEqual({ 'public-A': 12 }));
});

it('restores paused after cookie bootstrap without a cached session descriptor', async () => {
  const saved = seed(101);
  let resolve: (value: SessionUser | null) => void = () => undefined;
  snapshot = () => new Promise((done) => { resolve = done; });
  mountApp();
  await waitFor(() => expect(screen.getByTestId('actor').textContent).toBe('anonymous:true'));
  expect(localStorage.getItem(SESSION_STORAGE_KEY)).toBeNull();
  expect(player.currentTrack).toBeNull();
  await act(async () => { await new Promise((done) => setTimeout(done, 300)); });
  expect(localStorage.getItem(KEY)).toBe(saved);
  await act(async () => { resolve(userA); });
  await actor(101);
  await waitFor(() => expect(player.currentTime).toBe(12));
  expect(player.currentTrack?.id).toBe('public-A');
  expect(player.status).toBe('paused');
  expect(player.queue.shuffle).toBe(true);
  expect(player.queue.repeat).toBe('queue');
  expect(jest.mocked(HTMLMediaElement.prototype.play)).not.toHaveBeenCalled();
  await waitFor(() => expect(readSaved().positions).toEqual({ 'public-A': 12 }));
});

it('preserves a newer public selection when uncached bootstrap resolves', async () => {
  const saved = seed(101);
  let resolve: (value: SessionUser | null) => void = () => undefined;
  snapshot = () => new Promise((done) => { resolve = done; });
  mountApp();
  await waitFor(() => expect(screen.getByTestId('actor').textContent).toBe('anonymous:true'));
  await choose('B', 3);
  const audio = document.querySelector('audio')!;
  const plays = jest.mocked(audio.play).mock.calls.length;
  expect(localStorage.getItem(KEY)).toBe(saved);
  await act(async () => { resolve(userA); }); await actor(101);
  expect(player.currentTrack?.id).toBe('public-B');
  expect(player.currentTime).toBe(3); expect(player.status).toBe('playing');
  expect(document.querySelector('audio')).toBe(audio);
  expect(jest.mocked(audio.play).mock.calls.length).toBe(plays);
  await waitFor(() => expect(readSaved().queue?.tracks[0]?.id).toBe('public-B'));
});

it('does not expose another saved listener when uncached bootstrap resolves', async () => {
  const saved = seed(101);
  let resolve: (value: SessionUser | null) => void = () => undefined;
  snapshot = () => new Promise((done) => { resolve = done; });
  mountApp();
  await waitFor(() => expect(screen.getByTestId('actor').textContent).toBe('anonymous:true'));
  expect(player.currentTrack).toBeNull();
  expect(screen.queryByText('SYNTHETIC_LISTENER_A')).toBeNull();
  expect(localStorage.getItem(KEY)).toBe(saved);
  await act(async () => { resolve(userB); }); await actor(202);
  expect(player.currentTrack).toBeNull(); expect(player.queue.tracks).toEqual([]);
  await waitFor(() => expect(readSaved().ownerPartyId).toBe(202));
  expect(readSaved().positions).toEqual({});
});

it('ends initial-owner eligibility after the first anonymous bootstrap resolution', async () => {
  jest.useFakeTimers(); seed(101);
  const view = render(<PlayerProvider identity={{ ready: false, partyId: null }}><Probe /></PlayerProvider>);
  view.rerender(<PlayerProvider identity={{ ready: true, partyId: null }}><Probe /></PlayerProvider>);
  expect(player.currentTrack).toBeNull();
  view.rerender(<PlayerProvider identity={{ ready: true, partyId: 101 }}><Probe /></PlayerProvider>);
  expect(player.currentTrack).toBeNull(); expect(player.queue.tracks).toEqual([]);
  await act(async () => { jest.advanceTimersByTime(300); });
  expect(readSaved().ownerPartyId).toBe(101); expect(readSaved().positions).toEqual({});
});

it('reload as another listener discards previous metadata before GlobalPlayer exposure', async () => {
  seed(101); localStorage.setItem(SESSION_STORAGE_KEY, JSON.stringify(userB)); snapshot = async () => userB;
  mountApp(); await actor(202); expect(player.currentTrack).toBeNull(); expect(screen.queryByText('SYNTHETIC_LISTENER_A')).toBeNull();
  await waitFor(() => expect(readSaved().ownerPartyId).toBe(202)); expect(readSaved().positions).toEqual({});
});

it('actual App does not treat an account without a valid party ID as anonymous', async () => {
  const saved = seed(null);
  snapshot = async () => ({ ...userA, partyId: undefined });
  mountApp(); await actor('anonymous');
  expect(session.session?.username).toBe('synthetic-A');
  expect(player.currentTrack).toBeNull();
  await act(async () => { await new Promise((done) => setTimeout(done, 300)); });
  expect(localStorage.getItem(KEY)).toBe(saved);
});

it('preserves a newer public selection and decoder across same-owner initial identity resolution', async () => {
  const saved = seed(101); localStorage.setItem(SESSION_STORAGE_KEY, JSON.stringify(userA));
  let resolve: (value: SessionUser | null) => void = () => undefined;
  snapshot = () => new Promise((done) => { resolve = done; });
  mountApp(); await choose('B', 3);
  const pendingAudio = document.querySelector('audio')!;
  const plays = jest.mocked(pendingAudio.play).mock.calls.length;
  expect(session.loading).toBe(true); expect(localStorage.getItem(KEY)).toBe(saved);
  await act(async () => { resolve(userA); }); await actor(101);
  expect(player.currentTime).toBe(3);
  expect(player.currentTrack?.id).toBe('public-B'); expect(player.status).toBe('playing');
  expect(pendingAudio.paused).toBe(false); expect(document.querySelector('audio')).toBe(pendingAudio);
  expect(jest.mocked(pendingAudio.play).mock.calls.length).toBe(plays);
  await waitFor(() => expect(readSaved().queue?.tracks[0]?.id).toBe('public-B'));
});

it.each(['legacy', -1, '101', false] as const)('discards unowned/invalid listening state %s retaining preferences', async (owner) => {
  seed(owner); render(<PlayerProvider identity={{ ready: true, partyId: 101 }}><Probe /></PlayerProvider>);
  expect(player.queue.tracks).toEqual([]); expect(player.currentTrack).toBeNull();
  expect(player.volume).toBe(0.37); expect(player.muted).toBe(true); expect(player.quality).toBe('low');
  await waitFor(() => expect(readSaved().ownerPartyId).toBe(101)); expect(readSaved().positions).toEqual({});
});

it('keeps unknown account separate from explicit anonymous ownership without overwriting storage', async () => {
  const saved = seed(null);
  const view = render(<PlayerProvider identity={{ ready: true, partyId: undefined }}><Probe /></PlayerProvider>);
  expect(player.currentTrack).toBeNull();
  await act(async () => { await new Promise((done) => setTimeout(done, 300)); });
  expect(localStorage.getItem(KEY)).toBe(saved);
  view.rerender(<PlayerProvider identity={{ ready: true, partyId: null }}><Probe /></PlayerProvider>);
  expect(player.currentTrack).toBeNull();
  view.unmount();
  const anonymous = render(<PlayerProvider identity={{ ready: true, partyId: null }}><Probe /></PlayerProvider>);
  await waitFor(() => expect(player.currentTime).toBe(12)); expect(player.currentTrack?.id).toBe('public-A');
  anonymous.rerender(<PlayerProvider identity={{ ready: true, partyId: 101 }}><Probe /></PlayerProvider>);
  expect(player.currentTrack).toBeNull();
});

it.each(['clear', 'remove', 'move', 'enqueue', 'shuffle', 'repeat'] as const)('a pending explicit %s command supersedes saved queue hydration', async (command) => {
  seed(101);
  const view = render(<PlayerProvider identity={{ ready: false, partyId: 101 }}><Probe /></PlayerProvider>);
  act(() => {
    if (command === 'clear') player.clearQueue();
    if (command === 'remove') player.removeTrack(0);
    if (command === 'move') player.moveTrack(0, 1);
    if (command === 'enqueue') player.enqueue([]);
    if (command === 'shuffle') player.setShuffle(true);
    if (command === 'repeat') player.setRepeat('queue');
  });
  const audio = document.querySelector('audio');
  view.rerender(<PlayerProvider identity={{ ready: true, partyId: 101 }}><Probe /></PlayerProvider>);
  expect(player.currentTrack).toBeNull(); expect(player.queue.tracks).toEqual([]);
  expect(document.querySelector('audio')).toBe(audio);
  await waitFor(() => expect(readSaved().positions).toEqual({}));
});

it('does not revive stale saved A during rapid A to B to A changes and retains unsaved device preferences', async () => {
  jest.useFakeTimers(); seed(101);
  const view = render(<PlayerProvider identity={{ ready: true, partyId: 101 }}><Probe /></PlayerProvider>);
  await act(async () => undefined);
  expect(player.currentTrack?.id).toBe('public-A');
  act(() => { player.setVolume(0.19); player.setMuted(false); player.setQuality('lossless'); });
  view.rerender(<PlayerProvider identity={{ ready: true, partyId: 202 }}><Probe /></PlayerProvider>);
  expect(player.currentTrack).toBeNull();
  view.rerender(<PlayerProvider identity={{ ready: true, partyId: 101 }}><Probe /></PlayerProvider>);
  expect(player.currentTrack).toBeNull(); expect(player.currentTime).toBe(0);
  expect(player.volume).toBe(0.19); expect(player.muted).toBe(false); expect(player.quality).toBe('lossless');
  await act(async () => { jest.advanceTimersByTime(300); });
  expect(readSaved().queue?.tracks).toEqual([]); expect(readSaved().positions).toEqual({});
});

it('retains engine for same-owner background loading changes and route navigation', async () => {
  seed(101);
  const view = render(<PlayerProvider identity={{ ready: true, partyId: 101 }}><Probe /></PlayerProvider>);
  await waitFor(() => expect(player.currentTime).toBe(12)); await act(async () => { await player.play(); });
  const audio = document.querySelector('audio')!;
  view.rerender(<PlayerProvider identity={{ ready: false, partyId: 101 }}><Probe /></PlayerProvider>);
  expect(document.querySelector('audio')).toBe(audio); expect(player.status).toBe('playing');
  view.rerender(<PlayerProvider identity={{ ready: true, partyId: 101 }}><Probe /></PlayerProvider>);
  expect(document.querySelector('audio')).toBe(audio); expect(player.currentTime).toBe(12); expect(player.status).toBe('playing');
  view.unmount(); localStorage.clear();
  mountApp(); await actor('anonymous'); await choose('A', 8);
  const routeAudio = document.querySelector('audio'); fireEvent.click(screen.getByRole('button', { name: 'navigate' }));
  expect(document.querySelector('audio')).toBe(routeAudio); expect(player.currentTime).toBe(8); expect(player.status).toBe('playing');
});

it('cancels an old owner debounce before it can repopulate prior listening data', async () => {
  jest.useFakeTimers();
  const view = render(<PlayerProvider identity={{ ready: true, partyId: 101 }}><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(track()); });
  const setItem = jest.spyOn(Storage.prototype, 'setItem');
  view.rerender(<PlayerProvider identity={{ ready: true, partyId: 202 }}><Probe /></PlayerProvider>);
  await act(async () => { jest.advanceTimersByTime(300); });
  const saves = setItem.mock.calls.filter(([key]) => key === KEY).map(([, value]) => JSON.parse(value) as { ownerPartyId: number });
  expect(saves).toHaveLength(1); expect(saves[0]?.ownerPartyId).toBe(202);
  expect(readSaved().queue?.tracks).toEqual([]); expect(readSaved().positions).toEqual({});
});

it.each(['broken-json', 'unavailable'] as const)('uses empty state with usable playback when storage is %s', async (failure) => {
  if (failure === 'broken-json') localStorage.setItem(KEY, '{');
  else jest.spyOn(Storage.prototype, 'getItem').mockImplementation(() => { throw new Error('Storage denied'); });
  render(<PlayerProvider identity={{ ready: true, partyId: 101 }}><Probe /></PlayerProvider>);
  expect(player.currentTrack).toBeNull(); await act(async () => { player.playTrack(track()); }); expect(player.status).toBe('playing');
});

it('actual App rejects a corrupt matching-owner track with a null source while keeping playback usable', async () => {
  seed(101); localStorage.setItem(SESSION_STORAGE_KEY, JSON.stringify(userA));
  const saved = JSON.parse(localStorage.getItem(KEY)!);
  saved.queue.tracks[0].sources.push(null);
  localStorage.setItem(KEY, JSON.stringify(saved)); snapshot = async () => userA;
  mountApp(); await actor(101);
  expect(player.currentTrack).toBeNull(); expect(player.queue.tracks).toEqual([]);
  expect(screen.queryByText('SYNTHETIC_LISTENER_A')).toBeNull();
  await choose('B', 4); expect(player.currentTrack?.id).toBe('public-B');
  expect(player.status).toBe('playing');
});

it('releases the old source on listener replacement and only the new owner can play', async () => {
  snapshot = async () => userA;
  mountApp(); await actor(101); await choose('A', 12);
  const oldAudio = document.querySelector('audio')!;
  const load = jest.mocked(HTMLMediaElement.prototype.load);
  const oldLoads = () => load.mock.contexts.filter(context => context === oldAudio).length;
  const beforeLoads = oldLoads();
  expect(oldAudio.getAttribute('src')).toContain('/A.m4a');
  fireEvent.click(screen.getByRole('button', { name: 'login B' })); await actor(202);
  expect(oldAudio.isConnected).toBe(false);
  expect(oldAudio.paused).toBe(true);
  expect(oldAudio.getAttribute('src')).toBeNull();
  expect(oldLoads()).toBe(beforeLoads + 1);
  expect(player.currentTrack).toBeNull(); expect(player.queue.tracks).toEqual([]);
  await choose('B', 4);
  const newAudio = document.querySelector('audio')!;
  expect(newAudio).not.toBe(oldAudio);
  act(() => { oldAudio.dispatchEvent(new Event('loadedmetadata')); oldAudio.dispatchEvent(new Event('playing')); });
  expect(player.currentTrack?.id).toBe('public-B');
  expect(newAudio.paused).toBe(false);
  expect(oldLoads()).toBe(beforeLoads + 1);
});

it('releases the detached source once when the actual App unmounts', async () => {
  snapshot = async () => userA;
  const view = mountApp(); await actor(101); await choose('A', 12);
  const oldAudio = document.querySelector('audio')!;
  const load = jest.mocked(HTMLMediaElement.prototype.load);
  const oldLoads = () => load.mock.contexts.filter(context => context === oldAudio).length;
  const beforeLoads = oldLoads();
  expect(oldAudio.getAttribute('src')).toContain('/A.m4a');
  view.unmount();
  expect(oldAudio.isConnected).toBe(false);
  expect(oldAudio.paused).toBe(true);
  expect(oldAudio.getAttribute('src')).toBeNull();
  expect(oldLoads()).toBe(beforeLoads + 1);
  const play = jest.mocked(HTMLMediaElement.prototype.play);
  const afterPlay = play.mock.calls.length;
  await act(async () => { oldAudio.dispatchEvent(new Event('loadedmetadata')); oldAudio.dispatchEvent(new Event('playing')); });
  expect(oldLoads()).toBe(beforeLoads + 1);
  expect(play.mock.calls.length).toBe(afterPlay);
});

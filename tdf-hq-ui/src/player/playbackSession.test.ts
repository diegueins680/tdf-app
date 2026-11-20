import { jest } from '@jest/globals';
import type { DesktopPlaybackCommand, DesktopPlaybackReport } from '../desktop/shell';
import {
  claimPlaybackSession,
  dispatchPlaybackCommand,
  getActivePlaybackOwner,
  releasePlaybackSession,
  resetPlaybackSessionForTests,
  updatePlaybackSession,
} from './playbackSession';

type Listener = (command: DesktopPlaybackCommand) => void;

function installShell() {
  const reports: DesktopPlaybackReport[] = [];
  const listeners = new Set<Listener>();
  const api = {
    version: { app: '1.0.0', electron: '45', chromium: '156', platform: 'linux', arch: 'x64', channel: 'test', unsigned: true },
    playback: {
      report: (state: DesktopPlaybackReport) => { reports.push(state); },
      onCommand: (listener: Listener) => { listeners.add(listener); return () => listeners.delete(listener); },
    },
    links: { openExternal: async () => true },
  };
  Object.defineProperty(window, 'tdfDesktop', { configurable: true, value: api });
  return { reports, send: (command: DesktopPlaybackCommand) => listeners.forEach((listener) => listener(command)), listeners };
}

function installMediaSession() {
  const actions = new Map<string, MediaSessionActionHandler | null>();
  const mediaSession = {
    metadata: null as MediaMetadata | null,
    playbackState: 'none' as MediaSessionPlaybackState,
    setActionHandler: jest.fn((action: string, handler: MediaSessionActionHandler | null) => {
      if (action === 'seekto') throw new DOMException('Unsupported', 'NotSupportedError');
      actions.set(action, handler);
    }),
  };
  Object.defineProperty(navigator, 'mediaSession', { configurable: true, value: mediaSession });
  class FakeMetadata { constructor(public init: MediaMetadataInit) {} }
  Object.defineProperty(globalThis, 'MediaMetadata', { configurable: true, value: FakeMetadata });
  return { actions, mediaSession };
}

const handlers = () => ({ play: jest.fn(), pause: jest.fn(), next: jest.fn(), previous: jest.fn() });

afterEach(() => {
  resetPlaybackSessionForTests();
  Reflect.deleteProperty(window, 'tdfDesktop');
  Reflect.deleteProperty(navigator, 'mediaSession');
  Reflect.deleteProperty(globalThis, 'MediaMetadata');
});

it('routes system media actions to the owner that most recently started audio', () => {
  const { actions, mediaSession } = installMediaSession();
  const music = handlers();
  const radio = { play: jest.fn(), pause: jest.fn() };
  claimPlaybackSession('music', music, { status: 'playing', title: 'Pista', artist: 'Artista', album: 'release-1' });
  expect((mediaSession.metadata as unknown as { init: MediaMetadataInit }).init).toMatchObject({ title: 'Pista', artist: 'Artista', album: 'release-1' });
  expect(mediaSession.playbackState).toBe('playing');

  claimPlaybackSession('radio', radio, { status: 'playing', title: 'Emisora', artist: 'Radio TDF' });
  actions.get('pause')?.({ action: 'pause' });
  expect(radio.pause).toHaveBeenCalledTimes(1);
  expect(music.pause).not.toHaveBeenCalled();
  // The radio exposes no track navigation, so those actions are cleared.
  expect(actions.get('nexttrack')).toBeNull();
  expect(getActivePlaybackOwner()).toBe('radio');
});

it('does not let a passive state update steal the session from the active owner', () => {
  installMediaSession();
  const music = handlers();
  const radio = { play: jest.fn(), pause: jest.fn() };
  claimPlaybackSession('music', music, { status: 'playing', title: 'Pista', artist: 'Artista' });
  claimPlaybackSession('radio', radio, { status: 'playing', title: 'Emisora', artist: 'Radio TDF' });
  updatePlaybackSession('music', { status: 'paused', title: 'Pista', artist: 'Artista' });
  expect(getActivePlaybackOwner()).toBe('radio');
  expect(dispatchPlaybackCommand('toggle')).toBe(true);
  expect(radio.pause).toHaveBeenCalledTimes(1);
});

it('returns the session to the previous owner when the active one releases it', () => {
  const { actions, mediaSession } = installMediaSession();
  const music = handlers();
  claimPlaybackSession('music', music, { status: 'paused', title: 'Pista', artist: 'Artista' });
  claimPlaybackSession('radio', { play: jest.fn(), pause: jest.fn() }, { status: 'playing', title: 'Emisora', artist: 'Radio TDF' });
  releasePlaybackSession('radio');
  expect(getActivePlaybackOwner()).toBe('music');
  actions.get('nexttrack')?.({ action: 'nexttrack' });
  expect(music.next).toHaveBeenCalledTimes(1);
  releasePlaybackSession('music');
  expect(mediaSession.metadata).toBeNull();
  expect(mediaSession.playbackState).toBe('none');
  expect(actions.get('play')).toBeNull();
  expect(dispatchPlaybackCommand('play')).toBe(false);
});

it('ignores updates for owners that never claimed the session', () => {
  installMediaSession();
  updatePlaybackSession('radio', { status: 'paused', title: 'Emisora', artist: 'Radio TDF' });
  expect(getActivePlaybackOwner()).toBeNull();
});

it('reports every change to the desktop shell and executes its commands on the active owner', () => {
  const shell = installShell();
  const music = handlers();
  claimPlaybackSession('music', music, { status: 'playing', title: 'Pista', artist: 'Artista' });
  expect(shell.reports.at(-1)).toEqual({ owner: 'music', status: 'playing', title: 'Pista', artist: 'Artista', canNext: true, canPrevious: true });
  shell.send('toggle');
  expect(music.pause).toHaveBeenCalledTimes(1);
  updatePlaybackSession('music', { status: 'paused', title: 'Pista', artist: 'Artista' });
  expect(shell.reports.at(-1)?.status).toBe('paused');
  shell.send('toggle');
  expect(music.play).toHaveBeenCalledTimes(1);
  shell.send('next');
  expect(music.next).toHaveBeenCalledTimes(1);
  releasePlaybackSession('music');
  expect(shell.reports.at(-1)).toEqual({ owner: null, status: 'idle', title: null, artist: null, canNext: false, canPrevious: false });
  expect(shell.listeners.size).toBe(1);
});

it('keeps working without Media Session or a desktop shell', () => {
  const music = handlers();
  expect(() => claimPlaybackSession('music', music, { status: 'playing', title: 'Pista', artist: 'Artista' })).not.toThrow();
  expect(dispatchPlaybackCommand('previous')).toBe(true);
  expect(music.previous).toHaveBeenCalledTimes(1);
});

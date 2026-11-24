import { jest } from '@jest/globals';
import { act, cleanup, render, waitFor } from '@testing-library/react';
import type { PlayerTrack } from './types';

// MEDIA-MUSIC-CONTINUATION-001: a continuation still loading when the listener
// expresses a newer playback intent must not select or start anything.
jest.unstable_mockModule('./analytics', () => ({ installPlayerAnalyticsBridge: () => () => undefined }));
jest.unstable_mockModule('./assetAccess', () => ({ authorizePlayerAsset: jest.fn() }));
jest.unstable_mockModule('../utils/logger', () => ({ logger: { warn: jest.fn() } }));
const rolloutEnabled = jest.fn(() => true);
jest.unstable_mockModule('../config/queueContinuationRollout', () => ({ isQueueContinuationEnabled: rolloutEnabled }));
jest.unstable_mockModule('./loadQueueContinuation', () => ({ loadQueueContinuation: jest.fn() }));
const { PlayerProvider, usePlayer } = await import('./PlayerProvider');
const { loadQueueContinuation } = await import('./loadQueueContinuation');
const { PLAYER_TAKEOVER_EVENT, RADIO_TAKEOVER_EVENT } = await import('./types');

const track = (id: string): PlayerTrack => ({ id, title: id, artist: 'Synthetic', releaseId: 'release',
  sources: [{ url: `https://media.invalid/${id}.m4a`, quality: 'high', mediaType: 'audio/mp4' }] });

let player: ReturnType<typeof usePlayer>;
function Probe() { player = usePlayer(); return <div>{player.status}</div>; }

let resolveContinuation: (tracks: PlayerTrack[]) => void = () => undefined;
let continuationSignal: AbortSignal | undefined;

beforeEach(() => {
  localStorage.clear();
  rolloutEnabled.mockReturnValue(true);
  jest.spyOn(HTMLMediaElement.prototype, 'currentSrc', 'get').mockImplementation(function (this: HTMLMediaElement) { return this.src; });
  jest.spyOn(HTMLMediaElement.prototype, 'load').mockImplementation(() => undefined);
  jest.spyOn(HTMLMediaElement.prototype, 'pause').mockImplementation(() => undefined);
  jest.spyOn(HTMLMediaElement.prototype, 'play').mockResolvedValue(undefined);
  continuationSignal = undefined;
  jest.mocked(loadQueueContinuation).mockReset();
  jest.mocked(loadQueueContinuation).mockImplementation((_tracks, options) => {
    continuationSignal = options?.signal;
    return new Promise<PlayerTrack[]>((resolve) => { resolveContinuation = resolve; });
  });
});
afterEach(() => { cleanup(); jest.restoreAllMocks(); localStorage.clear(); });

async function endQueue(partyId: number | null = 42, ready = true) {
  const view = render(<PlayerProvider identity={{ ready, partyId }}><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(track('one')); });
  await waitFor(() => expect(player.status).toBe('playing'));
  const audio = view.container.querySelector('audio')!;
  await act(async () => { audio.dispatchEvent(new Event('ended')); });
  return audio;
}

it.each([
  { scenario: 'the feature is off', enabled: false, partyId: 42, ready: true },
  { scenario: 'the listener is anonymous', enabled: true, partyId: null, ready: true },
  { scenario: 'identity is still loading', enabled: true, partyId: 42, ready: false },
])('pauses at the end without requesting tracks when $scenario', async ({ enabled, partyId, ready }) => {
  rolloutEnabled.mockReturnValue(enabled);
  await endQueue(partyId, ready);
  expect(loadQueueContinuation).not.toHaveBeenCalled();
  expect(player.currentTrack?.id).toBe('one');
  expect(player.status).toBe('paused');
});

it('appends and starts the continuation when nothing intervenes', async () => {
  await endQueue();
  expect(loadQueueContinuation).toHaveBeenCalledTimes(1);
  await act(async () => { resolveContinuation([track('similar')]); });
  await waitFor(() => expect(player.currentTrack?.id).toBe('similar'));
  expect(player.queue.tracks.map((item) => item.id)).toEqual(['one', 'similar']);
});

it.each<[string, () => void]>([
  ['an explicit pause', () => { player.pause(); }],
  ['an explicit play', () => { void player.play(); }],
  ['a radio takeover', () => { window.dispatchEvent(new CustomEvent(RADIO_TAKEOVER_EVENT)); }],
  ['selecting a track', () => { player.selectTrack(0); }],
  ['going to the previous track', () => { player.previous(); }],
])('discards a pending continuation after %s', async (_name, intent) => {
  await endQueue();
  act(() => { intent(); });
  expect(continuationSignal?.aborted).toBe(true);
  const takeovers = jest.fn();
  window.addEventListener(PLAYER_TAKEOVER_EVENT, takeovers);
  const plays = jest.mocked(HTMLMediaElement.prototype.play).mock.calls.length;
  await act(async () => { resolveContinuation([track('similar')]); });
  window.removeEventListener(PLAYER_TAKEOVER_EVENT, takeovers);
  expect(player.queue.tracks.map((item) => item.id)).toEqual(['one']);
  expect(player.currentTrack?.id).toBe('one');
  expect(takeovers).not.toHaveBeenCalled();
  expect(jest.mocked(HTMLMediaElement.prototype.play).mock.calls.length).toBe(plays);
});

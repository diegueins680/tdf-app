import { jest } from '@jest/globals';
import { act, cleanup, render, waitFor } from '@testing-library/react';
import type { PlayerTrack } from './types';

// MEDIA-MUSIC-CONTINUATION-001: a continuation still loading when the listener
// expresses a newer playback intent must not select or start anything.
jest.unstable_mockModule('./analytics', () => ({ installPlayerAnalyticsBridge: () => () => undefined }));
jest.unstable_mockModule('./assetAccess', () => ({ authorizePlayerAsset: jest.fn() }));
jest.unstable_mockModule('../utils/logger', () => ({ logger: { warn: jest.fn() } }));
jest.unstable_mockModule('../config/queueContinuationRollout', () => ({ isQueueContinuationEnabled: () => true }));
jest.unstable_mockModule('./queueContinuation', () => ({ shouldContinueQueue: () => true }));
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

async function endQueue() {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(track('one')); });
  await waitFor(() => expect(player.status).toBe('playing'));
  const audio = view.container.querySelector('audio')!;
  await act(async () => { audio.dispatchEvent(new Event('ended')); });
  expect(loadQueueContinuation).toHaveBeenCalledTimes(1);
  return audio;
}

it('appends and starts the continuation when nothing intervenes', async () => {
  await endQueue();
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

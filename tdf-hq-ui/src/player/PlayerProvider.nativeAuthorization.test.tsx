import { jest } from '@jest/globals';
import { act, cleanup, render } from '@testing-library/react';
import type { PlayerTrack } from './types';

jest.unstable_mockModule('./analytics', () => ({ installPlayerAnalyticsBridge: () => () => undefined }));
jest.unstable_mockModule('./assetAccess', () => ({ authorizePlayerAsset: jest.fn() }));
jest.unstable_mockModule('../utils/logger', () => ({ logger: { warn: jest.fn() } }));
const { PlayerProvider, usePlayer } = await import('./PlayerProvider');
const { authorizePlayerAsset } = await import('./assetAccess');
let player: ReturnType<typeof usePlayer>;
function Probe() { player = usePlayer(); return null; }

beforeEach(() => {
  localStorage.clear();
  jest.mocked(authorizePlayerAsset).mockReset();
  jest.spyOn(HTMLMediaElement.prototype, 'currentSrc', 'get').mockImplementation(function (this: HTMLMediaElement) { return this.src; });
  jest.spyOn(HTMLMediaElement.prototype, 'load').mockImplementation(() => undefined);
  jest.spyOn(HTMLMediaElement.prototype, 'pause').mockImplementation(() => undefined);
  jest.spyOn(HTMLMediaElement.prototype, 'paused', 'get').mockReturnValue(true);
  jest.spyOn(HTMLMediaElement.prototype, 'play').mockResolvedValue(undefined);
});
afterEach(() => { cleanup(); jest.restoreAllMocks(); });

it.each([401, 403, 404])('does not downgrade denied native video access %s to an unbound audio URL', async status => {
  jest.mocked(authorizePlayerAsset).mockRejectedValue({ status });
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  const selected: PlayerTrack = { id: 'protected-native', title: 'Protected native', artist: 'Fixture', sources: [
    { assetId: 'protected-video', url: 'https://media.invalid/stale.mp4', quality: 'high', mediaType: 'video/mp4' },
    { url: 'https://media.invalid/unbound-audio.m4a', quality: 'high', mediaType: 'audio/mp4' },
  ] };
  await act(async () => player.playTrack(selected));
  expect(authorizePlayerAsset).toHaveBeenCalledWith('protected-video', expect.any(AbortSignal));
  expect(view.container.querySelector('video')?.hasAttribute('src')).toBe(false);
  expect(player.status).toBe('unavailable');
  expect(HTMLMediaElement.prototype.play).not.toHaveBeenCalled();
});

it('applies the complete track identity boundary before an explicit audio-mode filter', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => {
    player.setMediaMode('audio');
    player.playTrack({ id: 'malformed-native', title: 'Malformed native', artist: 'Fixture', sources: [
      { assetId: '', url: 'https://media.invalid/stale.mp4', quality: 'high', mediaType: 'video/mp4' },
      { url: 'https://media.invalid/unbound-audio.m4a', quality: 'high', mediaType: 'audio/mp4' },
    ] });
  });
  expect(authorizePlayerAsset).not.toHaveBeenCalled();
  expect(view.container.querySelector('video')?.hasAttribute('src')).toBe(false);
  expect(player.status).toBe('unavailable');
});

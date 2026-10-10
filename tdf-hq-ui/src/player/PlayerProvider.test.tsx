import { jest } from '@jest/globals';
import { act, cleanup, render, waitFor } from '@testing-library/react';
import type { PlayerTrack, RepeatMode } from './types';
import type { ReactNode } from 'react';
import { createPortal } from 'react-dom';

jest.unstable_mockModule('./analytics', () => ({ installPlayerAnalyticsBridge: () => () => undefined }));
jest.unstable_mockModule('./assetAccess', () => ({ authorizePlayerAsset: jest.fn() }));
jest.unstable_mockModule('../utils/logger', () => ({ logger: { warn: jest.fn() } }));
const { PlayerProvider, usePlayer } = await import('./PlayerProvider');
const { authorizePlayerAsset } = await import('./assetAccess');

const track = (id: string): PlayerTrack => ({ id, title: id, artist: 'Synthetic', releaseId: 'release',
  sources: [{ url: `https://media.invalid/${id}.m4a`, quality: 'high', mediaType: 'audio/mp4' }] });

let player: ReturnType<typeof usePlayer>;
function Probe() { player = usePlayer(); return <div>{player.status}</div>; }

beforeEach(() => {
  localStorage.clear();
  jest.spyOn(HTMLMediaElement.prototype, 'load').mockImplementation(() => undefined);
  jest.spyOn(HTMLMediaElement.prototype, 'pause').mockImplementation(() => undefined);
  jest.spyOn(HTMLMediaElement.prototype, 'play').mockResolvedValue(undefined);
});
afterEach(() => { cleanup(); jest.restoreAllMocks(); localStorage.clear(); });

it('does not replace authorized full audio with a same-quality preview', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => {
    player.setQuality('low');
    player.playTrack({ ...track('complete'), sources: [
      { url: 'https://media.invalid/full.m4a', quality: 'low', preview: false },
      { url: 'https://media.invalid/preview.m4a', quality: 'low', preview: true },
    ] });
  });
  expect(view.container.querySelector('audio')?.getAttribute('src')).toBe('https://media.invalid/full.m4a');
});

it('continues to play a preview when no full source was authorized', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack({ ...track('preview'), previewEndMs: 1750,
    sources: [{ url: 'https://media.invalid/preview.m4a', quality: 'low', preview: true }] }); });
  expect(view.container.querySelector('audio')?.getAttribute('src')).toBe('https://media.invalid/preview.m4a');
  expect(player.currentTrack?.previewEndMs).toBe(1750);
});

for (const mode of ['track', 'queue', 'release'] as RepeatMode[]) {
  it(`restarts the native element when ${mode} repetition selects the same item`, async () => {
    const view = render(<PlayerProvider><Probe /></PlayerProvider>);
    await act(async () => { player.playTrack(track('one')); });
    await waitFor(() => expect(player.status).toBe('playing'));
    act(() => { player.setRepeat(mode); });
    const audio = view.container.querySelector('audio')!;
    audio.currentTime = 24;
    const before = jest.mocked(audio.play).mock.calls.length;
    await act(async () => { audio.dispatchEvent(new Event('ended')); });
    expect(audio.currentTime).toBe(0);
    expect(jest.mocked(audio.play).mock.calls.length).toBe(before + 1);
    expect(player.currentTrack?.id).toBe('one');
    expect(player.status).toBe('playing');
  });
}

it('an obsolete play rejection cannot remove or stop the next source', async () => {
  let rejectOld: (error: Error) => void = () => undefined;
  jest.mocked(HTMLMediaElement.prototype.play).mockImplementationOnce(() => new Promise<void>((_resolve, reject) => { rejectOld = reject; }));
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(track('one')); });
  await act(async () => { player.playTrack(track('two')); });
  await waitFor(() => expect(player.status).toBe('playing'));
  await act(async () => { rejectOld(new DOMException('Previous source was replaced', 'AbortError')); });
  expect(view.container.querySelector('audio')?.getAttribute('src')).toBe('https://media.invalid/two.m4a');
  expect(player.status).toBe('playing');
  expect(player.error).toBeNull();
});

it('a late play resolution does not undo an explicit pause', async () => {
  let resolvePlay: () => void = () => undefined;
  jest.mocked(HTMLMediaElement.prototype.play).mockImplementationOnce(() => new Promise<void>((resolve) => { resolvePlay = resolve; }));
  render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(track('one')); });
  act(() => { player.pause(); });
  await act(async () => { resolvePlay(); });
  expect(player.status).toBe('paused');
});

it('does not save the previous source position onto a track awaiting authorization', async () => {
  let resolveSecond: (value: Awaited<ReturnType<typeof authorizePlayerAsset>>) => void = () => undefined;
  jest.mocked(authorizePlayerAsset)
    .mockResolvedValueOnce({ url: 'https://media.invalid/one.m4a' } as Awaited<ReturnType<typeof authorizePlayerAsset>>)
    .mockImplementationOnce(() => new Promise((resolve) => { resolveSecond = resolve; }));
  let nativeSource = '';
  jest.spyOn(HTMLMediaElement.prototype, 'currentSrc', 'get').mockImplementation(() => nativeSource);
  jest.mocked(HTMLMediaElement.prototype.load).mockImplementation(function (this: HTMLMediaElement) {
    nativeSource = this.getAttribute('src') ?? '';
    this.currentTime = 0;
  });
  const securedTrack = (id: string): PlayerTrack => ({ ...track(id),
    sources: [{ ...track(id).sources[0]!, assetId: id }] });
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(securedTrack('one')); });
  await waitFor(() => expect(player.status).toBe('playing'));
  const audio = view.container.querySelector('audio')!;
  act(() => { audio.currentTime = 8; audio.dispatchEvent(new Event('timeupdate')); });
  await act(async () => { player.playTrack(securedTrack('two')); });
  // pause/load queue native timeupdate events while the next URL is pending.
  act(() => { audio.dispatchEvent(new Event('timeupdate')); });
  await act(async () => { resolveSecond({ url: 'https://media.invalid/two.m4a' } as Awaited<ReturnType<typeof authorizePlayerAsset>>); });
  expect(audio.getAttribute('src')).toBe('https://media.invalid/two.m4a');
  expect(audio.currentTime).toBe(0);
});

it.each<[string, ReactNode]>([
  ['button', <button key="button" data-testid="shortcut-target">Opciones</button>],
  ['link child', <a key="link" href="/musica"><span data-testid="shortcut-target">Catálogo</span></a>],
  ['combobox', <div key="combo" role="combobox" aria-controls="shortcut-options" aria-expanded="false" data-testid="shortcut-target" />],
  ['dialog content', <div key="dialog" role="dialog"><div data-testid="shortcut-target" /></div>],
])('leaves Space to the focused %s instead of pausing playback', async (_name, control) => {
  const view = render(<PlayerProvider><Probe />{control}</PlayerProvider>);
  await act(async () => { player.playTrack(track('one')); });
  const event = new KeyboardEvent('keydown', { key: ' ', code: 'Space', bubbles: true, cancelable: true });
  act(() => { view.getByTestId('shortcut-target').dispatchEvent(event); });
  expect(event.defaultPrevented).toBe(false);
  expect(player.status).toBe('playing');
});

it.each<KeyboardEventInit>([
  { ctrlKey: true }, { metaKey: true }, { altKey: true }, { repeat: true }, { isComposing: true },
])('ignores modified, repeated or composing Space: %j', async (options) => {
  render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(track('one')); });
  const event = new KeyboardEvent('keydown', { key: ' ', code: 'Space', bubbles: true, cancelable: true, ...options });
  act(() => { window.dispatchEvent(event); });
  expect(event.defaultPrevented).toBe(false);
  expect(player.status).toBe('playing');
});

it('honors an already handled shortcut and still toggles unhandled Space once', async () => {
  render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(track('one')); });
  const handled = new KeyboardEvent('keydown', { key: ' ', code: 'Space', cancelable: true });
  handled.preventDefault();
  act(() => { window.dispatchEvent(handled); });
  expect(player.status).toBe('playing');
  const unhandled = new KeyboardEvent('keydown', { key: ' ', code: 'Space', cancelable: true });
  act(() => { window.dispatchEvent(unhandled); });
  expect(player.status).toBe('paused');
  expect(unhandled.defaultPrevented).toBe(true);
});

it.each(['loadedmetadata', 'canplay'])('finishes loading a changed quality while paused on %s without starting playback', async (eventType) => {
  jest.spyOn(HTMLMediaElement.prototype, 'currentSrc', 'get').mockImplementation(function (this: HTMLMediaElement) { return this.src; });
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(track('one')); });
  act(() => { player.pause(); });
  await act(async () => { player.setQuality('low'); });
  expect(player.status).toBe('loading');
  const audio = view.container.querySelector('audio')!;
  jest.spyOn(audio, 'paused', 'get').mockReturnValue(true);
  const attempts = jest.mocked(audio.play).mock.calls.length;
  act(() => { audio.dispatchEvent(new Event(eventType)); });
  expect(player.status).toBe('paused');
  expect(jest.mocked(audio.play).mock.calls.length).toBe(attempts);
});

it('does not mute on unmatched typeahead inside a portalled listbox', async () => {
  const view = render(<PlayerProvider><Probe />{createPortal(<div role="listbox" tabIndex={0} aria-label="Repetición">
    <div role="option" tabIndex={-1} aria-selected="true" data-testid="repeat-option">Repetir pista</div>
  </div>, document.body)}</PlayerProvider>);
  await act(async () => { player.playTrack(track('one')); });
  act(() => { view.getByTestId('repeat-option').dispatchEvent(new KeyboardEvent('keydown', { key: 'm', code: 'KeyM', bubbles: true })); });
  expect(player.muted).toBe(false);
});

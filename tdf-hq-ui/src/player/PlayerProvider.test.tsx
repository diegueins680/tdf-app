import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import type { PlayerTrack, RepeatMode } from './types';
import { PLAYER_ANALYTICS_EVENT, PLAYER_LOAD_TRACK_EVENT } from './types';
import type { ReactNode } from 'react';
import { useLayoutEffect } from 'react';
import { createPortal } from 'react-dom';

const recoveryRollout = { enabled: true };
jest.unstable_mockModule('../config/artistMediaRecoveryRollout', () => ({ isArtistMediaRecoveryEnabled: () => recoveryRollout.enabled }));
jest.unstable_mockModule('./analytics', () => ({ installPlayerAnalyticsBridge: () => () => undefined }));
jest.unstable_mockModule('./assetAccess', () => ({ authorizePlayerAsset: jest.fn() }));
jest.unstable_mockModule('../utils/logger', () => ({ logger: { warn: jest.fn() } }));
const { PlayerProvider, usePlayer } = await import('./PlayerProvider');
const { authorizePlayerAsset } = await import('./assetAccess');
const { default: GlobalPlayer } = await import('./GlobalPlayer');
const { dispatchReleaseToRadio } = await import('../features/releases/ReleasePlayerActions');

const track = (id: string): PlayerTrack => ({ id, title: id, artist: 'Synthetic', releaseId: 'release',
  sources: [{ url: `https://media.invalid/${id}.m4a`, quality: 'high', mediaType: 'audio/mp4' }] });

let player: ReturnType<typeof usePlayer>;
function Probe() { player = usePlayer(); return <div>{player.status}</div>; }

beforeEach(() => {
  recoveryRollout.enabled = true;
  localStorage.clear();
  jest.mocked(authorizePlayerAsset).mockReset();
  jest.spyOn(HTMLMediaElement.prototype, 'currentSrc', 'get').mockImplementation(function (this: HTMLMediaElement) { return this.src; });
  jest.spyOn(HTMLMediaElement.prototype, 'load').mockImplementation(() => undefined);
  jest.spyOn(HTMLMediaElement.prototype, 'pause').mockImplementation(() => undefined);
  jest.spyOn(HTMLMediaElement.prototype, 'paused', 'get').mockReturnValue(false);
  jest.spyOn(HTMLMediaElement.prototype, 'play').mockImplementation(function (this: HTMLMediaElement) {
    this.dispatchEvent(new Event('playing'));
    return Promise.resolve();
  });
});
afterEach(() => { cleanup(); jest.restoreAllMocks(); localStorage.clear(); });

describe('confirmed playback analytics', () => {
  const starts = () => {
    const events: Record<string, unknown>[] = [];
    const listener = (event: Event) => {
      const detail = (event as CustomEvent<Record<string, unknown>>).detail;
      if (detail['eventType'] === 'play_start') events.push(detail);
    };
    window.addEventListener(PLAYER_ANALYTICS_EVENT, listener);
    return { events, stop: () => window.removeEventListener(PLAYER_ANALYTICS_EVENT, listener) };
  };

  it('does not count a fulfilled play promise until the current source actually plays', async () => {
    jest.mocked(HTMLMediaElement.prototype.play).mockResolvedValue(undefined);
    const observed = starts();
    try {
      const view = render(<PlayerProvider><Probe /></PlayerProvider>);
      await act(async () => { player.playTrack(track('one')); });
      expect(observed.events).toHaveLength(0);
      const audio = view.container.querySelector('audio')!;
      act(() => { audio.dispatchEvent(new Event('playing')); });
      expect(observed.events).toHaveLength(1);
      expect(observed.events[0]).toMatchObject({ recordingId: 'one', quality: 'high' });
    } finally { observed.stop(); }
  });

  it('counts the native playing event even if the play promise is still pending, once per attempt', async () => {
    let settle: () => void = () => undefined;
    jest.mocked(HTMLMediaElement.prototype.play).mockImplementationOnce(() => new Promise<void>((resolve) => { settle = resolve; }));
    const observed = starts();
    try {
      const view = render(<PlayerProvider><Probe /></PlayerProvider>);
      await act(async () => { player.playTrack(track('one')); });
      const audio = view.container.querySelector('audio')!;
      act(() => { audio.dispatchEvent(new Event('playing')); });
      expect(observed.events).toHaveLength(1);
      act(() => { audio.dispatchEvent(new Event('waiting')); audio.dispatchEvent(new Event('playing')); });
      await act(async () => { settle(); });
      expect(observed.events).toHaveLength(1);
      act(() => { player.pause(); audio.dispatchEvent(new Event('playing')); });
      expect(observed.events).toHaveLength(1);
      await act(async () => { await player.play(); });
      expect(observed.events).toHaveLength(2);
    } finally { observed.stop(); }
  });

  it('does not count rejected autoplay or a stale source while the next authorization is pending', async () => {
    jest.mocked(HTMLMediaElement.prototype.play).mockRejectedValueOnce(new DOMException('Gesture required', 'NotAllowedError'));
    const observed = starts();
    try {
      const view = render(<PlayerProvider><Probe /></PlayerProvider>);
      await act(async () => { player.playTrack(track('blocked')); });
      act(() => { view.container.querySelector('audio')!.dispatchEvent(new Event('playing')); });
      expect(observed.events).toHaveLength(0);
      jest.mocked(authorizePlayerAsset).mockImplementationOnce(() => new Promise(() => undefined));
      await act(async () => { player.playTrack({ ...track('pending'), sources: [{ ...track('pending').sources[0]!, assetId: 'pending' }] }); });
      act(() => { view.container.querySelector('audio')!.dispatchEvent(new Event('playing')); });
      expect(observed.events).toHaveLength(0);
    } finally { observed.stop(); }
  });
});

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

it('a retired source error during the next render cannot cancel automatic recovery', async () => {
  jest.mocked(HTMLMediaElement.prototype.load).mockImplementation(function (this: HTMLMediaElement) {
    Object.defineProperty(this, 'error', { configurable: true, value: null });
  });
  function ErrorBeforeSourceCleanup() {
    const context = usePlayer();
    useLayoutEffect(() => {
      if (context.currentTrack?.id === 'two') {
        // Native events can arrive after the queue renders but before passive
        // source/listener cleanup. The retired element still identifies one.
        document.querySelector('audio')!.dispatchEvent(new Event('error'));
      }
    }, [context.currentTrack?.id]);
    return null;
  }
  const view = render(<PlayerProvider><Probe /><ErrorBeforeSourceCleanup /></PlayerProvider>);
  await act(async () => { player.playTrack(track('one'), [track('one'), track('two')]); });
  const audio = view.container.querySelector('audio')!;
  expect(player.status).toBe('playing');
  Object.defineProperty(audio, 'error', { configurable: true, value: { code: 3 } });
  await act(async () => { audio.dispatchEvent(new Event('error')); });
  expect(player.currentTrack?.id).toBe('two');
  expect(audio.getAttribute('src')).toBe('https://media.invalid/two.m4a');
  expect(player.status).toBe('playing');
  expect(player.error).toBeNull();
  expect(jest.mocked(audio.play)).toHaveBeenCalledTimes(2);
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


it('retains a direct source after autoplay policy blocks play so a gesture can retry', async () => {
  jest.mocked(HTMLMediaElement.prototype.play).mockRejectedValueOnce(new DOMException('Gesture required', 'NotAllowedError'));
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(track('blocked')); });
  expect(player.status).toBe('paused');
  expect(view.container.querySelector('audio')?.getAttribute('src')).toBe('https://media.invalid/blocked.m4a');
  await act(async () => { await player.play(); });
  expect(player.status).toBe('playing');
  expect(player.error).toBeNull();
});

it('does not present queued native playing or waiting events as playback while authorization is pending', async () => {
  jest.mocked(authorizePlayerAsset).mockImplementationOnce(() => new Promise(() => undefined));
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack({ ...track('pending'), sources: [{ ...track('pending').sources[0]!, assetId: 'pending' }] }); });
  const audio = view.container.querySelector('audio')!;
  act(() => { audio.dispatchEvent(new Event('playing')); audio.dispatchEvent(new Event('waiting')); });
  expect(player.status).toBe('loading');
});

it('ignores a queued native playing event after an explicit pause', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(track('one')); });
  act(() => { player.pause(); });
  act(() => { view.container.querySelector('audio')!.dispatchEvent(new Event('playing')); });
  expect(player.status).toBe('paused');
});

it('continues rendering when an exposed Media Session action is unsupported', () => {
  Object.defineProperty(navigator, 'mediaSession', { configurable: true, value: {
    metadata: null,
    setActionHandler: jest.fn((action: string) => { if (action === 'seekto') throw new DOMException('Unsupported', 'NotSupportedError'); }),
  } });
  try {
    expect(() => render(<PlayerProvider><Probe /></PlayerProvider>)).not.toThrow();
  } finally { Reflect.deleteProperty(navigator, 'mediaSession'); }
});

it('recovers a direct source at its listening position instead of restarting the recording', async () => {
  jest.useFakeTimers();
  jest.spyOn(HTMLMediaElement.prototype, 'currentSrc', 'get').mockImplementation(function (this: HTMLMediaElement) { return this.src; });
  jest.mocked(HTMLMediaElement.prototype.load).mockImplementation(function (this: HTMLMediaElement) { this.currentTime = 0; });
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  try {
    await act(async () => { player.playTrack(track('mix')); });
    const audio = view.container.querySelector('audio')!;
    act(() => { audio.currentTime = 1200; audio.dispatchEvent(new Event('timeupdate')); audio.dispatchEvent(new Event('error')); });
    await act(async () => { jest.advanceTimersByTime(1000); });
    expect(audio.currentTime).toBe(0);
    jest.spyOn(audio, 'readyState', 'get').mockReturnValue(HTMLMediaElement.HAVE_METADATA);
    await act(async () => { audio.dispatchEvent(new Event('loadedmetadata')); });
    expect(audio.currentTime).toBe(1200);
    expect(player.currentTrack?.id).toBe('mix');
  } finally { jest.useRealTimers(); }
});

it('does not repeatedly reload media that cannot be decoded', async () => {
  jest.useFakeTimers();
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  try {
    await act(async () => { player.playTrack(track('unsupported')); });
    const audio = view.container.querySelector('audio')!;
    Object.defineProperty(audio, 'error', { configurable: true, value: { code: 3 } });
    const before = jest.mocked(audio.load).mock.calls.length;
    act(() => { audio.dispatchEvent(new Event('error')); });
    await act(async () => { jest.advanceTimersByTime(10000); });
    expect(jest.mocked(audio.load).mock.calls.length).toBe(before);
    expect(player.status).toBe('unavailable');
  } finally { jest.useRealTimers(); }
});

it('resolves only the selected track and falls back from denied full audio to its own authorized preview', async () => {
  jest.mocked(authorizePlayerAsset)
    .mockRejectedValueOnce({ status: 403 })
    .mockResolvedValueOnce({ url: 'https://media.invalid/preview.m4a' } as Awaited<ReturnType<typeof authorizePlayerAsset>>);
  const one = { ...track('one'), previewEndMs: 30000, sources: [
    { url: '', assetId: 'one-full', quality: 'high' as const, mediaType: 'audio/mp4' },
    { url: '', assetId: 'one-preview', quality: 'low' as const, mediaType: 'audio/mp4', preview: true },
  ] };
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(one, [one, { ...track('two'), sources: [{ ...track('two').sources[0]!, assetId: 'two-full' }] }]); });
  expect(jest.mocked(authorizePlayerAsset).mock.calls.map(([id]) => id)).toEqual(['one-full', 'one-preview']);
  expect(player.currentTrack?.id).toBe('one');
  expect(player.resolvedQuality).toBe('low');
  const audio = view.container.querySelector('audio')!;
  expect(audio.src).toBe('https://media.invalid/preview.m4a');
  act(() => { audio.currentTime = 30; audio.dispatchEvent(new Event('timeupdate')); });
  expect(player.status).toBe('paused');
});

it('does not apply a preview limit to the authorized full rendition', async () => {
  jest.mocked(authorizePlayerAsset).mockResolvedValueOnce({ url: 'https://media.invalid/full.m4a' } as Awaited<ReturnType<typeof authorizePlayerAsset>>);
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack({ ...track('one'), previewEndMs: 30000, sources: [
    { url: '', assetId: 'one-full', quality: 'high', mediaType: 'audio/mp4' },
    { url: '', assetId: 'one-preview', quality: 'low', mediaType: 'audio/mp4', preview: true },
  ] }); });
  const audio = view.container.querySelector('audio')!;
  act(() => { audio.currentTime = 31; audio.dispatchEvent(new Event('timeupdate')); });
  expect(player.status).toBe('playing');
  expect(jest.mocked(authorizePlayerAsset).mock.calls.map(([id]) => id)).toEqual(['one-full']);
});

it('aborts superseded authorization and rejects its late result without substituting the newer selection', async () => {
  let resolveOld: (value: Awaited<ReturnType<typeof authorizePlayerAsset>>) => void = () => undefined;
  jest.mocked(authorizePlayerAsset)
    .mockImplementationOnce(() => new Promise((resolve) => { resolveOld = resolve; }))
    .mockResolvedValueOnce({ url: 'https://media.invalid/two.m4a' } as Awaited<ReturnType<typeof authorizePlayerAsset>>);
  const secured = (id: string) => ({ ...track(id), sources: [{ ...track(id).sources[0]!, assetId: id }] });
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(secured('one')); });
  const signal = jest.mocked(authorizePlayerAsset).mock.calls[0]?.[1];
  await act(async () => { player.playTrack(secured('two')); });
  expect(signal?.aborted).toBe(true);
  await act(async () => { resolveOld({ url: 'https://media.invalid/one.m4a' } as Awaited<ReturnType<typeof authorizePlayerAsset>>); });
  expect(view.container.querySelector('audio')?.src).toBe('https://media.invalid/two.m4a');
  expect(player.currentTrack?.id).toBe('two');
  expect(player.status).toBe('playing');
});

it('never substitutes another queued track when every rendition of the selection is denied', async () => {
  jest.mocked(authorizePlayerAsset).mockRejectedValue({ status: 403 });
  const secured = { ...track('one'), sources: [{ ...track('one').sources[0]!, assetId: 'one-full' }] };
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(secured, [secured, track('two')]); });
  expect(player.currentTrack?.id).toBe('one');
  expect(player.status).toBe('unavailable');
  expect(view.container.querySelector('audio')?.getAttribute('src')).toBeNull();
  expect(HTMLMediaElement.prototype.play).not.toHaveBeenCalled();
  expect(authorizePlayerAsset).toHaveBeenCalledTimes(1);
});

it('bounds transient authorization retries at two and exposes a terminal unavailable state', async () => {
  jest.useFakeTimers();
  jest.mocked(authorizePlayerAsset).mockRejectedValue({ status: 503 });
  render(<PlayerProvider><Probe /></PlayerProvider>);
  try {
    await act(async () => { player.playTrack({ ...track('one'), sources: [{ ...track('one').sources[0]!, assetId: 'one' }] }); });
    expect(player.status).toBe('loading');
    await act(async () => { jest.advanceTimersByTime(1000); });
    await act(async () => { jest.advanceTimersByTime(2000); });
    expect(authorizePlayerAsset).toHaveBeenCalledTimes(3);
    expect(player.status).toBe('unavailable');
    await act(async () => { jest.advanceTimersByTime(60000); });
    expect(authorizePlayerAsset).toHaveBeenCalledTimes(3);
  } finally { jest.useRealTimers(); }
});

it('cancels scheduled recovery on explicit pause and provider unmount', async () => {
  jest.useFakeTimers();
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  try {
    await act(async () => { player.playTrack(track('one')); });
    const audio = view.container.querySelector('audio')!;
    act(() => { audio.dispatchEvent(new Event('error')); });
    const before = jest.mocked(audio.load).mock.calls.length;
    act(() => { player.pause(); });
    await act(async () => { jest.advanceTimersByTime(10000); });
    expect(player.status).toBe('paused');
    expect(jest.mocked(audio.load).mock.calls.length).toBe(before);
    act(() => { audio.dispatchEvent(new Event('error')); });
    view.unmount();
    // One deliberate load releases the disposed source; recovery must not load again.
    expect(audio.getAttribute('src')).toBeNull();
    expect(jest.mocked(audio.load).mock.calls.length).toBe(before + 1);
    const playsAfterDisposal = jest.mocked(audio.play).mock.calls.length;
    const authorizationsAfterDisposal = jest.mocked(authorizePlayerAsset).mock.calls.length;
    await act(async () => { jest.advanceTimersByTime(10000); });
    expect(jest.mocked(audio.load).mock.calls.length).toBe(before + 1);
    expect(jest.mocked(audio.play).mock.calls.length).toBe(playsAfterDisposal);
    expect(jest.mocked(authorizePlayerAsset).mock.calls.length).toBe(authorizationsAfterDisposal);
  } finally { jest.useRealTimers(); }
});

it('ignores the failed native play promise once automatic recovery owns the attempt', async () => {
  jest.useFakeTimers();
  let rejectPlay: (reason: DOMException) => void = () => undefined;
  jest.mocked(HTMLMediaElement.prototype.play).mockImplementationOnce(() => new Promise<void>((_resolve, reject) => { rejectPlay = reject; }));
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  try {
    await act(async () => { player.playTrack(track('network')); });
    const audio = view.container.querySelector('audio')!;
    act(() => { audio.dispatchEvent(new Event('error')); });
    await act(async () => { rejectPlay(new DOMException('Delivery failed', 'NotSupportedError')); });
    await act(async () => { jest.advanceTimersByTime(1000); });
    expect(audio.play).toHaveBeenCalledTimes(2);
    expect(player.status).toBe('playing');
  } finally { jest.useRealTimers(); }
});

it('does not restart recovery after pending authorization fails following a pause', async () => {
  jest.useFakeTimers();
  let rejectAccess: (reason: { status: number }) => void = () => undefined;
  jest.mocked(authorizePlayerAsset).mockImplementationOnce(() => new Promise((_resolve, reject) => { rejectAccess = reject; }));
  render(<PlayerProvider><Probe /></PlayerProvider>);
  try {
    await act(async () => { player.playTrack({ ...track('one'), sources: [{ ...track('one').sources[0]!, assetId: 'one' }] }); });
    act(() => { player.pause(); });
    await act(async () => { rejectAccess({ status: 503 }); });
    await act(async () => { jest.advanceTimersByTime(10000); });
    expect(authorizePlayerAsset).toHaveBeenCalledTimes(1);
    expect(HTMLMediaElement.prototype.play).not.toHaveBeenCalled();
  } finally { jest.useRealTimers(); }
});

it('resumes the selected queue item without replacing its decoder when selected again', async () => {
  const selected = track('one');
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(selected); });
  const audio = view.container.querySelector('audio')!;
  const loads = jest.mocked(audio.load).mock.calls.length;
  act(() => { player.pause(); });
  await act(async () => { player.selectTrack(0); });
  expect(player.status).toBe('playing');
  act(() => { player.pause(); });
  await act(async () => { player.playTrack(selected); });
  expect(player.status).toBe('playing');
  expect(jest.mocked(audio.load).mock.calls.length).toBe(loads);
});

it('does not downgrade a protected recording to an unprotected cached URL after authorization denial', async () => {
  jest.mocked(authorizePlayerAsset).mockRejectedValue({ status: 403 });
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack({ ...track('protected'), sources: [
    { url: '', assetId: 'protected-full', quality: 'high', mediaType: 'audio/mp4' },
    { url: 'https://media.invalid/stale-grant.m4a', quality: 'low', mediaType: 'audio/mp4' },
  ] }); });
  expect(player.status).toBe('unavailable');
  expect(view.container.querySelector('audio')?.getAttribute('src')).toBeNull();
  expect(HTMLMediaElement.prototype.play).not.toHaveBeenCalled();
});

it('keeps the terminal native error when its pending play promise rejects afterwards', async () => {
  let rejectPlay: (reason: DOMException) => void = () => undefined;
  jest.mocked(HTMLMediaElement.prototype.play).mockImplementationOnce(() => new Promise<void>((_resolve, reject) => { rejectPlay = reject; }));
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(track('broken')); });
  const audio = view.container.querySelector('audio')!;
  Object.defineProperty(audio, 'error', { configurable: true, value: { code: 3 } });
  act(() => { audio.dispatchEvent(new Event('error')); });
  await act(async () => { rejectPlay(new DOMException('Decode failed', 'NotSupportedError')); });
  expect(player.error).toBe('La fuente dejó de estar disponible. Puedes reintentar o pasar a la siguiente pista.');
  expect(player.status).toBe('unavailable');
});

it('recovers when the browser rejects play before dispatching the protected-source native error', async () => {
  jest.useFakeTimers();
  jest.mocked(authorizePlayerAsset).mockResolvedValue({ url: 'https://media.invalid/signed.m4a' } as Awaited<ReturnType<typeof authorizePlayerAsset>>);
  jest.mocked(HTMLMediaElement.prototype.play).mockImplementationOnce(function (this: HTMLMediaElement) {
    Object.defineProperty(this, 'error', { configurable: true, value: { code: 4 } });
    return Promise.reject(new DOMException('Expired media grant', 'NotSupportedError'));
  });
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  try {
    await act(async () => { player.playTrack({ ...track('protected'), sources: [{ ...track('protected').sources[0]!, assetId: 'protected' }] }); });
    const audio = view.container.querySelector('audio')!;
    act(() => { audio.dispatchEvent(new Event('error')); });
    expect(player.status).toBe('loading');
    Object.defineProperty(audio, 'error', { configurable: true, value: null });
    await act(async () => { jest.advanceTimersByTime(1000); });
    expect(authorizePlayerAsset).toHaveBeenCalledTimes(2);
    expect(audio.play).toHaveBeenCalledTimes(2);
    expect(player.status).toBe('playing');
  } finally { jest.useRealTimers(); }
});

async function pendingQualityMetadata() {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack({ ...track('metadata'), sources: [
    { url: 'https://media.invalid/metadata.m4a', quality: 'high', mediaType: 'audio/mp4' },
    { url: 'https://media.invalid/metadata.flac', quality: 'lossless', mediaType: 'audio/flac' },
  ] }); });
  const audio = view.container.querySelector('audio')!;
  act(() => { audio.currentTime = 3.346; audio.dispatchEvent(new Event('timeupdate')); });
  let readiness = HTMLMediaElement.HAVE_NOTHING;
  jest.spyOn(audio, 'readyState', 'get').mockImplementation(() => readiness);
  const seek = jest.spyOn(audio, 'currentTime', 'set');
  jest.mocked(audio.play).mockClear();
  await act(async () => { player.setQuality('lossless'); });
  return { view, audio, seek, ready: async () => {
    readiness = HTMLMediaElement.HAVE_METADATA;
    await act(async () => { audio.dispatchEvent(new Event('loadedmetadata')); });
  } };
}

it('restores a fractional quality-switch position after the new source has metadata', async () => {
  const { audio, seek, ready } = await pendingQualityMetadata();
  expect(seek).not.toHaveBeenCalled();
  expect(audio.play).not.toHaveBeenCalled();
  expect(player.status).toBe('loading');
  await ready();
  expect(seek).toHaveBeenCalledTimes(1);
  expect(seek).toHaveBeenCalledWith(3.346);
  expect(audio.play).toHaveBeenCalledTimes(1);
  expect(player.status).toBe('playing');
  await ready();
  expect(seek).toHaveBeenCalledTimes(1);
  expect(audio.play).toHaveBeenCalledTimes(1);
});

it('does not autoplay when paused while restored source metadata is pending', async () => {
  const { audio, seek, ready } = await pendingQualityMetadata();
  act(() => { player.pause(); });
  await ready();
  expect(seek).toHaveBeenCalledWith(3.346);
  expect(audio.play).not.toHaveBeenCalled();
  expect(player.status).toBe('paused');
});

it('discards metadata preparation when another recording becomes selected', async () => {
  const { audio, seek, ready } = await pendingQualityMetadata();
  await act(async () => { player.playTrack(track('replacement')); });
  seek.mockClear();
  const attempts = jest.mocked(audio.play).mock.calls.length;
  await ready();
  expect(seek).not.toHaveBeenCalled();
  expect(audio.play).toHaveBeenCalledTimes(attempts);
  expect(player.currentTrack?.id).toBe('replacement');
  expect(audio.currentSrc).toBe('https://media.invalid/replacement.m4a');
});

it('does not overwrite the pending restoration with preparation timeupdate zero', async () => {
  const { audio, seek, ready } = await pendingQualityMetadata();
  act(() => { audio.currentTime = 0; audio.dispatchEvent(new Event('timeupdate')); });
  seek.mockClear();
  await ready();
  expect(seek).toHaveBeenCalledWith(3.346);
  expect(player.currentTime).toBe(3.346);
});

for (const position of [0, 8.75]) {
  it(`restores the latest explicit seek to ${position} instead of the previous quality position`, async () => {
    const { audio, seek, ready } = await pendingQualityMetadata();
    act(() => { player.seek(position); });
    expect(seek).not.toHaveBeenCalled();
    await ready();
    expect(seek).toHaveBeenCalledTimes(1);
    expect(seek).toHaveBeenCalledWith(position);
    expect(audio.play).toHaveBeenCalledTimes(1);
  });
}

it('keeps explicit play pending until the matching metadata restores its position', async () => {
  const { audio, ready } = await pendingQualityMetadata();
  act(() => { player.pause(); });
  await act(async () => { await player.play(); });
  expect(audio.play).not.toHaveBeenCalled();
  await ready();
  expect(audio.play).toHaveBeenCalledTimes(1);
  expect(player.status).toBe('playing');
});

it('ignores metadata still identifying the previous native source', async () => {
  const { audio, seek, ready } = await pendingQualityMetadata();
  const nativeSource = jest.spyOn(audio, 'currentSrc', 'get').mockReturnValue('https://media.invalid/metadata.m4a');
  await ready();
  expect(seek).not.toHaveBeenCalled();
  expect(audio.play).not.toHaveBeenCalled();
  nativeSource.mockImplementation(function (this: HTMLMediaElement) { return this.src; });
  await ready();
  expect(seek).toHaveBeenCalledTimes(1);
  expect(audio.play).toHaveBeenCalledTimes(1);
});

it('removes the pending metadata listener on unmount', async () => {
  const { view, audio, seek, ready } = await pendingQualityMetadata();
  const removeListener = jest.spyOn(audio, 'removeEventListener');
  view.unmount();
  expect(removeListener.mock.calls.filter(([event]) => event === 'loadedmetadata')).toHaveLength(2);
  await ready();
  expect(seek).not.toHaveBeenCalled();
  expect(audio.play).not.toHaveBeenCalled();
});

it('does not resurrect a terminal decoding failure when pending metadata arrives later', async () => {
  const { audio, seek, ready } = await pendingQualityMetadata();
  Object.defineProperty(audio, 'error', { configurable: true, value: { code: 3 } });
  await act(async () => { audio.dispatchEvent(new Event('error')); });
  // Exhaust both renditions before asserting terminal metadata cannot revive them.
  await act(async () => { audio.dispatchEvent(new Event('error')); });
  expect(player.status).toBe('unavailable');
  await ready();
  expect(player.status).toBe('unavailable');
  expect(seek).not.toHaveBeenCalled();
  expect(audio.play).not.toHaveBeenCalled();
});

it('ignores queued native transport events while the restored source is preparing', async () => {
  const { audio, ready } = await pendingQualityMetadata();
  jest.spyOn(audio, 'paused', 'get').mockReturnValue(false);
  act(() => {
    audio.dispatchEvent(new Event('playing'));
    audio.dispatchEvent(new Event('waiting'));
    audio.dispatchEvent(new Event('ended'));
  });
  expect(player.status).toBe('loading');
  expect(player.currentTrack?.id).toBe('metadata');
  await ready();
  expect(audio.play).toHaveBeenCalledTimes(1);
});

it('prepares a different artist silently after an explicit pause and resumes on user play', async () => {
  render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(track('one')); });
  act(() => player.pause());
  const before = jest.mocked(HTMLMediaElement.prototype.play).mock.calls.length;
  act(() => player.beginArtist('two'));
  await act(async () => player.loadArtist('two', [track('two')]));
  expect(player.currentTrack?.id).toBe('two');
  expect(jest.mocked(HTMLMediaElement.prototype.play).mock.calls.length).toBe(before);
  await act(async () => player.play());
  expect(jest.mocked(HTMLMediaElement.prototype.play).mock.calls.length).toBe(before + 1);
});

it('ignores obsolete artist results and preserves an unchanged profile selection', async () => {
  render(<PlayerProvider><Probe /></PlayerProvider>);
  act(() => player.beginArtist('A'));
  act(() => player.beginArtist('B'));
  await act(async () => player.loadArtist('B', [track('B')]));
  act(() => player.loadArtist('A', [track('A')]));
  const before = jest.mocked(HTMLMediaElement.prototype.load).mock.calls.length;
  act(() => player.beginArtist('B'));
  expect(player.currentTrack?.id).toBe('B');
  expect(jest.mocked(HTMLMediaElement.prototype.load).mock.calls.length).toBe(before);
});

it('exhausts broken tracks once even with queue repetition enabled', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => player.playTrack(track('one'), [track('one'), track('two')]));
  act(() => player.setRepeat('queue'));
  const audio = view.container.querySelector('audio')!;
  jest.mocked(audio.load).mockImplementation(() => {
    Object.defineProperty(audio, 'error', { configurable: true, value: null });
  });
  Object.defineProperty(audio, 'error', { configurable: true, value: { code: 3 } });
  await act(async () => { audio.dispatchEvent(new Event('error')); });
  expect(player.currentTrack?.id).toBe('two');
  Object.defineProperty(audio, 'error', { configurable: true, value: { code: 3 } });
  await act(async () => { audio.dispatchEvent(new Event('error')); });
  expect(player.status).toBe('unavailable');
  expect(player.currentTrack?.id).toBe('two');
});

it('does not skip or condemn a track when autoplay is blocked', async () => {
  jest.mocked(HTMLMediaElement.prototype.play).mockRejectedValue(new DOMException('Blocked', 'NotAllowedError'));
  render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => player.playTrack(track('one'), [track('one'), track('two')]));
  expect(player.currentTrack?.id).toBe('one');
  expect(player.status).toBe('paused');
});


it('tries another rendition before dropping a recording and exhausts each only once', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  const recording = { ...track('one'), sources: [track('one').sources[0]!,
    { url: 'https://media.invalid/alternate.flac', quality: 'lossless' as const, mediaType: 'audio/flac' }] };
  await act(async () => player.playTrack(recording, [recording, track('two')]));
  const audio = view.container.querySelector('audio')!;
  jest.mocked(audio.load).mockImplementation(() => {
    Object.defineProperty(audio, 'error', { configurable: true, value: null });
  });
  const fail = async () => {
    Object.defineProperty(audio, 'error', { configurable: true, value: { code: 3 } });
    await act(async () => { audio.dispatchEvent(new Event('error')); });
  };
  await fail();
  expect(player.currentTrack?.id).toBe('one');
  expect(audio.src).toBe('https://media.invalid/alternate.flac');
  expect(player.unavailableTrackIds).toEqual([]);
  await fail();
  expect(player.currentTrack?.id).toBe('two');
  expect(player.unavailableTrackIds).toEqual(['one']);
  await fail();
  expect(player.status).toBe('unavailable');
  expect(player.unavailableTrackIds).toEqual(['one', 'two']);
});

it('advances after an unsupported play rejection even before a native error event', async () => {
  jest.mocked(HTMLMediaElement.prototype.play).mockRejectedValueOnce(new DOMException('Unsupported', 'NotSupportedError'));
  render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => player.playTrack(track('one'), [track('one'), track('two')]));
  expect(player.currentTrack?.id).toBe('two');
  expect(player.status).toBe('playing');
  expect(player.unavailableTrackIds).toEqual(['one']);
});


it('can explicitly retry a recording after every rendition was exhausted', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => player.playTrack(track('one')));
  const audio = view.container.querySelector('audio')!;
  Object.defineProperty(audio, 'error', { configurable: true, value: { code: 3 } });
  await act(async () => { audio.dispatchEvent(new Event('error')); });
  expect(player.status).toBe('unavailable');
  expect(player.unavailableTrackIds).toEqual(['one']);
  jest.mocked(audio.load).mockImplementation(() => {
    Object.defineProperty(audio, 'error', { configurable: true, value: null });
  });
  await act(async () => player.play());
  expect(player.currentTrack?.id).toBe('one');
  expect(player.status).toBe('playing');
  expect(player.unavailableTrackIds).toEqual([]);
});


it.each(['waiting', 'timeupdate'])('skips stalled media despite repeated %s events without progress', async (event) => {
  jest.useFakeTimers();
  jest.mocked(HTMLMediaElement.prototype.play).mockResolvedValue(undefined);
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  try {
    await act(async () => player.playTrack(track('stalled'), [track('stalled'), track('good')]));
    const audio = view.container.querySelector('audio')!;
    await act(async () => { jest.advanceTimersByTime(10000); });
    act(() => { audio.dispatchEvent(new Event(event)); });
    await act(async () => { jest.advanceTimersByTime(5000); });
    expect(player.currentTrack?.id).toBe('good');
    expect(player.unavailableTrackIds).toContain('stalled');
  } finally {
    view.unmount();
    jest.useRealTimers();
  }
});

it('bounds a resumed selection whose metadata never arrives after an explicit pause', async () => {
  jest.useFakeTimers();
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  try {
    await act(async () => player.playTrack(track('pending-metadata')));
    const audio = view.container.querySelector('audio')!;
    audio.currentTime = 12;
    act(() => { audio.dispatchEvent(new Event('timeupdate')); });
    await act(async () => player.setQuality('low'));
    act(() => player.pause());
    await act(async () => player.play());
    expect(player.status).toBe('loading');
    await act(async () => { jest.advanceTimersByTime(15000); });
    expect(player.status).toBe('unavailable');
    expect(player.playbackRequested).toBe(false);
  } finally { view.unmount(); jest.useRealTimers(); }
});

it('does not let a stale playing event disarm a newer unresolved selection deadline', async () => {
  jest.useFakeTimers();
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  try {
    await act(async () => player.playTrack(track('old-playing')));
    jest.mocked(authorizePlayerAsset).mockImplementationOnce(() => new Promise(() => undefined));
    await act(async () => player.playTrack({ ...track('unresolved'), sources: [{ ...track('unresolved').sources[0]!, assetId: 'unresolved' }] }));
    act(() => { view.container.querySelector('audio')!.dispatchEvent(new Event('playing')); });
    await act(async () => { jest.advanceTimersByTime(15000); });
    expect(player.status).toBe('unavailable');
  } finally { view.unmount(); jest.useRealTimers(); }
});

it('ignores a delayed ended event after explicit pause, including repeat playback', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => player.playTrack(track('one'), [track('one'), track('two')]));
  const audio = view.container.querySelector('audio')!;
  act(() => player.pause());
  const attempts = jest.mocked(audio.play).mock.calls.length;
  await act(async () => { audio.dispatchEvent(new Event('ended')); });
  expect(player.currentTrack?.id).toBe('one');
  expect(player.status).toBe('paused');
  expect(audio.play).toHaveBeenCalledTimes(attempts);
});

it('keeps explicit pause when stepping the queue and entering another artist', async () => {
  render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => player.playTrack(track('one'), [track('one'), track('two')]));
  act(() => player.pause());
  await act(async () => player.next());
  expect(player.currentTrack?.id).toBe('two');
  expect(player.status).not.toBe('playing');
  act(() => player.beginArtist('other'));
  await act(async () => player.loadArtist('other', [track('three')]));
  expect(player.currentTrack?.id).toBe('three');
  expect(player.status).not.toBe('playing');
  await act(async () => player.play());
  expect(player.status).toBe('playing');
});

it('does not let an old native error retarget a newer command before React commits it', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => player.playTrack(track('old'), [track('old'), track('old-next')]));
  const audio = view.container.querySelector('audio')!;
  jest.mocked(audio.load).mockImplementation(() => {
    Object.defineProperty(audio, 'error', { configurable: true, value: null });
  });
  await act(async () => {
    player.playTrack(track('chosen'), [track('chosen'), track('not-chosen')]);
    Object.defineProperty(audio, 'error', { configurable: true, value: { code: 3 } });
    audio.dispatchEvent(new Event('error'));
  });
  expect(player.currentTrack?.id).toBe('chosen');
  expect(player.status).toBe('playing');
});

it('applies rapid queue commands in order before React renders their result', async () => {
  render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => player.playTrack(track('one'), [track('one'), track('two'), track('three')]));
  await act(async () => { player.next(); player.next(); });
  expect(player.currentTrack?.id).toBe('three');
});

it('lets the global Pause control cancel pending authorization before it becomes audible', async () => {
  let resolve: (value: { url: string; mediaType: string }) => void = () => undefined;
  jest.mocked(authorizePlayerAsset).mockImplementationOnce(() => new Promise((done) => { resolve = done; }));
  render(<PlayerProvider><Probe /><GlobalPlayer /></PlayerProvider>);
  await act(async () => player.playTrack({ ...track('pending'), sources: [{ ...track('pending').sources[0]!, assetId: 'pending-asset' }] }));
  fireEvent.click(screen.getByRole('button', { name: 'Pausar', exact: true }));
  await act(async () => resolve({ url: 'https://media.invalid/late.m4a', mediaType: 'audio/mp4' }));
  expect(HTMLMediaElement.prototype.play).not.toHaveBeenCalled();
  expect(player.status).toBe('paused');
  expect(screen.getByRole('button', { name: 'Reproducir', exact: true })).toBeTruthy();
});

it('restores explicit pause for the same listener until an explicit resume', async () => {
  jest.useFakeTimers();
  let view = render(<PlayerProvider><Probe /></PlayerProvider>);
  try {
    await act(async () => player.playTrack(track('first')));
    act(() => player.pause());
    await act(async () => { jest.advanceTimersByTime(250); });
    expect(JSON.parse(localStorage.getItem('tdf-global-player/v1')!).pausedByUser).toBe(true);
    view.unmount();
    const attempts = jest.mocked(HTMLMediaElement.prototype.play).mock.calls.length;
    view = render(<PlayerProvider><Probe /></PlayerProvider>);
    act(() => player.beginArtist('after-reload'));
    await act(async () => player.loadArtist('after-reload', [track('new-artist')]));
    expect(HTMLMediaElement.prototype.play).toHaveBeenCalledTimes(attempts);
    await act(async () => player.play());
    expect(player.status).toBe('playing');
  } finally { view.unmount(); jest.useRealTimers(); }
});

it('stops a selected asset when a background permission recheck is denied', async () => {
  jest.useFakeTimers();
  jest.spyOn(document, 'visibilityState', 'get').mockReturnValue('visible');
  jest.mocked(authorizePlayerAsset).mockResolvedValueOnce({ url: 'https://media.invalid/allowed.m4a', expiresAt: '2099-01-01', mediaType: 'audio/mp4' });
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  try {
    await act(async () => player.playTrack({ ...track('protected'), sources: [{ ...track('protected').sources[0]!, assetId: 'asset' }] }));
    expect(player.status).toBe('playing');
    jest.mocked(authorizePlayerAsset).mockRejectedValue(Object.assign(new Error('Withdrawn'), { status: 404 }));
    await act(async () => { jest.advanceTimersByTime(60000); });
    expect(player.status).toBe('unavailable');
    expect(player.unavailableTrackIds).toContain('protected');
    expect(view.container.querySelector('audio')?.pause).toHaveBeenCalled();
  } finally { view.unmount(); jest.useRealTimers(); }
});

it('retains the selected track and clears its source when background permission is denied with another queued track', async () => {
  jest.useFakeTimers();
  jest.spyOn(document, 'visibilityState', 'get').mockReturnValue('visible');
  jest.mocked(authorizePlayerAsset).mockResolvedValueOnce({ url: 'https://media.invalid/allowed.m4a', expiresAt: '2099-01-01', mediaType: 'audio/mp4' });
  const selected = { ...track('protected'), sources: [{ ...track('protected').sources[0]!, assetId: 'asset' }] };
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  try {
    await act(async () => player.playTrack(selected, [selected, track('next')]));
    expect(player.status).toBe('playing');
    const playsBeforeDenial = jest.mocked(HTMLMediaElement.prototype.play).mock.calls.length;
    jest.mocked(authorizePlayerAsset).mockRejectedValue(Object.assign(new Error('Denied'), { status: 403 }));
    await act(async () => { jest.advanceTimersByTime(60000); });
    expect(player.currentTrack?.id).toBe('protected');
    expect(player.status).toBe('unavailable');
    expect(player.unavailableTrackIds).toContain('protected');
    expect(view.container.querySelector('audio')?.getAttribute('src')).toBeNull();
    expect(HTMLMediaElement.prototype.play).toHaveBeenCalledTimes(playsBeforeDenial);
    act(() => {
      const media = view.container.querySelector('audio')!;
      media.dispatchEvent(new Event('playing'));
      media.dispatchEvent(new Event('ended'));
    });
    await act(async () => { jest.advanceTimersByTime(120000); });
    expect(player.currentTrack?.id).toBe('protected');
    expect(player.status).toBe('unavailable');
    expect(HTMLMediaElement.prototype.play).toHaveBeenCalledTimes(playsBeforeDenial);
    expect(authorizePlayerAsset).toHaveBeenCalledTimes(2);
  } finally { view.unmount(); jest.useRealTimers(); }
});

it('does not clear a newer native selection when a denied source finishes its pending pause', async () => {
  jest.useFakeTimers();
  jest.spyOn(document, 'visibilityState', 'get').mockReturnValue('visible');
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  const media = view.container.querySelector('video')!;
  let paused = true;
  const pauseEvents: (() => void)[] = [];
  Object.defineProperty(media, 'paused', { configurable: true, get: () => paused });
  Object.defineProperty(media, 'readyState', { configurable: true, value: 1 });
  jest.mocked(media.pause).mockImplementation(() => {
    if (!paused) { paused = true; pauseEvents.push(() => media.dispatchEvent(new Event('pause'))); }
  });
  jest.mocked(media.play).mockImplementation(async () => {
    paused = false; media.dispatchEvent(new Event('play')); media.dispatchEvent(new Event('playing'));
  });
  const selected = { ...track('protected-native'), startMs: 0,
    sources: [{ url: '', assetId: 'native-asset', mediaType: 'video/mp4', quality: 'high' as const }] };
  jest.mocked(authorizePlayerAsset).mockResolvedValueOnce({ url: 'https://media.invalid/allowed.mp4', expiresAt: '2099-01-01', mediaType: 'video/mp4' });
  try {
    await act(async () => { player.playTrack(selected); });
    expect(player.status).toBe('playing');
    jest.mocked(authorizePlayerAsset).mockRejectedValue(Object.assign(new Error('Denied'), { status: 403 }));
    await act(async () => { jest.advanceTimersByTime(60000); });
    expect(player.currentTrack?.id).toBe('protected-native');
    expect(player.status).toBe('unavailable');
    expect(pauseEvents).toHaveLength(1);
    const replacement = { ...track('replacement-native'), startMs: 0,
      sources: [{ url: 'https://media.invalid/replacement.mp4', mediaType: 'video/mp4', quality: 'high' as const }] };
    await act(async () => { player.playTrack(replacement); });
    await act(async () => { pauseEvents.shift()!(); });
    expect(player.currentTrack?.id).toBe('replacement-native');
    expect(media.getAttribute('src')).toBe('https://media.invalid/replacement.mp4');
    expect(player.status).toBe('playing');
  } finally { view.unmount(); jest.useRealTimers(); }
});

it('ignores a delayed permission denial after a newer selection', async () => {
  jest.useFakeTimers();
  jest.spyOn(document, 'visibilityState', 'get').mockReturnValue('visible');
  let deny: (reason: unknown) => void = () => undefined;
  jest.mocked(authorizePlayerAsset)
    .mockResolvedValueOnce({ url: 'https://media.invalid/old.m4a', expiresAt: '2099-01-01', mediaType: 'audio/mp4' })
    .mockImplementationOnce(() => new Promise((_resolve, reject) => { deny = reject; }));
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  try {
    await act(async () => player.playTrack({ ...track('old'), sources: [{ ...track('old').sources[0]!, assetId: 'old-asset' }] }));
    await act(async () => { jest.advanceTimersByTime(60000); });
    await act(async () => player.playTrack(track('new')));
    await act(async () => deny(Object.assign(new Error('Old denial'), { status: 403 })));
    expect(player.currentTrack?.id).toBe('new');
    expect(player.status).toBe('playing');
    expect(player.unavailableTrackIds).toEqual([]);
  } finally { view.unmount(); jest.useRealTimers(); }
});

it('refreshes a replaced legacy source without clearing an explicit pause', async () => {
  jest.useFakeTimers();
  jest.spyOn(document, 'visibilityState', 'get').mockReturnValue('visible');
  jest.mocked(authorizePlayerAsset)
    .mockResolvedValueOnce({ url: 'https://media.invalid/old.m4a', mediaType: 'audio/mp4', sourceVersion: 'v1' })
    .mockResolvedValue({ url: 'https://media.invalid/new.m4a', mediaType: 'audio/mp4', sourceVersion: 'v2' });
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  try {
    await act(async () => player.playTrack({ ...track('legacy'), sources: [{ ...track('legacy').sources[0]!, legacySourceId: '1:spotify' }] }));
    const audio = view.container.querySelector('audio')!;
    act(() => player.pause());
    const attempts = jest.mocked(audio.play).mock.calls.length;
    await act(async () => { jest.advanceTimersByTime(60000); });
    expect(audio.src).toBe('https://media.invalid/new.m4a');
    expect(player.currentTrack?.id).toBe('legacy');
    expect(player.status).not.toBe('playing');
    expect(audio.play).toHaveBeenCalledTimes(attempts);
  } finally { view.unmount(); jest.useRealTimers(); }
});

it('does not quarantine tracks on an offline failure or resume an explicit pause on reconnect', async () => {
  jest.useFakeTimers();
  const online = jest.spyOn(navigator, 'onLine', 'get').mockReturnValue(false);
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  try {
    await act(async () => player.playTrack(track('offline'), [track('offline'), track('next')]));
    const audio = view.container.querySelector('audio')!;
    act(() => player.pause());
    Object.defineProperty(audio, 'error', { configurable: true, value: { code: 2 } });
    await act(async () => { audio.dispatchEvent(new Event('error')); jest.advanceTimersByTime(30000); });
    expect(player.unavailableTrackIds).toEqual([]);
    expect(player.currentTrack?.id).toBe('offline');
    online.mockReturnValue(true);
    const attempts = jest.mocked(audio.play).mock.calls.length;
    await act(async () => { window.dispatchEvent(new Event('online')); jest.advanceTimersByTime(30000); });
    expect(audio.play).toHaveBeenCalledTimes(attempts);
    expect(player.status).not.toBe('playing');
  } finally { view.unmount(); jest.useRealTimers(); }
});


it.each(['progress', 'pause'])('does not skip media after actual %s cancels the buffering deadline', async (action) => {
  jest.useFakeTimers();
  jest.mocked(HTMLMediaElement.prototype.play).mockResolvedValue(undefined);
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  try {
    await act(async () => player.playTrack(track('one'), [track('one'), track('two')]));
    const audio = view.container.querySelector('audio')!;
    await act(async () => { jest.advanceTimersByTime(10000); });
    act(() => {
      if (action === 'pause') player.pause();
      else { audio.currentTime = 1; audio.dispatchEvent(new Event('timeupdate')); }
    });
    await act(async () => { jest.advanceTimersByTime(5000); });
    expect(player.currentTrack?.id).toBe('one');
    expect(player.unavailableTrackIds).toEqual([]);
    if (action === 'progress') {
      act(() => { audio.dispatchEvent(new Event('waiting')); });
      await act(async () => { jest.advanceTimersByTime(15000); });
      expect(player.currentTrack?.id).toBe('two');
    }
  } finally {
    view.unmount();
    jest.useRealTimers();
  }
});

it('preserves explicit pause through a silent external load and subsequent artist navigation', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => player.playTrack(track('initial')));
  act(() => player.pause());
  const attempts = jest.mocked(HTMLMediaElement.prototype.play).mock.calls.length;
  await act(async () => window.dispatchEvent(new CustomEvent(PLAYER_LOAD_TRACK_EVENT, {
    detail: { track: track('prepared'), autoplay: false },
  })));
  act(() => player.beginArtist('next-profile'));
  await act(async () => player.loadArtist('next-profile', [track('next-profile')]));
  jest.mocked(Object.getOwnPropertyDescriptor(HTMLMediaElement.prototype, 'paused')!.get!).mockReturnValue(true);
  act(() => { view.container.querySelector('audio')!.dispatchEvent(new Event('canplay')); });
  expect(HTMLMediaElement.prototype.play).toHaveBeenCalledTimes(attempts);
  expect(player.status).toBe('paused');
});


describe('backend-compatible artist media rollout', () => {
  const release = { arReleaseId: 42, arArtistId: 17, arTitle: 'Synthetic public legacy',
    arReleaseDate: null, arDescription: null, arCoverImageUrl: null,
    arSpotifyUrl: 'https://media.invalid/public-legacy.mp3', arYoutubeUrl: null };
  it.each(['absent', 'false'])('retains current-main direct playback for %s rollout and an old backend', async () => {
    recoveryRollout.enabled = false;
    jest.mocked(authorizePlayerAsset).mockRejectedValue({ status: 404 });
    const view = render(<PlayerProvider><Probe /></PlayerProvider>);
    await act(async () => { dispatchReleaseToRadio(release, release.arSpotifyUrl); });
    expect(authorizePlayerAsset).not.toHaveBeenCalled();
    expect(player.currentTrack?.sources[0]?.legacySourceId).toBeUndefined();
    expect(player.status).toBe('playing');
    expect(view.container.querySelector('audio')?.src).toBe(release.arSpotifyUrl);
    expect(view.container.querySelectorAll('audio')).toHaveLength(1);
  });
  it.each([401, 403, 404])('never downgrades an enabled legacy access denial %s to its stale URL', async (status) => {
    recoveryRollout.enabled = true;
    jest.mocked(authorizePlayerAsset).mockRejectedValue({ status });
    const view = render(<PlayerProvider><Probe /></PlayerProvider>);
    await act(async () => { dispatchReleaseToRadio(release, release.arSpotifyUrl); });
    expect(authorizePlayerAsset).toHaveBeenCalledWith('legacy:42:spotify', expect.any(AbortSignal));
    expect(player.currentTrack?.sources[0]?.legacySourceId).toBe('42:spotify');
    expect(player.status).toBe('unavailable');
    expect(view.container.querySelector('audio')?.getAttribute('src')).not.toBe(release.arSpotifyUrl);
  });
  it.each([401, 403, 404])('keeps a persisted explicit legacy identity protected with rollout off and denial %s', async (status) => {
    recoveryRollout.enabled = false;
    const stored = { ...track('persisted-legacy'), sources: [{ ...track('persisted-legacy').sources[0]!, legacySourceId: '42:spotify' }] };
    localStorage.setItem('tdf-global-player/v1', JSON.stringify({ ownerPartyId: null,
      queue: { tracks: [stored], currentIndex: 0, shuffle: false, repeat: 'off' },
      volume: 0.8, muted: false, quality: 'auto', positions: {}, pausedByUser: false }));
    jest.mocked(authorizePlayerAsset).mockRejectedValue({ status });
    const view = render(<PlayerProvider><Probe /></PlayerProvider>);
    await waitFor(() => expect(authorizePlayerAsset).toHaveBeenCalled());
    expect(authorizePlayerAsset).toHaveBeenCalledWith('legacy:42:spotify', expect.any(AbortSignal));
    expect(player.currentTrack?.sources[0]?.legacySourceId).toBe('42:spotify');
    expect(player.status).toBe('unavailable');
    expect(view.container.querySelector('audio')?.getAttribute('src')).not.toBe(stored.sources[0]!.url);
  });
});


describe('explicit malformed media identity never becomes a public URL', () => {
  const malformedIdentities: [string, unknown][] = [
    ['assetId', ''], ['assetId', ' '], ['assetId', null], ['assetId', 0],
    ['legacySourceId', ''], ['legacySourceId', ' '], ['legacySourceId', null], ['legacySourceId', 0],
  ];
  it.each(malformedIdentities)('does not downgrade a selected %s=%p identity with rollout off', async (field, value) => {
    recoveryRollout.enabled = false;
    const malformed = { ...track('malformed'), sources: [
      { ...track('malformed').sources[0]!, [field]: value },
      { ...track('public-downgrade').sources[0]! },
    ] } as PlayerTrack;
    const view = render(<PlayerProvider><Probe /></PlayerProvider>);
    await act(async () => { player.playTrack(malformed); });
    expect(player.status).toBe('unavailable');
    expect(view.container.querySelector('audio')?.getAttribute('src')).toBeNull();
    expect(authorizePlayerAsset).not.toHaveBeenCalled();
  });
  it.each(malformedIdentities)('does not downgrade a persisted %s=%p identity with rollout off', async (field, value) => {
    recoveryRollout.enabled = false;
    const malformed = { ...track('persisted-malformed'), sources: [
      { ...track('persisted-malformed').sources[0]!, [field]: value },
      { ...track('public-downgrade').sources[0]! },
    ] } as PlayerTrack;
    localStorage.setItem('tdf-global-player/v1', JSON.stringify({ ownerPartyId: null,
      queue: { tracks: [malformed], currentIndex: 0, shuffle: false, repeat: 'off' },
      volume: 0.8, muted: false, quality: 'auto', positions: {} }));
    const view = render(<PlayerProvider><Probe /></PlayerProvider>);
    await waitFor(() => expect(player.status).toBe('unavailable'));
    expect(view.container.querySelector('audio')?.getAttribute('src')).toBeNull();
    expect(authorizePlayerAsset).not.toHaveBeenCalled();
  });
});

describe('native session transport', () => {
  beforeEach(() => {
    // Native-control tests model actual paused state and native transport
    // events; a fulfilled play promise alone is not a playing event.
    const paused = new WeakMap<HTMLMediaElement, boolean>();
    jest.spyOn(HTMLMediaElement.prototype, 'paused', 'get').mockImplementation(function (this: HTMLMediaElement) {
      return paused.get(this) ?? true;
    });
    jest.mocked(HTMLMediaElement.prototype.pause).mockImplementation(function (this: HTMLMediaElement) {
      if (paused.get(this) ?? true) return;
      paused.set(this, true);
      queueMicrotask(() => this.dispatchEvent(new Event('pause')));
    });
    jest.mocked(HTMLMediaElement.prototype.play).mockImplementation(function (this: HTMLMediaElement) {
      paused.set(this, false);
      this.dispatchEvent(new Event('play'));
      this.dispatchEvent(new Event('playing'));
      return Promise.resolve();
    });
  });

const nativeTrack = (): PlayerTrack => ({ ...track('native'), durationMs: 10000, startMs: 10000, endMs: 20000,
  sources: [{ url: 'https://media.invalid/session.mp4', quality: 'high', mediaType: 'video/mp4' },
    { url: 'https://media.invalid/session.m4a', quality: 'high', mediaType: 'audio/mp4' }] });
it('uses one native engine, seeks within chapter bounds and switches to audio without loading video', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(nativeTrack()); });
  const video = view.container.querySelector('video')!;
  Object.defineProperty(video, 'readyState', { configurable: true, value: 1 });
  await act(async () => { video.dispatchEvent(new Event('loadedmetadata')); });
  expect(video.src).toBe('https://media.invalid/session.mp4');
  expect(video.currentTime).toBe(10);
  expect(view.container.querySelector('audio')?.hasAttribute('src')).toBe(false);
  act(() => { player.seek(4); });
  expect(video.currentTime).toBe(14);
  expect(player.currentTime).toBe(4);
  await act(async () => { player.setMediaMode('audio'); });
  await act(async () => { video.dispatchEvent(new Event('loadedmetadata')); });
  expect(video.src).toBe('https://media.invalid/session.m4a');
  expect(video.currentTime).toBe(14);
  expect(player.currentTime).toBe(4);
  expect(player.status).toBe('playing');
  act(() => { player.seek(500); });
  expect(video.currentTime).toBe(20);
  act(() => { player.setPlaybackRate(1.5); });
  expect(video.playbackRate).toBe(1.5);
});
it('keeps the chosen playback speed across a replacement source load', async () => {
  // Real media elements reset playbackRate to defaultPlaybackRate on load().
  jest.mocked(HTMLMediaElement.prototype.load).mockImplementation(function (this: HTMLMediaElement) {
    this.playbackRate = this.defaultPlaybackRate;
    this.dispatchEvent(new Event('ratechange'));
  });
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(nativeTrack()); });
  const video = view.container.querySelector('video')!;
  Object.defineProperty(video, 'readyState', { configurable: true, value: 1 });
  await act(async () => { video.dispatchEvent(new Event('loadedmetadata')); });
  act(() => { player.setPlaybackRate(1.5); });
  expect(video.playbackRate).toBe(1.5);
  await act(async () => { player.setMediaMode('audio'); });
  await act(async () => { video.dispatchEvent(new Event('loadedmetadata')); });
  expect(video.src).toBe('https://media.invalid/session.m4a');
  expect(video.playbackRate).toBe(1.5);
  expect(player.playbackRate).toBe(1.5);
});
it('continues native playback across page content changes and yields to radio', async () => {
  const view = render(<PlayerProvider><Probe /><div>First page</div></PlayerProvider>);
  await act(async () => { player.playTrack({ ...nativeTrack(), startMs: 0 }); });
  const video = view.container.querySelector('video')!;
  await act(async () => { view.rerender(<PlayerProvider><Probe /><div>Second page</div></PlayerProvider>); });
  expect(view.container.querySelector('video')).toBe(video);
  expect(player.currentTrack?.id).toBe('native');
  await act(async () => { window.dispatchEvent(new Event('tdf-radio-takeover')); });
  expect(player.status).toBe('paused');
});
it('stops a bounded song at its endpoint instead of playing the following song', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(nativeTrack()); });
  const video = view.container.querySelector('video')!;
  Object.defineProperty(video, 'readyState', { configurable: true, value: 1 });
  Object.defineProperty(video, 'paused', { configurable: true, get: () => false });
  await act(async () => { video.dispatchEvent(new Event('loadedmetadata')); });
  video.currentTime = 20;
  await act(async () => { video.dispatchEvent(new Event('timeupdate')); });
  expect(player.status).toBe('paused');
  expect(HTMLMediaElement.prototype.pause).toHaveBeenCalled();
});
it.each([
  ['video', 20], ['video', 20.1], ['audio', 20], ['audio', 20.1],
] as const)('rewinds a completed bounded %s track at %s before global Play resumes it', async (mode, endpoint) => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.setMediaMode(mode); player.playTrack(nativeTrack()); });
  const media = view.container.querySelector('video')!;
  let paused = true;
  const playedPositions: number[] = [];
  Object.defineProperty(media, 'readyState', { configurable: true, value: 1 });
  Object.defineProperty(media, 'paused', { configurable: true, get: () => paused });
  jest.mocked(media.pause).mockImplementation(() => { paused = true; });
  jest.mocked(media.play).mockImplementation(async () => { playedPositions.push(media.currentTime); paused = false; media.dispatchEvent(new Event('play')); media.dispatchEvent(new Event('playing')); });
  await act(async () => { media.dispatchEvent(new Event('loadedmetadata')); });
  await waitFor(() => expect(player.status).toBe('playing'));
  media.currentTime = endpoint;
  await act(async () => { media.dispatchEvent(new Event('timeupdate')); });
  expect(player.status).toBe('paused');
  await act(async () => { await player.play(); });
  expect(playedPositions.at(-1)).toBe(10);
  expect(media.currentTime).toBe(10);
  expect(player.currentTime).toBe(0);
  media.currentTime = 10.25;
  await act(async () => { media.dispatchEvent(new Event('timeupdate')); });
  expect(player.status).toBe('playing');
});
it.each([
  ['midchapter', nativeTrack(), 15], ['unbounded', track('ordinary-resume'), 24],
] as const)('preserves a paused %s position on global Play', async (_name, selected, position) => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(selected); });
  const media = view.container.querySelector(selected.startMs != null ? 'video' : 'audio')!;
  Object.defineProperty(media, 'readyState', { configurable: true, value: 1 });
  await act(async () => { media.dispatchEvent(new Event('loadedmetadata')); });
  media.currentTime = position;
  act(() => { player.pause(); });
  const playedPositions: number[] = [];
  jest.mocked(media.play).mockImplementation(async () => { playedPositions.push(media.currentTime); });
  await act(async () => { await player.play(); });
  expect(playedPositions.at(-1)).toBe(position);
  expect(media.currentTime).toBe(position);
});
it.each([
  ['single', 20], ['single', 20.1], ['final', 20], ['final', 20.1],
] as const)('rewinds a completed %s chapter at %s before native Play continues it', async (queueKind, endpoint) => {
  const selected = nativeTrack();
  const tracks = queueKind === 'final' ? [track('preceding'), selected] : [selected];
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(selected, tracks); });
  const video = view.container.querySelector('video')!;
  let paused = true;
  Object.defineProperty(video, 'readyState', { configurable: true, value: 1 });
  Object.defineProperty(video, 'paused', { configurable: true, get: () => paused });
  jest.mocked(video.pause).mockImplementation(() => { paused = true; });
  jest.mocked(video.play).mockImplementation(async () => { paused = false; video.dispatchEvent(new Event('play')); video.dispatchEvent(new Event('playing')); });
  await act(async () => { video.dispatchEvent(new Event('loadedmetadata')); });
  await waitFor(() => expect(player.status).toBe('playing'));
  video.currentTime = endpoint;
  await act(async () => { video.dispatchEvent(new Event('timeupdate')); });
  expect(player.status).toBe('paused');
  expect(player.currentTrack?.id).toBe(selected.id);
  const calls = jest.mocked(video.play).mock.calls.length;
  // A native control starts the element itself; it does not call global play().
  await act(async () => {
    video.dispatchEvent(new Event('pointerdown', { bubbles: true }));
    paused = false;
    video.dispatchEvent(new Event('play', { bubbles: true }));
    video.dispatchEvent(new Event('playing'));
  });
  expect(video.currentTime).toBe(10);
  expect(player.currentTime).toBe(0);
  expect(jest.mocked(video.play).mock.calls.length).toBe(calls);
  expect(player.currentTrack?.id).toBe(selected.id);
  video.currentTime = 10.25;
  await act(async () => { video.dispatchEvent(new Event('timeupdate')); });
  expect(player.status).toBe('playing');
  expect(player.currentTime).toBe(0.25);
});
it.each([
  ['midchapter', 20000, 15], ['unbounded', undefined, 24],
] as const)('preserves a paused %s native position instead of restarting it', async (_name, endMs, position) => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack({ ...nativeTrack(), endMs }); });
  const video = view.container.querySelector('video')!;
  Object.defineProperty(video, 'readyState', { configurable: true, value: 1 });
  await act(async () => { video.dispatchEvent(new Event('loadedmetadata')); });
  video.currentTime = position;
  act(() => { player.pause(); });
  await act(async () => {
    video.dispatchEvent(new Event('pointerdown', { bubbles: true }));
    video.dispatchEvent(new Event('play', { bubbles: true }));
  });
  expect(video.currentTime).toBe(position);
});
it('does not rewind a completed native source without a play gesture', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(nativeTrack()); });
  const video = view.container.querySelector('video')!;
  Object.defineProperty(video, 'readyState', { configurable: true, value: 1 });
  let paused = false;
  Object.defineProperty(video, 'paused', { configurable: true, get: () => paused });
  jest.mocked(video.pause).mockImplementation(() => { paused = true; video.dispatchEvent(new Event('pause')); });
  await act(async () => { video.dispatchEvent(new Event('loadedmetadata')); });
  video.currentTime = 20.1;
  act(() => { player.pause(); });
  const pauses = jest.mocked(video.pause).mock.calls.length;
  await act(async () => { paused = false; video.dispatchEvent(new Event('play', { bubbles: true })); });
  expect(video.currentTime).toBe(20.1);
  expect(jest.mocked(video.pause).mock.calls.length).toBe(pauses + 1);
});
it('does not rewind a completed native event belonging to another source', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(nativeTrack()); });
  const video = view.container.querySelector('video')!;
  Object.defineProperty(video, 'readyState', { configurable: true, value: 1 });
  await act(async () => { video.dispatchEvent(new Event('loadedmetadata')); });
  video.currentTime = 20.1;
  act(() => { player.pause(); });
  jest.spyOn(HTMLMediaElement.prototype, 'currentSrc', 'get').mockReturnValue('https://media.invalid/obsolete-source.mp4');
  await act(async () => {
    video.dispatchEvent(new Event('pointerdown', { bubbles: true }));
    video.dispatchEvent(new Event('play', { bubbles: true }));
  });
  expect(video.currentTime).toBe(20.1);
  expect(player.status).toBe('paused');
});
it('refuses to rewind or play an unowned source when replaying a completed chapter', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(nativeTrack()); });
  const media = view.container.querySelector('video')!;
  Object.defineProperty(media, 'readyState', { configurable: true, value: 1 });
  await act(async () => { media.dispatchEvent(new Event('loadedmetadata')); });
  media.currentTime = 20.1;
  jest.spyOn(HTMLMediaElement.prototype, 'currentSrc', 'get').mockReturnValue('https://media.invalid/obsolete-source.mp4');
  const calls = jest.mocked(media.play).mock.calls.length;
  await act(async () => { await player.play(); });
  expect(media.currentTime).toBe(20.1);
  expect(jest.mocked(media.play).mock.calls.length).toBe(calls);
  expect(player.status).toBe('loading');
});
it('releases the ordinary audio source when a native video takes over', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  await act(async () => { player.playTrack(track('ordinary')); });
  const audio = view.container.querySelector('audio')!;
  expect(audio.hasAttribute('src')).toBe(true);
  await act(async () => { player.playTrack({ ...nativeTrack(), startMs: 0 }); });
  expect(audio.hasAttribute('src')).toBe(false);
  expect(view.container.querySelector('video')?.getAttribute('src')).toBe('https://media.invalid/session.mp4');
});
it('keeps the global controls usable for older saved audio sources without MIME metadata', async () => {
  const { default: GlobalPlayer } = await import('./GlobalPlayer');
  const view = render(<PlayerProvider><Probe /><GlobalPlayer /></PlayerProvider>);
  await act(async () => { player.playTrack({ ...track('Legacy cached audio'), sources: [
    { url: 'https://media.invalid/legacy.mp3', quality: 'high' },
  ] } as PlayerTrack); });
  expect(view.getByRole('region', { name: 'Reproductor global' })).toBeTruthy();
  expect(view.getByText('Legacy cached audio')).toBeTruthy();
});


it.each(['video', 'audio'] as const)('preserves next-chapter %s autoplay when an internal pause arrives after the source transition', async mode => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  const media = view.container.querySelector('video')!;
  let paused = true;
  let ready = 0;
  const pauseEvents: (() => void)[] = [];
  Object.defineProperty(media, 'paused', { configurable: true, get: () => paused });
  Object.defineProperty(media, 'readyState', { configurable: true, get: () => ready });
  jest.mocked(media.pause).mockImplementation(() => {
    if (!paused) { paused = true; pauseEvents.push(() => media.dispatchEvent(new Event('pause'))); }
  });
  jest.mocked(media.load).mockImplementation(() => { media.currentTime = 0; ready = 0; });
  jest.mocked(media.play).mockImplementation(async () => { paused = false; media.dispatchEvent(new Event('play')); media.dispatchEvent(new Event('playing')); });
  const first = { ...nativeTrack(), id: 'chapter-one' };
  const second = { ...nativeTrack(), id: 'chapter-two', startMs: 20000, endMs: 30000 };
  await act(async () => { player.setMediaMode(mode); player.playTrack(first, [first, second]); });
  ready = 1;
  await act(async () => { media.dispatchEvent(new Event('loadedmetadata')); });
  expect(player.status).toBe('playing');
  const before = jest.mocked(media.play).mock.calls.length;
  media.currentTime = 20;
  await act(async () => { media.dispatchEvent(new Event('timeupdate')); });
  expect(player.currentTrack?.id).toBe('chapter-two');
  expect(pauseEvents).toHaveLength(1);
  // Native pause tasks can run after React has installed the next source's listeners.
  await act(async () => { pauseEvents.shift()!(); });
  ready = 1;
  await act(async () => { media.dispatchEvent(new Event('loadedmetadata')); });
  expect(media.currentTime).toBe(20);
  expect(jest.mocked(media.play).mock.calls.length).toBe(before + 1);
  expect(player.status).toBe('playing');
  // A later real native-control pause must still cancel intent after that internal task is consumed.
  media.pause();
  await act(async () => { pauseEvents.shift()!(); });
  expect(player.status).toBe('paused');
  await act(async () => { player.setQuality('high'); });
  ready = 1;
  await act(async () => { media.dispatchEvent(new Event('loadedmetadata')); });
  expect(player.status).toBe('paused');
  expect(jest.mocked(media.play).mock.calls.length).toBe(before + 1);
});

it('honors an explicit user pause while the next chapter awaits metadata even with an old internal pause queued', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  const media = view.container.querySelector('video')!;
  let paused = true;
  const pauseEvents: (() => void)[] = [];
  Object.defineProperty(media, 'paused', { configurable: true, get: () => paused });
  Object.defineProperty(media, 'readyState', { configurable: true, value: 1 });
  jest.mocked(media.pause).mockImplementation(() => {
    if (!paused) { paused = true; pauseEvents.push(() => media.dispatchEvent(new Event('pause'))); }
  });
  jest.mocked(media.play).mockImplementation(async () => { paused = false; media.dispatchEvent(new Event('play')); media.dispatchEvent(new Event('playing')); });
  const first = { ...nativeTrack(), id: 'chapter-one' };
  const second = { ...nativeTrack(), id: 'chapter-two', startMs: 20000, endMs: 30000 };
  await act(async () => { player.playTrack(first, [first, second]); });
  await act(async () => { media.dispatchEvent(new Event('loadedmetadata')); });
  const before = jest.mocked(media.play).mock.calls.length;
  media.currentTime = 20;
  await act(async () => { media.dispatchEvent(new Event('timeupdate')); });
  expect(player.currentTrack?.id).toBe('chapter-two');
  act(() => { player.pause(); });
  await act(async () => { pauseEvents.shift()!(); media.dispatchEvent(new Event('loadedmetadata')); });
  expect(player.status).toBe('paused');
  expect(jest.mocked(media.play).mock.calls.length).toBe(before);
});

it('loads an already-paused chapter without waiting for a pause event that the browser will not queue', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  const media = view.container.querySelector('video')!;
  Object.defineProperty(media, 'paused', { configurable: true, value: true });
  Object.defineProperty(media, 'readyState', { configurable: true, value: 1 });
  const first = { ...nativeTrack(), id: 'paused-first' };
  const second = { ...nativeTrack(), id: 'paused-second', startMs: 20000, endMs: 30000 };
  await act(async () => { player.playTrack(first, [first, second], false); });
  await act(async () => { media.dispatchEvent(new Event('loadedmetadata')); });
  await act(async () => { player.playTrack(second, [first, second], false); });
  await act(async () => { media.dispatchEvent(new Event('loadedmetadata')); });
  expect(player.currentTrack?.id).toBe('paused-second');
  expect(media.currentTime).toBe(20);
  expect(player.status).toBe('paused');
  expect(media.play).not.toHaveBeenCalled();
});

it('only loads the latest selected source after an earlier internal pause task completes', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  const media = view.container.querySelector('video')!;
  let paused = true;
  const pauseEvents: (() => void)[] = [];
  Object.defineProperty(media, 'paused', { configurable: true, get: () => paused });
  Object.defineProperty(media, 'readyState', { configurable: true, value: 1 });
  jest.mocked(media.pause).mockImplementation(() => {
    if (!paused) { paused = true; pauseEvents.push(() => media.dispatchEvent(new Event('pause'))); }
  });
  jest.mocked(media.play).mockImplementation(async () => { paused = false; media.dispatchEvent(new Event('play')); media.dispatchEvent(new Event('playing')); });
  await act(async () => { player.playTrack(nativeTrack()); });
  await act(async () => { media.dispatchEvent(new Event('loadedmetadata')); });
  const obsolete = { ...nativeTrack(), id: 'obsolete', sources: nativeTrack().sources.map(s => ({ ...s, url: s.url.replace('session.', 'obsolete.') })) };
  const latest = { ...nativeTrack(), id: 'latest', sources: nativeTrack().sources.map(s => ({ ...s, url: s.url.replace('session.', 'latest.') })) };
  const before = jest.mocked(media.play).mock.calls.length;
  await act(async () => { player.playTrack(obsolete); });
  await act(async () => { player.playTrack(latest); });
  expect(pauseEvents).toHaveLength(1);
  await act(async () => { pauseEvents.shift()!(); });
  await act(async () => { media.dispatchEvent(new Event('loadedmetadata')); });
  expect(media.src).toBe('https://media.invalid/latest.mp4');
  expect(player.currentTrack?.id).toBe('latest');
  expect(player.status).toBe('playing');
  expect(jest.mocked(media.play).mock.calls.length).toBe(before + 1);
});

it('cancels a pending internal pause listener on unmount without loading or playing an obsolete source', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  const media = view.container.querySelector('video')!;
  let paused = true;
  const pauseEvents: (() => void)[] = [];
  Object.defineProperty(media, 'paused', { configurable: true, get: () => paused });
  Object.defineProperty(media, 'readyState', { configurable: true, value: 1 });
  jest.mocked(media.pause).mockImplementation(() => {
    if (!paused) { paused = true; pauseEvents.push(() => media.dispatchEvent(new Event('pause'))); }
  });
  jest.mocked(media.play).mockImplementation(async () => { paused = false; media.dispatchEvent(new Event('play')); media.dispatchEvent(new Event('playing')); });
  const removed = jest.spyOn(media, 'removeEventListener');
  await act(async () => { player.playTrack(nativeTrack()); });
  await act(async () => { media.dispatchEvent(new Event('loadedmetadata')); });
  await act(async () => { player.playTrack({ ...nativeTrack(), id: 'obsolete-unmounted' }); });
  const before = jest.mocked(media.play).mock.calls.length;
  expect(pauseEvents).toHaveLength(1);
  await act(async () => { view.unmount(); });
  await act(async () => { pauseEvents.shift()!(); media.dispatchEvent(new Event('loadedmetadata')); });
  expect(media.hasAttribute('src')).toBe(false);
  expect(jest.mocked(media.play).mock.calls.length).toBe(before);
  expect(removed.mock.calls.some(([name, , options]) => name === 'pause' && options === true)).toBe(true);
});

it('cancels a discarded pause wait when switching engines so returning to native playback cannot hang', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  const states = new WeakMap<HTMLMediaElement, { paused: boolean }>();
  const state = (media: HTMLMediaElement) => {
    const existing = states.get(media) ?? { paused: true };
    states.set(media, existing);
    return existing;
  };
  const pauseTasks: { media: HTMLMediaElement; dispatch: () => void }[] = [];
  jest.spyOn(HTMLMediaElement.prototype, 'paused', 'get').mockImplementation(function (this: HTMLMediaElement) { return state(this).paused; });
  jest.spyOn(HTMLMediaElement.prototype, 'readyState', 'get').mockReturnValue(1);
  jest.mocked(HTMLMediaElement.prototype.pause).mockImplementation(function (this: HTMLMediaElement) {
    if (!state(this).paused) {
      state(this).paused = true;
      pauseTasks.push({ media: this, dispatch: () => this.dispatchEvent(new Event('pause')) });
    }
  });
  jest.mocked(HTMLMediaElement.prototype.play).mockImplementation(async function (this: HTMLMediaElement) { state(this).paused = false; this.dispatchEvent(new Event('play')); this.dispatchEvent(new Event('playing')); });
  jest.mocked(HTMLMediaElement.prototype.load).mockImplementation(function (this: HTMLMediaElement) {
    // The native load algorithm discards this element's pending media tasks.
    for (let i = pauseTasks.length - 1; i >= 0; i -= 1) if (pauseTasks[i]!.media === this) pauseTasks.splice(i, 1);
    this.currentTime = 0;
  });
  const video = view.container.querySelector('video')!;
  await act(async () => { player.playTrack(nativeTrack()); });
  await act(async () => { video.dispatchEvent(new Event('loadedmetadata')); });
  await act(async () => { player.playTrack({ ...nativeTrack(), id: 'aborted-native-selection' }); });
  expect(pauseTasks.filter(task => task.media === video)).toHaveLength(1);
  await act(async () => { player.playTrack(track('legacy-between')); });
  expect(pauseTasks.filter(task => task.media === video)).toHaveLength(0);
  const audio = view.container.querySelector('audio')!;
  expect(audio.src).toBe('https://media.invalid/legacy-between.m4a');
  await act(async () => { player.playTrack({ ...nativeTrack(), id: 'return-native' }); });
  await act(async () => { video.dispatchEvent(new Event('loadedmetadata')); });
  expect(video.src).toBe('https://media.invalid/session.mp4');
  expect(player.currentTrack?.id).toBe('return-native');
  expect(player.status).toBe('playing');
});

it('falls back to independent audio for the same session before advancing to another track', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  const first = nativeTrack();
  const second = track('next-after-session');
  const video = view.container.querySelector('video')!;
  Object.defineProperty(video, 'readyState', { configurable: true, value: 1 });
  await act(async () => player.playTrack(first, [first, second]));
  await act(async () => video.dispatchEvent(new Event('loadedmetadata')));
  video.currentTime = 14;
  await act(async () => {
    Object.defineProperty(video, 'error', { configurable: true, value: { code: 3 } });
    video.dispatchEvent(new Event('error'));
  });
  Object.defineProperty(video, 'error', { configurable: true, value: null });
  await act(async () => video.dispatchEvent(new Event('loadedmetadata')));
  expect(player.currentTrack?.id).toBe(first.id);
  expect(video.src).toBe('https://media.invalid/session.m4a');
  expect(video.currentTime).toBe(14);
  expect(player.mediaMode).toBe('audio');
  expect(player.status).toBe('playing');
});

it('never falls back from an explicitly selected audio mode to a video download', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  const first = nativeTrack();
  const second = track('next-after-audio');
  const video = view.container.querySelector('video')!;
  Object.defineProperty(video, 'readyState', { configurable: true, value: 1 });
  await act(async () => { player.setMediaMode('audio'); player.playTrack(first, [first, second]); });
  await act(async () => video.dispatchEvent(new Event('loadedmetadata')));
  await act(async () => {
    Object.defineProperty(video, 'error', { configurable: true, value: { code: 3 } });
    video.dispatchEvent(new Event('error'));
  });
  expect(player.currentTrack?.id).toBe(second.id);
  expect(video.hasAttribute('src')).toBe(false);
  expect(view.container.querySelector('audio')?.src).toBe('https://media.invalid/next-after-audio.m4a');
});

it('preserves a pause from native controls when navigating to another artist', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  const video = view.container.querySelector('video')!;
  Object.defineProperty(video, 'readyState', { configurable: true, value: 1 });
  await act(async () => { player.playTrack(nativeTrack()); });
  await act(async () => { video.dispatchEvent(new Event('loadedmetadata')); });
  expect(player.status).toBe('playing');
  await act(async () => { video.pause(); });
  expect(player.status).toBe('paused');
  const audio = view.container.querySelector('audio')!;
  const plays = jest.mocked(audio.play).mock.calls.length;
  await act(async () => { player.beginArtist('artist-after-native-pause'); player.loadArtist('artist-after-native-pause', [track('artist-after-native-pause')]); });
  expect(player.currentTrack?.id).toBe('artist-after-native-pause');
  expect(player.playbackRequested).toBe(false);
  expect(jest.mocked(audio.play).mock.calls.length).toBe(plays);
});

it('clears explicit pause only after native controls explicitly resume and then follows artist navigation', async () => {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  const video = view.container.querySelector('video')!;
  Object.defineProperty(video, 'readyState', { configurable: true, value: 1 });
  await act(async () => { player.playTrack(nativeTrack()); });
  await act(async () => { video.dispatchEvent(new Event('loadedmetadata')); });
  await act(async () => { player.pause(); });
  expect(player.status).toBe('paused');
  await act(async () => { fireEvent.pointerDown(video); await video.play(); });
  expect(player.status).toBe('playing');
  expect(player.playbackRequested).toBe(true);
  await act(async () => { player.beginArtist('artist-after-native-resume'); player.loadArtist('artist-after-native-resume', [track('artist-after-native-resume')]); });
  expect(player.currentTrack?.id).toBe('artist-after-native-resume');
  expect(player.status).toBe('playing');
});
async function mountNativeRateResetFixture() {
  const view = render(<PlayerProvider><Probe /></PlayerProvider>);
  const media = view.container.querySelector('video')!;
  let paused = true;
  let readiness = 0;
  let rate = 1;
  let defaultRate = 1;
  const changedRate = () => queueMicrotask(() => media.dispatchEvent(new Event('ratechange')));
  Object.defineProperty(media, 'paused', { configurable: true, get: () => paused });
  Object.defineProperty(media, 'readyState', { configurable: true, get: () => readiness });
  Object.defineProperty(media, 'playbackRate', { configurable: true, get: () => rate, set: (value: number) => {
    if (rate !== value) { rate = value; changedRate(); }
  } });
  Object.defineProperty(media, 'defaultPlaybackRate', { configurable: true, get: () => defaultRate, set: (value: number) => {
    if (defaultRate !== value) { defaultRate = value; changedRate(); }
  } });
  jest.mocked(media.pause).mockImplementation(() => {
    if (!paused) { paused = true; queueMicrotask(() => media.dispatchEvent(new Event('pause'))); }
  });
  jest.mocked(media.play).mockImplementation(async () => {
    paused = false;
    media.dispatchEvent(new Event('play'));
    media.dispatchEvent(new Event('playing'));
  });
  jest.mocked(media.load).mockImplementation(() => {
    readiness = 0;
    media.currentTime = 0;
    // Native load restores defaultPlaybackRate and queues ratechange when it
    // changes the effective rate. Do not model load as a no-op for this test.
    media.playbackRate = media.defaultPlaybackRate;
  });
  const sources = [...nativeTrack().sources, ...nativeTrack().sources.map(source => ({
    ...source, quality: 'low' as const, url: source.url.replace('session.', 'session-low.'),
  }))];
  const first = { ...nativeTrack(), id: 'rate-first', sources };
  const second = { ...first, id: 'rate-second', startMs: 20000, endMs: 30000 };
  const ready = async () => {
    readiness = 1;
    await act(async () => { media.dispatchEvent(new Event('loadedmetadata')); });
  };
  await act(async () => { player.playTrack(first, [first, second]); });
  await ready();
  expect(player.status).toBe('playing');
  return { media, ready };
}

it.each(['mode', 'quality', 'chapter'] as const)('preserves the chosen playback speed after native load during a %s transition', async transition => {
  const { media, ready } = await mountNativeRateResetFixture();
  await act(async () => { player.setPlaybackRate(1.5); });
  expect(media.playbackRate).toBe(1.5);
  expect(player.playbackRate).toBe(1.5);
  await act(async () => {
    if (transition === 'mode') player.setMediaMode('audio');
    else if (transition === 'quality') player.setQuality('low');
    else player.next();
  });
  await ready();
  expect(media.playbackRate).toBe(1.5);
  expect(player.playbackRate).toBe(1.5);
  expect(player.status).toBe('playing');
  if (transition === 'chapter') expect(player.currentTrack?.id).toBe('rate-second');
  if (transition === 'mode') expect(media.src).toBe('https://media.invalid/session.m4a');
  if (transition === 'quality') expect(media.src).toBe('https://media.invalid/session-low.mp4');
});

it('preserves a native-user speed selection through a replacement while keeping zero-speed commands invalid', async () => {
  const { media, ready } = await mountNativeRateResetFixture();
  await act(async () => { media.playbackRate = 0.75; });
  expect(player.playbackRate).toBe(0.75);
  act(() => { player.setPlaybackRate(0); });
  expect(player.playbackRate).toBe(0.75);
  expect(media.playbackRate).toBe(0.75);
  await act(async () => { player.setMediaMode('audio'); });
  await ready();
  expect(media.playbackRate).toBe(0.75);
  expect(player.playbackRate).toBe(0.75);
  expect(player.status).toBe('playing');
});

});

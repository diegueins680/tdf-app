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
  jest.mocked(authorizePlayerAsset).mockReset();
  jest.spyOn(HTMLMediaElement.prototype, 'currentSrc', 'get').mockImplementation(function (this: HTMLMediaElement) { return this.src; });
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
    expect(player.status).toBe('error');
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
    await act(async () => { jest.advanceTimersByTime(10000); });
    expect(jest.mocked(audio.load).mock.calls.length).toBe(before);
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
  expect(player.status).toBe('error');
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
  act(() => { audio.dispatchEvent(new Event('error')); });
  expect(player.status).toBe('error');
  await ready();
  expect(player.status).toBe('error');
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
